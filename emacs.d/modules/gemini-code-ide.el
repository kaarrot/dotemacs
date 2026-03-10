;;; gemini-code-ide.el --- Gemini provider bridge for claude-code-ide -*- lexical-binding: t; -*-

;; This module keeps Gemini-specific logic out of your main init:
;; - starts claude-code-ide sessions using Gemini CLI
;; - wires Emacs MCP server into Gemini settings.json
;; - registers a current-selection MCP tool

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'claude-code-ide)
(require 'claude-code-ide-emacs-tools)
(require 'claude-code-ide-mcp-server)

(defgroup gemini-code-ide nil
  "Gemini integration layer for claude-code-ide."
  :group 'tools
  :prefix "gemini-code-ide-")

(defcustom gemini-code-ide-cli-path "gemini"
  "Path to the Gemini CLI executable."
  :type 'string
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-settings-file nil
  "Gemini settings file used for MCP wiring.
When nil, use <project>/.gemini/settings.json."
  :type '(choice (const :tag "Use project .gemini/settings.json" nil)
                 (string :tag "Custom settings file path"))
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-register-selection-tool t
  "When non-nil, register current-selection MCP tool for Gemini."
  :type 'boolean
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-selection-tool-name "gemini_code_ide_current_selection"
  "MCP tool name used to expose current selection context."
  :type 'string
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-register-edit-tools t
  "When non-nil, register text-edit MCP tools for Gemini."
  :type 'boolean
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-replace-tool-name "gemini_code_ide_replace_text"
  "MCP tool name used for text replacement edits."
  :type 'string
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-save-after-tool-edits t
  "When non-nil, save buffers after successful tool-driven edits."
  :type 'boolean
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-disable-codex-on-enable t
  "When non-nil, disable codex-code-ide bridge before enabling Gemini bridge."
  :type 'boolean
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-mcp-startup-wait-timeout 2.0
  "Seconds to wait for MCP HTTP endpoint readiness before launching Gemini."
  :type 'number
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-mcp-startup-wait-interval 0.05
  "Polling interval in seconds while waiting for MCP HTTP endpoint readiness."
  :type 'number
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-disable-auto-update t
  "When non-nil, set `general.enableAutoUpdate` to false in Gemini settings.
This prevents detached self-update jobs from running during active IDE sessions."
  :type 'boolean
  :group 'gemini-code-ide)

(defcustom gemini-code-ide-disable-update-notifications t
  "When non-nil, set `general.enableAutoUpdateNotification` to false.
This removes update prompts/noise that are not useful in IDE-driven sessions."
  :type 'boolean
  :group 'gemini-code-ide)

(defvar gemini-code-ide--original-cli-path nil
  "Saved value of `claude-code-ide-cli-path' before enabling Gemini mode.")

(defvar gemini-code-ide--last-selection-snapshot nil
  "Last observed file selection snapshot for Gemini selection tool.")

(defun gemini-code-ide--alist-set (key value alist)
  "Set KEY to VALUE in ALIST and return ALIST."
  (let ((cell (assq key alist)))
    (if cell
        (setcdr cell value)
      (push (cons key value) alist))
    alist))

(defun gemini-code-ide--settings-path (working-dir)
  "Resolve Gemini settings path for WORKING-DIR."
  (let ((path (or gemini-code-ide-settings-file
                  ".gemini/settings.json")))
    (if (file-name-absolute-p path)
        path
      (expand-file-name path working-dir))))

(defun gemini-code-ide--default-settings-path (working-dir)
  "Return Gemini default project settings path for WORKING-DIR."
  (expand-file-name ".gemini/settings.json" working-dir))

(defun gemini-code-ide--target-settings-paths (working-dir)
  "Return settings paths to update for WORKING-DIR.
Always updates the default project settings file because newer Gemini CLI
loads MCP config from there and no longer supports `--settings`."
  (delete-dups
   (list (gemini-code-ide--settings-path working-dir)
         (gemini-code-ide--default-settings-path working-dir))))

(defun gemini-code-ide--working-directory ()
  "Best-effort working directory for current project context."
  (condition-case _err
      (claude-code-ide--get-working-directory)
    (error default-directory)))

(defun gemini-code-ide--read-json-alist (path)
  "Read JSON object from PATH as alist, or nil on parse error."
  (when (file-exists-p path)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (condition-case _err
          (json-read-file path)
        (error nil)))))

(defun gemini-code-ide--settings-mcp-url (settings-path)
  "Read Emacs MCP URL from SETTINGS-PATH.
Supports both `httpUrl` and `url` field names."
  (when-let* ((settings (gemini-code-ide--read-json-alist settings-path))
              (mcp-servers (alist-get 'mcpServers settings))
              (emacs-tools (alist-get 'emacs-tools mcp-servers)))
    (or (alist-get 'httpUrl emacs-tools)
        (alist-get 'url emacs-tools))))

(defun gemini-code-ide--normalize-mcp-url (mcp-url)
  "Normalize MCP-URL for local Gemini settings."
  (if (and (stringp mcp-url)
           (string-prefix-p "http://localhost:" mcp-url))
      (replace-regexp-in-string "\\`http://localhost:" "http://127.0.0.1:" mcp-url)
    mcp-url))

(defun gemini-code-ide--mcp-endpoint-host-port (mcp-url)
  "Extract host/port pair from MCP-URL."
  (when (and (stringp mcp-url)
             (string-match "\\`http://\\([^/:]+\\):\\([0-9]+\\)\\(?:/\\|\\'\\)" mcp-url))
    (list (match-string 1 mcp-url)
          (string-to-number (match-string 2 mcp-url)))))

(defun gemini-code-ide--mcp-endpoint-ready-p (mcp-url)
  "Return non-nil when MCP-URL endpoint accepts TCP connections."
  (when-let ((host-port (gemini-code-ide--mcp-endpoint-host-port mcp-url)))
    (let ((host (car host-port))
          (port (cadr host-port))
          (proc nil))
      (unwind-protect
          (condition-case _err
              (progn
                (setq proc (open-network-stream "gemini-code-ide-mcp-probe" nil host port))
                t)
            (error nil))
        (when (and (processp proc) (process-live-p proc))
          (delete-process proc))))))

(defun gemini-code-ide--wait-for-mcp-endpoint (mcp-url)
  "Wait for MCP-URL endpoint readiness up to configured timeout."
  (let ((deadline (+ (float-time) (max 0.0 gemini-code-ide-mcp-startup-wait-timeout)))
        (ready nil))
    (while (and (not ready) (< (float-time) deadline))
      (setq ready (gemini-code-ide--mcp-endpoint-ready-p mcp-url))
      (unless ready
        (sleep-for (max 0.01 gemini-code-ide-mcp-startup-wait-interval))))
    ready))

(defun gemini-code-ide--write-mcp-settings-file (settings-path mcp-url)
  "Write MCP URL MCP-URL into SETTINGS-PATH."
  (let* ((settings (or (gemini-code-ide--read-json-alist settings-path) '()))
         (existing-servers (or (alist-get 'mcpServers settings) '()))
         (general-settings (or (alist-get 'general settings) '()))
         ;; Keep both `url` (current) and `httpUrl` (legacy compatibility).
         (server-entry `((url . ,mcp-url)
                         (type . "http")
                         (httpUrl . ,mcp-url))))
    (setq existing-servers
          (gemini-code-ide--alist-set 'emacs-tools server-entry existing-servers))
    (setq settings
          (gemini-code-ide--alist-set 'mcpServers existing-servers settings))
    (when gemini-code-ide-disable-auto-update
      (setq general-settings
            (gemini-code-ide--alist-set 'enableAutoUpdate :json-false general-settings)))
    (when gemini-code-ide-disable-update-notifications
      (setq general-settings
            (gemini-code-ide--alist-set 'enableAutoUpdateNotification :json-false general-settings)))
    (when (or gemini-code-ide-disable-auto-update
              gemini-code-ide-disable-update-notifications)
      (setq settings
            (gemini-code-ide--alist-set 'general general-settings settings)))
    (make-directory (file-name-directory settings-path) t)
    (let ((json-encoding-pretty-print t))
      (with-temp-file settings-path
        (insert (json-encode settings))))
    settings-path))

(defun gemini-code-ide--write-mcp-settings (working-dir session-id)
  "Write Gemini MCP settings for WORKING-DIR and SESSION-ID.
Returns settings path, or nil if MCP server config is unavailable."
  (when (claude-code-ide-mcp-server-ensure-server)
    (when-let* ((mcp-config (claude-code-ide-mcp-server-get-config session-id))
                (mcp-servers (alist-get 'mcpServers mcp-config))
                (emacs-tools (alist-get 'emacs-tools mcp-servers))
                (mcp-url (gemini-code-ide--normalize-mcp-url (alist-get 'url emacs-tools))))
      (unless (gemini-code-ide--wait-for-mcp-endpoint mcp-url)
        (message "gemini-code-ide: MCP endpoint not ready after %.2fs (%s)"
                 gemini-code-ide-mcp-startup-wait-timeout
                 mcp-url))
      (let ((paths (gemini-code-ide--target-settings-paths working-dir))
            (primary nil))
        (dolist (settings-path paths)
          (unless primary
            (setq primary settings-path))
          (gemini-code-ide--write-mcp-settings-file settings-path mcp-url))
        primary))))

(defun gemini-code-ide--build-command (_continue _resume session-id)
  "Build Gemini CLI command for SESSION-ID."
  (let* ((working-dir (gemini-code-ide--working-directory))
         (cmd gemini-code-ide-cli-path))
    (when claude-code-ide-cli-debug
      (setq cmd (concat cmd " --debug")))
    (gemini-code-ide--write-mcp-settings working-dir session-id)
    (when (and claude-code-ide-cli-extra-flags
               (not (string-empty-p claude-code-ide-cli-extra-flags)))
      (setq cmd (concat cmd " " claude-code-ide-cli-extra-flags)))
    cmd))

(defun gemini-code-ide--build-command-around (orig-fn continue resume session-id)
  "Around advice for `claude-code-ide--build-claude-command'."
  (if (and gemini-code-ide--original-cli-path
           (string= claude-code-ide-cli-path gemini-code-ide-cli-path))
      (gemini-code-ide--build-command continue resume session-id)
    (funcall orig-fn continue resume session-id)))

(defun gemini-code-ide--session-context ()
  "Return current MCP session context, or nil when unavailable."
  (when (fboundp 'claude-code-ide-mcp-server-get-session-context)
    (ignore-errors (claude-code-ide-mcp-server-get-session-context))))

(defun gemini-code-ide--session-project-dir ()
  "Return project directory from current MCP session context, if any."
  (when-let ((context (gemini-code-ide--session-context)))
    (plist-get context :project-dir)))

(defun gemini-code-ide--file-in-project-p (file-path project-dir)
  "Return non-nil when FILE-PATH belongs to PROJECT-DIR, or when PROJECT-DIR is nil."
  (and file-path
       (or (not project-dir)
           (string-prefix-p (file-name-as-directory (expand-file-name project-dir))
                            (file-name-as-directory (expand-file-name file-path))))))

(defun gemini-code-ide--buffer-file-in-project-p (buffer project-dir)
  "Return non-nil when BUFFER visits a file in PROJECT-DIR."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (gemini-code-ide--file-in-project-p (buffer-file-name) project-dir))))

(defun gemini-code-ide--region-active-in-buffer-p (buffer)
  "Return non-nil when BUFFER has an active region."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (use-region-p))))

(defun gemini-code-ide--preferred-selection-buffer ()
  "Pick the best buffer to read selection context from."
  (let* ((context (gemini-code-ide--session-context))
         (project-dir (plist-get context :project-dir))
         (last-active-buffer (plist-get context :last-active-buffer))
         (visible-buffers (delete-dups (mapcar #'window-buffer (window-list nil 'no-minibuf)))))
    (or
     (and (gemini-code-ide--buffer-file-in-project-p last-active-buffer project-dir)
          last-active-buffer)
     (cl-find-if (lambda (buf)
                   (and (gemini-code-ide--buffer-file-in-project-p buf project-dir)
                        (gemini-code-ide--region-active-in-buffer-p buf)))
                 visible-buffers)
     (cl-find-if (lambda (buf)
                   (gemini-code-ide--buffer-file-in-project-p buf project-dir))
                 visible-buffers)
     (cl-find-if (lambda (buf)
                   (gemini-code-ide--buffer-file-in-project-p buf project-dir))
                 (buffer-list))
     (current-buffer))))

(defun gemini-code-ide--selection-snapshot ()
  "Capture selection snapshot from current buffer, or nil when no active region."
  (when (and (buffer-file-name) (use-region-p))
    (let* ((start (region-beginning))
           (end (region-end)))
      (list :file (buffer-file-name)
            :start-line (line-number-at-pos start)
            :end-line (line-number-at-pos end)
            :start-col (save-excursion (goto-char start) (current-column))
            :end-col (save-excursion (goto-char end) (current-column))
            :text (buffer-substring-no-properties start end)))))

(defun gemini-code-ide--track-selection-snapshot ()
  "Track most recent active selection in file-backed buffers."
  (when-let ((snapshot (gemini-code-ide--selection-snapshot)))
    (setq gemini-code-ide--last-selection-snapshot snapshot)))

(defun gemini-code-ide--format-selection-snapshot (snapshot)
  "Format SNAPSHOT plist as plain text selection context."
  (format (concat
           "file: %s\n"
           "selection.start: line %d col %d\n"
           "selection.end: line %d col %d\n"
           "selection.text:\n%s")
          (or (plist-get snapshot :file) "<no-file-buffer>")
          (or (plist-get snapshot :start-line) 0)
          (or (plist-get snapshot :start-col) 0)
          (or (plist-get snapshot :end-line) 0)
          (or (plist-get snapshot :end-col) 0)
          (or (plist-get snapshot :text) "")))

(defun gemini-code-ide--format-current-buffer-selection ()
  "Format current buffer selection or cursor context as plain text."
  (let* ((file-path (or (buffer-file-name) ""))
         (display-path (if (string-empty-p file-path) "<no-file-buffer>" file-path))
         (cursor-line (line-number-at-pos))
         (cursor-col (current-column)))
    (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (start-line (line-number-at-pos start))
               (end-line (line-number-at-pos end))
               (start-col (save-excursion (goto-char start) (current-column)))
               (end-col (save-excursion (goto-char end) (current-column)))
               (text (buffer-substring-no-properties start end)))
          (format (concat
                   "file: %s\n"
                   "selection.start: line %d col %d\n"
                   "selection.end: line %d col %d\n"
                   "selection.text:\n%s")
                  display-path start-line start-col end-line end-col text))
      (format (concat
               "file: %s\n"
               "cursor: line %d col %d\n"
               "selection: empty")
              display-path cursor-line cursor-col))))

(defun gemini-code-ide-current-selection ()
  "Return active file and selection/cursor context as plain text."
  (let* ((buffer (gemini-code-ide--preferred-selection-buffer))
         (context (gemini-code-ide--session-context))
         (project-dir (plist-get context :project-dir))
         (snapshot gemini-code-ide--last-selection-snapshot))
    (cond
     ((and (buffer-live-p buffer)
           (with-current-buffer buffer (buffer-file-name)))
      (with-current-buffer buffer
        (gemini-code-ide--format-current-buffer-selection)))
     ((and snapshot
           (gemini-code-ide--file-in-project-p (plist-get snapshot :file) project-dir))
      (gemini-code-ide--format-selection-snapshot snapshot))
     (t
      (gemini-code-ide--format-current-buffer-selection)))))

(defun gemini-code-ide--resolve-tool-file-path (file-path)
  "Resolve FILE-PATH for MCP edit tools.
Accepts absolute paths, project-relative paths, or nil."
  (let ((raw (and (stringp file-path) (string-trim file-path))))
    (if (and raw (not (string-empty-p raw)))
        (if (file-name-absolute-p raw)
            raw
          (expand-file-name raw (or (gemini-code-ide--session-project-dir)
                                     default-directory)))
      (or (when-let ((buf (gemini-code-ide--preferred-selection-buffer)))
            (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (buffer-file-name))))
          (user-error "file_path is required when no file buffer is active")))))

(defun gemini-code-ide-replace-text (file-path old-text new-text &optional replace-all)
  "Replace OLD-TEXT with NEW-TEXT in FILE-PATH.
When REPLACE-ALL is non-nil, replace all matches. Otherwise replace first match."
  (when (or (not (stringp old-text))
            (string-empty-p old-text))
    (user-error "old_text must be a non-empty string"))
  (unless (stringp new-text)
    (user-error "new_text must be a string"))
  (let* ((resolved-path (gemini-code-ide--resolve-tool-file-path file-path))
         (buffer (or (find-buffer-visiting resolved-path)
                     (find-file-noselect resolved-path)))
         (count 0))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (and (search-forward old-text nil t)
                    (or replace-all (= count 0)))
          (replace-match new-text t t)
          (setq count (1+ count))))
      (when (and (> count 0) gemini-code-ide-save-after-tool-edits)
        (save-buffer)))
    (if (> count 0)
        (format "Updated %s: replaced %d occurrence%s."
                resolved-path
                count
                (if (= count 1) "" "s"))
      (format "No changes made: '%s' not found in %s."
              old-text
              resolved-path))))

(defun gemini-code-ide--tool-name (tool-spec)
  "Extract normalized tool name from TOOL-SPEC."
  (cond
   ((and (listp tool-spec) (keywordp (car tool-spec)))
    (or (plist-get tool-spec :name)
        (when-let ((fn (plist-get tool-spec :function)))
          (symbol-name fn))))
   ((and (consp tool-spec) (symbolp (car tool-spec)))
    (symbol-name (car tool-spec)))
   (t nil)))

(defun gemini-code-ide-register-selection-tool ()
  "Register current selection MCP tool."
  (interactive)
  (setq claude-code-ide-enable-mcp-server t)
  (unless (member gemini-code-ide-selection-tool-name
                  (delq nil (mapcar #'gemini-code-ide--tool-name
                                    claude-code-ide-mcp-server-tools)))
    (claude-code-ide-make-tool
     :function #'gemini-code-ide-current-selection
     :name gemini-code-ide-selection-tool-name
     :description "Return active file path and current selection (or cursor location) from Emacs."
     :args nil)))

(defun gemini-code-ide-register-edit-tools ()
  "Register edit MCP tools for Gemini."
  (interactive)
  (setq claude-code-ide-enable-mcp-server t)
  (unless (member gemini-code-ide-replace-tool-name
                  (delq nil (mapcar #'gemini-code-ide--tool-name
                                    claude-code-ide-mcp-server-tools)))
    (claude-code-ide-make-tool
     :function #'gemini-code-ide-replace-text
     :name gemini-code-ide-replace-tool-name
     :description
     "Replace text in a file buffer without emacsclient. Use this for typo/spelling fixes and small targeted edits."
     :args '((:name "file_path"
                     :type string
                     :description "Absolute or project-relative file path. Optional if the active selection buffer has a file."
                     :optional t)
             (:name "old_text"
                     :type string
                     :description "Exact text to replace.")
             (:name "new_text"
                     :type string
                     :description "Replacement text.")
             (:name "replace_all"
                     :type boolean
                     :description "When true, replace every occurrence. Defaults to false."
                     :optional t)))))

(defun gemini-code-ide-status ()
  "Show current Gemini bridge status for claude-code-ide."
  (interactive)
  (let* ((working-dir (gemini-code-ide--working-directory))
         (settings-path (gemini-code-ide--settings-path working-dir))
         (settings-exists (file-exists-p settings-path))
         (settings-url (and settings-exists
                            (gemini-code-ide--settings-mcp-url settings-path)))
         (advice-on (advice-member-p #'gemini-code-ide--build-command-around
                                     'claude-code-ide--build-claude-command))
         (enabled (and advice-on
                       (string= claude-code-ide-cli-path gemini-code-ide-cli-path)))
         (mcp-port (when (fboundp 'claude-code-ide-mcp-server-get-port)
                     (claude-code-ide-mcp-server-get-port)))
         (endpoint-ready (and settings-url (gemini-code-ide--mcp-endpoint-ready-p settings-url)))
         (selection-tool-on (member gemini-code-ide-selection-tool-name
                                    (delq nil (mapcar #'gemini-code-ide--tool-name
                                                      claude-code-ide-mcp-server-tools))))
         (replace-tool-on (member gemini-code-ide-replace-tool-name
                                  (delq nil (mapcar #'gemini-code-ide--tool-name
                                                    claude-code-ide-mcp-server-tools)))))
    (with-help-window "*Gemini Code IDE Status*"
      (princ (format "enabled: %s\n" (if enabled "yes" "no")))
      (princ (format "cli path: %s\n" claude-code-ide-cli-path))
      (princ (format "expected gemini cli: %s\n" gemini-code-ide-cli-path))
      (princ (format "mcp server enabled: %s\n"
                     (if claude-code-ide-enable-mcp-server "yes" "no")))
      (princ (format "mcp server port: %s\n"
                     (if mcp-port (number-to-string mcp-port) "not-running")))
      (princ (format "working dir: %s\n" working-dir))
      (princ (format "settings path: %s\n" settings-path))
      (princ (format "settings file exists: %s\n" (if settings-exists "yes" "no")))
      (princ (format "settings mcp url: %s\n" (or settings-url "<missing>")))
      (princ (format "auto update disabled: %s\n"
                     (if gemini-code-ide-disable-auto-update "yes" "no")))
      (princ (format "update notifications disabled: %s\n"
                     (if gemini-code-ide-disable-update-notifications "yes" "no")))
      (princ (format "mcp endpoint ready: %s\n" (if endpoint-ready "yes" "no")))
      (princ (format "selection tool registered: %s\n"
                     (if selection-tool-on "yes" "no")))
      (princ (format "replace tool registered: %s\n"
                     (if replace-tool-on "yes" "no"))))))

;;;###autoload
(defun gemini-code-ide-enable ()
  "Enable Gemini support in claude-code-ide."
  (interactive)
  (when (and gemini-code-ide-disable-codex-on-enable
             (fboundp 'codex-code-ide-disable))
    (ignore-errors (codex-code-ide-disable)))
  (unless gemini-code-ide--original-cli-path
    (setq gemini-code-ide--original-cli-path claude-code-ide-cli-path))
  (setq claude-code-ide-cli-path gemini-code-ide-cli-path
        claude-code-ide-enable-mcp-server t)
  (unless (advice-member-p #'gemini-code-ide--build-command-around
                           'claude-code-ide--build-claude-command)
    (advice-add 'claude-code-ide--build-claude-command :around
                #'gemini-code-ide--build-command-around))
  (add-hook 'post-command-hook #'gemini-code-ide--track-selection-snapshot)
  (when gemini-code-ide-register-selection-tool
    (gemini-code-ide-register-selection-tool))
  (when gemini-code-ide-register-edit-tools
    (gemini-code-ide-register-edit-tools)))

;;;###autoload
(defun gemini-code-ide-disable ()
  "Disable Gemini support and restore previous CLI path."
  (interactive)
  (advice-remove 'claude-code-ide--build-claude-command
                 #'gemini-code-ide--build-command-around)
  (remove-hook 'post-command-hook #'gemini-code-ide--track-selection-snapshot)
  (when gemini-code-ide--original-cli-path
    (setq claude-code-ide-cli-path gemini-code-ide--original-cli-path))
  (setq gemini-code-ide--original-cli-path nil
        gemini-code-ide--last-selection-snapshot nil))

(provide 'gemini-code-ide)
;;; gemini-code-ide.el ends here
