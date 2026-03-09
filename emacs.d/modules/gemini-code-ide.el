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

(defvar gemini-code-ide--original-cli-path nil
  "Saved value of `claude-code-ide-cli-path' before enabling Gemini mode.")

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

(defun gemini-code-ide--write-mcp-settings (working-dir session-id)
  "Write Gemini MCP settings for WORKING-DIR and SESSION-ID.
Returns settings path, or nil if MCP server config is unavailable."
  (when (claude-code-ide-mcp-server-ensure-server)
    (when-let* ((mcp-config (claude-code-ide-mcp-server-get-config session-id))
                (mcp-servers (alist-get 'mcpServers mcp-config))
                (emacs-tools (alist-get 'emacs-tools mcp-servers))
                (mcp-url (alist-get 'url emacs-tools)))
      (let* ((settings-path (gemini-code-ide--settings-path working-dir))
             (settings (or (gemini-code-ide--read-json-alist settings-path) '()))
             (existing-servers (or (alist-get 'mcpServers settings) '()))
             (server-entry `((httpUrl . ,mcp-url))))
        (setq existing-servers
              (gemini-code-ide--alist-set 'emacs-tools server-entry existing-servers))
        (setq settings
              (gemini-code-ide--alist-set 'mcpServers existing-servers settings))
        (make-directory (file-name-directory settings-path) t)
        (let ((json-encoding-pretty-print t))
          (with-temp-file settings-path
            (insert (json-encode settings))))
        settings-path))))

(defun gemini-code-ide--build-command (_continue _resume session-id)
  "Build Gemini CLI command for SESSION-ID."
  (let* ((working-dir (gemini-code-ide--working-directory))
         (cmd gemini-code-ide-cli-path))
    (when claude-code-ide-cli-debug
      (setq cmd (concat cmd " --debug")))
    (when-let ((settings-path (gemini-code-ide--write-mcp-settings working-dir session-id)))
      (setq cmd (concat cmd " --settings " (shell-quote-argument settings-path))))
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

(defun gemini-code-ide-current-selection ()
  "Return active file and selection/cursor context as plain text."
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
         (selection-tool-on (member gemini-code-ide-selection-tool-name
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
      (princ (format "selection tool registered: %s\n"
                     (if selection-tool-on "yes" "no"))))))

;;;###autoload
(defun gemini-code-ide-enable ()
  "Enable Gemini support in claude-code-ide."
  (interactive)
  (unless gemini-code-ide--original-cli-path
    (setq gemini-code-ide--original-cli-path claude-code-ide-cli-path))
  (setq claude-code-ide-cli-path gemini-code-ide-cli-path
        claude-code-ide-enable-mcp-server t)
  (unless (advice-member-p #'gemini-code-ide--build-command-around
                           'claude-code-ide--build-claude-command)
    (advice-add 'claude-code-ide--build-claude-command :around
                #'gemini-code-ide--build-command-around))
  (when gemini-code-ide-register-selection-tool
    (gemini-code-ide-register-selection-tool)))

;;;###autoload
(defun gemini-code-ide-disable ()
  "Disable Gemini support and restore previous CLI path."
  (interactive)
  (advice-remove 'claude-code-ide--build-claude-command
                 #'gemini-code-ide--build-command-around)
  (when gemini-code-ide--original-cli-path
    (setq claude-code-ide-cli-path gemini-code-ide--original-cli-path))
  (setq gemini-code-ide--original-cli-path nil))

(provide 'gemini-code-ide)
;;; gemini-code-ide.el ends here
