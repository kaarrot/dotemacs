;;; codex-code-ide.el --- Codex provider bridge for claude-code-ide -*- lexical-binding: t; -*-

;; This module keeps Codex-specific logic out of your main init:
;; - starts claude-code-ide sessions using Codex CLI (via codex-run by default)
;; - wires Emacs MCP server URL into Codex at runtime (no global config writes by default)
;; - optionally persists MCP server registration with `codex mcp add`
;; - registers a current-selection MCP tool for agent context

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'claude-code-ide)
(require 'claude-code-ide-emacs-tools)
(require 'claude-code-ide-mcp-server)

(defgroup codex-code-ide nil
  "Codex integration layer for claude-code-ide."
  :group 'tools
  :prefix "codex-code-ide-")

(defcustom codex-code-ide-wrapper-command "codex-run"
  "Command used to launch Codex from shell.
Defaults to your shell wrapper `codex-run`."
  :type 'string
  :group 'codex-code-ide)

(defcustom codex-code-ide-fallback-command "codex"
  "Fallback command when wrapper is unavailable."
  :type 'string
  :group 'codex-code-ide)

(defcustom codex-code-ide-shell "bash"
  "Shell used to invoke wrapper commands."
  :type 'string
  :group 'codex-code-ide)

(defcustom codex-code-ide-source-bashrc t
  "When non-nil, source `codex-code-ide-bashrc-file` before invoking Codex."
  :type 'boolean
  :group 'codex-code-ide)

(defcustom codex-code-ide-bashrc-file "~/.bashrc"
  "Bash rc file sourced before Codex command execution."
  :type 'string
  :group 'codex-code-ide)

(defcustom codex-code-ide-mcp-server-name "emacs_tools"
  "Codex MCP server name for Emacs tools."
  :type 'string
  :group 'codex-code-ide)

(defcustom codex-code-ide-runtime-mcp-override t
  "When non-nil, inject MCP URL at runtime using `-c mcp_servers....url=...`.
This is per-process and does not modify ~/.codex/config.toml."
  :type 'boolean
  :group 'codex-code-ide)

(defcustom codex-code-ide-persist-mcp-server nil
  "When non-nil, also run `codex mcp add` to persist server URL in config.toml."
  :type 'boolean
  :group 'codex-code-ide)

(defcustom codex-code-ide-require-persist-success nil
  "When non-nil, fail launch if persistent MCP registration fails."
  :type 'boolean
  :group 'codex-code-ide)

(defcustom codex-code-ide-disable-gemini-on-enable t
  "When non-nil, disable gemini-code-ide bridge before enabling Codex bridge."
  :type 'boolean
  :group 'codex-code-ide)

(defcustom codex-code-ide-register-selection-tool t
  "When non-nil, register current-selection MCP tool for Codex."
  :type 'boolean
  :group 'codex-code-ide)

(defcustom codex-code-ide-selection-tool-name "codex_code_ide_current_selection"
  "MCP tool name used to expose current selection context."
  :type 'string
  :group 'codex-code-ide)

(defvar codex-code-ide--enabled nil
  "Whether Codex bridge is currently enabled.")

(defvar codex-code-ide--original-cli-path nil
  "Saved value of `claude-code-ide-cli-path` before enabling Codex mode.")

(defvar codex-code-ide--launcher-cache nil
  "Cached launcher command name.")

(defvar codex-code-ide--last-mcp-url nil
  "Last MCP URL injected into Codex command line.")

(defun codex-code-ide--tool-name (tool-spec)
  "Extract normalized tool name from TOOL-SPEC."
  (cond
   ((and (listp tool-spec) (keywordp (car tool-spec)))
    (or (plist-get tool-spec :name)
        (when-let ((fn (plist-get tool-spec :function)))
          (symbol-name fn))))
   ((and (consp tool-spec) (symbolp (car tool-spec)))
    (symbol-name (car tool-spec)))
   (t nil)))

(defun codex-code-ide--working-directory ()
  "Best-effort project working directory."
  (condition-case _err
      (claude-code-ide--get-working-directory)
    (error default-directory)))

(defun codex-code-ide--shell-bootstrap ()
  "Shell snippet that prepares command environment."
  (if codex-code-ide-source-bashrc
      (let ((rc (expand-file-name codex-code-ide-bashrc-file)))
        (format "if [ -f %s ]; then . %s >/dev/null 2>&1; fi; "
                (shell-quote-argument rc)
                (shell-quote-argument rc)))
    ""))

(defun codex-code-ide--shell-join (argv)
  "Join ARGV into a safely quoted shell command string."
  (mapconcat #'shell-quote-argument argv " "))

(defun codex-code-ide--run-shell-script (script)
  "Run SCRIPT via `codex-code-ide-shell -lc` and capture output.
Returns plist: (:exit <code> :output <string>)."
  (with-temp-buffer
    (let ((exit-code (call-process codex-code-ide-shell nil (current-buffer) nil "-lc" script))
          (output nil))
      (setq output (buffer-string))
      (list :exit exit-code :output output))))

(defun codex-code-ide--run-launcher (argv)
  "Run launcher ARGV in configured shell and capture output."
  (codex-code-ide--run-shell-script
   (concat (codex-code-ide--shell-bootstrap)
           (codex-code-ide--shell-join argv))))

(defun codex-code-ide--command-available-p (command)
  "Return non-nil when COMMAND exists in configured shell context."
  (let* ((script (concat (codex-code-ide--shell-bootstrap)
                         "type " (shell-quote-argument command) " >/dev/null 2>&1"))
         (result (codex-code-ide--run-shell-script script)))
    (eq (plist-get result :exit) 0)))

(defun codex-code-ide--resolve-launcher ()
  "Resolve preferred Codex launcher command."
  (or codex-code-ide--launcher-cache
      (setq codex-code-ide--launcher-cache
            (cond
             ((and codex-code-ide-wrapper-command
                   (not (string-empty-p codex-code-ide-wrapper-command))
                   (codex-code-ide--command-available-p codex-code-ide-wrapper-command))
              codex-code-ide-wrapper-command)
             ((and codex-code-ide-fallback-command
                   (not (string-empty-p codex-code-ide-fallback-command))
                   (codex-code-ide--command-available-p codex-code-ide-fallback-command))
              codex-code-ide-fallback-command)
             (t nil)))))

(defun codex-code-ide--toml-quoted (value)
  "Return VALUE encoded as a TOML basic string literal."
  (concat "\""
          (replace-regexp-in-string "[\\\"]" "\\\\&" value)
          "\""))

(defun codex-code-ide--runtime-mcp-config-arg (mcp-url)
  "Build `-c` override string for MCP URL MCP-URL."
  (format "mcp_servers.%s.url=%s"
          codex-code-ide-mcp-server-name
          (codex-code-ide--toml-quoted mcp-url)))

(defun codex-code-ide--mcp-url (&optional session-id)
  "Extract current Emacs MCP URL.
When SESSION-ID is non-nil, include it in endpoint path."
  (when-let* ((mcp-config (claude-code-ide-mcp-server-get-config session-id))
              (mcp-servers (alist-get 'mcpServers mcp-config))
              (emacs-tools (alist-get 'emacs-tools mcp-servers)))
    (alist-get 'url emacs-tools)))

(defun codex-code-ide--persist-mcp-url (launcher mcp-url)
  "Persist MCP URL MCP-URL for LAUNCHER via `mcp add`.
Returns non-nil on success."
  (let* ((result (codex-code-ide--run-launcher
                  (list launcher "mcp" "add" codex-code-ide-mcp-server-name
                        "--url" mcp-url)))
         (exit-code (plist-get result :exit))
         (output (string-trim (or (plist-get result :output) ""))))
    (if (eq exit-code 0)
        t
      (message "codex-code-ide: failed to persist MCP url (%s)" output)
      nil)))

(defun codex-code-ide--extra-flag-args ()
  "Parse additional CLI flags from `claude-code-ide-cli-extra-flags`."
  (if (or (not claude-code-ide-cli-extra-flags)
          (string-empty-p claude-code-ide-cli-extra-flags))
      nil
    (condition-case err
        (split-string-shell-command claude-code-ide-cli-extra-flags)
      (error
       (message "codex-code-ide: invalid extra flags: %s" (error-message-string err))
       nil))))

(defun codex-code-ide--resume-args (continue resume)
  "Map CONTINUE/RESUME semantics onto Codex subcommand args."
  (cond
   (continue '("resume" "--last"))
   (resume '("resume"))
   (t nil)))

(defun codex-code-ide--build-argv (continue resume session-id)
  "Build Codex argv for CONTINUE, RESUME, and SESSION-ID."
  (let* ((launcher (or (codex-code-ide--resolve-launcher)
                       (user-error "Codex launcher not found. Tried `%s` and `%s`"
                                   codex-code-ide-wrapper-command
                                   codex-code-ide-fallback-command)))
         (working-dir (codex-code-ide--working-directory))
         (resume-args (codex-code-ide--resume-args continue resume))
         (extra-args (codex-code-ide--extra-flag-args))
         (mcp-url nil)
         (argv (list launcher "-C" working-dir)))
    (when (claude-code-ide-mcp-server-ensure-server)
      (setq mcp-url (codex-code-ide--mcp-url session-id)
            codex-code-ide--last-mcp-url mcp-url)
      (when (and mcp-url codex-code-ide-persist-mcp-server)
        (unless (codex-code-ide--persist-mcp-url launcher mcp-url)
          (when codex-code-ide-require-persist-success
            (user-error "Failed to persist Codex MCP server URL"))))
      (when (and mcp-url codex-code-ide-runtime-mcp-override)
        (setq argv (append argv (list "-c" (codex-code-ide--runtime-mcp-config-arg mcp-url))))))
    (setq argv (append argv extra-args resume-args))
    argv))

(defun codex-code-ide--wrap-command (argv)
  "Wrap ARGV for execution through configured shell."
  (let ((script (concat (codex-code-ide--shell-bootstrap)
                        "exec "
                        (codex-code-ide--shell-join argv))))
    (format "%s -lc %s"
            codex-code-ide-shell
            (shell-quote-argument script))))

(defun codex-code-ide--build-command (continue resume session-id)
  "Build full Codex command string for claude-code-ide backend."
  (codex-code-ide--wrap-command
   (codex-code-ide--build-argv continue resume session-id)))

(defun codex-code-ide--build-command-around (orig-fn continue resume session-id)
  "Around advice for `claude-code-ide--build-claude-command`."
  (if codex-code-ide--enabled
      (codex-code-ide--build-command continue resume session-id)
    (funcall orig-fn continue resume session-id)))

(defun codex-code-ide--ensure-cli-around (orig-fn)
  "Around advice for `claude-code-ide--ensure-cli`."
  (if (not codex-code-ide--enabled)
      (funcall orig-fn)
    (let ((launcher (codex-code-ide--resolve-launcher)))
      (unless launcher
        (user-error "Codex CLI not available. Expected `%s` (wrapper) or `%s` (binary)"
                    codex-code-ide-wrapper-command
                    codex-code-ide-fallback-command))
      (let* ((result (codex-code-ide--run-launcher (list launcher "--version")))
             (exit-code (plist-get result :exit))
             (output (string-trim (or (plist-get result :output) ""))))
        (if (eq exit-code 0)
            (progn
              (when (boundp 'claude-code-ide--cli-available)
                (setq claude-code-ide--cli-available t))
              t)
          (user-error "Codex launcher check failed: %s" output))))))

(defun codex-code-ide-current-selection ()
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

(defun codex-code-ide-register-selection-tool ()
  "Register current selection MCP tool for Codex."
  (interactive)
  (setq claude-code-ide-enable-mcp-server t)
  (unless (member codex-code-ide-selection-tool-name
                  (delq nil (mapcar #'codex-code-ide--tool-name
                                    claude-code-ide-mcp-server-tools)))
    (claude-code-ide-make-tool
     :function #'codex-code-ide-current-selection
     :name codex-code-ide-selection-tool-name
     :description "Return active file path and current selection (or cursor location) from Emacs."
     :args nil)))

(defun codex-code-ide--active-session-id ()
  "Return active claude-code-ide session ID for current project, if available."
  (when (and (boundp 'claude-code-ide--session-ids)
             (hash-table-p claude-code-ide--session-ids))
    (gethash (codex-code-ide--working-directory) claude-code-ide--session-ids)))

(defun codex-code-ide--persistent-config-url ()
  "Read persisted MCP URL from Codex configuration, if available."
  (when-let ((launcher (codex-code-ide--resolve-launcher)))
    (let* ((result (codex-code-ide--run-launcher
                    (list launcher "mcp" "get" codex-code-ide-mcp-server-name "--json")))
           (exit-code (plist-get result :exit))
           (output (or (plist-get result :output) "")))
      (when (eq exit-code 0)
        (condition-case _err
            (let* ((start (string-match "{" output))
                   (json-str (and start (substring output start)))
                   (json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (parsed (and json-str (json-read-from-string json-str)))
                   (transport (alist-get 'transport parsed)))
              (alist-get 'url transport))
          (error nil))))))

(defun codex-code-ide-status ()
  "Show current Codex bridge status for claude-code-ide."
  (interactive)
  (let* ((working-dir (codex-code-ide--working-directory))
         (launcher (codex-code-ide--resolve-launcher))
         (session-id (codex-code-ide--active-session-id))
         (live-mcp-url (or (and session-id (codex-code-ide--mcp-url session-id))
                           codex-code-ide--last-mcp-url))
         (persisted-url (codex-code-ide--persistent-config-url))
         (runtime-arg (and live-mcp-url (codex-code-ide--runtime-mcp-config-arg live-mcp-url)))
         (advice-build (advice-member-p #'codex-code-ide--build-command-around
                                        'claude-code-ide--build-claude-command))
         (advice-cli (advice-member-p #'codex-code-ide--ensure-cli-around
                                      'claude-code-ide--ensure-cli))
         (mcp-port (when (fboundp 'claude-code-ide-mcp-server-get-port)
                     (claude-code-ide-mcp-server-get-port)))
         (selection-tool-on (member codex-code-ide-selection-tool-name
                                    (delq nil (mapcar #'codex-code-ide--tool-name
                                                      claude-code-ide-mcp-server-tools)))))
    (with-help-window "*Codex Code IDE Status*"
      (princ (format "enabled: %s\n" (if codex-code-ide--enabled "yes" "no")))
      (princ (format "launcher: %s\n" (or launcher "<not-found>")))
      (princ (format "wrapper command: %s\n" codex-code-ide-wrapper-command))
      (princ (format "fallback command: %s\n" codex-code-ide-fallback-command))
      (princ (format "build advice active: %s\n" (if advice-build "yes" "no")))
      (princ (format "cli-check advice active: %s\n" (if advice-cli "yes" "no")))
      (princ (format "claude-code-ide-cli-path: %s\n" claude-code-ide-cli-path))
      (princ (format "working dir: %s\n" working-dir))
      (princ (format "active session id: %s\n" (or session-id "<none>")))
      (princ (format "mcp server enabled: %s\n"
                     (if claude-code-ide-enable-mcp-server "yes" "no")))
      (princ (format "mcp server port: %s\n"
                     (if mcp-port (number-to-string mcp-port) "not-running")))
      (princ (format "live mcp url: %s\n" (or live-mcp-url "<missing>")))
      (princ (format "runtime override enabled: %s\n"
                     (if codex-code-ide-runtime-mcp-override "yes" "no")))
      (princ (format "runtime override arg: %s\n" (or runtime-arg "<none>")))
      (princ (format "persist mcp server: %s\n"
                     (if codex-code-ide-persist-mcp-server "yes" "no")))
      (princ (format "persisted mcp url: %s\n" (or persisted-url "<none>")))
      (princ (format "selection tool registered: %s\n"
                     (if selection-tool-on "yes" "no"))))))

;;;###autoload
(defun codex-code-ide-enable ()
  "Enable Codex support in claude-code-ide."
  (interactive)
  (when (and codex-code-ide-disable-gemini-on-enable
             (fboundp 'gemini-code-ide-disable))
    (ignore-errors (gemini-code-ide-disable)))
  (unless codex-code-ide--original-cli-path
    (setq codex-code-ide--original-cli-path claude-code-ide-cli-path))
  (setq codex-code-ide--enabled t
        codex-code-ide--launcher-cache nil
        claude-code-ide-enable-mcp-server t
        ;; Keep value executable for internal call-process paths.
        claude-code-ide-cli-path codex-code-ide-shell)
  (unless (advice-member-p #'codex-code-ide--build-command-around
                           'claude-code-ide--build-claude-command)
    (advice-add 'claude-code-ide--build-claude-command :around
                #'codex-code-ide--build-command-around))
  (unless (advice-member-p #'codex-code-ide--ensure-cli-around
                           'claude-code-ide--ensure-cli)
    (advice-add 'claude-code-ide--ensure-cli :around
                #'codex-code-ide--ensure-cli-around))
  (when codex-code-ide-register-selection-tool
    (codex-code-ide-register-selection-tool)))

;;;###autoload
(defun codex-code-ide-disable ()
  "Disable Codex support and restore previous claude-code-ide settings."
  (interactive)
  (advice-remove 'claude-code-ide--build-claude-command
                 #'codex-code-ide--build-command-around)
  (advice-remove 'claude-code-ide--ensure-cli
                 #'codex-code-ide--ensure-cli-around)
  (setq codex-code-ide--enabled nil
        codex-code-ide--launcher-cache nil
        codex-code-ide--last-mcp-url nil)
  (when codex-code-ide--original-cli-path
    (setq claude-code-ide-cli-path codex-code-ide--original-cli-path))
  (setq codex-code-ide--original-cli-path nil))

(provide 'codex-code-ide)
;;; codex-code-ide.el ends here
