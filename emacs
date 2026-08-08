;; Requires Emacs 26.2 or higher
(require 'package)

;; Set to nil for vendored/offline setups (no internet access at startup)
(defvar my-enable-package-install nil
  "When non-nil, automatically install missing packages from MELPA/ELPA.
Set to nil for offline/vendored Emacs setups.")

;; Disable melpa signiture check
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

;; run gc only when idle
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


(setq package-selected-packages '(
                  f           ; required by desktop+
				  flycheck
				  company
				  multiple-cursors
				  dumb-jump
				  yasnippet
				  avy
				  use-package
				  ;;dap-mode
				  which-key
                  go-mode
                  gomacro-mode
                  undo-fu
                  fzf
                  org-download
                  key-chord
                  rust-mode
                  eat
                  diff-hl
                  xclip
	  ))
    
;; Install all missing packages from package-selected-packages
(when my-enable-package-install
  (let ((missing-packages (seq-filter (lambda (pkg) (not (package-installed-p pkg)))
                                      package-selected-packages)))
    (when missing-packages
      (package-refresh-contents)
      (dolist (pkg missing-packages)
        (package-install pkg)))))

;; Emacs 27 compat: fido-vertical-mode ships in 28+; back-port via icomplete-vertical
(when (version< emacs-version "28")
  (add-to-list 'package-selected-packages 'icomplete-vertical)
  (when my-enable-package-install
    (unless (package-installed-p 'icomplete-vertical)
      (unless package-archive-contents (package-refresh-contents))
      (package-install 'icomplete-vertical)))
  (require 'icomplete-vertical nil t)
  (defvar fido-vertical-mode nil)
  (defun fido-vertical-mode (&optional arg)
    (interactive (list (or current-prefix-arg 'toggle)))
    (let ((enable (if (eq arg 'toggle)
                      (not fido-vertical-mode)
                    (> (prefix-numeric-value arg) 0))))
      (setq fido-vertical-mode enable)
      (fido-mode (if enable 1 -1))
      (when (fboundp 'icomplete-vertical-mode)
        (icomplete-vertical-mode (if enable 1 -1))))))

(when (>= emacs-major-version 30)
  ;; Refresh package metadata if claude-code-ide not installed yet
  (unless (package-installed-p 'claude-code-ide)
    (unless package-archive-contents
      (package-refresh-contents)))

  ;; Emacs 30+: Use built-in VC support
  (use-package claude-code-ide
    :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
              :rev "760240d7f03ff16f90ede9d4f4243cd94f3fed73")
    :bind ("C-c '" . claude-code-ide-menu)
    :config
    (setq claude-code-ide-terminal-backend 'eat)  ; Use eat instead of vterm

    (claude-code-ide-emacs-tools-setup)

    ;; Gemini bridge disabled for now.
    ;; When enabled, it uses project-local .gemini/settings.json for MCP wiring.
    ;;     general.enableAutoUpdate: false
    ;;     general.enableAutoUpdateNotification: false
    ;; (load (expand-file-name "~/.emacs.d/modules/gemini-code-ide.el") t t) ; disabled for now
    
    (load (expand-file-name "~/.emacs.d/modules/codex-code-ide.el") t t)
    (when (featurep 'codex-code-ide)
      (setq codex-code-ide-wrapper-command "codex-run"
            codex-code-ide-fallback-command "codex"
            codex-code-ide-runtime-mcp-override t
            codex-code-ide-mcp-client-startup-timeout-sec 45
            codex-code-ide-register-selection-tool t
            codex-code-ide-register-edit-tools t))
    (cond
     ((featurep 'codex-code-ide)
      (codex-code-ide-enable))
     ;; ((featurep 'gemini-code-ide)
     ;;  (gemini-code-ide-enable))
     )))

;; Only execute if Emacs is running in terminal mode (no GUI)
;; Copy via an OSC 52 escape sequence, which asks the terminal emulator itself
;; to set the clipboard.  Under Crostini the terminal is a host-side ChromeOS
;; app reached over vsh, so it is a client of neither the container's Wayland
;; compositor nor its Xwayland: wl-copy blocks forever waiting for keyboard
;; focus it never receives, and xclip writes into a headless X server that
;; nothing outside the container can read.  OSC 52 rides the terminal's own
;; byte stream, so it is the only path here that reaches the real clipboard --
;; and it keeps working over SSH and inside containers generally.
(defun my-enable-terminal-mouse ()
  "Enable mouse support for xterm-compatible terminal frames."
  (unless (or noninteractive (display-graphic-p))
    (require 'xt-mouse)
    (xterm-mouse-mode 1)
    (mouse-wheel-mode 1)))

(add-hook 'tty-setup-hook #'my-enable-terminal-mouse)

(defun my-osc52-copy (text)
  "Set the terminal's clipboard to TEXT using an OSC 52 escape sequence."
  (send-string-to-terminal
   (format "\e]52;c;%s\a"
           (base64-encode-string (encode-coding-string text 'utf-8) t))))

(unless (display-graphic-p)
  (my-enable-terminal-mouse)
  (setq select-enable-clipboard t)
  ;; Terminals accept OSC 52 writes but almost always refuse reads, since
  ;; letting a remote host siphon the clipboard is a security hole.  So there
  ;; is no way to pull the system clipboard back in: leave yanks to the kill
  ;; ring and use the terminal's own paste (Ctrl+Shift+V) for outside text.
  (setq interprogram-cut-function #'my-osc52-copy
        interprogram-paste-function nil))
    
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 3
    )


;;;;;;;;;;;;;;;;;;;;;;

(setq HOME (expand-file-name "~"))

(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/anything")
(load (message "%s/.emacs.d/modules/myfuncs.el" HOME ))
(load (message "%s/.emacs.d/modules/myfuncs_ediff.el" HOME ))

(if (version< emacs-version "29.1")
    (progn
      (install-eglot-from-github "1.9" (message "%s/dotemacs/emacs.d/elpa/" HOME))

      ;; Emulate project-root for Emacs 27.2 - requires by Eglot
      (unless (fboundp 'project-root)
        (defun project-root (project)
          (car (project-roots project))))
      (require 'eglot)
    )
)


;; Use local version of company-mode, multiple-cursors, cquery and dumbjump
;; The company mode completion is lacking in older versions.
(if (version< emacs-version "26.2")
    (progn 
      (add-to-list 'load-path (message "%s/.emacs.d/modules/legacy_emacs25/multiple-cursors" HOME))
      (add-to-list 'load-path (message "%s/.emacs.d/modules/legacy_emacs25/company-mode" HOME))
      (add-to-list 'load-path (message "%s/.emacs.d/modules/legacy_emacs25/cquery" HOME))
      (add-to-list 'load-path (message "%s/.emacs.d/modules/legacy_emacs25/" HOME))
      (require 'company)
      (require 'ace-jump-mode)
      )
  )


(require 'bm)
(require 'desktop+)  ;; custom tweaks to list in recent order
;; essential
(require 'ansi-color)
(require 'multiple-cursors)
(require 'dumb-jump)
(require 'tabbar)
;;(require 'anything-match-plugin)
;;(require 'anything-config)

;; 27.2 Emavs had to declare these before require key-chord 
(defvar read-key-full-map (make-sparse-keymap))
(defvar read-key-empty-map (make-sparse-keymap))
(require 'key-chord)

;; good to have
(require 'wgrep)
(require 'markdown-mode)
(require 'cmake-mode)
(require 'yasnippet)
(require 'clang-format) ;; assumes clang-format is on the PATH
(require 'go-mode)
(require 'diff-hl)
(global-diff-hl-mode)

(setq key-chord-typing-detection t)
;; If this is set to 2.0, you could press j, wait two full seconds,
;;   then press k, and Emacs would still trigger the chord command.
;; Comments:
;; - Setting this too high (like 2.0) will make regular typing frustrating because Emacs will constantly "wait"
;;   to see if you're about to complete a chord instead of just displaying the character you typed.
;; - If you press keys within this threshold, chords are suppressed.
;; - First key stroke after 2 secs is considered a key-chord
(setq key-chord-typing-speed-threshold 0.5)

;; It takes contrl immediately after you press any key.
;; Circumstance: If you press j (the start of a chord) and then get distracted,
;; Emacs waits for this duration before it  "forgets" that j was ever pressed as a potential chord starter.
;; Comments:
;; - It resets the interal state when you get back to typing after longer break
(setq key-chord-typing-reset-delay 2.0)

;; This is specifically for double-tap chords
;; This is the maximum window allowed between the first and second tap of the same key.
;; Comments:
;; - Longer delay work better on Termux with external keyboard
(setq key-chord-one-key-delay 0.5)

(key-chord-mode 1)


(dumb-jump-mode t)
(tabbar-mode)
(yas-global-mode 1)
(global-hl-line-mode -1)

(setq FIND_CMD "find")
(setq XARGS_CMD "xargs")
(setq ECHO_CMD "echo")
(if (eq system-type 'windows-nt)
    (progn
      (setq FIND_CMD "\"C:/cygwin64/bin/find.exe\"")
      (setq XARGS_CMD "\"C:/cygwin64/bin/xargs.exe\"")
      (setq ECHO_CMD "\"C:/cygwin64/bin/echo.exe\"")
      ;; NOTE: grep is available already on the path
      (grep-apply-setting 'grep-find-command '("C:/cygwin64/bin/find.exe . -type f -exec grep -nIH --null  \"\{\}\" \";\"" . 58))      
      )
)

; Use it in the grep and find commands
(setq my-root-directory default-directory)

; Disable check if all files are saved before running grep/find
(setq grep-save-buffers nil)

; Increase font size in all buffers
; (set-face-attribute 'default nil :height 120)
   
;;;;;;;;;;;;;;;;;;; escape color in eshell and compilation modes
(defun my-colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)
(add-hook 'eshell-preoutput-filter-functions #'ansi-color-apply)

;;;;;;;;;;;;;;;;;;; gpg
(require 'epa-file)
(epa-file-enable)
(setf epa-pinentry-mode 'loopback)  ;; enable command line password entry
(setq epa-file-cache-passphrase-for-symmetric-encryption t)  ;; no need to retype passphrase after each save


;;;;;;;;;;;;;;;;;;; aspell termux
;; In latest Termux apspell version has no linking issues
;; build gospell in ~/bin and rename it to aspell, so that ispell can find it
;;(if (search "termux" HOME)
;;    (progn
;;    ;; We need to setup aspell dictionary using origianl aspell binary or otherwise ispell setup will fail.
;;    ;; Once we have that we switch to gospell
;;    (require 'ispell)
;;    (ispell-find-aspell-dictionaries)
;;    (setq ispell-program-name (message "%s/bin/aspell" HOME))
;;    )
;;)

(if (search "termux" HOME)
(setq ispell-alternate-dictionary (message "%s/temp/ispell_words" HOME))
)


;;;;;;;;;;;;;;;;;;; Configuration


(setq python-shell-interpreter "python3")
(if (eq system-type 'windows-nt)
    (progn
    (setq hython_path "C:/Program Files/Side Effects Software/Houdini 18.5.675/bin/hython.exe")
    (if (file-exists-p hython_path)
        (setq python-shell-interpreter hython_path)
        (setq python-shell-interpreter (expand-file-name (message "%s/../../scoop/shims/python3.exe" HOME)))
      )
    (setq visible-bell 1) ;; disable bell sound on Windows
    )
  )

;; Disable bell on Termux
(setq ring-bell-function 'ignore)

(if (eq system-type 'linux)
    (setq-default shell-file-name "/bin/bash")
  )

(setq inhibit-splash-screen t)
(setq tramp-default-method "ssh")  ;; tramp
(setq require-final-newline nil)   ;; disable inserting a new line at the end of the file
(setq compilation-scroll-output t)

;; Disable default tab-indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq vc-follow-symlinks t)

(recentf-mode 1)

(if (not (search "arm" system-configuration)) ;
      (show-paren-mode 1)  ;; Disable on Arm as it slow things down significantly.
)

;; Disable blinking cursor
(blink-cursor-mode 0)

(setq x-select-enable-clipboard t)
(global-visual-line-mode t)
(setq use-file-dialog nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq mouse-buffer-menu-mode-mult 10)
(global-auto-revert-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(when (display-graphic-p)
  (tool-bar-mode -1)
)

;; While pasting to incremental search text gets converted to lower case
;; This results with both upper and lower case matches
(setq search-upper-case 'not-yanks)

;;;;;;;;;;;;;;;;;;;; Bookmarks - 'bm   (needs to be loade first)
;;(when (display-graphic-p)
  (setq bm-repository-file (message "%s/.emacs.d/bm-repository" HOME ))
  (setq bm-restore-repository-on-load t)
  (setq-default bm-buffer-persistence t)

  ;; Load bookmarks on file load
  (add-hook 'find-file-hooks '(lambda nil (bm-load-and-restore)))
  ;; Save bookmarks on emacs exit
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  ;; Update bookmark repository when saving the file.
  (add-hook 'after-save-hook '(lambda nil
                                (bm-buffer-save)
                                (bm-repository-save)
                                ))
  ;;)

;;;;;;;;;;;;;;;;;; Theme
(load-theme 'tango-dark t)

;;;;;;;;;;;;;;;;;;;;; Save minibuffer commands accross sessions
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring) savehist-file (message "%s/.emacs.d/savehist" HOME ))
(savehist-mode t)

;;;;;;;;;;;;;;;;;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

; Insert space in front to skip adding Ibuffer to buffer history and work with 'xx' navigation
(define-key my-keys-minor-mode-map (kbd "C-x C-b") (lambda () (interactive) (ibuffer nil " *Ibuffer*") (toggle-truncate-lines 1) (goto-char (point-min)) (isearch-forward)))

;;;;;;;;;;;;;;;;;;;; Isearch
(eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "RET") 'isearch-repeat-forward))
    
;;;;;;;;;;;;;;;;;;;; Tabbar
(define-key my-keys-minor-mode-map (kbd "M-c <left>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "M-c <right>") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "C-S-M-<left>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "C-S-M-<right>") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "M-c M-<right>") 'tabbar-move-current-tab-one-place-right)
(define-key my-keys-minor-mode-map (kbd "M-c M-<left>") 'tabbar-move-current-tab-one-place-left)
; Moving tabs with Page-Up/Down
(define-key my-keys-minor-mode-map (kbd "C-<prior>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "M-c M-,") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "C-<next>") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "M-c M-.") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "<C-M-prior>") 'tabbar-move-current-tab-one-place-left)  ; C-S-M-Page Up
(define-key my-keys-minor-mode-map (kbd "C-M-<next>") 'tabbar-move-current-tab-one-place-right) ; C-S-M-Page Down

(if (version< emacs-version "26.2")
    (progn 
    ;;;;;;;;;;;;;;;;;;;; ACE Jump
    (define-key my-keys-minor-mode-map (kbd "M-SPC") 'ace-jump-char-mode)
    (define-key my-keys-minor-mode-map (kbd "M-c SPC") 'ace-jump-char-mode)
    )
    (progn
    ;;;;;;;;;;;;;;;;;;;; Avy
    (define-key my-keys-minor-mode-map (kbd "ESC 1") 'avy-goto-char)
    (define-key my-keys-minor-mode-map (kbd "M-SPC") 'avy-goto-char)
    )
)

;;;;;;;;;;;;;;;;;;; Dumb Jump
(define-key my-keys-minor-mode-map (kbd "C-M-<down>") 'dumb-jump-go-other-window)
(define-key my-keys-minor-mode-map (kbd "M-<down>") 'dumb-jump-go-current-window)
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'dumb-jump-back)

;;;;;;;;;;;;;;;;;;; Cursor history
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "S-SPC") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "M-<left>") 'go-ring-back)
(define-key my-keys-minor-mode-map (kbd "M-<right>") 'go-ring-forward)

;; Sync xref jumps (M-. via eglot/LSP) to the custom cursor history ring.
;; xref saves position to its own xref--history before jumping, but go-ring-back
;; only reads global-mark-ring, so M-<left> couldn't navigate back after M-.
(advice-add 'xref-push-marker-stack :before
            (lambda (&rest _) (add-to-global-ring)))

;;;;;;;;;;;;;;;;;;;; Multiple cursors
(define-key my-keys-minor-mode-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-c m") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-c C-v") 'mc/mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "C-c C-<SPC>") 'mc/mark-all-in-region)
(define-key my-keys-minor-mode-map (kbd "C-c C-d") 'mc/keyboard-quit)

;;;;;;;;;;;;;;;;;;;; Bookmarks - 'bm
(define-key my-keys-minor-mode-map (kbd "C-q <up>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-q <right>") 'bm-next)
(define-key my-keys-minor-mode-map (kbd "C-q <left>") 'bm-previous)

;;;;;;;;;;;;;;;;;;;; Move between windows
(define-key my-keys-minor-mode-map (kbd "C-c <left>") 'windmove-left) ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "C-c C-<left>") 'windmove-left) ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "C-c C-b") 'windmove-left)    
(define-key my-keys-minor-mode-map (kbd "s-<left>") 'windmove-left) ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-c C-<right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-c C-f") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "s-<right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-c <up>") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-c C-<up>") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "s-<up>") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-c <down>") 'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-c C-<down>") 'windmove-down)
(define-key my-keys-minor-mode-map (kbd "s-<down>") 'windmove-down)

;;;;;;;;;;;;;;;;;;; Common
(define-key my-keys-minor-mode-map (kbd "<f1>") 'toggle-themes)
(define-key my-keys-minor-mode-map (kbd "C-c 1") 'toggle-themes)
(define-key my-keys-minor-mode-map (kbd "<f2>") 'grep-locations)
;(define-key my-keys-minor-mode-map (kbd "C-c 2") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "C-c 2") 'grep-locations)

(define-key my-keys-minor-mode-map (kbd "C-c <f2>") (lambda (search-phrase) (interactive "Msearch file:")
    (grep-find (message "%s . -name \"%s\" -print | %s -I %% %s %%:1:" FIND_CMD search-phrase XARGS_CMD ECHO_CMD))))

(define-key my-keys-minor-mode-map (kbd "ESC 2") 'grep-locations)

(define-key my-keys-minor-mode-map (kbd "C-c 3") (lambda (search-file) (interactive "MSearch file:")
    (find-locations search-file))
)

(define-key my-keys-minor-mode-map (kbd "ESC 3") (lambda (search-file) (interactive "MSearch file:")
    (find-locations search-file))
)

(define-key my-keys-minor-mode-map (kbd "<f4>") 'get-file-path)
(define-key my-keys-minor-mode-map (kbd "C-c 4") 'get-file-path)
(define-key my-keys-minor-mode-map(kbd "<f6>") 'whitespace-mode)
(define-key my-keys-minor-mode-map (kbd "C-c 6") 'whitespace-mode)
(define-key my-keys-minor-mode-map (kbd "<f7>")
  (lambda () (interactive) (if indent-tabs-mode (progn (setq indent-tabs-mode nil) (message "spaces")) (progn (setq indent-tabs-mode t) (python-indent-guess-indent-offset) (message "tabs")) )))
(define-key my-keys-minor-mode-map (kbd "<C-c 7>")
  (lambda () (interactive) (if indent-tabs-mode (progn (setq indent-tabs-mode nil) (message "spaces")) (progn (setq indent-tabs-mode t) (python-indent-guess-indent-offset) (message "tabs")) )))
(global-set-key (kbd "<f8>") 'ispell-word) ;; Flyspel
(global-set-key (kbd "C-c 8") 'ispell-word)
(global-set-key  [M-backspace] 'lazy-backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c 9") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c 0") 'electric-indent-mode)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'occur-methods)
(define-key my-keys-minor-mode-map (kbd "C-c t") (lambda () (interactive) (setq tab-width 4)))

(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "C-<SPC>") 'set-mark-command)

(define-key my-keys-minor-mode-map (kbd "C-;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-c ;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo-fu-only-undo)
 (define-key my-keys-minor-mode-map (kbd "M-z") 'undo-fu-only-redo)
(define-key my-keys-minor-mode-map (kbd "C-c C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c a") 'org-agenda)
(define-key my-keys-minor-mode-map "\C-l" 'goto-line)
(define-key my-keys-minor-mode-map (kbd "M-m") 'anything)


(define-key my-keys-minor-mode-map (kbd "s-<SPC>") 'goto-previous-point)
(define-key my-keys-minor-mode-map (kbd "C-c C-i") 'iimage-mode)
(define-key my-keys-minor-mode-map (kbd "C-c C-r") 'refresh-iimages)

;;;;;;;;;;;;;;;;;;; Save desktops
(define-key my-keys-minor-mode-map (kbd "C-M-p") 'desktop+-load)
(define-key my-keys-minor-mode-map (kbd "S-C-M-p") 'desktop+-create)
(define-key my-keys-minor-mode-map (kbd "C-x C-p") 'desktop+-create)

;;;;;;;;;;;;;;;;;;; Other
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-!") 'async-shell-command)
(define-key my-keys-minor-mode-map (kbd "<f10> c")
  (lambda ()  (interactive)  (occur-1 "{$\\|)$" 1 (list (current-buffer))) ) )
(define-key my-keys-minor-mode-map (kbd "<f10> p")
  (lambda ()  (interactive)  (occur-1 "def\\|class" 1 (list (current-buffer))) ))
(global-set-key "\C-x\C-b" (lambda ()  (interactive) (buffer-menu) (toggle-truncate-lines 1)))

(global-set-key (kbd "C-x SPC") 'set-mark-command)

;;;;;;;;;;;;;;;;;;; Occur mode customization
;; Make occur buffer open in the same window instead of splitting
(add-to-list 'display-buffer-alist
             '("\\*Occur\\*"
               (display-buffer-same-window)))

;; Customize occur mode keybindings
(defun my-occur-mode-keys ()
  "Customize occur mode keybindings."
  ;; RET goes to occurrence in the same window (replaces occur buffer)
  (local-set-key (kbd "RET")
                 (lambda ()
                   (interactive)
                   (let ((occur-window (selected-window))
                         (pos (occur-mode-find-occurrence)))
                     (pop-to-buffer (marker-buffer pos) '(display-buffer-same-window))
                     (goto-char pos))))
  ;; TAB goes to occurrence in a new split
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (occur-mode-goto-occurrence-other-window))))

(add-hook 'occur-mode-hook 'my-occur-mode-keys)

;; Open preview in a split each time cursor moves to new line
;(add-hook 'occur-mode-hook #'next-error-follow-minor-mode)

(global-set-key (kbd "C-x x")  (lambda () (interactive) (switch-to-buffer nil)))

;;;;;;;;;;;;;;;;;;; Diff gutter (diff-hk)
(define-key my-keys-minor-mode-map (kbd "C-c v n") 'diff-hl-next-hunk)
(define-key my-keys-minor-mode-map (kbd "C-c v p") 'diff-hl-previous-hunk)

;;;;;;;;;;;;;;;;;;; Winner mode
;; Unbind existing key sequence first

(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c ,") 'winner-undo)
(global-set-key (kbd "M-c .") 'winner-redo)

;;;;;;;;;;;;;;;;;;; End of Global bindings


;;;;;;;;;;;;;;;;;;; Key chords
(key-chord-define-global "ww" 'avy-goto-char)
(key-chord-define-global "kk" 'kill-buffer)

;(key-chord-define-global "aa" 'match-paren)
(key-chord-define-global "aa" (lambda ()
                                (interactive)
                                (set-mark (line-beginning-position))
                                (end-of-line)))


(key-chord-define-global "xx" (lambda () (interactive) (switch-to-buffer nil)))
(key-chord-define-global "vv"  (lambda () (interactive)
                                (ibuffer nil " *Ibuffer*") (toggle-truncate-lines 1) (goto-char (point-min)) (isearch-forward)))

;;(key-chord-define-global "xp" 'desktop+-load)
(key-chord-define-global "t1" 'tabbar-backward-tab)
(key-chord-define-global "t2" 'tabbar-forward-tab)
(key-chord-define-global "b1" 'previous-buffer)
(key-chord-define-global "b2" 'next-buffer)

;;(key-chord-define-global "11" 'shell-command)
(key-chord-define-global "22" 'grep-find)
(key-chord-define-global "33" (lambda (search-file) (interactive "MSearch file:")
    (find-locations search-file))
    )
(key-chord-define-global "44" 'get-file-path)
(key-chord-define-global "88" 'ispell-word)
(key-chord-define-global "99" 'toggle-truncate-lines)

(key-chord-define-global "zz" 'winner-undo)
(key-chord-define-global "\\\\" 'occur)

(key-chord-define-global "ss" 'ispell-word)
;;(key-chord-define-global "dd" 'flyspell-goto-next-error)

;;; Org
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks")
         "* TODO %?\n  %u")))

(key-chord-define-global "qq" (lambda () (interactive) (org-capture nil "t")))
(key-chord-define-global ".." 'org-timestamp-up)
(key-chord-define-global ",," 'org-timestamp-down)
(defun my/ii-org-time-stamp ()
  "Insert an active timestamp with time.
If point is on an existing timestamp, keep its date (and any repeater
or warning cookie) but replace the time portion with the current time."
  (interactive)
  (let ((ts (save-excursion
              (let ((bol (line-beginning-position)))
                (while (and (> (point) bol)
                            (not (memq (char-after) '(?< ?[))))
                  (backward-char)))
              (and (memq (char-after) '(?< ?[))
                   (org-element-timestamp-parser)))))
    (if (and ts (org-element-property :year-start ts))
        (let* ((year (org-element-property :year-start ts))
               (month (org-element-property :month-start ts))
               (day (org-element-property :day-start ts))
               (now (decode-time))
               (new-time (encode-time 0 (nth 1 now) (nth 2 now) day month year))
               (begin (org-element-property :begin ts))
               (end (- (org-element-property :end ts)
                       (or (org-element-property :post-blank ts) 0)))
               (cookie (my/org-timestamp-cookie-string ts)))
          (delete-region begin end)
          (goto-char begin)
          (org-insert-time-stamp new-time t nil)
          (when (and cookie (not (string-empty-p cookie)))
            (save-excursion
              (search-backward ">")
              (insert " " cookie))))
      (org-time-stamp '(16)))))

(defun my/org-timestamp-cookie-string (ts)
  "Return the repeater/warning cookie suffix for timestamp element TS."
  (let* ((rep-type (org-element-property :repeater-type ts))
         (rep-val (org-element-property :repeater-value ts))
         (rep-unit (org-element-property :repeater-unit ts))
         (warn-type (org-element-property :warning-type ts))
         (warn-val (org-element-property :warning-value ts))
         (warn-unit (org-element-property :warning-unit ts))
         (unit-char (lambda (u) (pcase u
                                  ('hour "h") ('day "d") ('week "w")
                                  ('month "m") ('year "y"))))
         (parts nil))
    (when (and rep-type rep-val rep-unit)
      (push (format "%s%d%s"
                    (pcase rep-type
                      ('cumulate "+") ('catch-up "++") ('restart ".+"))
                    rep-val (funcall unit-char rep-unit))
            parts))
    (when (and warn-type warn-val warn-unit)
      (push (format "%s%d%s"
                    (pcase warn-type ('all "-") ('first "--"))
                    warn-val (funcall unit-char warn-unit))
            parts))
    (mapconcat #'identity (nreverse parts) " ")))
(key-chord-define-global "ii" 'my/ii-org-time-stamp)

;(key-chord-define-global " f" 'project-find-file)
(key-chord-define-global " f" 'my/project-find-file-fido)
(key-chord-define-global " /" 'project-find-regexp)

(key-chord-define-global " e" 'end-of-buffer)


(define-minor-mode my-keys-minor-mode
"A minor mode so that my key settings override annoying major modes."
t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)


;;;;;;;;;;;;;;;;;;;; IBuffer 
; Replace *Buffer List* - for configurable filepaths 
; - Compact columns - use filapath and in memory buffers in the same column
; - Use relative paths where possible (file from the same file tree)
(define-ibuffer-column buffer-or-file
  (:name "Buffer/File")
  (if buffer-file-name
      (let* ((abs-path buffer-file-name)
             (rel-path (file-relative-name abs-path command-line-default-directory)))
        (if (and (not (string-prefix-p ".." rel-path))
                 (< (length rel-path) (length abs-path)))
            rel-path
          abs-path))
    (buffer-name)))

(setq ibuffer-formats
      '((mark modified read-only " "
              buffer-or-file)))


;;;;;;;;;;;;;;;;;;;; C-key-bindings
(defun c-mode-keys()
  (setq-default indent-tabs-mode nil)
  (c-set-style "user")  ; this seems to control indentations
  (setq tab-width 4)
  (local-set-key (kbd "C-c C-c") 'compile)
  ;(local-set-key (kbd "<f5>") 'gud-gdb)
  ;; toggle between .h and .cpp
  (local-set-key (kbd "C-c 4") 'ff-find-other-file)
  (local-set-key (kbd "<f4>") 'ff-find-other-file)

  ; compile_flags.txt (in root) - specify -I/path_to_include

  (local-set-key (kbd "S-<f5>") 'toggle-window-dedicated)
  (local-set-key (kbd "C-c <RET>") (lambda () (interactive)
    (setq compile-command (message "g++ -O0 -fsanitize=address -g -std=c++17 -I. %s -o a.out" (buffer-file-name))) )) 

  (local-set-key [pause] 'toggle-window-dedicated)
  (setq comment-start "//" comment-end "")
  (setq compilation-scroll-output 'first-error) ;; scroll compilation buffer
  (set-default 'truncate-lines nil)
  ;; aligment on the curly braces on new line
  (c-set-offset 'inline-open '0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset ' func-decl-cont 0)
  )

;; Changing style of comments in CC-mode
(add-hook 'c-mode-hook  (lambda () (setq comment-start "//" comment-end "") ))
(add-hook 'c++-mode-hook 'c-mode-keys)
(add-hook 'c-mode-hook 'c-mode-keys)

;;;;;;;;;;;;;;;;;;; Gdb
(defun gdb-mode-keys()
  (local-set-key (kbd "C-S-<up>") 'comint-previous-matching-input-from-input)
  (setq gdb-display-io-nopopup t)
  ;; (setq gdb-many-windows t)
  )
(add-hook 'gdb-mode-hook 'gdb-mode-keys)



;;;;;;;;;;;;;;;;;;; Rust

(defun my-rust-mode-keys ()
  (local-set-key
   (kbd "C-c <RET>")
   (lambda ()
     (interactive)
     (let* ((file (buffer-file-name))
            (exe (file-name-base file))
            (test-name (read-string "Test name (leave empty to run main): "))
            (cmd (if (string-empty-p test-name)
                     (format "rustc %s -o %s && ./%s" file exe exe)
                     (format "rustc --test %s -o %s && ./%s %s --nocapture" file exe exe test-name))))
       (shell-command cmd)))))

(add-hook 'rust-mode-hook #'my-rust-mode-keys)

;;;;;;;;;;;;;;;;;;; Python
(defun python-mode-keys()
  (python-indent-guess-indent-offset)
  (infer-indentation-style)

  (local-set-key (kbd "C->") 'python-indent-shift-right)
  (local-set-key (kbd "C-<") 'python-indent-shift-left)
  (local-set-key (kbd "C-c C-c") 'python-shell-send-buffer)

  (local-set-key (kbd "C-c <RET>") 'selectback-exec)
  (local-set-key (kbd "S-C-c <RET>") 'selectback)

  (setq tab-width 4)
  )

(add-hook 'python-mode-hook 'python-mode-keys)

;; Disable auto indent on new line in text-mode
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'paragraph-indent-minor-mode)

;; Disable auto indent on new line in fundamental-mode and new buffers
(add-hook 'fundamental-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)))


;;;;;;;;;;;;;;;;;;; Gomacro
(defun gomacro-mode-keys()

  (local-set-key (kbd "C-c C-c") 'gomacro-eval-region)

  ;; Main binding to evaluate region
  (local-set-key (kbd "C-c <RET>") (lambda () (interactive)
    (destructuring-bind (start end)  ;; extract list entires returned from (selectback)
      (selectback)
      (gomacro-eval-region start end)
      (keyboard-quit)
    )
  ))

  (local-set-key (kbd "C-c p") 'gomacro-run)
  (local-set-key (kbd "C-c C-p") 'gomacro-run)

  (local-set-key (kbd "S-C-c <RET>") 'selectback)

  (local-set-key (kbd "C-c l") 'bm-toggle) ;; since gomacro-mode overrides C-c C-l add new binding for bookmark toggle
  )

(add-hook 'gomacro-mode-hook 'gomacro-mode-keys)


;;;;;;;;;;;;;;;;;;;; Partial completion to shell and python-inferior mode
(defun shell-mode-keys()
  (local-set-key (kbd "C-S-<up>") 'comint-previous-matching-input-from-input)
  (local-set-key (kbd "C-S-<down>") 'comint-next-matching-input-from-input)

  ;; fix cd when using aliased commands
  (track-shell-directory/procfs)

  (when (fboundp 'python-shell-completion-native-turn-on)
              (python-shell-completion-native-turn-on))
  ;; Fallback option to mute the warning
  (setq python-shell-completion-native-disabled-interpreters '("python"))

  )

(add-hook 'shell-mode-hook 'shell-mode-keys)
(if (eq system-type 'linux) ;; TODO: test (again) if this works on linux
    (add-hook 'inferior-python-mode-hook 'shell-mode-keys)
  )

;;;;;;;;;;;;;;;;;;;; Golang
(defun my-go-mode-hook ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           (message "go run %s" (buffer-file-name))))
  ; Godef jump key binding
  ; go get github.com/rogpeppe/godef
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-c") 'compile)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)


;;;;;;;;;;;;;;;;;;;; Dired

;; auto-hide details in dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; https://stackoverflow.com/a/2650987
(defmacro disallow-cd-in-function (fun)
  "Prevent FUN (or any function that FUN calls) from changing directory."
  `(defadvice ,fun (around dissallow-cd activate)
     (let ((old-dir default-directory) ; Save old directory
           (new-buf ad-do-it)) ; Capture new buffer
       ;; If FUN returns a buffer, operate in that buffer in addition
       ;; to current one.
       (when (bufferp new-buf)
         (set-buffer new-buf)
         (setq default-directory old-dir))
       ;; Set default-directory in the current buffer
       (setq default-directory old-dir))))

;; The purpose of this functions is maintain location emacs was opened as the root directory
;; so that grep and find could scan entire project. This however impacts ergonomics when
;; attempting to open a file within the same directory
;; Let's disable it for now and pass project root location to grep/find
;(disallow-cd-in-function dired-find-file)
;(disallow-cd-in-function find-file-noselect-1)
;(disallow-cd-in-function set-visited-file-name)

(setq Buffer-menu-name-width 40)
(eval-after-load 'dired '(progn (require 'single-dired)))
(defun dired-mode-keys()
  (setq Buffer-menu-name-width 40)
  (local-set-key (kbd "C-w") 'wdired-change-to-wdired-mode )
  (local-set-key (kbd "C-k") 'kill-dired-buffers)
  ;; (set-default 'truncate-lines nil)
 )
(add-hook 'dired-mode-hook 'dired-mode-keys)

;;;;;;;;;;;;;;;;;;;; ORG

(add-to-list 'auto-mode-alist '(".notes" . org-mode))
(load (message "%s/.emacs.d/modules/base64image.el" HOME))  ;; support for base64 images

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

;; Expand all headings when jumped agenda view
(defun my-org-show-context-level-2 ()
  "Show context and ensure levels 1-2 are visible, preserving deeper levels."
  (org-show-context 'agenda)

  ;; Explicitly show all immediate children of the specific task we jumped to
  ;; It expects to keep 'ancestors' settings 
  (if (fboundp 'org-fold-show-children)
      (org-fold-show-children)
    (org-show-children))

  (save-excursion
    (goto-char (point-min))
    (while (outline-next-heading)
      (when (= (org-outline-level) 1)
        (if (fboundp 'org-fold-show-children)
            (org-fold-show-children)
          (org-show-children))))))

(if (boundp 'org-fold-show-context-detail)
    (setq org-fold-show-context-detail
          '((agenda . ancestors)
           (isearch . tree)
           (default . ancestors)))
  (setq org-show-context-detail
        '((agenda . ancestors)
         (isearch . tree)
         (default . ancestors))))

;; Prevent accidental deletion of hidden/folded content
(setq org-catch-invisible-edits 'error)

(setq org-agenda-window-setup 'only-window)

;; Persistent clock-in/out history across sessions.
;;
;; Org's own history is stored as raw character offsets: `org-clock-save' writes
;; (FILE . POSITION) pairs and `org-clock-load' rebuilds markers from them.  Any
;; text inserted earlier in the file shifts every later offset, so entries deep
;; in a big notes file drift onto a neighbouring heading, or off the front into
;; the preamble where `org-get-heading' signals "before first heading" and the
;; task silently disappears from the selector.  `global-auto-revert-mode' makes
;; it worse: reverting relocates live markers, and the next save persists the
;; collapsed positions as truth.
;;
;; So keep our own recency list keyed by what a heading *is* (id / outline path /
;; heading text) rather than where it currently sits, and leave org responsible
;; only for resuming a running clock.
(setq org-clock-history-length 20)
(setq org-clock-persist 'clock)         ;; resume only -- history is ours, below
(org-clock-persistence-insinuate)
(add-hook 'org-clock-out-hook #'org-clock-save)

(defvar my-org-clock-history-file
  (expand-file-name "my-org-clock-history.el" user-emacs-directory)
  "File `my-org-clock-history' is persisted to.  Data only, never loaded as code.")

(defvar my-org-clock-history-length 20
  "How many recently clocked tasks to remember.")

(defvar my-org-clock-history nil
  "Recently clocked tasks, most recent first.
Each entry is a plist of :file :id :olp :heading :pos :time.  The position is
only a hint for disambiguating identical headings -- entries are resolved by
id, outline path or heading text, so they survive edits elsewhere in the file.")

(defun my-org-clock-history--entry-at-point ()
  "Return a history entry plist for the Org heading at point, or nil."
  (let ((file (buffer-file-name (org-base-buffer (current-buffer)))))
    (when (and file (derived-mode-p 'org-mode))
      (org-with-wide-buffer
       (when (ignore-errors (org-back-to-heading t) t)
         (list :file (file-truename file)
               :id (org-id-get)
               :olp (org-get-outline-path t)
               :heading (org-get-heading t t t t)
               :pos (point)
               :time (format-time-string "%Y-%m-%d %a %H:%M")))))))

(defun my-org-clock-history--key (entry)
  "Identity of ENTRY for de-duplication."
  (or (plist-get entry :id)
      (cons (plist-get entry :file) (plist-get entry :olp))))

(defun my-org-clock-history--read ()
  "Return the entry list stored in `my-org-clock-history-file'.
A missing or corrupt file just reads as empty."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents my-org-clock-history-file)
        (goto-char (point-min))
        (let ((data (read (current-buffer)))) ;; data, never `load-file'
          (and (listp data) data)))
    (file-missing nil)
    (error (message "my-org-clock-history: unreadable, ignoring: %s"
                    (error-message-string err))
           nil)))

(defun my-org-clock-history--merge (a b)
  "Union of entry lists A and B: newest :time first, de-duplicated by identity.
Ties keep A's order, so the entry just clocked in stays at the front."
  (cond
   ((null b) a)
   ((null a) b)
   (t (let (out seen)
        (dolist (e (sort (append a b)
                         (lambda (x y) (string> (or (plist-get x :time) "")
                                                (or (plist-get y :time) "")))))
          (let ((key (my-org-clock-history--key e)))
            (unless (member key seen)
              (push key seen)
              (push e out))))
        (nreverse out)))))

(defun my-org-clock-history--write (entries)
  "Write ENTRIES to `my-org-clock-history-file', replacing its contents."
  (condition-case err
      (with-temp-file my-org-clock-history-file
        (insert ";; -*- lisp-data -*-\n"
                ";; Recently clocked org tasks; written by `my-org-clock-history-save'.\n")
        (let ((print-length nil) (print-level nil))
          (prin1 entries (current-buffer)))
        (insert "\n"))
    (error (message "my-org-clock-history: save failed: %s"
                    (error-message-string err)))))

(defun my-org-clock-history-save ()
  "Persist `my-org-clock-history', merging with whatever is already on disk.
Merging means a second Emacs instance cannot clobber this one's entries, and an
empty in-memory list never wipes a good file -- that wipe is exactly what used
to poison org's own history file."
  (let ((merged (seq-take (my-org-clock-history--merge
                           my-org-clock-history (my-org-clock-history--read))
                          my-org-clock-history-length)))
    (when merged
      (setq my-org-clock-history merged)
      (my-org-clock-history--write merged))))

(defun my-org-clock-history-load ()
  "Read `my-org-clock-history' back from `my-org-clock-history-file'."
  (setq my-org-clock-history (my-org-clock-history--read)))

(defun my-org-clock-history-push (entry)
  "Move ENTRY to the front of `my-org-clock-history' and persist the list."
  (when entry
    (let ((key (my-org-clock-history--key entry)))
      (setq my-org-clock-history
            (cons entry
                  (seq-remove (lambda (e)
                                (equal key (my-org-clock-history--key e)))
                              my-org-clock-history))))
    (when (> (length my-org-clock-history) my-org-clock-history-length)
      (setq my-org-clock-history
            (seq-take my-org-clock-history my-org-clock-history-length)))
    (my-org-clock-history-save)))

(defun my-org-clock-history-record ()
  "Remember the task just clocked in.  For `org-clock-in-hook'."
  (let ((m org-clock-hd-marker))
    (when (and (markerp m) (marker-buffer m))
      (with-current-buffer (org-base-buffer (marker-buffer m))
        (org-with-wide-buffer
         (goto-char m)
         (my-org-clock-history-push (my-org-clock-history--entry-at-point)))))))

(defun my-org-clock-history-add-current ()
  "Remember the heading at point as a recent task, without clocking in.
Useful for seeding the selector with tasks you return to often."
  (interactive)
  (let ((entry (my-org-clock-history--entry-at-point)))
    (unless entry (user-error "Not on an Org heading in a file"))
    (my-org-clock-history-push entry)
    (message "Added to clock history: %s" (plist-get entry :heading))))

(defun my-org-clock-history--heading-positions (heading)
  "Return the positions of all headings in this buffer whose text is HEADING.
Matches like `org-find-exact-headline-in-buffer', i.e. tolerating a TODO
keyword, a priority cookie and tags around HEADING."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((re (format org-complex-heading-regexp-format (regexp-quote heading)))
         (case-fold-search nil)
         hits)
     (while (re-search-forward re nil t)
       (push (match-beginning 0) hits)
       (goto-char (match-end 0)))
     (nreverse hits))))

(defun my-org-clock-history--nearest (positions pos)
  "Return the element of POSITIONS closest to POS, or nil if POSITIONS is empty."
  (car (sort (copy-sequence positions)
             (lambda (a b) (< (abs (- a pos)) (abs (- b pos)))))))

(defun my-org-clock-history-resolve (entry)
  "Return a fresh marker on the heading ENTRY refers to, or nil if it is gone.
Tries the stored id, then the outline path, then the heading text.  Never falls
back to the stored position on its own: clocking into whatever now sits at an
old offset is worse than failing, because it silently mis-records time."
  (let ((id (plist-get entry :id))
        (file (plist-get entry :file))
        (heading (plist-get entry :heading))
        (olp (plist-get entry :olp))
        (pos (or (plist-get entry :pos) 1)))
    (or (and id (ignore-errors (org-id-find id 'marker)))
        (and file heading (file-readable-p file)
             (let ((buf (ignore-errors (find-file-noselect file t))))
               (when (buffer-live-p buf)
                 (with-current-buffer (org-base-buffer buf)
                   (when (derived-mode-p 'org-mode)
                     (let* ((hits (my-org-clock-history--heading-positions heading))
                            ;; `org-find-olp' would do this, but it errors out on
                            ;; non-unique headings -- and duplicated heading text
                            ;; is normal in these notes.  Prefer the hit whose
                            ;; whole outline path matches, then the nearest one.
                            (same-olp
                             (seq-filter
                              (lambda (p)
                                (equal olp (org-with-wide-buffer
                                            (goto-char p)
                                            (org-get-outline-path t))))
                              hits))
                            (best (my-org-clock-history--nearest
                                   (or same-olp hits) pos)))
                       (when best
                         (org-with-wide-buffer
                          (goto-char best)
                          (point-marker))))))))))))

(defun my-org-clock-history-prune ()
  "Drop history entries whose heading can no longer be found."
  (interactive)
  (let* ((before (length my-org-clock-history))
         (kept (seq-filter (lambda (e)
                             (let ((m (my-org-clock-history-resolve e)))
                               (when m (set-marker m nil) t)))
                           my-org-clock-history)))
    (setq my-org-clock-history kept)
    ;; Direct write: a merging save would read the dropped entries straight back.
    (my-org-clock-history--write kept)
    (message "Clock history: dropped %d, kept %d" (- before (length kept))
             (length kept))))

(defun my-org-clock-history--label (entry multi-file)
  "Return the completion label for ENTRY.
Appends the parent outline path (and the file name when MULTI-FILE) so that
repeated heading text stays distinguishable."
  (let* ((context (append (when multi-file
                            (list (file-name-nondirectory (plist-get entry :file))))
                          (butlast (plist-get entry :olp)))))
    (concat (plist-get entry :heading)
            (when context
              (concat "  " (propertize (mapconcat #'identity context "/")
                                       'face 'shadow))))))

(defun my-org-clock-history--unique-label (label pairs)
  "Return LABEL, suffixed if needed so it does not collide within PAIRS."
  (if (not (assoc label pairs))
      label
    (let ((n 2))
      (while (assoc (format "%s <%d>" label n) pairs) (setq n (1+ n)))
      (format "%s <%d>" label n))))

(defun my-org-clock-select-task (&optional prompt)
  "Like `org-clock-select-task' but uses completing-read for incremental filtering.
Replaces the single-key selection buffer with a searchable minibuffer.
Called by `org-clock-in' when invoked with a universal prefix (C-u C-c C-x C-i).

Candidates come from `my-org-clock-history', built from the stored heading text,
so the list needs no open buffers and cannot come back empty just because a
buffer was killed.  Only the chosen entry is resolved to a marker."
  (let ((multi-file (> (length (seq-uniq (mapcar (lambda (e) (plist-get e :file))
                                                 my-org-clock-history)))
                       1))
        pairs)
    (dolist (spec `(("[default] "    . ,org-clock-default-task)
                    ("[interrupted] " . ,org-clock-interrupted-task)
                    ,@(when (org-clocking-p)
                        `(("[current] " . ,org-clock-marker)))))
      (let ((prefix (car spec)) (m (cdr spec)))
        (when (and m (marker-buffer m))
          (with-current-buffer (org-base-buffer (marker-buffer m))
            (org-with-wide-buffer
             (ignore-errors
               (goto-char m)
               (push (cons (concat prefix (org-get-heading 'notags)) m) pairs)))))))
    (dolist (entry my-org-clock-history)
      (let ((label (my-org-clock-history--unique-label
                    (my-org-clock-history--label entry multi-file) pairs)))
        (push (cons label entry) pairs)))
    (setq pairs (nreverse pairs))
    (unless pairs (user-error "No recent clock"))
    (let* ((fido-vertical-was-active fido-vertical-mode)
           (completion-extra-properties
            '(:display-sort-function identity :cycle-sort-function identity))
           chosen)
      (unless fido-vertical-was-active (fido-vertical-mode 1))
      (unwind-protect
          (setq chosen (completing-read
                        (or prompt "Clock in on task: ")
                        (mapcar #'car pairs)
                        nil t))
        (unless fido-vertical-was-active (fido-vertical-mode -1)))
      (let ((sel (cdr (assoc chosen pairs))))
        (cond
         ;; [default]/[interrupted]/[current]: copy, because `org-clock-in'
         ;; clears the marker it is handed.
         ((markerp sel) (copy-marker sel))
         (sel (or (my-org-clock-history-resolve sel)
                  (user-error "Heading no longer found: %s (in %s) -- M-x my-org-clock-history-prune"
                              (plist-get sel :heading)
                              (file-name-nondirectory (plist-get sel :file))))))))))

(add-hook 'org-clock-in-hook #'my-org-clock-history-record)
(add-hook 'kill-emacs-hook #'my-org-clock-history-save)
(my-org-clock-history-load)

;; Selector relies on fido-vertical-mode (shimmed for Emacs 27; see top of file).
(with-eval-after-load 'org-clock
  (advice-add 'org-clock-select-task :override #'my-org-clock-select-task))

;; `org-clock-load' honours `org-clock-persist' only when *saving*: it pushes
;; `org-clock-stored-history' unconditionally, so an old persist file (any host
;; not yet updated, or one written before this change) would still inject
;; offset-based markers.  Drop them -- the recency list above is the only source.
(defun my-org-clock-drop-restored-history (&rest _)
  "Forget the offset-based markers `org-clock-load' restores from old files."
  (setq org-clock-history nil))
(with-eval-after-load 'org-clock
  (advice-add 'org-clock-load :after #'my-org-clock-drop-restored-history))

;; `org-clock-history' now starts out empty each session (org only persists the
;; running clock), so plain C-c C-x C-j would error with "No active or recent
;; clock task".  Offer the recency list instead.
(defun my-org-clock-goto-fallback (orig &optional select)
  "Fall back to the task selector when org has no in-session clock history."
  (if (or select (org-clocking-p) org-clock-history)
      (funcall orig select)
    (funcall orig '(4))))
(with-eval-after-load 'org-clock
  (advice-add 'org-clock-goto :around #'my-org-clock-goto-fallback))

;; following up the task unfolds the heading
(add-hook 'org-agenda-after-show-hook 'my-org-show-context-level-2)


(setq org-agenda-custom-commands
      '(
        (" a" "ACTIVE tasks" todo "ACTIVE")
        (" n" "NEXT tasks" todo "NEXT")
        (" w" "WAITING tasks" todo "WAITING")
        (" r" "READ tasks" todo "READ")
        (" t" "TODO tasks (toggle sort with h)" todo "TODO"
         ((org-agenda-sorting-strategy '(user-defined-up))
          (org-agenda-cmp-user-defined #'my/org-cmp-todo)))
        (" y" "TODAY tasks" todo "TODAY")
        ))


(with-eval-after-load 'org
    (define-key org-mode-map (kbd "RET")
      (lambda ()
        (interactive)
        (if (org-at-table-p)
            (org-table-next-row)
          (newline)
          (indent-relative-first-indent-point)))))


(defun org-mode-keys()
            (org-indent-mode t)
            (setq org-agenda-prefix-format "%t ")
            (org-display-inline-images)
            (setq org-image-actual-width nil) ;; so that we could scale them down #+ATTR_ORG: :width 123
            (org-id-update-id-locations)

            (setq org-return-follows-link  t)

            ;; Make org agenda clock report wider
            (setq org-agenda-clockreport-parameter-plist 
                  '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 123 :score 0))

    
            ;; Persistent org-clock is configured once at load time (see
            ;; `my-org-clock-history' section above); only per-mode behavior here.
            (setq org-clock-persist-query-resume nil
                  org-clock-auto-clock-resolution 'when-no-clock-is-running
                  org-clock-in-resume t)

            (setq org-refile-use-outline-path 'file)
            (setq org-refile-targets '((org-agenda-files :level . 1)))
            ;; When refilling add new headings to the top so that Task is last in the file.
            ;; This allows to have most recent Headings/Tasks added to task listed as the last in the agenda.
            ;; This is a workaround until there is better way to sort agenda Tasks
            ;; Reversing order put the first item at the bottom of the list, so it is more in line
            ;; with the way how quick notes are ordered 
            (setq org-reverse-note-order t)

            (add-to-list 'org-emphasis-alist
             '("*" (:foreground "gold")
               ))
            (if (not (search "termux" HOME))
               (flyspell-prog-mode))
            (setq org-src-fontify-natively t)
            ;;(my-keys-minor-mode 0) ;; disable my keys
            ;;Need to override iimge recenter which shadows C-l
            (local-set-key (kbd "\C-l") 'goto-line )
            (local-set-key (kbd "M-<up>") 'org-table-move-row-up )
            (local-set-key (kbd "C-c C-<down>") 'org-move-subtree-down )
            (local-set-key (kbd "C-c C-<up>") 'org-move-subtree-up )
            (local-set-key (kbd "C-c l") 'org-insert-link )
            (local-set-key (kbd "C-c <tab>") 'org-babel-goto-src-block-head )

            (local-set-key (kbd "M-c M-n") 'org-next-visible-heading)
            (local-set-key (kbd "M-c M-p") 'org-backward-heading-same-level)
 
            ;; Create heading ID and copy into clipboard
            (local-set-key (kbd "C-c i")
                           (lambda () (interactive)
                             (progn 
                               (org-id-get-create)
                               (org-id-copy))
                             ))


            ;; Create heading ID and copy into clipboard
            (local-set-key (kbd "C-c i")
                            (lambda () (interactive)
                              (progn
                                (condition-case nil            ;;
                                    (org-id-get-create)
                                  (error (message "Copy Id"))) ;; Needed to silent error and execute next expression
                                (org-id-copy))
                              ))
            (local-set-key (kbd "C-c d d") 'org-deadline)
            (local-set-key (kbd "C-c s s") 'org-schedule)
            (local-set-key (kbd "C-c s h") 'search-headlines)
            (local-set-key (kbd "C-c c c") 'org-archive-subtree-default)

            (setq org-ditaa-jar-path "~/bin/ditaa0_9.jar")
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((ditaa . t) (python . t) ))

            ;; artist-mode + org-ditta
            ;; C-c C-a y    paste
            ;; C-c C-a M-w  copy
            ;; C-c C-a r    rectangle
            ;; C-c C-a l    line
            ;; S-mouse3     rectangle erase
            ;; C-c C-c : executes the code
            ;; C-c ' : edits region


     ; key-chords  for org mode: need to define a new key map first
     (with-eval-after-load "org"
        (define-key org-mode-map (kbd "C-c C-j") #'org-global-cycle)
        (define-key org-mode-map (kbd "M-u") #'org-move-subtree-up)
        (define-key org-mode-map (kbd "M-i") #'org-move-subtree-down)
        (define-key org-mode-map (kbd "C-c r") #'org-redisplay-inline-images)
        (define-key org-mode-map (kbd "C-c e") #'iimage-mode)
        (define-key org-mode-map (kbd "C-c t") #'occur-timestamp-sort)
     )

     (key-chord-define org-mode-map "jj" 'org-global-cycle )
     (key-chord-define org-mode-map "77" 'occur-timestamp-sort)
     )


(add-hook 'org-mode-hook 'org-mode-keys)
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'org-tab-first-hook #'yas-expand nil t)))

;; When follow mode is enabled 'F' - this also expand selected heading
(add-hook 'org-agenda-after-show-hook 'org-show-entry)

(with-eval-after-load 'org-capture
  (key-chord-define org-capture-mode-map "qq" 'org-capture-finalize)
  (key-chord-define org-capture-mode-map "yy" 'org-capture-kill))


;;;;;;;;;;;;;;;;;;;; Org-agenda

;; Disable nested heading from inheriting tags and displaying the color of the parent
(setq org-agenda-use-tag-inheritance nil)

;; Keep generated [inactive] creation timestamps out of agenda comments.
(setq org-agenda-include-inactive-timestamps nil)

;; Place tags in org buffer and agenda just after the main text
(setq org-agenda-tags-column 0)
(setq org-tags-column 0)

(defface my/org-agenda-urgent
  '((t :inherit error :weight bold))
  "Face used for urgent entries in Org agenda.")

(defface my/org-agenda-task
  '((t :inherit font-lock-keyword-face))
  "Face used for one-off task entries in Org agenda.")

(defface my/org-agenda-recurring-scheduled
  '((t :inherit font-lock-builtin-face))
  "Face used for recurring scheduled entries in Org agenda.")

(setq org-tag-faces
      '(("urgent" . my/org-agenda-urgent)
        ("task" . my/org-agenda-task)))

(defconst my/org-timestamp-repeater-regexp "[.+]?[+][0-9]+[hdwmy]"
  "Regexp matching Org timestamp repeater intervals.")

(defun my/org-entry-has-active-date-p (date)
  "Return non-nil when the current Org entry already has active DATE."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion
                 (or (outline-next-heading) (point-max)))))
      (re-search-forward
       (concat "<" (regexp-quote date) "\\(?:[^>\n]*\\)>")
       end t))))

(defun my/org-entry-scheduled-repeater-p ()
  "Return non-nil when the current Org entry has a repeating SCHEDULED timestamp."
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (and scheduled
         (string-match-p my/org-timestamp-repeater-regexp scheduled))))

(defun my/org-entry-insert-active-timestamp-at-top (timestamp)
  "Insert active Org TIMESTAMP near the top of the current entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((indent (make-string (org-outline-level) ?\s)))
      (if (fboundp 'org-end-of-meta-data)
          (org-end-of-meta-data t)
        (forward-line 1))
      (insert indent timestamp "\n"))))

(defun my/org-add-done-timestamp ()
  "Add today's active timestamp to non-repeating headings marked DONE."
  (when (and (boundp 'org-state)
             (equal org-state "DONE"))
    (let ((date (format-time-string "%Y-%m-%d"))
          (timestamp (format-time-string "<%Y-%m-%d %a>")))
      (unless (or (my/org-entry-has-active-date-p date)
                  (my/org-entry-scheduled-repeater-p))
        (my/org-entry-insert-active-timestamp-at-top timestamp)))))

(add-hook 'org-after-todo-state-change-hook #'my/org-add-done-timestamp)

(defun my/org-bump-state-changes (&rest _)
  "Increment :STATE_CHANGES: on the entry at point, skipping repeaters."
  (unless (my/org-entry-scheduled-repeater-p)
    (let ((n (string-to-number
              (or (org-entry-get nil "STATE_CHANGES") "0"))))
      (org-entry-put nil "STATE_CHANGES" (number-to-string (1+ n))))))

(defun my/org-bump-state-changes-on-trigger (change)
  "Bump :STATE_CHANGES: only when a task returns to TODO from another state."
  (let ((from (plist-get change :from))
        (to   (plist-get change :to)))
    (when (and (eq (plist-get change :type) 'todo-state-change)
               (equal to "TODO")
               from
               (not (equal from ""))
               (not (equal from to)))
      (my/org-bump-state-changes))))

(add-hook 'org-trigger-hook #'my/org-bump-state-changes-on-trigger)
(advice-add 'org-schedule :after #'my/org-bump-state-changes)

(defun my/org-entry-state-changes ()
  (string-to-number (or (org-entry-get nil "STATE_CHANGES") "0")))

(defun my/org-entry-creation-time ()
  "Float-time of the first inactive timestamp under the heading, else 0.
Relies on the capture template's `%u'."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (if (re-search-forward org-ts-regexp-inactive end t)
          (float-time (org-time-string-to-time (match-string 0)))
        0))))

(defvar my/org-todo-sort-mode 'churn
  "Sort key for the TODO agenda view: `churn' or `created'.
Sorted ascending in both modes so the high value (most changes /
newest) lands at the bottom of the buffer.")

(defun my/org-cmp-todo (a b)
  (let* ((ma (get-text-property 0 'org-hd-marker a))
         (mb (get-text-property 0 'org-hd-marker b))
         (getter (if (eq my/org-todo-sort-mode 'created)
                     #'my/org-entry-creation-time
                   #'my/org-entry-state-changes))
         (va (org-with-point-at ma (funcall getter)))
         (vb (org-with-point-at mb (funcall getter))))
    (cond ((< va vb) -1) ((> va vb) +1))))

(defun my/org-agenda-toggle-todo-sort ()
  "Flip TODO agenda sort key between churn and created, then refresh."
  (interactive)
  (setq my/org-todo-sort-mode
        (if (eq my/org-todo-sort-mode 'churn) 'created 'churn))
  (message "TODO sort: by %s" my/org-todo-sort-mode)
  (org-agenda-redo))

(defun my/org-agenda-hide-global-todo-header ()
  "Remove Org's generated help header from global TODO agenda buffers."
  (when (eq (get-text-property (point-min) 'org-agenda-type) 'todo)
    (save-excursion
      (let ((first-entry nil))
        (goto-char (point-min))
        (while (and (not first-entry) (not (eobp)))
          (if (or (get-text-property (point) 'org-marker)
                  (get-text-property (point) 'org-hd-marker))
              (setq first-entry (line-beginning-position))
            (goto-char (or (next-single-property-change
                            (point) 'org-marker nil (point-max))
                           (point-max)))))
        (when (and first-entry (> first-entry (point-min)))
          (delete-region (point-min) first-entry))))))

(defun my/org-agenda-goto-last-todo-entry ()
  "Move point to the last entry in global TODO agenda buffers."
  (when (eq (get-text-property (point-min) 'org-agenda-type) 'todo)
    (let (last-entry)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (or (get-text-property (line-beginning-position) 'org-marker)
                    (get-text-property (line-beginning-position) 'org-hd-marker))
            (setq last-entry (line-beginning-position)))
          (forward-line 1)))
      (when last-entry
        (goto-char last-entry)))))

(require 'subr-x)
(require 'org-element)

(defun my/org-marker-in-headline-drawer-p (marker)
  "Return non-nil when MARKER points inside an Org drawer."
  (when (and marker (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let ((context (org-element-context)))
          (or (memq (org-element-type context)
                    '(drawer property-drawer node-property))
              (org-element-lineage context '(drawer property-drawer) t)))))))

(defun my/org-agenda-timestamp-context-text (marker)
  "Return one-line context text from the active timestamp line at MARKER."
  (when (and marker
             (marker-buffer marker)
             (not (my/org-marker-in-headline-drawer-p marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (unless (org-at-heading-p)
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (when (string-match-p org-ts-regexp line)
              (let ((text (string-trim
                           (replace-regexp-in-string
                            org-ts-regexp-both "" line))))
                (when (string-match-p
                       "\\`\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):"
                       text)
                  (setq text
                        (string-trim
                         (replace-regexp-in-string
                          "\\(?:\\`\\|[[:space:]]+\\)\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):"
                          " " text))))
                (unless (string-empty-p text)
                  text)))))))))

(defun my/org-agenda-remove-drawer-timestamp-lines ()
  "Remove agenda lines sourced from timestamps inside Org drawers."
  (when (eq org-agenda-type 'agenda)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((marker (get-text-property
                         (line-beginning-position) 'org-marker)))
            (if (my/org-marker-in-headline-drawer-p marker)
                (delete-region
                 (line-beginning-position)
                 (min (point-max) (1+ (line-end-position))))
              (forward-line 1))))))))

(defun my/org-agenda-remove-timestamp-context-lines ()
  "Remove timestamp context lines inserted into the current agenda buffer."
  (let ((inhibit-read-only t)
        pos)
    (save-excursion
      (goto-char (point-min))
      (while (setq pos (text-property-any
                        (point) (point-max)
                        'my-org-agenda-timestamp-context t))
        (goto-char pos)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))))))

(defun my/org-agenda-add-timestamp-context-lines ()
  "Show one source timestamp line below matching agenda items."
  (when (eq org-agenda-type 'agenda)
    (let ((inhibit-read-only t))
      (my/org-agenda-remove-timestamp-context-lines)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (marker (get-text-property line-start 'org-marker))
                 (context (my/org-agenda-timestamp-context-text marker)))
            (when context
              (end-of-line)
              (let ((props (append (text-properties-at line-start)
                                   '(my-org-agenda-timestamp-context t
                                     face shadow))))
                (insert "\n")
                (add-text-properties
                 (point)
                 (progn (insert "    " context) (point))
                 props))))
          (forward-line 1))))))

(defun my/org-agenda-line-marker ()
  "Return the Org source marker for the current agenda line."
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position)))
    (or (my/org-agenda-line-marker-property 'org-hd-marker line-start line-end)
        (my/org-agenda-line-marker-property 'org-marker line-start line-end))))

(defun my/org-agenda-line-marker-property (property line-start line-end)
  "Return PROPERTY value between LINE-START and LINE-END."
  (let ((pos line-start)
        value)
    (while (and (not value) (< pos line-end))
      (setq value (get-text-property pos property))
      (setq pos (or (next-single-property-change pos property nil line-end)
                    line-end)))
    value))

(defun my/org-agenda-line-scheduled-repeater-p ()
  "Return non-nil when the current agenda line is a repeating scheduled entry."
  (let ((marker (my/org-agenda-line-marker)))
    (when (and marker (marker-buffer marker))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (my/org-entry-scheduled-repeater-p))))))

(defun my/org-agenda-color-lines ()
  "Color entire Org agenda lines based on tags and repeaters."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (line (buffer-substring-no-properties line-start line-end)))
        (unless (get-text-property line-start 'my-org-agenda-timestamp-context)
          (cond
           ((string-match-p ":urgent:" line)
            (add-face-text-property line-start line-end
                                    'my/org-agenda-urgent))
           ((my/org-agenda-line-scheduled-repeater-p)
            (add-face-text-property line-start line-end
                                    'my/org-agenda-recurring-scheduled))
           ((string-match-p ":task:" line)
            (add-face-text-property line-start line-end
                                    'my/org-agenda-task)))))
      (forward-line 1))))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-remove-drawer-timestamp-lines)
(add-hook 'org-agenda-finalize-hook #'my/org-agenda-add-timestamp-context-lines)
(add-hook 'org-agenda-finalize-hook #'my/org-agenda-color-lines)
(add-hook 'org-agenda-finalize-hook #'my/org-agenda-hide-global-todo-header)
(add-hook 'org-agenda-finalize-hook #'my/org-agenda-goto-last-todo-entry t)

;; Search org files associated with the Agenda view
(with-eval-after-load 'org-agenda
  (when (fboundp 'my/org-agenda-todo-with-done-timestamp)
    (advice-remove 'org-agenda-todo #'my/org-agenda-todo-with-done-timestamp))
  (define-key org-agenda-mode-map (kbd "S") #'org-occur-in-agenda-files)
  (define-key org-agenda-mode-map (kbd "C-c d d") #'org-agenda-deadline)
  (define-key org-agenda-mode-map (kbd "C-c s s") #'org-agenda-schedule)
  (define-key org-agenda-mode-map (kbd "C-c c c") #'org-agenda-archive-default)
  (define-key org-agenda-mode-map (kbd "h") #'my/org-agenda-toggle-todo-sort))

;;;;;;;;;;;;;;;;;;;; Calendar
(defun my-calendar-hook ()
  "Turn line truncation on."
    (progn
      (visual-line-mode -1)
      (toggle-truncate-lines 1)
    ))

(add-hook 'calendar-mode-hook #'my-calendar-hook)


;;;;;;;;;;;;;;;;;;; Org sound notification - Termux

(if (string-match-p "com.termux" (or (getenv "PREFIX") ""))
    (progn
      ;; Define a GENERIC Termux Notification function
      ;; This handles any text Org throws at it
      (defun my-termux-notify (msg)
        "Send a notification via Termux for any Org event."
        (let ((clean-msg (replace-regexp-in-string "\"" "'" msg))) ;; Escape quotes
          (start-process "termux-notify" nil
                         "termux-notification"
                         "--title" "Org Mode"
                         "--content" clean-msg
                         "--sound")
          (start-process "termux-sound" nil
                         "termux-media-player" "play" "Nudge.wav")))

      ;; Override Org's default handler - The Fix for DBus Error
      ;; This stops Org from trying to look for D-Bus
      (setq org-show-notification-handler 'my-termux-notify)

      ;;Appointment System Setup
      (require 'appt)
      (appt-activate 1)
      (setq appt-time-msg-list nil)
      (setq appt-display-interval 5)
      (setq appt-message-warning-time 15)
      (setq appt-display-mode-line t)
      (setq appt-display-format 'window)

      ;; Bridge Appointment System to our Generic Function
      ;; Appt passes 2 arguments, so we wrap our 1-arg function
      (defun my-appt-to-termux (min-to-app new-time msg)
        (my-termux-notify (format "In %s min: %s" min-to-app msg)))

      (setq appt-disp-window-function 'my-appt-to-termux)
      (setq appt-delete-window-function (lambda () t)) ;; Prevent window errors

      ;; Sync Agenda
      ;; After scheduling new task - open agenda to register new reminder event      
      (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt)))
    
;;;;;;;;;;;;;;;;;;;; Positioning of clock - Termux (small screens)

(if (string-match-p "com.termux" (or (getenv "PREFIX") ""))
    (setq-default mode-line-format
                  (list
                   "%e"
                   mode-line-front-space
                   mode-line-client
                   mode-line-modified    ; The "**" or "--" status
                   " "

                   ;; The Timer - Placed First
                   '(:eval (if (and (fboundp 'org-timer-value-string)
                                    (org-timer-value-string))
                               (propertize (concat " [" (org-timer-value-string) "] ")
                                           'face 'error
                                           'weight 'bold)
                               ""))

                   ;; Buffer Name
                   mode-line-buffer-identification

                   "  "
                   ;; Line Number
                   mode-line-position

                   ;; Spacer - pushes everything else to the right
                   "      "

                   ;; Modes are moved to the end or hidden so they don't block the view
                   mode-line-misc-info)))

    
;;;;;;;;;;;;;;;;;;;; Ediff

;; don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
;; Exclude Buffer List from winner-mode
(add-to-list 'winner-boring-buffers "*Buffer List*")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;;;;;;;;;;;;;;;;;;; Ediff with git
;;[difftool "ediff"]
;;cmd = emacs --eval \"(ediff-files \\\"$LOCAL\\\" \\\"$REMOTE\\\")\"
;;# git difftool --tool=ediff --diff-filter=M tagname subdir
;(add-hook 'ediff-prepare-buffer-hook (lambda () (whitespace-mode 1) ) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extend the find-file to handle line/columns when open from Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open files and go places like we see from error messages, i e: path:line:col
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
(defadvice find-file (around find-file-line-number
                             (path &optional wildcards)
                             activate)
  "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
  (save-match-data
    (let* ((match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$" path))
           (line-no (and match
                         (match-string 2 path)
                         (string-to-number (match-string 2 path))))
           (col-no (and match
                        (match-string 3 path)
                        (string-to-number (match-string 3 path))))
           (path (if match (match-string 1 path) path)))
      ad-do-it
      (when line-no
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-no))
        (when (> col-no 0)
          (forward-char (1- col-no)))))))

;; in order to override it we need to require it first
(require 'ffap)
(defun ffap-prompter (&optional guess)
  ;; Does guess and prompt step for find-file-at-point.
  ;; Extra complication for the temporary highlighting.
  (unwind-protect
      ;; This catch will let ffap-alist entries do their own prompting
      ;; and then maybe skip over this prompt (ff-paths, for example).
      (catch 'ffap-prompter
        (ffap-read-file-or-url
         (if ffap-url-regexp "Find file or URL: " "Find file: ")
         (prog1
             (let ((mark-active nil)
                   (currentline (ffap-string-at-point))
                   (path_guess (ffap-guesser))
                   (match_data))
               ;; Don't use the region here, since it can be something
               ;; completely unwieldy. If the user wants that, she could
               ;; use M-w before and then C-y. --Stef

;; Check beginning of the line for the line number (pylint mode)
               ;; (save-match-data (and (setq match_data (string-match "\\([0-9]+\\)" currentline))
               (if (string-match "^\\([0-9]+\\)" currentline)
                   (progn ; pylit case path\n\nline_number:
                     ;; store the line number and traverse up to extract a file
                     (let ((line_number))
                       ;; To match pylint result remember to go to line beginning
                       ;; Extract the first match which should be the line number
                       (setq line_number (match-string 1 currentline))

                       (save-excursion
                         (previous-line)
                         (while (not (ffap-guesser))
                           (previous-line)
                           )
                         (setq path_guess (concat (ffap-guesser) ":" line_number))
                         ) ; excursion end
                       )
                     ) ; progn end
                 (progn ;; else - regular case path:line_number
                   (setq path_guess (ffap-string-at-point))
                   )
                 )

               (setq guess (or guess path_guess ))) ; using ffap-alist here
           (and guess (ffap-highlight))
           )))
    (ffap-highlight t)))


;; for newer version override order of sorted file timestamps
(if (version< "26.2" emacs-version )
    (progn

    ;;; Override order of desktop+ listed desktop - based on most recently modified in stead alphabetical
(defun desktop+-load (name)
  (interactive
   (list
    (completing-read "Desktop name: "
    ;; List desktops in the order of modification time
    ;; Build custom completion-table with display-sort-function property
         (lambda (string pred action)
           (if (eq action 'metadata)
         '(metadata (display-sort-function . identity))
       (complete-with-action
        action
        ;; Build list of directory entries sorted by time stamp
        (remove "." (remove ".." (mapcar #'car
                                         (sort (directory-files-and-attributes desktop+-base-dir)
                                               ;; x y inverted in 27.1 - return chenged in time-less-p
                                               #'(lambda (y x) (time-less-p (nth 6 x) (nth 6 y)))
                                          )
                 )))
        string pred ))))))


  (desktop-change-dir (desktop+--dirname name))
  (desktop+--set-frame-title)
  (desktop-save-mode 1))
))

(defun grep-locations (command-args)
  "Run grep via find, and search all locations specified in  dumb-jump-project
More locations can be included into the search using:  dumb-jump-append-include-paths
Setting empty dumb-jump-set-include-paths will reset search tree to the current directory
NOTE: moved from myfunc.el as 'grep-locations key binding did not corectly register
      and update the default FIND_CMD"

   (interactive
   (progn

     (grep-compute-defaults)
     (if dumb-jump-project (setq kuba-roots dumb-jump-project) (setq kuba-roots "."))

     (let (kuba-grep-string)
     (setq kuba-grep-string (message "%s %s -type f -exec grep -nIH --null --exclude-dir={target,build*}  \"\{\}\" \";\"" FIND_CMD my-root-directory))

     ;; Don'tupdate grep-find-command as it is global. Instead pass kuba-grep-string directly
     ;; (grep-apply-setting 'grep-find-command (cons kuba-grep-string (- (length kuba-grep-string) 8 )))

     (if grep-find-command
  	 (list (read-shell-command "Grep locations: "
                                   (cons kuba-grep-string (- (length kuba-grep-string) 8 )) 'grep-find-history))
         ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil)) )) )

   (when command-args
    (let ((null-device nil))
      (grep command-args)))
   )

(defun find-locations (search-phrase)
 (let ((exclude-dirs (mapconcat 'identity '(
    "-path target -prune -o"
    "-path build* -prune -o"
    "-path *.git* -prune -o"
    ) " ")))
    (grep-find (message "%s %s %s -name \"\*%s\*\" -print | %s -I %% %s %%:1:" FIND_CMD my-root-directory exclude-dirs search-phrase XARGS_CMD ECHO_CMD))
))

(defun search-headlines (search-phrase) (interactive "MSearch headlines:")
    (org-occur (message "^\\*+ .*%s" search-phrase))
)

(defun my/project-find-file-fido ()
  "Run project-find-file with fido-vertical-mode temporarily enabled."
  (interactive)
  (if (version<= "28.0" emacs-version)
      ;; Emacs 28+: Use fido-vertical-mode
      (let ((fido-was-active fido-mode)
            (fido-vertical-was-active fido-vertical-mode))
        (unless fido-vertical-was-active
          (fido-vertical-mode 1))
        (unwind-protect
            (call-interactively 'project-find-file)
          (unless fido-vertical-was-active
            (fido-vertical-mode -1))
          (unless fido-was-active
            (fido-mode -1))))
    ;; Emacs 27: Use fido-mode with icomplete-vertical-mode
    (let ((fido-was-active fido-mode)
          (icomplete-vertical-was-active (and (fboundp 'icomplete-vertical-mode)
                                               icomplete-vertical-mode)))
      (unless fido-was-active
        (fido-mode 1))
      (when (and (fboundp 'icomplete-vertical-mode) (not icomplete-vertical-was-active))
        (icomplete-vertical-mode 1))
      (unwind-protect
          (call-interactively 'project-find-file)
        (when (and (fboundp 'icomplete-vertical-mode) (not icomplete-vertical-was-active))
          (icomplete-vertical-mode -1))
        (unless fido-was-active
          (fido-mode -1))))))

;; Required in rust-analyzer for large codebase
(setq eglot-connect-timeout 60)
(setq eglot-sync-connect nil)


(setq eglot-server-programs
      (list
       ;; Use clangd for C++
       '((c++-mode c-mode) . ("clangd"))

       ;; Use Ty for Python
       (cons 'python-mode
             (list (concat (getenv "HOME") "/bin/ty") "server"))

       '(rust-mode . ("rust-analyzer"
        :initializationOptions
        (:inlayHints (
        :typeHints (:enable :json-false)
        :parameterHints (:enable :json-false)
        ))))
    ))

;; Auto-start eglot for configured languages
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;;;;;;;;;;;;;;;;;;;; Eat - terminal emulator

; Fix hard to read colors in dark mode
(with-eval-after-load 'eat
    (set-face-foreground 'eat-term-color-red "tomato")
    (set-face-foreground 'eat-term-color-blue "skyblue")
    (set-face-foreground 'eat-term-color-bright-blue "DeepSkyBlue")
    )
 
;;;;;;;;;;;;;;;;;;;; Configured by Emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/.notes_archive" "~/.notes"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url
                      "https://github.com/manzaltu/claude-code-ide.el"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-A ((t (:background "#3a2020"))))
 '(ediff-even-diff-B ((t (:background "#203a20"))))
 '(ediff-fine-diff-A ((t (:background "#552222"))))
 '(ediff-fine-diff-B ((t (:background "#225522"))))
 '(ediff-odd-diff-A ((t (:background "#3a2020"))))
 '(ediff-odd-diff-B ((t (:background "#203a20")))))
