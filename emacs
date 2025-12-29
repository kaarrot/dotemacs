;; Requires Emacs 26.2 or higher
(require 'package)
;; Disable melpa signiture check
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

;; run gc only when idle
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


(setq package-selected-packages '(
                  f           ; required by desktop±
				  flycheck
				  company
				  multiple-cursors
				  dumb-jump
				  yasnippet
				  avy
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
	  ))


;; Auto install required packages
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

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
(require 'key-chord)



;; good to have
(require 'wgrep)
(require 'markdown-mode)
(require 'cmake-mode)
(require 'yasnippet)
(require 'clang-format) ;; assumes clang-format is on the PATH
(require 'go-mode)


(setq key-chord-typing-detection t)
;(setq key-chord-typing-speed-threashold 2.0)
;(setq key-chord-typing-reset-delay 2.0)
(setq key-chord-one-key-delay 0.5)
(key-chord-mode 1)

(dumb-jump-mode t)
(tabbar-mode)
(yas-global-mode 1)

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

(if (not (search "arm" system-configuration))
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
(define-key my-keys-minor-mode-map (kbd "C-x C-b") (lambda () (interactive) (ibuffer nil " *Ibuffer*") (toggle-truncate-lines 1)))

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
(define-key my-keys-minor-mode-map (kbd "C-x p") 'desktop+-load)
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


(global-set-key (kbd "C-x x")  (lambda () (interactive) (switch-to-buffer nil)))

;;;;;;;;;;;;;;;;;;; Winner mode
;; Unbind existing key sequence first

(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c ,") 'winner-undo)
(global-set-key (kbd "M-c .") 'winner-redo)

;;;;;;;;;;;;;;;;;;; End of Global bindings


;;;;;;;;;;;;;;;;;;; Key chords
(key-chord-define-global "qq" 'avy-goto-char)
(key-chord-define-global "ww" 'occur)
(key-chord-define-global "uy" 'winner-undo)
(key-chord-define-global "kk" 'kill-buffer)
(key-chord-define-global "aa" 'match-paren)
(key-chord-define-global "xx" (lambda () (interactive) (switch-to-buffer nil)))
(key-chord-define-global "bb" (lambda () (interactive)
                                (ibuffer nil " *Ibuffer*") (toggle-truncate-lines 1)))

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

(key-chord-define-global " z" 'winner-undo)
(key-chord-define-global "\\\\" 'occur)

(key-chord-define-global "ss" 'ispell-word)
(key-chord-define-global "dd" 'flyspell-goto-next-error)

;;; Org
(key-chord-define-global ".." 'org-timestamp-up)
(key-chord-define-global ",," 'org-timestamp-down)
(key-chord-define-global " ." (lambda () (interactive) (org-time-stamp '(16))))

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
(setq org-fold-show-context-detail
      '((agenda . tree)
        (isearch . tree)
        (default . ancestors)))


(setq org-agenda-custom-commands
      '(
        (" a" "ACTIVE tasks" todo "ACTIVE")
        (" n" "NEXT tasks" todo "NEXT")
        (" w" "WAITING tasks" todo "WAITING")
        (" r" "READ tasks" todo "READ")
        (" t" "TODO tasks" todo "TODO")
        ))

(defun org-mode-keys()
            (org-indent-mode t)
            (setq org-agenda-prefix-format "%t %s")
            (org-display-inline-images)
            (setq org-image-actual-width nil) ;; so that we could scale them down #+ATTR_ORG: :width 123
            (org-id-update-id-locations)

            (setq org-return-follows-link  t)

            ;; Make org agenda clock report wider
            (setq org-agenda-clockreport-parameter-plist 
                  '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 123 :score 0))

    
            ;; Enable persistent org-clock across emacs sessions - jumps to active clock
            (org-clock-persistence-insinuate)
            (setq org-clock-persist t
                  org-clock-persist-query-resume nil
                  org-clock-auto-clock-resolution 'when-no-clock-is-running
                  org-clock-history-length 23
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
             '("*" (:foreground "IndianRed")
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
            (local-set-key (kbd "C-c s") 'search-headlines)

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

;;;;;;;;;;;;;;;;;;;; Org-agenda

;; Disable nested heading from inheriting tags and displaying the color of the parent
(setq org-agenda-use-tag-inheritance nil)

(setq org-tag-faces
      '(("urgent" . (:foreground "red" :weight bold))
        ("task" . (:foreground "orange"))))

(defun my/org-agenda-color-lines ()
  "Color entire Org Agenda lines based on tags."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (thing-at-point 'line t)))
        (cond
         ((and line (string-match ":urgent:" line))
          (add-text-properties (line-beginning-position) (line-end-position)
                               '(face (:foreground "white" :background "red" :weight bold))))
         ((and line (string-match ":task:" line))
          (add-text-properties (line-beginning-position) (line-end-position)
                               '(face (:foreground "black" :background "lightblue"))))))
      (forward-line 1))))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-color-lines)

;; Search org files associated with the Agenda view
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "S") #'org-occur-in-agenda-files))

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
                   '(:eval (if (org-timer-value-string)
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
(add-hook 'ediff-prepare-buffer-hook (lambda () (whitespace-mode 1) ) t)


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

;;;;;;;;;;;;;;;;;;;; Eat - terminal emulator

; Fix hard to read colors in dark mode
(with-eval-after-load 'eat
    (set-face-foreground 'eat-term-color-red "tomato")
    (set-face-foreground 'eat-term-color-blue "skyblue")
    (set-face-foreground 'eat-term-color-bright-blue "DeepSkyBlue")
    )
 
;; Auto-start eglot for configured languages
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/.notes_archive" "~/.notes"))
 '(org-babel-C++-compiler "g++ -v")
 '(org-babel-load-languages '((ditaa . t) (python . t) (C . t)))
 '(package-selected-packages
   '(eglot rust-mode dockerfile-mode gomacro-mode p4 go-mode flycheck
           company multiple-cursors dumb-jump yasnippet avy dap-mode
           which-key)))
