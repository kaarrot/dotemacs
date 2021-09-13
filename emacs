;; Requires Emacs 26.2 or higher
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; run gc only when idle
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(setq package-selected-packages '(lsp-mode
				  flycheck
				  company
				  multiple-cursors
				  dumb-jump
				  yasnippet
				  lsp-treemacs 
				  avy
				  dap-mode
				  which-key
                  go-mode
                  undo-fu
				  )
      )

;; Auto install required packages
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 3
      lsp-idle-delay 0.1 ;; clangd is fast
      ;; be more ide-ish
      lsp-headerline-breadcrumb-enable nil)   ;; disable breadcrumbs by default


;;;;;;;;;;;;;;;;;;;;;;

(setq HOME (expand-file-name "~"))

(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/anything")
 

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

(load (message "%s/.emacs.d/modules/myfuncs.el" HOME ))

(require 'bm)
(require 'desktop+)  ;; custom tweaks to list in recent order
;; essential
(require 'multiple-cursors)
(require 'dumb-jump)
(require 'tabbar)
(require 'anything-match-plugin)
(require 'anything-config)

;; good to have
(require 'wgrep)
(require 'markdown-mode)
(require 'cmake-mode)
(require 'yasnippet)
(require 'clang-format) ;; assumes clang-format is on the PATH
(require 'go-mode)

(dumb-jump-mode t)
(tabbar-mode)
(yas-global-mode 1)

(setq FIND_CMD "find")
(setq XARGS_CMD "xargs")
(setq ECHO_CMD "echo")
(if (eq system-type 'windows-nt)
    (progn
      (setq FIND_CMD "C:/cygwin64/bin/find.exe")
      (setq XARGS_CMD "C:/cygwin64/bin/xargs.exe")
      (setq ECHO_CMD "C:/cygwin64/bin/echo.exe")
      ;; NOTE: grep is available already on the path
      (grep-apply-setting 'grep-find-command '("C:/cygwin64/bin/find.exe . -type f -exec grep -nH --null  \"\{\}\" \";\"" . 58))      
      )
)
  

;;;;;;;;;;;;;;;;;;; gpg
(require 'epa-file)
(epa-file-enable)
(setf epa-pinentry-mode 'loopback)  ;; enable command line password entry
(setq epa-file-cache-passphrase-for-symmetric-encryption t)  ;; no need to retype passphrase after each save


;;;;;;;;;;;;;;;;;;; aspell termux
;; build gospell in ~/bin and rename it to aspell, so that ispell can find it
(if (search "termux" HOME)
    (progn
    ;; We need to setup aspell dictionary using origianl aspell binary or otherwise ispell setup will fail.
    ;; Once we have that we switch to gospell
    (require 'ispell)
    (ispell-find-aspell-dictionaries)
    (setq ispell-program-name (message "%s/bin/aspell" HOME))
    )
)

;;;;;;;;;;;;;;;;;;; Configuration


(setq python-shell-interpreter "python3")
(if (eq system-type 'windows-nt)
    (progn
    (setq python-shell-interpreter (message "%s/scoop/shims/python2.exe" HOME))
    (setq visible-bell 1) ;; disable bell sound on Windows
    )
  )

(if (eq system-type 'linux)
    (setq-default shell-file-name "/bin/bash")
  )

(setq inhibit-splash-screen t)
(setq tramp-default-method "ssh")  ;; tramp

;; Disable default tab-indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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
(define-key my-keys-minor-mode-map (kbd "M-c ,") 'tabbar-move-current-tab-one-place-left)
(define-key my-keys-minor-mode-map (kbd "C-M-<next>") 'tabbar-move-current-tab-one-place-right) ; C-S-M-Page Down
(define-key my-keys-minor-mode-map (kbd "M-c .") 'tabbar-move-current-tab-one-place-right)

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
(define-key my-keys-minor-mode-map (kbd "C-c C-l") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-M-<up>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-c C-.") 'bm-next) ; >
(define-key my-keys-minor-mode-map (kbd "C-M-<right>") 'bm-next) ; >
(define-key my-keys-minor-mode-map (kbd "C-c C-,") 'bm-previous) ;   <
(define-key my-keys-minor-mode-map (kbd "C-M-<left>") 'bm-previous) ; <

;;;;;;;;;;;;;;;;;;;; Move between windows
(define-key my-keys-minor-mode-map (kbd "C-c <left>") 'windmove-left) ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "C-c C-<left>") 'windmove-left) ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "s-<left>") 'windmove-left) ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-c C-<right>") 'windmove-right)
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
(define-key my-keys-minor-mode-map (kbd "<f2>") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "C-c 2") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "C-c <f2>") (lambda (search-phrase) (interactive "Msearch file:")
    (grep-find (message "%s . -name \"%s\" -print | %s -I %% %s %%:1:" FIND_CMD search-phrase XARGS_CMD ECHO_CMD))))

(define-key my-keys-minor-mode-map (kbd "ESC 2") 'grep-locations)

(define-key my-keys-minor-mode-map (kbd "C-c 3") (lambda (search-phrase) (interactive "MSearch file:")
  (grep-find (message "%s . -name \"%s\" -print | %s -I %% %s %%:1:" FIND_CMD search-phrase XARGS_CMD ECHO_CMD))))

(define-key my-keys-minor-mode-map (kbd "ESC 3") (lambda (search-phrase) (interactive "MSearch find:")
    (if dumb-jump-project (setq kuba-roots dumb-jump-project) (setq kuba-roots "."))
    (grep-find (message "%s %s -name \"%s\" -print | %s -I %% %s %%:1:" FIND_CMD kuba-roots search-phrase XARGS_CMD ECHO_CMD))))

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
(define-key my-keys-minor-mode-map (kbd "C-c 0") electric-indent-mode)
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

;;;;;;;;;;;;;;;;;;; Winner mode
;; Unbind existing key sequence first
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c u") 'winner-undo)
(global-set-key (kbd "S-<left>") 'winner-undo)
(global-set-key (kbd "C-c 0") 'winner-undo)
(global-set-key (kbd "M-c 0") 'winner-redo)
(global-set-key (kbd "S-<right>") 'winner-redo)

;;;;;;;;;;;;;;;;;;; End of Global bindings
(define-minor-mode my-keys-minor-mode
"A minor mode so that my key settings override annoying major modes."
t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

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
  (local-set-key (kbd "C-x 5") (lambda () (interactive)
                                (setq lsp-clients-clangd-executable "clangd-9")
                                (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))       
                                (lsp)
                                ))
  (local-set-key (kbd "C-c 5") (lambda () (interactive)
                                    (require 'cquery)
                                    (setq cquery-executable "~/bin/cquery")
                                    ;(setq cquery-extra-args  '("--log-all-to-stderr") )
                                    (setq cquery-cache-dir "~/tmp/.cquery_cached_index")
                                    (lsp)
                                    ))
  (local-set-key (kbd "S-<f5>") 'toggle-window-dedicated)
  (local-set-key (kbd "C-c <RET>") (lambda () (interactive)
    (setq compile-command (message "g++ -O0 -g -std=c++14 -I. %s -o a.out" (buffer-file-name))) )) 

  (local-set-key [pause] 'toggle-window-dedicated)
  (setq comment-start "//" comment-end "")
  (setq compilation-scroll-output 'first-error) ;; scroll compilation buffer
  (set-default 'truncate-lines nil)

  )

;; Changing styl of comments in CC-mode
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

;;;;;;;;;;;;;;;;;;;; Partial completion to shell and python-inferior mode
(defun shell-mode-keys()
  (local-set-key (kbd "C-S-<up>") 'comint-previous-matching-input-from-input)
  (local-set-key (kbd "C-S-<down>") 'comint-next-matching-input-from-input)

  ;; fix cd when using aliased commands
  (track-shell-directory/procfs)

  (python-shell-completion-native-turn-on)
  ;; Fallback option to mute the warning
  (setq python-shell-completion-native-disabled-interpreters '("python"))

  )

(add-hook 'shell-mode-hook 'shell-mode-keys)
(add-hook 'inferior-python-mode-hook 'shell-mode-keys)

;;;;;;;;;;;;;;;;;;;; Dired
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

(defun org-mode-keys()
            (org-indent-mode t)
            (flyspell-prog-mode)
            (setq org-src-fontify-natively t)
            ;;(my-keys-minor-mode 0) ;; disable my keys
            (local-set-key (kbd "M-<up>") 'org-table-move-row-up )
            (local-set-key (kbd "C-x C-<down>") 'org-move-subtree-down )
            (local-set-key (kbd "C-x C-<up>") 'org-move-subtree-up )
            (local-set-key (kbd "C-c l") 'org-insert-link )

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

    )

(add-hook 'org-mode-hook 'org-mode-keys)



;;;;;;;;;;;;;;;;;;;; Ediff

;; don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
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
               (message "%s" currentline)
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
     (if dumb-jump-project (setq kuba-roots dumb-jump-project) (setq kuba-roots ".")
     (setq kuba-grep-string (message "%s %s -type f -exec grep -nH --null  \"\{\}\" \";\"" FIND_CMD kuba-roots))

     ;; Don'tupdate grep-find-command as it is global. Instead pass kuba-grep-string directly
     ;; (grep-apply-setting 'grep-find-command (cons kuba-grep-string (- (length kuba-grep-string) 8 )))

     (if grep-find-command
  	 (list (read-shell-command "Grep locations: "
                                   (cons kuba-grep-string (- (length kuba-grep-string) 8 )) 'grep-find-history))
         ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil)))))

   (when command-args
    (let ((null-device nil))
      (grep command-args)))
   )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (p4 go-mode lsp-mode flycheck company multiple-cursors dumb-jump yasnippet lsp-treemacs avy dap-mode which-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
