
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/modules/multiple-cursors")
(add-to-list 'load-path "~/.emacs.d/modules/auto-complete")
(add-to-list 'load-path "~/.emacs.d/modules/company-mode")
(add-to-list 'load-path "~/.emacs.d/modules/cquery")

(load "~/.emacs.d/modules/myfuncs.el")
(require 'dumb-jump)
(require 'multiple-cursors)
(require 'ace-jump-mode)
(require 'tabbar)
(require 'dumb-jump)
(require 'company)
(require 'bm)
(require 'markdown-mode)
(require 'desktop+)
(require 'yasnippet)
(require 'cmake-mode)
(when (require 'scramble-mode nil 'noerror))

(dumb-jump-mode t)
(tabbar-mode)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;; Configuration

(setq inhibit-splash-screen t)
(setq tramp-default-method "ssh")
(setq tab-width 2)

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
(setq mouse-buffer-menu-mode-mult 10)
(global-auto-revert-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(when (display-graphic-p)
  (tool-bar-mode -1)
)

;;;;;;;;;;;;;;;;;;;; Bookmarks - 'bm
;;(when (display-graphic-p)
  (setq bm-repository-file "~/.emacs.d/bm-repository")
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
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring) savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

;;;;;;;;;;;;;;;;;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;; Autocomplete
;(ac-config-default)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;;;;;;;;;;;;;;;;;;;; Tabbar
(define-key my-keys-minor-mode-map (kbd "C-S-c C-<left>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "C-S-c C-<right>") 'tabbar-forward-tab)

;;;;;;;;;;;;;;;;;;;; ACE Jump
(define-key my-keys-minor-mode-map (kbd "M-SPC") 'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "C-d") 'ace-jump-char-mode)

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
(define-key my-keys-minor-mode-map (kbd "C-c l") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-M-<up>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-c .") 'bm-next) ; >
(define-key my-keys-minor-mode-map (kbd "C-M-<right>") 'bm-next) ; >
(define-key my-keys-minor-mode-map (kbd "C-c ,") 'bm-previous) ;   <
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
(define-key my-keys-minor-mode-map (kbd "<f3>") 'get-file-path)
(define-key my-keys-minor-mode-map (kbd "C-c 3") 'get-file-path)
(define-key my-keys-minor-mode-map(kbd "<f6>") 'whitespace-mode)
(define-key my-keys-minor-mode-map (kbd "C-c 6") 'whitespace-mode)
(define-key my-keys-minor-mode-map (kbd "<C-f6>")
  (lambda () (interactive) (if indent-tabs-mode (progn (setq indent-tabs-mode nil) (message "spaces")) (progn (setq indent-tabs-mode t) (message "tabs")) )))
(global-set-key (kbd "<f8>") 'ispell-word) ;; Flyspel
(global-set-key (kbd "C-c 8") 'ispell-word)
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c 9") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c t") (lambda () (interactive) (setq tab-width 4)))

(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "C-z") 'set-mark-command)
(define-key my-keys-minor-mode-map (kbd "C-<SPC>") 'set-mark-command)

(define-key my-keys-minor-mode-map (kbd "C-;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-c ;") 'comment-or-uncomment-this)
;(define-key my-keys-minor-mode-map (kbd "C-z") 'undo)
(define-key my-keys-minor-mode-map (kbd "C-c C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map "\C-l" 'goto-line)

(define-key my-keys-minor-mode-map (kbd "C-c <RET>") 'selectback-exec)
(define-key my-keys-minor-mode-map (kbd "S-C-c <RET>") 'selectback)
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
(define-key my-keys-minor-mode-map (kbd "<f10> c")
  (lambda ()  (interactive)  (occur-1 "{$\\|)$" 1 (list (current-buffer))) ) )
(define-key my-keys-minor-mode-map (kbd "<f10> p")
  (lambda ()  (interactive)  (occur-1 "def\\|class" 1 (list (current-buffer))) ))

;;;;;;;;;;;;;;;;;;;
(define-minor-mode my-keys-minor-mode
"A minor mode so that my key settings override annoying major modes."
t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;; C-key-bindings
(defun c-mode-keys()
  (setq compile-command (message "g++ -O0 -g -std=c++11 %s -o a.out" (buffer-file-name)))
  (setq tab-width 2)
  (local-set-key (kbd "C-c <RET>") 'compile)
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-b") 'compile)
  (local-set-key (kbd "C-q") 'compile)
  (local-set-key (kbd "<f5>") 'gud-gdb)
  (local-set-key (kbd "S-<f5>") 'toggle-window-dedicated)
  (local-set-key [pause] 'toggle-window-dedicated)
  (setq comment-start "//" comment-end "")
  (set-default 'truncate-lines nil)

  (when (require 'lsp-mode nil 'noerror)
      (require 'cquery)
      (setq cquery-executable "~/bin/cquery")
      (setq cquery-cache-dir "/tmp/.cquery_cached_index")
      ;;(lsp)  ;; Not sure if this is worth it - when restoring bookmarks for each cpp file lsp will ask for root location
      )

  )

;; Changing styl of comments in CC-mode
(add-hook 'c-mode-hook  (lambda () (setq comment-start "//" comment-end "") ))
(add-hook 'c++-mode-hook 'c-mode-keys)
(add-hook 'c-mode-hook 'c-mode-keys)

;;;;;;;;;;;;;;;;;;; Gdb
(defun gdb-mode-keys()
  (local-set-key (kbd "C-S-<up>") 'comint-previous-matching-input-from-input)
  )
(add-hook 'gdb-mode-hook 'gdb-mode-keys)

;;;;;;;;;;;;;;;;;;; Python
(defun python-mode-keys()
  (python-indent-guess-indent-offset)
  (infer-indentation-style)

  (local-set-key (kbd "C->") 'python-indent-shift-right)
  (local-set-key (kbd "C-<") 'python-indent-shift-left)
  (setq tab-width 2)
  )

(add-hook 'python-mode-hook 'python-mode-keys)

;;;;;;;;;;;;;;;;;;;; Partial completion to shell and python-inferior mode
(defun shell-mode-keys()
  (local-set-key (kbd "C-S-<up>") 'comint-previous-matching-input-from-input)
  (local-set-key (kbd "C-S-<down>") 'comint-next-matching-input-from-input)

  (python-shell-completion-native-turn-on)
  ;; Fallback option to mute the warning
  (setq python-shell-completion-native-disabled-interpreters '("python"))

  )

(add-hook 'shell-mode-hook 'shell-mode-keys)
(add-hook 'inferior-python-mode-hook 'shell-mode-keys)

;;;;;;;;;;;;;;;;;;;; Dired
(eval-after-load 'dired '(progn (require 'single-dired)))
(defun dired-mode-keys()
  (local-set-key (kbd "C-w") 'wdired-change-to-wdired-mode )
  (local-set-key (kbd "C-k") 'kill-dired-buffers)
  (setq truncate-lines nil)
 )
(add-hook 'dired-mode-hook 'dired-mode-keys)

;;;;;;;;;;;;;;;;;;;; ORG

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (flyspell-prog-mode)
            (setq org-src-fontify-natively t)
            ;;(my-keys-minor-mode 0) ;; disable my keys
            )
          t)

;;;;;;;;;;;;;;;;;;;; Ediff with git
;;[difftool "ediff"]
;;cmd = emacs --eval \"(ediff-files \\\"$LOCAL\\\" \\\"$REMOTE\\\")\"
;;# git difftool --tool=ediff --diff-filter=M tagname subdir
(add-hook 'ediff-prepare-buffer-hook (lambda () (whitespace-mode 1) ) t)

