; .emacs

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/multiple-cursors")
(load "~/.emacs.d/lisp/myfuncs.el")

(setq inhibit-splash-screen t)


(recentf-mode 1)
(tool-bar-mode -1)
(show-paren-mode 1)
;;(semantic-mode 1)
(global-visual-line-mode t)        ;; wrap text nicely
(setq x-select-enable-clipboard t)
;;(setq use-file-dialog nil)
(setq make-backup-files nil)
(set-face-attribute 'default nil :height 100) ;; default font size 10pt
(global-auto-revert-mode t)


;;;;;;;;;;;;;;;;;;; Mouse smooth-scroll
;;(set-variable 'scroll-conservatively 5)
(setq mouse-buffer-menu-mode-mult 100)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;;

;;(recentf-max-menu-items 20)
;;(recentf-max-saved-items 60)

;; Save sessions history
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring) savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

;;;;;;;;;;;;;;;;;;;; Set path to Anaconda python
;;(setq python-shell-interpreter "/home/kuba/anaconda2/bin/python")

;;;;;;;;;;;;;;;;;;;; Bookmarks - 'bm
(setq bm-restore-repository-on-load t)
(require 'bm)
(setq-default bm-buffer-persistence t)

;; Load bookmarks on file load
(add-hook 'find-file-hooks '(lambda nil (bm-load-and-restore)))

;; Save bookmarks on emacs exit
(add-hook 'kill-emacs-hook '(lambda nil            
            (bm-buffer-save-all)
            (bm-repository-save)))
;Update bookmark repository when saving the file.
(add-hook 'after-save-hook '(lambda nil 
            (bm-buffer-save)
            (bm-repository-save)
            ))

(require 'cl)                ;; otherwise loop macros will not be recognised
(require 'recentf)
;;(require 'wdired)
(require 'ace-jump-mode)
(require 'tabbar)
(tabbar-mode)
(require 'multiple-cursors)
;;;;;;;;;;;;;;;;;;; Initialize package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;;;; ELPA
;;(global-flycheck-mode)


;;;;;;;;;;;;;;;;;; Hooks
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t) (flyspell-prog-mode)
      (setq org-src-fontify-natively t)

      (org-babel-do-load-languages      
           'org-babel-load-languages
          '((ditaa . t)))
  
  (setq org-ditaa-jar-path         "/home/kuba/bin/emacs/share/emacs/25.3/lisp/contrib/scripts/ditaa0_9.jar")
      )
          t)

;;;;;;;;;;;;;;;;;; Company mode
;(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;; Autocomplete
(ac-config-default)


;;;;;;;;;;;;;;;;;;; Theme
;(push (substitute-in-file-name "~/.emacs.d/lisp/idea-darkula-theme") custom-theme-load-path)
 ;   (load-theme 'idea-darkula t)
  (load-theme 'tango-dark t)


;;;;;;;;;;;;;;;;;;; Key bindings


;; In order to make sure all the key bindgins work in all the moudules
;; defiene them in the minor mode. This will guarantee C-c C-a in c++ mode will work
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
;; ;;;;;;;;;;;;;;;;;;;; Bookmarks - 'bm
(define-key my-keys-minor-mode-map (kbd "C-c l") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-M-<up>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-c .")   'bm-next)   ;   >
(define-key my-keys-minor-mode-map (kbd "C-M-<right>")   'bm-next)   ;   >
(define-key my-keys-minor-mode-map (kbd "C-c ,") 'bm-previous) ;   <
(define-key my-keys-minor-mode-map (kbd "C-M-<left>") 'bm-previous) ;   <
(define-key my-keys-minor-mode-map (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(define-key my-keys-minor-mode-map (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(define-key my-keys-minor-mode-map (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

;;;;;;;;;;;;;;;;;;;; Windows move
(define-key my-keys-minor-mode-map (kbd "C-c <left>")  'windmove-left)   ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-c <up>")    'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-c <down>") 'windmove-down)

(define-key my-keys-minor-mode-map (kbd "s-<left>")  'windmove-left)   ;work also in terminal
(define-key my-keys-minor-mode-map (kbd "s-<right>") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "s-<up>")    'windmove-up)
(define-key my-keys-minor-mode-map (kbd "s-<down>") 'windmove-down)

;;;;;;;;;;;;;;;;;;;; Tabbar
(define-key my-keys-minor-mode-map (kbd "S-C-M-<left>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "S-C-M-<right>") 'tabbar-forward-tab)

;;;;;;;;;;;;;;;;;;;; ACE Jump
(define-key my-keys-minor-mode-map (kbd "C-c q")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "M-SPC")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "S-SPC")  'get-file-path)
(define-key my-keys-minor-mode-map (kbd "C-c w")  'ace-jump-word-mode)
(define-key my-keys-minor-mode-map (kbd "C-c e")  'ace-jump-line-mode)

(define-key my-keys-minor-mode-map (kbd "C-;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map "\C-l" 'goto-line) ; [Ctrl]-[L]   ; go to specifi line
(define-key my-keys-minor-mode-map (kbd "C-%") 'vi-type-paren-match)

;;;;;;;;;;;;;;;;;;;; Multiple cursors
(define-key my-keys-minor-mode-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-c C-v") 'mc/mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "C-c C-<SPC>") 'mc/mark-all-in-region)
(define-key my-keys-minor-mode-map (kbd "C-c C-d") 'mc/keyboard-quit)

;;;;;;;;;;;;;;;;;;;; Company
;(global-set-key "\t" 'company-complete-common)

;;;;;;;;;;;;;;;;;; F-keys
(define-key my-keys-minor-mode-map (kbd "<f2>") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'get-file-path)
(define-key my-keys-minor-mode-map [f4] 'desktop-save-in-desktop-dir)
(define-key my-keys-minor-mode-map (kbd "S-<f4>") 'desktop-change-dir)
(define-key my-keys-minor-mode-map (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f8>") 'ispell-word)   ;; Flyspel
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "<f10> c") (lambda ()
                      (interactive)
          (occur-1 "{$\\|)$" 1 (list (current-buffer)))  ) )
(define-key my-keys-minor-mode-map (kbd "<f10> p") (lambda ()
                      (interactive)
          (occur-1 "def\\|class" 1 (list (current-buffer))) ))      
;;(define-key my-keys-minor-mode-map (kbd "S-<f12>") 'goto-pydef)
(define-key my-keys-minor-mode-map (kbd "<f12>")  'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "<C-f12>") 'artist-mode)


;;;;;;;;;;;;;;;;;;; Jump around
;; across multiple buffers
(define-key my-keys-minor-mode-map (kbd "M-<left>") 'pop-global-mark)
(define-key my-keys-minor-mode-map (kbd "M-<right>") 'g-ring-unpop)
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>")  'g-ring-add-to)

;; within a buffer
(define-key my-keys-minor-mode-map (kbd "C-S-<left>") 'pop-to-mark-command)
(define-key my-keys-minor-mode-map (kbd "C-S-<right>") 'unpop-to-mark-command)

(define-key my-keys-minor-mode-map (kbd "S-C-<down>") 'semantic-complete-jump)
(define-key my-keys-minor-mode-map (kbd "C-M-<down>") 'dumb-jump-go-other-window)
(define-key my-keys-minor-mode-map (kbd "M-<down>") 'dumb-jump-go-current-window)
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'dumb-jump-back)

;; Overwrites the default behaviour and does not copy text into kill-ring
;;(global-set-key (kbd "C-k") 'delete-line)

;;; to jump back use - C-u C-<space>

;;;;;;;;;;;;;;;;;;;; Other
(define-key my-keys-minor-mode-map (kbd "C-c C-a")  'mark-whole-buffer)
;;; Partial completion
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)
(global-set-key (kbd "M-<backspace>") 'backward-kill-char-or-word)


;; C-h k - find the key-binding

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
   t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;; C-key-bindings
(defun c-mode-keys()
   (local-set-key (kbd "C-c <RET>") 'compile)
   (local-set-key (kbd "C-c C-c") 'compile)
   (local-set-key (kbd "<f5>") 'gud-gdb)
   (local-set-key [pause] 'toggle-window-dedicated)
   )

;;;;;;;;;;;;;;;;;;;; C-mode
;; Changing styl of comments in CC-mode
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))
(add-hook 'c-mode-hook 'c-mode-keys)
(add-hook 'c++-mode-hook 'c-mode-keys)  ;; TODO - pass a string to the compile


;;;;;;;;;;;;;;;;;;;; Gdb
(defun gdb-mode-keys()
   (local-set-key (kbd "S-<up>") 'comint-previous-matching-input-from-input)
)
(add-hook 'gdb-mode-hook 'gdb-mode-keys)

;;;;;;;;;;;;;;;;;;;; Python key-bindings
(defun python-mode-keys()
   (python-indent-guess-indent-offset)
   (infer-indentation-style)
   ;;(setq indent-tabs-mode t)
   (python-shell-completion-native-turn-on)
   
   (local-set-key (kbd "C-<") 'python-indent-shift-left)
   (local-set-key (kbd "C->") 'python-indent-shift-right)
   (local-set-key (kbd "C-c i") 'iimage-mode)
   (local-set-key (kbd "C-c r") 'refresh-iimages)
   (local-set-key (kbd "C-f <down>") 'python-nav-forward-defun)
   (local-set-key (kbd "C-f <up>") 'python-nav-backward-defun)
   ;(local-set-key (kbd "C-S-M <up>") 'previous-complete-history-element)
   ;(local-set-key (kbd "C-S-M  (kbd "<down>") 'next-complete-history-element)

)


(define-key my-keys-minor-mode-map (kbd "C-c <RET>") 'selectback-exec)
(define-key my-keys-minor-mode-map (kbd "S-C-c <RET>") 'selectback)
(define-key my-keys-minor-mode-map (kbd "s-<SPC>") 'goto-previous-point)

(add-hook 'python-mode-hook 'python-mode-keys)


;;;;;;;;;;;;;;;;;;;; Partial completion to shell and python-inferior mode
(defun shell-mode-keys()
   (local-set-key (kbd "S-<up>") 'comint-previous-matching-input-from-input)
   (local-set-key (kbd "S-<down>") 'comint-next-matching-input-from-input)
   ;;(local-set-key (kbd "<tab>") 'completion-at-point) 
   ;;(company-mode -1) ;; locks up emacs with the huge buffers
   )
(add-hook 'shell-mode-hook 'shell-mode-keys)

(add-hook 'inferior-python-mode-hook 'shell-mode-keys)

;;;;;;;;;;;;;;;;;;;; Dired
; open dired in the same window, mouseclicks not affected
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t) ;; pick the second open buffer as a destination
(defun dired-mode-keys()

   (local-set-key   (kbd "C-w") 'wdired-change-to-wdired-mode )
   (local-set-key (kbd "C-k") 'kill-dired-buffers)
   )

(add-hook 'dired-mode-hook 'dired-mode-keys)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-complete dumb-jump markdown-mode multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
