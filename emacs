;; .emacs

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
(set-variable 'scroll-conservatively 5)
(setq mouse-buffer-menu-mode-mult 10)

;; (recentf-max-menu-items 20)
;; (recentf-max-saved-items 60)

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
(global-flycheck-mode)


;;;;;;;;;;;;;;;;;; Hooks
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t) (flyspell-prog-mode)
	    (setq org-src-fontify-natively t)
	    )
          t)

(add-hook 'after-init-hook 'global-company-mode)


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
(define-key my-keys-minor-mode-map (kbd "M-<right>")   'bm-next)   ;   >
(define-key my-keys-minor-mode-map (kbd "C-c ,") 'bm-previous) ;   <
(define-key my-keys-minor-mode-map (kbd "M-<left>") 'bm-previous) ;   <
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
;;(define-key my-keys-minor-mode-map (kbd "C-M-<left>") 'tabbar-backward)
;;(define-key my-keys-minor-mode-map (kbd "C-M-<right>") 'tabbar-forward)

;;;;;;;;;;;;;;;;;;;; ACE Jump
(define-key my-keys-minor-mode-map (kbd "C-c q")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "M-SPC")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "S-SPC")  'get-file-path)
(define-key my-keys-minor-mode-map (kbd "C-c w")  'ace-jump-word-mode)
(define-key my-keys-minor-mode-map (kbd "C-c e")  'ace-jump-line-mode)

(define-key my-keys-minor-mode-map (kbd "C-;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map "\C-l" 'goto-line) ; [Ctrl]-[L]   ; go to specifi line

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
(define-key my-keys-minor-mode-map [f4] 'bubble-buffer) 
(define-key my-keys-minor-mode-map (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f8>") 'ispell-word)   ;; Flyspel
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "S-<f12>") 'goto-pydef)

;;;;;;;;;;;;;;;;;;; Jump around
(define-key my-keys-minor-mode-map (kbd "C-M-<left>") 'pop-global-mark)
(define-key my-keys-minor-mode-map (kbd "C-M-<right>") 'pop-to-mark-command)
;;(define-key my-keys-minor-mode-map (kbd "C-M-<down>") 'semantic-complete-jump)
(define-key my-keys-minor-mode-map (kbd "C-M-<down>") 'dumb-jump-go-other-window)
(define-key my-keys-minor-mode-map (kbd "M-<down>") 'dumb-jump-go-current-window)
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'dumb-jump-back)


;;; to jump back use - C-u C-<space>

;;;;;;;;;;;;;;;;;;;; Other
(define-key my-keys-minor-mode-map (kbd "C-c C-a")  'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "<f12>")  'revert-buffer)
;;; Partial completion
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)


;; C-h k - find the key-binding

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
   t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;; C-key-bindings
(defun c-mode-keys()
   (local-set-key (kbd "C-c <RET>") 'compile)
   (local-set-key (kbd "C-c C-C") 'compile))

;;;;;;;;;;;;;;;;;;;; C-mode
;; Changing styl of comments in CC-mode
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))
(add-hook 'c-mode-hook 'c-mode-keys)
(add-hook 'c++-mode-hook 'c-mode-keys)  ;; TODO - pass a string to the compile

;;;;;;;;;;;;;;;;;;;; Python key-bindings
(defun python-mode-keys()
   (local-set-key (kbd "<backtab>") 'python-indent-shift-left)
   (local-set-key (kbd "C-<tab>") 'python-indent-shift-right)
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
(setq python-shell-interpreter "/home/kuba/miniconda2/bin/python")

;;;;;;;;;;;;;;;;;;;; Partial completion to shell and python-inferior mode
(defun shell-mode-keys()
   (local-set-key (kbd "S-<up>") 'comint-previous-matching-input-from-input)
   (local-set-key (kbd "S-<down>") 'comint-next-matching-input-from-input)
   )
(add-hook 'shell-mode-hook 'shell-mode-keys)
(add-hook 'inferior-python-mode-hook 'shell-mode-keys)

;;;;;;;;;;;;;;;;;;;; Dired
; open dired in the same window, mouseclicks not affected
(put 'dired-find-alternate-file 'disabled nil)
(defun dired-mode-keys()

   (local-set-key   (kbd "C-w") 'wdired-change-to-wdired-mode )
   (local-set-key (kbd "C-k") 'kill-dired-buffers)
   )

(add-hook 'dired-mode-hook 'dired-mode-keys)


(load "/home/kuba/.opam/4.02.1/share/emacs/site-lisp/tuareg-site-file")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-abort-manual-when-too-short t)
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (41 46)))
 '(dumb-jump-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
