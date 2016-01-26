
;; ;; .emacs


(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(load "~/.emacs.d/myfuncs.el")

(setq inhibit-splash-screen t)


(recentf-mode 1)
(tool-bar-mode -1)
(show-paren-mode 1)
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
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;;;; ELPA
(add-hook 'after-init-hook 'global-company-mode)
(global-flycheck-mode)


;;;;;;;;;;;;;;;;;; Hooks
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t) (flyspell-prog-mode)
	    (setq org-src-fontify-natively t)
	    )
          t)

;;;;;;;;;;;;;;;;;;; Theme
(push (substitute-in-file-name "~/.emacs.d/idea-darkula-theme/") custom-theme-load-path)
    (load-theme 'idea-darkula t)

;;;;;;;;;;;;;;;;;;; Key bindings

;; In order to make sure all the key bindgins work in all the moudules
;; defiene them in the minor mode. This will guarantee C-c C-a in c++ mode will work
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
;; ;;;;;;;;;;;;;;;;;;;; Bookmarks - 'bm
(define-key my-keys-minor-mode-map (kbd "C-c l") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'bm-toggle)
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

;;;;;;;;;;;;;;;;;;;; ACE Jump
(define-key my-keys-minor-mode-map (kbd "C-c q")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "M-SPC")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "S-SPC")  'ace-jump-char-mode)
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
(global-set-key "\t" 'company-complete-common)

;;;;;;;;;;;;;;;;;; F-keys
(define-key my-keys-minor-mode-map (kbd "<f2>") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'get-file-path)
(define-key my-keys-minor-mode-map [f4] 'bubble-buffer) 
(define-key my-keys-minor-mode-map (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f8>") 'ispell-word)   ;; Flyspel
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "S-<f12>") 'goto-pydef)

;;;;;;;;;;;;;;;;;;;; Other
(define-key my-keys-minor-mode-map (kbd "C-c C-a")  'mark-whole-buffer)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
   t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
