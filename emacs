
;; ;; .emacs


(add-to-list 'load-path "~/.emacs.d/")
(load "~/.emacs.d/myfuncs.el")

(setq inhibit-splash-screen t)


(recentf-mode 1)
(tool-bar-mode -1)
(show-paren-mode 1)
(setq x-select-enable-clipboard t)
;;(setq use-file-dialog nil)
(setq make-backup-files nil)
(set-variable 'scroll-conservatively 5)
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

;; ;;;;;;;;;;;;;;;;;;;; ACE Jump
(define-key my-keys-minor-mode-map (kbd "C-c q")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "M-SPC")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "S-SPC")  'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "C-c w")  'ace-jump-word-mode)
(define-key my-keys-minor-mode-map (kbd "C-c e")  'ace-jump-line-mode)

;;;;;;;;;;;;;;;;;; F-keys
(define-key my-keys-minor-mode-map (kbd "<f2>") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'get-file-path)
(define-key my-keys-minor-mode-map [f4] 'bubble-buffer) 
(define-key my-keys-minor-mode-map (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f8>") 'ispell-word)   ;; Flyspel
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "S-<f12>") 'goto-pydef)


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
   t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)
