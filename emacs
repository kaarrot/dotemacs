
;; ;; .emacs


(add-to-list 'load-path "~/.emacs.d/")

(setq inhibit-splash-screen t)


(recentf-mode 1)
(tool-bar-mode -1)
(show-paren-mode 1)
(setq x-select-enable-clipboard t)
;;(setq use-file-dialog nil)
(setq make-backup-files nil)
(set-variable 'scroll-conservatively 5)


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
(require 'wdired)


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

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
   t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)
