
(add-to-list 'load-path "~/.emacs.d/modules")
;(add-to-list 'load-path "~/.emacs.d/modules/multiple-cursor")

(load "~/.emacs.d/modules/myfuncs.el")
;(require 'dumb-jump')

(defun comment-or-uncomment-this (&optional lines)
(interactive "P")
(if mark-active
(if (< (mark) (point))
(comment-or-uncomment-region (mark) (point))
(comment-or-uncomment-region (point) (mark)))
(comment-or-uncomment-region
(line-beginning-position)
(line-end-position lines))))


(defun infer-indentation-style ()
;; if our source file uses tabs, we use tabs, if spaces spaces, and if 
;; neither, we use the current indent-tabs-mode
(let ((space-count (how-many "^ " (point-min) (point-max)))
(tab-count (how-many "^\t" (point-min) (point-max))))
(if (> space-count tab-count) (setq indent-tabs-mode nil))
(if (> tab-count space-count) (setq indent-tabs-mode t))))


(defun g-ring()
(interactive)
(message "%s" global-mark-ring)
)

(defun unpop-global-mark()
;; Move cursor forward to the next mark sotred in the ring
(interactive)
(let (_buf
_pos
aaa)

;; (message "%s" global-mark-ring)
(setq _buf (marker-buffer (nth 0 (last global-mark-ring ))) )
(setq _pos (marker-position (nth 0 (last global-mark-ring ))) )
;; (message "buffer:%s pos %s" _buf _pos)
(setq m (point-marker))

(set-marker m _pos)
(switch-to-buffer _buf)

(goto-char _pos)

(setq aaa global-mark-ring)
;; (message "%s\n" aaa)
(setq _last (nth 0 (last aaa)))
;; (setq _first (first aaa))
;; (message "_first %s _last %s" _first _last)

;; (setq aaa (delete _first aaa)) ---
(setq aaa (delete _last aaa))
;; (message "%s\n" aaa)
(add-to-list 'aaa _last)
;; (setq aaa (append aaa _first) )
;; (message "%s\n----\n" aaa)

(setq global-mark-ring aaa)
)
)

(defun add-to-global-ring()
;; Force push a mark into a global ring even if it already exists
(interactive)
(let (_marker )
;; (activate-mark nil)
(setq _marker (make-marker))
(set-marker _marker (point))
(setq global-mark-ring (append (list _marker) global-mark-ring ) )

;; (setq deactivate-mark nil)

)
)

(defun toggle-window-dedicated ()
"Toggle whether the current active window is dedicated or not"
(interactive)
(message
(if (let (window (get-buffer-window (current-buffer)))
(set-window-dedicated-p window
(not (window-dedicated-p window))))
"Window '%s' is dedicated"
"Window '%s' is normal")
(current-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Save sessions history
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring) savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

(setq inhibit-splash-screen t)
(setq tramp-default-method "ssh")
(setq tab-width 2)

(recentf-mode 1)
(show-paren-mode 1)
;;(semantic-mode 1)
(setq x-select-enable-clipboard t)
(global-visual-line-mode t)
;;(setq use-file-dialog nil)
(setq make-backup-files nil)
(setq mouse-buffer-menu-mode-mult 10)
(global-auto-revert-mode t)

(define-key my-keys-minor-mode-map (kbd "<f2>") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "C-c 2") 'grep-find)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'get-file-path)
(define-key my-keys-minor-mode-map (kbd "C-c 3") 'get-file-path)
(define-key my-keys-minor-mode-map [f4] 'desktop-save-in-desktop-dir)
(define-key my-keys-minor-mode-map (kbd "<f6>") 'whitespace-mode)
(define-key my-keys-minor-mode-map (kbd "C-c 6") 'whitespace-mode)
(global-set-key (kbd "<f8>") 'ispell-word) ;; Flyspel
(global-set-key (kbd "C-c 8") 'ispell-word);; Flyspel
(define-key my-keys-minor-mode-map (kbd "<f9>") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c 9") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c t") (lambda () (setq tab-width 4)))

(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "M-<SPC>") 'set-mark-command)
(define-key my-keys-minor-mode-map (kbd "C-<SPC>") 'set-mark-command)

(define-key my-keys-minor-mode-map (kbd "C-;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-b") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo)


(define-key my-keys-minor-mode-map (kbd "C-c C-a") 'mark-whole-buffer)

;;;;;;;;;;;;;;;;;;; Jump around
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "C-c <up>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "M <up>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "M-<left>") 'pop-global-mark)
(define-key my-keys-minor-mode-map (kbd "M-<right>") 'unpop-global-mark)
(define-key my-keys-minor-mode-map (kbd "C-c  <left>") 'pop-global-mark)
(define-key my-keys-minor-mode-map (kbd "C-c <right>") 'unpop-global-mark)

(define-minor-mode my-keys-minor-mode
"A minor mode so that my key settings override annoying major modes."
t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;; C-key-bindings
(defun c-mode-keys()
(local-set-key (kbd "C-c <RET>") 'compile)
(local-set-key (kbd "C-c C-C") 'compile)
(local-set-key (kbd "<f5>") 'gdb)
(local-set-key [pause] 'toggle-window-dedicated)
)

(add-hook 'c++-mode-hook 'c-mode-keys) ;; TODO - pass a
(add-hook 'c++-mode-hook (lambda ()
(setq tab-width 2)
(setq comment-start "//" comment-end "")
(set-default 'truncate-lines nil)
))

;;;;;;;;;;;;;;;;;;; Gdb
(defun gdb-mode-keys()
(local-set-key (kbd "S-<up>") 'comint-previous-matching-input-from-input)
)
(add-hook 'gdb-mode-hook 'gdb-mode-keys)

;;;;;;;;;;;;;;;;;;; Python
(defun python-mode-keys()
(python-indent-guess-indent-offset)
(infer-indentation-style)
;;(setq indent-tabs-mode t)
(local-set-key (kbd "C->") 'python-indent-shift-right)
(local-set-key (kbd "C-<") 'python-indent-shift-left)
(setq tab-width 4)
)
(add-hook 'python-mode-hook 'python-mode-keys)

;;;;;;;;;;;;;;;;;;;; Dired
(defun dired-mode-keys()
(local-set-key (kbd "C-w") 'wdired-change-to-wdired-mode )
(local-set-key (kbd "C-k") 'kill-dired-buffers)
)
(add-hook 'dired-mode-hook 'dired-mode-keys) 

