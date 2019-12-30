
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

(dumb-jump-mode t)
(tabbar-mode)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;; Configuration


(setq python-shell-interpreter "python3")
(setq inhibit-splash-screen t)
(setq tramp-default-method "ssh")

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
(define-key my-keys-minor-mode-map (kbd "M-c <left>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "M-c <right>") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "C-S-M-<left>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "C-S-M-<right>") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "M-c M-<right>") 'tabbar-move-current-tab-one-place-right)
(define-key my-keys-minor-mode-map (kbd "M-c M-<left>") 'tabbar-move-current-tab-one-place-left)
; Moving tabs with Page-Up/Down
(define-key my-keys-minor-mode-map (kbd "C-<prior>") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "C-c ,") 'tabbar-backward-tab)
(define-key my-keys-minor-mode-map (kbd "C-<next>") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "C-c .") 'tabbar-forward-tab)
(define-key my-keys-minor-mode-map (kbd "<C-M-prior>") 'tabbar-move-current-tab-one-place-left)  ; C-S-M-Page Up
(define-key my-keys-minor-mode-map (kbd "M-c ,") 'tabbar-move-current-tab-one-place-left)
(define-key my-keys-minor-mode-map (kbd "C-M-<next>") 'tabbar-move-current-tab-one-place-right) ; C-S-M-Page Down
(define-key my-keys-minor-mode-map (kbd "M-c .") 'tabbar-move-current-tab-one-place-right)

;;;;;;;;;;;;;;;;;;;; ACE Jump
(define-key my-keys-minor-mode-map (kbd "M-SPC") 'ace-jump-char-mode)
(define-key my-keys-minor-mode-map (kbd "M-c SPC") 'ace-jump-char-mode)

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
    (grep-find (message "find . -name \"%s\" -print | xargs -I %% echo %%:1:" search-phrase))))
(define-key my-keys-minor-mode-map (kbd "M-c 2") (lambda (search-phrase) (interactive "Msearch file:")
    (grep-find (message "find . -name \"%s\" -print | xargs -I %% echo %%:1:" search-phrase))))

(define-key my-keys-minor-mode-map (kbd "<f3>") 'get-file-path)
(define-key my-keys-minor-mode-map (kbd "C-c 3") 'get-file-path)
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
(define-key my-keys-minor-mode-map (kbd "C-c t") (lambda () (interactive) (setq tab-width 4)))

(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)
(define-key my-keys-minor-mode-map (kbd "C-c <SPC>") 'add-to-global-ring)
(define-key my-keys-minor-mode-map (kbd "C-<SPC>") 'set-mark-command)

(define-key my-keys-minor-mode-map (kbd "C-;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-c ;") 'comment-or-uncomment-this)
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo)
(define-key my-keys-minor-mode-map (kbd "C-c C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map "\C-l" 'goto-line)

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

;; Unbind existing key sequence first
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c u") 'winner-undo)
(global-set-key (kbd "C-c 0") 'winner-undo)
(global-set-key (kbd "M-c r") 'winner-redo)

;;;;;;;;;;;;;;;;;;;
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
  ; compile_flags.txt (in root) - specify -I/path_to_include
  (local-set-key (kbd "C-x 5") (lambda () (interactive)
                                (setq lsp-clients-clangd-executable "~/bin/clangd")
                                (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))       
                                (lsp)
                                ))
  (local-set-key (kbd "C-c 5") (lambda () (interactive)
                                    (require 'cquery)
                                    (setq cquery-executable "~/bin/cquery")
                                    (setq cquery-extra-args  '("--log-all-to-stderr") )
                                    (setq cquery-cache-dir "/tmp/.cquery_cached_index")
                                    (lsp)
                                    ))
  (local-set-key (kbd "S-<f5>") 'toggle-window-dedicated)
  (local-set-key (kbd "C-c <RET>") (lambda () (interactive)
    (setq compile-command (message "g++ -O0 -g -std=c++11 -I. %s -o a.out" (buffer-file-name))) )) 

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
(eval-after-load 'dired '(progn (require 'single-dired)))
(defun dired-mode-keys()
  (local-set-key (kbd "C-w") 'wdired-change-to-wdired-mode )
  (local-set-key (kbd "C-k") 'kill-dired-buffers)
  ;; (set-default 'truncate-lines nil)
 )
(add-hook 'dired-mode-hook 'dired-mode-keys)

;;;;;;;;;;;;;;;;;;;; ORG

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (flyspell-prog-mode)
            (setq org-src-fontify-natively t)
            ;;(my-keys-minor-mode 0) ;; disable my keys
            (local-set-key (kbd "M-<up>") 'org-table-move-row-up )
            (local-set-key (kbd "M-<down>") 'org-table-move-row-down )
            (local-set-key (kbd "M-S-<up>") 'org-table-move-row-down )


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
          t)



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
