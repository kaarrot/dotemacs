(defun toggle-themes()
  (interactive)
   (if (string= (car custom-enabled-themes) 'tango-dark)
       (disable-theme 'tango-dark)
     (enable-theme 'tango-dark))
   )

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
    (when (> space-count tab-count)
        (message "using spaces")
        (setq indent-tabs-mode nil))
    (when (> tab-count space-count)
	(message "using tabs")
	(setq indent-tabs-mode t))
  ))


(defun g-ring()
  (interactive)
  (message "%s" global-mark-ring)
  )

(defun go-ring-back()
  (interactive)
  ;; Does the standard pop-from-global-mark but also
  ;; marks the current location in case we want to go forward.
  (add-to-global-ring)
  (pop-global-mark)  ;; pop current location
  (pop-global-mark))  
  
(defun go-ring-forward()
  ;; Reverse of pop-global-mark - Move cursor forward to the next mark sotred in the ring
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

; ^ ^ ^ ^ ^
; | | | | | 
; essentials 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-file-path ()
  (interactive)
  (kill-new (buffer-file-name)) ;; copy to clipboard
  (message (buffer-file-name))
)

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))


(defun goto-pydef()
  (interactive) 
  (let (start
	end
	phrase
	pos)
    (save-excursion
      (search-backward " ")
      (setq start (goto-char (+ (point) 1)) )
      (search-forward "(") 
      (setq end (goto-char (- (point) 1)) )

      (setq phrase (buffer-substring-no-properties start end))
					;(message "%s" phrase)
      
      (goto-char 0)
      (search-forward (message "def %s(" phrase ))
      (setq pos (point))
      )
    (goto-char pos)
    )
  )

;;;;; Python helper function
(defun selectback ()
  (interactive)
  
  (let (start
	end)

    (setq previous-point (point))
    
    (bm-previous)
    (beginning-of-line)
    (setq start (point))
    (set-mark start)

    (bm-next)
    (end-of-line)
    (setq end (+ 1 (point)))

    (message "%s %s"start end)

;;; this makes the selection python specific - not sure if useful
    (python-shell-send-region start end)
    (goto-char previous-point)
    ))

;;;;; Python helper function
(defun selectback-exec ()
  (interactive)
  (let (start
	end)

    (setq previous-point (point))

    (bm-previous)
    (beginning-of-line) ;; make sure the mark will be at the begining of the next line
    (next-line)
    (setq start (point))
    (set-mark start)

    (bm-next)
    (end-of-line)
    (setq end (+ 1 (point)))

    ;; (message "%s... " positions)
    (message "%s %s"start end)


;;; this makes the selection python specific - not sure if useful
    (python-shell-send-region start end)

    ;; (sleep-for 1)
    ;; (refresh-iimages) ;; this is optional

    (goto-char previous-point)
    (keyboard-quit) ;; disable selected region (emacs25)
    ))

(defun goto-previous-point ()
  (interactive)
  (goto-char previous-point)
  )

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun dumb-jump-set-include-paths (include-path)
  "Input Space separated list of include directories.
In order to avoid interfference form project denoters we set them off. To restore defaults just feed in empty input."
  (interactive "MSpecify include paths: ")
  (if (string= include-path "")  ;;restore project-denoters
      (setq dumb-jump-project-denoters (".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "PkgInfo" "-pkg.el"))
    (progn  ;;Disable project denoters
      (setq dumb-jump-project-denoters '())
      (setq dumb-jump-project include-path)
      (setq dumb-jump-grep-prefix "") ;; this was failing grep
      ;; In case things go wrong
      ;;(setq dumb-jump-debug 1)   if need to see the output of the command
      ;; dumb-jump-run-command  -> (message "_____%s " cmd)
      )
    )
  )
