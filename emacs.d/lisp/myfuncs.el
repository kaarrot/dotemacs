(defun get-file-path ()
  (interactive)
  (message (buffer-file-name))
  (kill-new (buffer-file-name)) ;; copy to clipboard
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

;; Comment function
(defun comment-or-uncomment-this (&optional lines)
   (interactive "P")
   (if mark-active
      (if (< (mark) (point))
         (comment-or-uncomment-region (mark) (point))
         (comment-or-uncomment-region (point) (mark)))
      (comment-or-uncomment-region
         (line-beginning-position)
         (line-end-position lines))))

(defun goto-pydef()
  (interactive)	
  (let (start
	end
	phrase
	pos)
    (save-excursion
      (search-backward " ")
      (setq start  (goto-char (+ (point) 1)) )
      (search-forward "(") 
      (setq end  (goto-char (- (point) 1))   )
      
      (setq phrase (buffer-substring-no-properties start end))
					;(message "%s" phrase)
      
      (goto-char 0)
      (search-forward (message "def %s(" phrase ))
      (setq pos (point))
      )
    (goto-char pos)
    )
  )


;;;;; Disaply iamged
(defun refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t))

(add-to-list 'compilation-finish-functions 
             (lambda (buffer msg)
               (save-excursion
                 (set-buffer buffer)
                 (refresh-iimages))))

;;;;; Python helper function
(defun selectback-exec ()
  (interactive)

  (let (start
        end)

    (setq previous-point (point))

    (bm-previous)
    (beginning-of-line)  ;; make sure the mark will be at the begining of the next line
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
    ;; (refresh-iimages)  ;; this is optional



    (goto-char previous-point)
    (set-mark (point)) ;; (setq quit-flag t) ;; disable selected region (emacs25)

    ))

(defun selectback ()
  (interactive)

  (let (start
        end)

    (setq previous-point (point))

    (bm-previous)
    (beginning-of-line)
    (next-line)
    (setq start (point))
    (set-mark start)

    (bm-next)
    (end-of-line)
    (setq end (+ 1 (point)))

    ;; (message "%s... " positions)
    ;; (message "%s %s"start end)


    ;;; this makes the selection python specific - not sure if useful
    ;; (python-shell-send-region start end)

    ;; (sleep-for 1)
    ;; (refresh-iimages)  ;; this is optional



    ;; (goto-char previous-point)    
    ))


(defun goto-previous-point ()
  (interactive)
  (goto-char previous-point)
  )

(defun increment-nums (start-num times-num)
  (interactive "*r")
  (dotimes (start-num times-num) (insert (format "%d." (1+ start-num))))
  )


(defun my-insert-image (image-file) (interactive "fImage File: ") (insert-image (create-image image-file)))

(defun kill-dired-buffers ()
  "Kill all dired buffers except current buffer."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (kill-buffer buffer))
	   )
      )
    )
  )
