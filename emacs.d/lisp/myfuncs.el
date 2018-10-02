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

(defun vi-type-paren-match (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (let ((oldpos (point)))
    (cond
     ((looking-at "[{([]")
      (forward-sexp 1) (backward-char))
     ((looking-at "[])}]")
      (forward-char)
      (condition-case nil
          (progn
            (backward-sexp 1)
            (while (not (looking-at "[{([]")) (forward-char)))
        (error (progn
                 (backward-char)
                 (error (message "Unbalanced parentheses"))))))
     (t (self-insert-command (or arg 1))))))

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
    (keyboard-quit)  ;; disable selected region (emacs25)

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

(defun delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))


(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
	  (goto-char pos)
	(setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
	(set-marker (mark-marker) pos)
	(setq mark-ring (nbutlast mark-ring))
	(goto-char (marker-position (car (last mark-ring))))))))

(defun g-ring() ;; print the current globali ring list
  (interactive)
  (message "%s" global-mark-ring)
  )



(defun g-ring-unpop()
        (interactive)
        ;; (message "%s" global-mark-ring)
        (setq _buf (marker-buffer (nth 0 (last global-mark-ring  ))) )
        (setq _pos (marker-position (nth 0 (last global-mark-ring  ))) )
        ;; (message "buffer:%s"_buf)
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


(defun g-ring-add-to()
  ;; Force push a mark into a global ring even if it already exists
  (interactive)
  (let (_marker )
        (setq deactivate-mark nil)

         (setq _marker (make-marker))
         (set-marker _marker (point))
         (setq global-mark-ring (append (list _marker) global-mark-ring ) )
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


(defvar previous-major-mode nil)
(make-variable-buffer-local 'previous-major-mode)
(put 'previous-major-mode 'permanent-local t)


(defun ditaa/insert-src-template ()
  (interactive)
  (insert
"#+begin_src ditaa :file ./helloworld.png

+--------+   +-------+    +-------+
|        +---+ ditaa +--> |       |
|  Text  |   +-------+    |diagram|
|Document|   |!magic!|    |       |
|     {d}|   |       |    |       |
+---+----+   +-------+    +-------+
    :                         ^
    |       Lots of work      |
    +-------------------------+

/--------\   +-------+
|cAAA    +---+Version|
|  Data  |   |   V3  |
|  Base  |   |cRED{d}|
|     {s}|   +-------+
\---+----/
     
     
+---+-----+   +----------+
| cBLU    |   | {io}     |
| Ext-Foo |   |  S-ATA   |
|   +-----+   |   cFEA   |
|   |cPNK |   +----------+
|   | Foo |  
+---+-----+


/-------------+-------------\
|cRED RED     |cBLU BLU     |
+-------------+-------------+
|cGRE GRE     |cPNK PNK     |
+-------------+-------------+
|cAAA               AAA     |
+-------------+-------------+
|cCCC               CCC     |
+-------------+-------------+
|cBLK BLK     |cYEL YEL     |
\-------------+-------------/

#+end_src"))


