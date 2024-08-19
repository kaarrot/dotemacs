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

(defun reindent-buffer (_from _to)
  (interactive "nFrom width:\nnTo width:" )
  (let (endpos)
    (save-excursion (end-of-buffer)
                    (setq endpos (point)))

    (setq tab-width _from)
    (setq indent-tabs-mode t)
    (tabify 0 endpos)
    (setq tab-width _to)

    ;(setq-default indent-tabs-mode nil)
    ;(setq indent-line-function 'insert-tab)
    ;(indent-region 0 endpos)

    (save-excursion (end-of-buffer)
                    (setq endpos (point)))

    (untabify 0 endpos)
    (setq tab-width _to)
    (setq indent-tabs-mode nil)
    )
  )

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


; helper function to match-paren
(defun move-until (direction start-char stop-char)

  (let (index)

    ;; if cursor is not on valid start-char
    ;; set index to 0 and exit
    (setq index 1)
    (if (not (= (following-char) start-char))
        (setq index 0))

   (while (> index 0)
      (if (= (point) 0) ; if we reach begin of buffer
          (setq (index 0))
        )
      ;; chose which direction to move
      (if (= direction -1)
          (backward-char 1)
        (forward-char 1))
        
      ;;(message "%s" (char-to-string (following-char)))
      
      ;; TODO check if one character before is not escape character (backslash)
      ;; decrement
      (if (= (following-char) stop-char )
          (setq index (- index 1))
          ;(message "stop-char, index %s" index)
        )
      ;; increment
      (if (= (following-char) start-char )
          (setq index (+ index 1))
          ;(message "start-char, index %s" index)
        )
      ) ;; end of while
   ) ;; end of let
  )

(defun match-paren (arg)
  "Go to the matching paren if on a paren;"
  (interactive "p")
  (let (index
        start-char
        stop-char
        completed
        )

    (setq completed nil)

    ;; matching: ()
    (cond ( (= (following-char) ?) ) 
          ;; Move backward
          (progn
            (setq start-char ?) ) 
          (setq stop-char ?( )
          (move-until -1 start-char stop-char)
          (setq completed t)
          ) ;; end of progn
    )) 
  
    ;; The previous opperation can not be avaulated (completed == nil)
    ;; In order to this one to take place
    (cond ( (and (eq completed nil) (= (following-char) ?( ) )
          ;; Move forward
          (progn
            (setq start-char ?( ) 
            (setq stop-char ?) )
            (move-until 1 start-char stop-char)
            (setq completed t)
            )
    ))

    ;; matching: {}
    (cond ( (and (eq completed nil) (= (following-char) ?} ) )
          ;; Move backward
          (progn
            (setq start-char ?} ) 
            (setq stop-char ?{ )
            (move-until -1 start-char stop-char)
            (setq completed t)
          ) ;; end of progn
    )) 
  
    ;; The previous opperation can not be avaulated (completed == nil)
    ;; In order to this one to take place
    (cond ( (and (eq completed nil) (= (following-char) ?{ ) )
          ;; Move forward
          (progn
            (setq start-char ?{ ) 
            (setq stop-char ?} )
            (move-until 1 start-char stop-char)
            (setq completed t)
          )
    ))
          
          
   ) ;; end of let
  ) ;;end of fun


;; This function works in combination with Occur mode and org timestamps
;; If we filter out lines with timestamps from the org-mode buffer
;; we an sort them (starting from the latest) in the Occur buffer and
;; still jump to corresponding line.
(defun occur-mode-sort-by-org-timestamp ()
(interactive)
   (if (get-buffer "*Occur*")
       (save-excursion
         (set-buffer (get-buffer "*Occur*"))
         
         (toggle-read-only 0)
         ;;(inhibit-read-only t)
         
         (let (START END)
           (progn
             ;; make sure we dont wrap lines pr next-line still may be read only
             (toggle-truncate-lines t)
             ;; Skips first line in Occur buffer
             ;; If you get read-only error and the Occur buffer is not sorted,
             ;; the (Occur) buffer most likely wraps around multiple lines and needs to be unwrapped using by user
             (beginning-of-buffer)
             (next-line)
             (setq START (point))
             (end-of-buffer)
             (setq END (point))
             ;;(sort-regexp-fields t "TODO.*"  ".*" START  END)
             ;; This take into account line numberso in occur mode but skips then while sorting
             ;; It is still not perfect and jumping to exact line may not work
             ;; <.*> should match any org-mode timestamp
             (sort-regexp-fields t "[0-9]+.*"  "<.*>" START END) 
             ) ; end of progn
         ) ; end of let 
       )
     (message "There is no buffer named \"*Occur*\".")) )


(setq occur-timestamp-sort-substring "TODAY")
(defun occur-timestamp-sort(search_string)
  "A helper function to automate frequent 3 steps to sort search string starting from the latest.
One requirement is follow the search string by Org timestamp: C-u C-c .
"
  (interactive
   (list (read-string (format "Search and sort string [%s]: " occur-timestamp-sort-substring) nil nil occur-timestamp-sort-substring)))

  (occur search_string)
  (switch-to-buffer-other-window "*Occur*")
  (occur-mode-sort-by-org-timestamp)
  (next-error-follow-minor-mode 1) ;; disable if 0 or negative
  )


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

;;;;; Select text region between two bookmarks (bm-toggle)
(defun selectback ()
  (interactive)
  ;; Returns list of (start end) positions
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

    ;(message "%s %s"start end)

    (list start end) 
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
  (interactive "GSpecify include paths: ")
  ;; (message "%s" include-path)
  (if (string= include-path "") ;; set dumb-jump-project to nil so that grep will search current directory
      (setq dumb-jump-project nil)
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

(defun dumb-jump-append-include-path (include-path)
  "Append path to the existing dumb-jump-project."
  (interactive "GSpecify path to append to dumb-jump-project: ")
  (if dumb-jump-project
      (setq dumb-jump-project (message "%s %s" dumb-jump-project include-path))
    (setq dumb-jump-project include-path))
  )

;;; Dropbox
(defun dropbox-send (file-path)
  (interactive "bSpecify buffer name: ")
  (setq rclone-send (message "rclone copy %s dropbox:" (buffer-file-name)))
  (message "%s" rclone-send)
  (save-buffer)
  
  (shell-command rclone-send)
  )


(defun dropbox-get (file-path)
  (interactive "bSpecify buffer name: ")
  (setq rclone-receive (message "rclone copy dropbox:%s ." (buffer-file-name)))
  (message "%s" rclone-receive)
  (shell-command rclone-receive)
  (revert-buffer nil t)
  )

(defun dropbox-ls (total)
  (interactive "nShow recently modified files: ")
  (setq rclone-recent (message "rclone lsl --max-depth 1 dropbox: 2>&1 | grep -oP \"([0-9]{4}-[0-9]{2}-[0-9]{2}).*\" | sort | tail -%s" total ))
  (shell-command rclone-recent)
  )

;;; Google drive
(defun google-send (file-path)
  (interactive "bSpecify buffer name: ")
  (setq rclone-send (message "rclone copy %s google:" (buffer-file-name)))
  (message "%s" rclone-send)
  (save-buffer)
  (shell-command rclone-send)
  )


(defun google-get (file-path)
  (interactive "bSpecify buffer name: ")
  (setq rclone-receive (message "rclone copy google:%s ." (buffer-file-name)))
  (message "%s" rclone-receive)
  (shell-command rclone-receive)
  (revert-buffer nil t)
  )

(defun google-ls (total)
  (interactive "nShow recently modified files: ")
  (setq rclone-recent (message "rclone lsl --max-depth 1 google: 2>&1 | grep -oP \"([0-9]{4}-[0-9]{2}-[0-9]{2}).*\" | sort | tail -%s" total ))
  (shell-command rclone-recent)
  )




(defun occur-methods (search-phrase)
  (interactive "Msearch phrase (empty will list methods):")

  ;; If no phrease was specified the search will list all functions in the file
  (if (string= search-phrase "")
      (progn
        ;; Extract extension of the current buffer
        (let ((extension (pop (cdr (s-split "\\." (message "%s" (current-buffer))))) ))
          (message "---%s---" extension)
          (when (member extension '("C" "c" "cpp" "cc" "cxx" "h" "hh" "hpp"))
            (message "searching cpp")
            ;; command regex: \([a-zA-Z_]\)\([a-zA-Z0-9_]*\)\([ ]+\)\([a-zA-Z_][a-zA-Z0-9_:]*\)(
            ;;(occur "\\([a-zA-Z_]\\)\\([a-zA-Z0-9_]*\\)\\([ ]+\\)\\([a-zA-Z_][a-zA-Z0-9_:]*\\)(") # previous version
            ;; perl (^ *(?!for|if|throw)\b([A-Za-z_ ][A-Za-z0-9_]*+\b|::)*+) *(\() # with negative lookahead
            ;; emacs does not support that - no way to exlude words
            ;; for now it is better to list more, so also includes if|while|for etc
            ;; command: \(^\( \)*\b\([A-Z_a-z ][0-9A-Z_a-z_]*+\b\|::\)*+\) *\((\)
            ;; elisp expression:
            (occur "\\(^\\( \\)*\\b\\([A-Z_a-z ][0-9A-Z_a-z_]*+\\b\\|::\\)*+\\) *\\((\\)")
            )
          (when (member extension '("py"))
            (message "searchin python")
            (occur "\\(def\\|class\\)")
            )
          )
        )
      (progn
        ;;Equivalent to  "^\* .*cmake" in plain occur
        (message "%s" "search top level in org-mode")
        (occur (message "^\\* .*%s" search-phrase))
      )
    )
)

(defun sort-buffers ()
    "Put the buffer list in alphabetical order."
    (interactive)
    (dolist (buff (buffer-list-sorted)) (bury-buffer buff))
    (when (interactive-p) (list-buffers))
    )

(defun buffer-list-sorted ()

    (sort (buffer-list)
        (function
         (lambda
           (a b) (string<
                  ;; (downcase (buffer-name a))
                  ;; (downcase (buffer-name b))
                  (buffer-file-name a)
                  (buffer-file-name b)
                  )))))

(defun tabbar-move-current-tab-one-place-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
	 (old-bufs (tabbar-tabs bufset))
	 (first-buf (car old-bufs))
	 (new-bufs (list)))
    (if (string= (buffer-name) (format "%s" (car first-buf)))
	old-bufs ; the current tab is the leftmost
      (setq not-yet-this-buf first-buf)
      (setq old-bufs (cdr old-bufs))
      (while (and
	      old-bufs
	      (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
	(push not-yet-this-buf new-bufs)
	(setq not-yet-this-buf (car old-bufs))
	(setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
	  (progn
	    (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
	    (push not-yet-this-buf new-bufs)
	    (setq new-bufs (reverse new-bufs))
	    (setq new-bufs (append new-bufs (cdr old-bufs))))
	(error "Error: current buffer's name was not found in Tabbar's buffer list."))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update))))

(defun tabbar-move-current-tab-one-place-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
	 (old-bufs (tabbar-tabs bufset))
	 (first-buf (car old-bufs))
	 (new-bufs (list)))
    (while (and
	    old-bufs
	    (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
      (push (car old-bufs) new-bufs)
      (setq old-bufs (cdr old-bufs)))
    (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
	(progn
	  (setq the-buffer (car old-bufs))
	  (setq old-bufs (cdr old-bufs))
	  (if old-bufs ; if this is false, then the current tab is the rightmost
	      (push (car old-bufs) new-bufs))
	  (push the-buffer new-bufs)) ; this is the tab that was to be moved
      (error "Error: current buffer's name was not found in Tabbar's buffer list."))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (tabbar-set-template bufset nil)
    (tabbar-display-update)))

 (defun track-shell-directory/procfs ()
    (shell-dirtrack-mode 0)
    (add-hook 'comint-preoutput-filter-functions
              (lambda (str)
                (prog1 str
                  (when (string-match comint-prompt-regexp str)
                    (cd (file-symlink-p
                         (format "/proc/%s/cwd" (process-id
                                                 (get-buffer-process
                                                  (current-buffer)))))))))
              nil t))

(defun lazy-backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))

(setq show-dirs-substring "png")
(defun show-dirs-without-substring(search_string)
  "List directories from the current dired level which do not contain specific substring"
  (interactive
   (list (read-string (format "Search for substring [%s]: " show-dirs-substring) nil nil show-dirs-substring)))
  
  (let (my-list)
    (setq my-list (f-directories (expand-file-name default-directory)))
    (while my-list
      (setq this_dir (car my-list))
      (setq my-list (cdr my-list))
      (setq found (directory-files this_dir nil search_string))
      (if (not found)
          (message "%s" this_dir)
        )
      )
    (message "-------")
    )
  )
