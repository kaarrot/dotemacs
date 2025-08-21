
;; Limit prompts when opening new ediff session and previous file already exist
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)  ;; optional: suppress messages

;; Red / Green diff colors
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)  ;; optional: suppress messages

(custom-set-faces
 '(ediff-fine-diff-A ((t (:background "dark red" :foreground "white"))))
 '(ediff-fine-diff-B ((t (:background "dark green" :foreground "white"))))
 '(ediff-even-diff-A ((t (:background "red4"))))
 '(ediff-even-diff-B ((t (:background "green4"))))
 '(ediff-odd-diff-A ((t (:background "red3"))))
 '(ediff-odd-diff-B ((t (:background "green3")))))

(add-hook 'ediff-mode-hook
          (lambda ()
            (font-lock-mode -1)))

;; Helper to start ediff session from linline vc-diff buffer 
(defun my-vc-diff-ediff-current-hunk ()
  "Run Ediff on the file under point in a `vc-diff` buffer using the revision shown in the diff.
The temporary file is named after the original file and revision."
  (interactive)
  (unless (derived-mode-p 'diff-mode)
    (user-error "Not in a vc-diff buffer"))

  ;; Quit any running Ediff session
  (when (get-buffer "*Ediff Control Panel*")
    (with-current-buffer "*Ediff Control Panel*"
      (ediff-quit t)))

  ;; Get repository root
  (let* ((repo-root (vc-call-backend 'Git 'root default-directory))
         (files (diff-hunk-file-names))
         (raw-file (or (cdr files) (car files)))
         ;; Strip Git diff prefixes 'a/' or 'b/'
         (file-name (replace-regexp-in-string "\\`[ab]/" "" 
                                              (substring-no-properties
                                               (if (listp raw-file) (car raw-file) raw-file))))
         (file (expand-file-name file-name repo-root))
         old-rev tmp-file)
    (unless (and file (file-exists-p file))
      (user-error "Working file does not exist: %s" file))

    ;; Get old revision
    (setq old-rev (car diff-vc-revisions))
    (unless old-rev
      (user-error "Could not determine old revision from diff"))

    ;; Create temporary file with meaningful name
    (setq tmp-file (expand-file-name
                    (format "%s.%s.diff"
                            (file-name-nondirectory file)
                            (substring old-rev 0 (min 8 (length old-rev))))
                    temporary-file-directory))

    ;; Fill temp file with old revision content
    (with-temp-buffer
      (call-process "git" nil t nil "show" (concat old-rev ":" file-name))
      (write-region (point-min) (point-max) tmp-file))

    

    ;; Run Ediff
    (ediff-files tmp-file file)))


(defun my-ediff-kill-buffers-on-quit ()
  "Kill both Ediff buffers when quitting Ediff."
  (when (buffer-live-p ediff-buffer-A)
    (kill-buffer ediff-buffer-A))
  (when (buffer-live-p ediff-buffer-B)
    (kill-buffer ediff-buffer-B)))

(add-hook 'ediff-quit-hook #'my-ediff-kill-buffers-on-quit)
    
(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "e") #'my-vc-diff-ediff-current-hunk))   


