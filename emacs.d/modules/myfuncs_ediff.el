
;; Limit prompts when opening new ediff session and previous file already exist
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)  ;; optional: suppress messages

;; Red / Green diff colors

(with-eval-after-load 'ediff
  (custom-set-faces
   ;; '(ediff-fine-diff-A ((t (:background "dark red" :foreground "white"))))
   ;; '(ediff-fine-diff-B ((t (:background "dark green" :foreground "white"))))
   '(ediff-even-diff-A ((t (:background "red4"))))
   '(ediff-even-diff-B ((t (:background "green4"))))
   ;; '(ediff-odd-diff-A ((t (:background "red3"))))
   ;; '(ediff-odd-diff-B ((t (:background "green3"))))
   )
  ;; Override ediff-quit to skip all prompts and clean up temp buffers
  (defun ediff-quit (reverse-default-keep-variants)
    "Quit ediff without confirmation, killing temp revision buffers."
    (interactive "P")
    (let ((buf-a ediff-buffer-A)
          (buf-b ediff-buffer-B)
          (ediff-keep-variants t))  ;; tell ediff to keep buffers (no prompt)
      (ediff-really-quit reverse-default-keep-variants)
      ;; now kill temp revision buffers ourselves
      (let ((kill-buffer-query-functions nil))
        (when (and (buffer-live-p buf-a)
                   (string-match "\\.~.*~$" (buffer-name buf-a)))
          (with-current-buffer buf-a (set-buffer-modified-p nil))
          (kill-buffer buf-a))
        (when (and (buffer-live-p buf-b)
                   (string-match "\\.~.*~$" (buffer-name buf-b)))
          (with-current-buffer buf-b (set-buffer-modified-p nil))
          (kill-buffer buf-b))))))

 ;; Store revisions for later ediff
 (defvar my/stored-revisions nil
   "Stored revisions from log-view for later ediff.")

 (defun my/log-view-store-region-revisions ()
   "Store revisions from mark and point, ordered old to new."
   (when (derived-mode-p 'log-view-mode)
     (let* ((mark-pos (mark))
            (point-pos (point))
            (first-rev (save-excursion
                         (goto-char mark-pos)
                         (log-view-current-tag)))
            (second-rev (log-view-current-tag)))
       (when (and first-rev second-rev)
         ;; In reverse chronological log: earlier position = newer commit
         ;; We want (older-rev, newer-rev) for ediff
         (setq my/stored-revisions
               (if (< mark-pos point-pos)
                   ;; Mark is earlier (newer), point is later (older)
                   (list second-rev first-rev)
                 ;; Point is earlier (newer), mark is later (older)
                 (list first-rev second-rev)))
         (message "Stored revisions (old→new): %s and %s"
                  (car my/stored-revisions)
                  (cadr my/stored-revisions))))))


(defun my/ediff-stored-revisions ()
  "Ediff file at point using stored revisions or parse from diff buffer."
  (interactive)
  (require 'ediff-vers)
  (let ((file (diff-find-file-name))
        (revert-without-query '(".*")))
    (if (not file)
        (message "Cannot find file at point")
      (let ((clean-file (substring-no-properties file)))
        ;; Ensure absolute path
        (unless (file-name-absolute-p clean-file)
          (setq clean-file (expand-file-name clean-file default-directory)))
        (cond
         ;; Case 1: Two stored revisions
         ((and my/stored-revisions (= (length my/stored-revisions) 2))
          (with-current-buffer (find-file-noselect clean-file)
            (ediff-vc-internal (car my/stored-revisions)
                              (cadr my/stored-revisions)
                              nil)))
         
         ;; Case 2: Single revision vs working tree
         (t
          (let ((rev (my/extract-revision-from-diff)))
            (if rev
                ;; Call with empty string for working tree
                (let ((rev1 rev)
                      (rev2 ""))
                  (with-current-buffer (find-file-noselect clean-file)
                    (ediff-vc-internal rev1 rev2 nil)))
              (message "No stored revisions and couldn't parse diff header")))))))))
         
(defun my/extract-revision-from-diff ()
  "Extract revision from current diff buffer header."
  (save-excursion
    (goto-char (point-min))
    ;; Look for "diff --git" or similar patterns
    (when (re-search-forward "^diff.*" nil t)
      (forward-line -1)
      ;; Try to find revision in various formats
      (or
       ;; Git: look for commit hash in header comments
       (when (re-search-forward "^# \\([0-9a-f]\\{7,40\\}\\)" nil t)
         (match-string 1))
       ;; Fall back to HEAD
       "HEAD"))))


 ;; Keybindings
 (with-eval-after-load 'log-view
   (define-key log-view-mode-map (kbd "D")
               (lambda ()
                 (interactive)
                 (my/log-view-store-region-revisions)
                 (call-interactively 'log-view-diff))))

 (with-eval-after-load 'diff-mode
   (define-key diff-mode-map (kbd "e") 'my/ediff-stored-revisions))
 

