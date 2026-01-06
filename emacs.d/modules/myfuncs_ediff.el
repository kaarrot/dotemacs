
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
   ))

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
   "Ediff file at point in diff using stored revisions."
   (interactive)
   (require 'ediff-vers)
   (if (and my/stored-revisions (= (length my/stored-revisions) 2))
       (let ((file (diff-find-file-name)))
         (if file
             (let ((clean-file (substring-no-properties file)))
               (with-current-buffer (find-file-noselect clean-file)
                 (ediff-vc-internal (car my/stored-revisions)
                                    (cadr my/stored-revisions)
                                    nil)))
           (message "Cannot find file at point")))
     (message "No stored revisions. Press D in log-view first")))



 ;; Keybindings
 (with-eval-after-load 'log-view
   (define-key log-view-mode-map (kbd "D")
               (lambda ()
                 (interactive)
                 (my/log-view-store-region-revisions)
                 (call-interactively 'log-view-diff))))

 (with-eval-after-load 'diff-mode
   (define-key diff-mode-map (kbd "e") 'my/ediff-stored-revisions))
 

