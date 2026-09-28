;;; org-open-items.el --- last open checkbox of every headline  -*- lexical-binding: t; -*-

;; A notes file grows a trailing "what is left here" checkbox under most task
;; headlines.  A plain `occur' for "- [ ]" answers a different question: it lists
;; *every* open box in the file, historical ones included (566 of them in
;; cs/notes.org).  What is wanted is one line per headline -- the *last* open
;; checkbox in that headline's own body:
;;
;;     ** TODO TD-49211 - publish not version up / TD-49213 publish on LIB ...
;;     ...
;;     - [ ] Research 'opalias' - perhaps this could be used to combine HDA   <- this one
;;
;; An Emacs regexp cannot say "last one before the next headline" (no lookahead),
;; so `my/org-open-items-occur' computes the candidate lines itself and then
;; prunes a real *Occur* buffer down to them.
;;
;; Pruning rather than building the buffer by hand is deliberate: the
;; `occur-target' text property is a single marker in Emacs 27 and a list of
;; (BEG . END) marker pairs in Emacs 29+.  Letting `occur' create the entries and
;; only deleting whole lines keeps jumping, `next-error' and follow mode working
;; on every version without version-specific code.
;;
;; DONE/CANCEL/... headlines are skipped unless called with a prefix argument.

(require 'org)

(defcustom my/org-open-items-show-heading t
  "Whether to label each pruned *Occur* line with its headline.
The label is an overlay, so the occur line itself -- and the markers
that make it clickable -- are left untouched."
  :type 'boolean
  :group 'org)

(defconst my/org-open-items-re "^[ \t]*[-+] \\[ \\] "
  "An unchecked Org checkbox item, at any indent level.
`*' bullets are deliberately not matched: at column 0 they are
headlines, and an indented `*' bullet is vanishingly rare in practice.")

(defun my/org-open-items--last-lines (&optional include-done)
  "Return a hash table mapping LINE-NUMBER to HEADLINE text.
Each entry is the last line in one headline's own body (not its
subtree) matching `my/org-open-items-re'.  Headlines in a done state
are skipped unless INCLUDE-DONE is non-nil, as are matches inside a
#+begin_.../#+end_... block and matches before the first headline."
  (let ((table (make-hash-table :test #'eql))
        (lnum 1)
        (depth 0)
        heading done line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at org-outline-regexp-bol)
            (when (and line (or include-done (not done)))
              (puthash line heading table))
            (setq line nil
                  depth 0
                  heading (org-get-heading t t t t)
                  done (and (member (org-get-todo-state) org-done-keywords) t)))
           ((looking-at "[ \t]*#\\+begin_")
            (setq depth (1+ depth)))
           ((looking-at "[ \t]*#\\+end_")
            (setq depth (max 0 (1- depth))))
           ((and heading (zerop depth) (looking-at my/org-open-items-re))
            (setq line lnum)))
          (setq lnum (1+ lnum))
          (forward-line 1))
        (when (and line (or include-done (not done)))
          (puthash line heading table))))
    table))

;;;###autoload
(defun my/org-open-items-occur (&optional include-done)
  "List the last open \"- [ ]\" item of every headline in an *Occur* buffer.
With a prefix argument INCLUDE-DONE, keep DONE/CANCEL headlines too."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let* ((source (current-buffer))
         (table (my/org-open-items--last-lines include-done))
         (buf (progn (occur my/org-open-items-re) (get-buffer "*Occur*"))))
    (unless buf
      (user-error "No open items in %s" (buffer-name source)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (kept 0))
        ;; `occur' erases the buffer but leaves overlays behind, so labels from
        ;; an earlier run (a `g' revert, say) would pile up on top of the new ones.
        (remove-overlays (point-min) (point-max) 'my/org-open-items t)
        (save-excursion
          (goto-char (point-max))
          (forward-line 0)
          ;; Back to front, so deleting a line cannot disturb the ones still to
          ;; be looked at.  Line 1 is occur's own header.
          (while (> (line-number-at-pos) 1)
            (let* ((num (and (looking-at "[ \t]*\\([0-9]+\\):")
                             (string-to-number (match-string 1))))
                   (heading (and num (gethash num table))))
              (if heading
                  (progn
                    (setq kept (1+ kept))
                    (when my/org-open-items-show-heading
                      (let ((ov (make-overlay (point) (line-end-position))))
                        (overlay-put ov 'my/org-open-items t)
                        (overlay-put ov 'before-string
                                     (concat (propertize heading 'face 'shadow)
                                             "\n")))))
                (delete-region (point) (min (point-max) (1+ (line-end-position))))))
            (forward-line -1)))
        (goto-char (point-min))
        (delete-region (point) (line-end-position))
        (insert (format "%d headline%s with an open item in buffer: %s"
                        kept (if (= kept 1) "" "s") (buffer-name source)))
        (forward-line 1)
        (set-buffer-modified-p nil)
        ;; `g' should re-run us, not resurrect the unpruned match list.
        (setq-local revert-buffer-function
                    (lambda (&rest _)
                      (with-current-buffer source
                        (my/org-open-items-occur include-done))))
        (if (zerop kept)
            (message "No open items in %s" (buffer-name source))
          (switch-to-buffer-other-window buf)
          (next-error-follow-minor-mode 1))))))

(provide 'org-open-items)

;;; org-open-items.el ends here
