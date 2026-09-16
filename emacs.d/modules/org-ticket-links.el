;;; org-ticket-links.el --- clickable TD-/DEV- ticket mentions  -*- lexical-binding: t; -*-

;; Turn every plain-text mention of a Jira ticket id (TD-1234, DEV-1234) into a
;; link to the headline in the same buffer that carries that id.  Nothing in the
;; buffer is rewritten: no org link syntax, no radio targets, no properties.
;;
;; A mention is highlighted only when some headline actually carries the id, so
;; the highlighting tells you which mentions are live.  Mentions that org already
;; owns -- ids inside a Jira URL, a link description, code, verbatim or a src
;; block -- are left alone.
;;
;; The recognised prefixes default to TD- and DEV- and can be set per buffer
;; with an org keyword near the top of the file:
;;
;;     #+GOTO_LINK_PREFIX: DEV- TD-
;;
;; mouse-1/mouse-2 and C-c C-o (`org-open-at-point') jump.  RET does not, and
;; needs no special handling: `org-return' only defers to `org-open-at-point' for
;; real links, timestamps and citations, none of which a plain mention is.

(require 'org)
(require 'org-element)
(require 'cl-lib)                       ;for `cl-intersection'
(require 'subr-x)                       ;for `string-trim'

(defvar my/org-ticket-links-mode)       ;defined by `define-minor-mode' below

(defcustom my/org-ticket-links-prefixes '("TD" "DEV")
  "Ticket prefixes recognised when a buffer sets no keyword of its own.
A per-buffer setting overrides this; see `my/org-ticket-links-keyword'."
  :type '(repeat string)
  :group 'org)

(defconst my/org-ticket-links-keyword "GOTO_LINK_PREFIX"
  "Org keyword naming the ticket prefixes for one buffer, as in

    #+GOTO_LINK_PREFIX: DEV- TD-

Read with `org-collect-keywords', so a line inside a src or example
block is correctly ignored, and `#+SETUPFILE' is honoured.")

(defvar my/org-ticket-links--id-re nil
  "Regexp matching a bare ticket id in this buffer.
Rebuilt from the buffer's prefixes by `my/org-ticket-links-refresh'.
Boundaries are deliberately not part of it -- they are checked in Lisp
by `my/org-ticket-links--bounded-p'.  In `org-mode' the syntax table
makes `-', `_' and `/' symbol constituents while `_' is not a word
constituent, so \\_>, \\_< and \\b all give wrong answers here; and a
boundary that consumed characters would make adjacent ids such as
\"TD-1/TD-2\" lose the second match.")
(make-variable-buffer-local 'my/org-ticket-links--id-re)

(defun my/org-ticket-links--prefixes ()
  "Ticket prefixes to recognise in this buffer.
Taken from a `#+GOTO_LINK_PREFIX:' line when the buffer has one, else
from `my/org-ticket-links-prefixes'.  A trailing hyphen is optional:
\"DEV-\" and \"DEV\" mean the same thing, since an id is always
PREFIX-digits."
  (let* ((values (cdr (assoc my/org-ticket-links-keyword
                             (org-collect-keywords
                              (list my/org-ticket-links-keyword)))))
         (given (when values
                  (delete-dups
                   (delq nil
                         (mapcar (lambda (word)
                                   (let ((word (string-trim
                                                (replace-regexp-in-string
                                                 "-\\'" "" word))))
                                     (unless (string= word "") word)))
                                 (split-string (string-join values " ")
                                               "[ \t]+" t)))))))
    (or given my/org-ticket-links-prefixes)))

(defun my/org-ticket-links--id-regexp ()
  "Regexp matching a bare ticket id, built from this buffer's prefixes."
  (concat "\\(?:" (regexp-opt (my/org-ticket-links--prefixes)) "\\)-[0-9]+"))

(defconst my/org-ticket-links--inert-faces '(org-code org-verbatim org-block)
  "Faces marking regions where a ticket mention must stay inert.")

(defconst my/org-ticket-links--org-owned
  '(citation citation-reference clock comment comment-block
    footnote-definition footnote-reference headline inline-src-block
    inlinetask keyword link node-property planning src-block timestamp)
  "Element types `org-open-at-point' handles on its own.
Copied from its own dispatch list so that our
`org-open-at-point-functions' entry can be its exact complement and
cannot swallow a real link, timestamp or footnote.")

(defvar my/org-ticket-links--ids nil
  "Hash set of ticket ids carried by a headline in this buffer.")
(make-variable-buffer-local 'my/org-ticket-links--ids)

(defvar my/org-ticket-links--tick nil
  "Value of `buffer-chars-modified-tick' when the index was last built.")
(make-variable-buffer-local 'my/org-ticket-links--tick)

(defvar my/org-ticket-links-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'my/org-ticket-links-open-at-mouse)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap placed on live ticket mentions.
Mouse only -- no keyboard bindings, so nothing typed inside a mention is
shadowed.  The `follow-link' entry is what makes mouse-1 follow, the same
way `org-mouse-map' does it.")

;;; Matching

(defsubst my/org-ticket-links--free-char-p (char)
  "Non-nil when CHAR may abut a ticket id.
A nil CHAR means the edge of the buffer, which is free."
  (not (and char (string-match-p "[[:alnum:]]" (char-to-string char)))))

(defun my/org-ticket-links--bounded-p (beg end)
  "Non-nil when the match between BEG and END is a whole ticket id.
Rejects only an abutting alphanumeric, so `xTD-1012' and `TD-101x' are
not ids while `[TD-46397]', `(TD-39228)' and `DEV-15013-' are.  An
underscore is deliberately allowed: branch names such as
`DEV-15113_houdini_sanity_validators_registration' are mentions of the
ticket, and a handful of headlines name their ticket only in that form.
Only letters need rejecting on the right, since the id regexp ends in a
greedy [0-9]+."
  (and (my/org-ticket-links--free-char-p (char-before beg))
       (my/org-ticket-links--free-char-p (char-after end))))

(defun my/org-ticket-links--inert-at-p (pos)
  "Non-nil when POS lies in markup that must not become a ticket link.
Tests `keymap' before face: every org construct that is already
clickable carries one, which covers links, footnotes and any link type
with a custom `:face'.  Relies on this keyword running after all of
org's own font-lock keywords."
  (or (get-text-property pos 'keymap)
      (let ((face (get-text-property pos 'face)))
        ;; `face' may be a bare symbol (org-level-1) or a list ((org-code)).
        (and face (cl-intersection (if (listp face) face (list face))
                                   my/org-ticket-links--inert-faces)))))

;;; Index of live ids

(defun my/org-ticket-links--build-index ()
  "Return a hash set of the ticket ids carried by headlines in this buffer.
Only group 4 of `org-complex-heading-regexp' is searched, so stars, the
TODO keyword, the priority cookie and the tag field cannot contribute an
id.  An id that appears in a headline only inside a URL -- as in
\"* FA Support - https://jira.../browse/TD-46216\" -- still counts: the
headline does carry the ticket."
  (let ((ids (make-hash-table :test #'equal))
        (my/org-ticket-links--id-re (or my/org-ticket-links--id-re
                                        (my/org-ticket-links--id-regexp)))
        (case-fold-search nil))         ;TODO keywords are case-sensitive
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-outline-regexp-bol nil t)
       (beginning-of-line)
       (when (looking-at org-complex-heading-regexp)
         (let ((title-beg (match-beginning 4))
               (title-end (match-end 4)))
           (when title-beg
             (save-excursion
               (goto-char title-beg)
               (while (re-search-forward my/org-ticket-links--id-re title-end t)
                 (when (my/org-ticket-links--bounded-p
                        (match-beginning 0) (match-end 0))
                   (puthash (match-string-no-properties 0) t ids)))))))
       (goto-char (line-end-position))))
    ids))

(defun my/org-ticket-links--same-ids-p (old new)
  "Non-nil when hash sets OLD and NEW hold the same keys."
  (and (hash-table-p old)
       (= (hash-table-count old) (hash-table-count new))
       (catch 'differs
         (maphash (lambda (id _) (unless (gethash id new) (throw 'differs nil)))
                  old)
         t)))

(defun my/org-ticket-links-refresh (&optional force)
  "Re-read this buffer's ticket prefixes and rebuild the index of live ids.
Does nothing when the buffer has not changed since the last build unless
FORCE is non-nil.  Refontifies only when the set of live ids actually
changed, so an ordinary save does not flush a large buffer for nothing.
Returns non-nil when the set changed."
  (interactive (list t))
  (when (and my/org-ticket-links-mode
             (or force
                 (null my/org-ticket-links--ids)
                 (not (eq my/org-ticket-links--tick
                          (buffer-chars-modified-tick)))))
    (let ((old-re my/org-ticket-links--id-re))
      (setq my/org-ticket-links--id-re (my/org-ticket-links--id-regexp))
      (let* ((new (my/org-ticket-links--build-index))
             (changed (or (not (equal old-re my/org-ticket-links--id-re))
                          (not (my/org-ticket-links--same-ids-p
                                my/org-ticket-links--ids new)))))
        (setq my/org-ticket-links--ids new
              my/org-ticket-links--tick (buffer-chars-modified-tick))
        (when (and changed font-lock-mode)
          (font-lock-flush))
        changed))))

;;; Font lock

(defun my/org-ticket-links--activate (limit)
  "Font-lock matcher activating live ticket mentions up to LIMIT.
Applies its own properties rather than using a highlight spec, the way
`org-activate-links' does, because `add-face-text-property' with a nil
APPEND argument prepends -- and prepending is required, not cosmetic:
most mentions sit on headlines that already carry `org-level-N', where a
plain facespec would do nothing."
  (when (and my/org-ticket-links-mode
             my/org-ticket-links--ids
             my/org-ticket-links--id-re)
    (let ((case-fold-search nil)
          (hit nil))
      ;; Keep scanning past skipped candidates -- returning nil on the first
      ;; Jira URL would stall this matcher for the rest of the chunk.
      (while (and (not hit)
                  (re-search-forward my/org-ticket-links--id-re limit t))
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (id (match-string-no-properties 0)))
          (when (and (my/org-ticket-links--bounded-p beg end)
                     (gethash id my/org-ticket-links--ids)
                     (not (my/org-ticket-links--inert-at-p beg)))
            (add-face-text-property beg end 'org-link)
            (add-text-properties
             beg end
             (list 'mouse-face 'highlight
                   'keymap my/org-ticket-links-map
                   'help-echo (format "%s -- mouse-1 or C-c C-o jumps to its headline" id)
                   'my/org-ticket id))
            ;; Without this, `insert-and-inherit' drags `keymap' onto whatever
            ;; is typed right after a mention, and unlike `face' it is never
            ;; corrected by refontification.
            (org-rear-nonsticky-at end)
            (setq hit t))))
      hit)))

(defun my/org-ticket-links--install-keyword ()
  "Append the ticket matcher to `org-font-lock-extra-keywords'.
Runs from `org-font-lock-set-keywords-hook' rather than using
`font-lock-add-keywords', because `org-set-font-lock-defaults' ends with
\(kill-local-variable \\='font-lock-keywords), which would silently
discard an appended keyword on every `org-restart-font-lock'.  Appending
also guarantees this matcher sees the faces and keymaps org's own
keywords have already applied.  The hook is global; the matcher
self-disables where `my/org-ticket-links-mode' is off."
  (setq org-font-lock-extra-keywords
        (append org-font-lock-extra-keywords
                '((my/org-ticket-links--activate)))))

(add-hook 'org-font-lock-set-keywords-hook #'my/org-ticket-links--install-keyword)

;;; Navigation

(defun my/org-ticket-links--mention-at-point ()
  "Return the ticket id at point, or nil.
Re-derives the id from the buffer text rather than reading the
`my/org-ticket' text property, which is absent wherever jit-lock has not
fontified yet."
  (unless my/org-ticket-links--id-re
    (setq my/org-ticket-links--id-re (my/org-ticket-links--id-regexp)))
  (save-excursion
    (let ((pos (point))
          (case-fold-search nil)
          (found nil))
      (beginning-of-line)
      (while (and (not found)
                  (re-search-forward my/org-ticket-links--id-re
                                     (line-end-position) t))
        (when (and (<= (match-beginning 0) pos)
                   (<= pos (match-end 0))
                   (my/org-ticket-links--bounded-p
                    (match-beginning 0) (match-end 0)))
          (setq found (match-string-no-properties 0))))
      found)))

(defun my/org-ticket-links--current-headline-pos ()
  "Position of the headline point is inside, or nil before the first one."
  (save-excursion
    (unless (org-before-first-heading-p)
      (org-back-to-heading t)
      (point))))

(defun my/org-ticket-links--candidates (id)
  "Return an alist of (LABEL . POSITION) for headlines carrying ID.
LABEL embeds the line number because the headline text alone does not
identify a target: a buffer can hold several byte-identical headlines
for the same ticket, and they may be siblings, so the outline path does
not separate them either."
  (let ((case-fold-search nil)
        (candidates nil))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-outline-regexp-bol nil t)
       (beginning-of-line)
       (when (looking-at org-complex-heading-regexp)
         (let ((title-beg (match-beginning 4))
               (title-end (match-end 4))
               (headline (point)))
           (when (and title-beg
                      (save-excursion
                        (goto-char title-beg)
                        (let ((hit nil))
                          (while (and (not hit)
                                      (re-search-forward (regexp-quote id)
                                                         title-end t))
                            (when (my/org-ticket-links--bounded-p
                                   (match-beginning 0) (match-end 0))
                              (setq hit t)))
                          hit)))
             (push (cons (format "%6d  %s"
                                 (line-number-at-pos headline)
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))
                         headline)
                   candidates))))
       (goto-char (line-end-position))))
    (nreverse candidates)))

(defun my/org-ticket-links--goto (position)
  "Jump to POSITION, pushing the mark ring so \\[org-mark-ring-goto] comes back."
  (org-mark-ring-push)
  (goto-char position)
  (org-back-to-heading t)
  (if (fboundp 'org-fold-show-context)
      (org-fold-show-context 'link-search)
    ;; org 9.4 (Emacs 27.2 on other hosts); obsolete alias on 9.6+.
    (with-no-warnings (org-show-context 'link-search)))
  t)

(defun my/org-ticket-links--follow (id)
  "Jump to the headline carrying ID.  Always returns t."
  (my/org-ticket-links-refresh)
  (let* ((here (my/org-ticket-links--current-headline-pos))
         (all (my/org-ticket-links--candidates id))
         (candidates (if here
                         (cl-remove-if (lambda (c) (eq (cdr c) here)) all)
                       all)))
    (cond
     ((null all)
      (message "No headline for %s in %s" id (buffer-name)))
     ((null candidates)
      (message "%s: this headline is its only occurrence" id))
     ((null (cdr candidates))
      (my/org-ticket-links--goto (cdar candidates)))
     (t
      (let ((choice (completing-read
                     (format "%s (%d headlines): " id (length candidates))
                     (mapcar #'car candidates) nil t)))
        (my/org-ticket-links--goto (cdr (assoc choice candidates))))))
    t))

(defun my/org-ticket-links-open-at-point ()
  "Jump to the headline carrying the ticket id at point.
Entry on `org-open-at-point-functions', which short-circuits all of
`org-open-at-point' when it returns non-nil.  It therefore returns nil
unless point is on a plain-text mention that org itself would not
handle: the element test below is the exact complement of
`org-open-at-point''s own dispatch list, so a real link, timestamp,
footnote or src block can never be swallowed."
  (and my/org-ticket-links-mode
       (let ((case-fold-search nil))
         (let ((id (my/org-ticket-links--mention-at-point)))
           (and id
                (memq (org-element-type
                       (org-element-lineage (org-element-context)
                                            my/org-ticket-links--org-owned t))
                      '(nil headline inlinetask))
                (my/org-ticket-links--follow id))))))

(defun my/org-ticket-links-open-at-mouse (event)
  "Follow the ticket mention clicked on by EVENT."
  (interactive "e")
  (mouse-set-point event)
  (org-open-at-point))

;;; Mode

;;;###autoload
(define-minor-mode my/org-ticket-links-mode
  "Make plain-text TD-/DEV- ticket mentions jump to their headline."
  :lighter nil
  (if my/org-ticket-links-mode
      (progn
        ;; `org-unfontify-region' clears mouse-face and keymap but not these,
        ;; so a stale tooltip would outlive a headline being deleted.
        (setq-local font-lock-extra-managed-props
                    (append '(help-echo my/org-ticket)
                            font-lock-extra-managed-props))
        ;; Buffer-locally: the hook is global by default, and a global entry
        ;; would run in every org buffer including the agenda files.
        (add-hook 'org-open-at-point-functions
                  #'my/org-ticket-links-open-at-point nil t)
        (add-hook 'before-save-hook #'my/org-ticket-links-refresh nil t)
        (my/org-ticket-links-refresh t))
    (remove-hook 'org-open-at-point-functions
                 #'my/org-ticket-links-open-at-point t)
    (remove-hook 'before-save-hook #'my/org-ticket-links-refresh t)
    (setq my/org-ticket-links--ids nil
          my/org-ticket-links--tick nil)
    (when font-lock-mode (font-lock-flush))))

(provide 'org-ticket-links)

;;; org-ticket-links.el ends here
