(require 'org)
(require 'org-element)
(require 'subr-x) ;; for when-let

(defun org-image-update-overlay (file link &optional data-p refresh)
  "Create image overlay for FILE associtated with org-element LINK.
If DATA-P is non-nil FILE is not a file name but a string with the image data.
If REFRESH is non-nil don't download the file but refresh the image.
See also `create-image'.
This function is almost a duplicate of a part of `org-display-inline-images'."
  (when (or data-p (file-exists-p file))
    (let ((width
       ;; Apply `org-image-actual-width' specifications.
       (cond
        ((not (image-type-available-p 'imagemagick)) nil)
        ((eq org-image-actual-width t) nil)
        ((listp org-image-actual-width)
         (or
          ;; First try to find a width among
          ;; attributes associated to the paragraph
          ;; containing link.
          (let ((paragraph
             (let ((e link))
               (while (and (setq e (org-element-property
                        :parent e))
                   (not (eq (org-element-type e)
                        'paragraph))))
               e)))
        (when paragraph
          (save-excursion
            (goto-char (org-element-property :begin paragraph))
            (when
            (re-search-forward
             "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
             (org-element-property
              :post-affiliated paragraph)
             t)
              (string-to-number (match-string 1))))))
          ;; Otherwise, fall-back to provided number.
          (car org-image-actual-width)))
        ((numberp org-image-actual-width)
         org-image-actual-width)))
      (old (get-char-property-and-overlay
        (org-element-property :begin link)
        'org-image-overlay)))
      (if (and (car-safe old) refresh)
      (image-refresh (overlay-get (cdr old) 'display))
    (let ((image (create-image file
                   (and width 'imagemagick)
                   data-p
                   :width width)))
      (when image
        (let* ((link
            ;; If inline image is the description
            ;; of another link, be sure to
            ;; consider the latter as the one to
            ;; apply the overlay on.
            (let ((parent
               (org-element-property :parent link)))
              (if (eq (org-element-type parent) 'link)
              parent
            link)))
           (ov (make-overlay
            (org-element-property :begin link)
            (progn
              (goto-char
               (org-element-property :end link))
              (skip-chars-backward " \t")
              (point)))))
          (overlay-put ov 'display image)
          (overlay-put ov 'face 'default)
          (overlay-put ov 'org-image-overlay t)
          (overlay-put
           ov 'modification-hooks
           (list 'org-display-inline-remove-overlay))
          (push ov org-inline-image-overlays)
          ov)))))))

(defun org-display-user-inline-images (&optional _include-linked _refresh beg end)
  "Like `org-display-inline-images' but for image data links.
_INCLUDE-LINKED and _REFRESH are ignored.
Restrict to region between BEG and END if both are non-nil.
Image data links have a :image-data-fun parameter.
\(See `org-link-set-parameters'.)
The value of the :image-data-fun parameter is a function
taking the PROTOCOL, the LINK, and the DESCRIPTION as arguments.
If that function returns nil the link is not interpreted as image.
Otherwise the return value is the image data string to be displayed.

Note that only bracket links are allowed as image data links
with one of the formats [[PROTOCOL:LINK]] or [[PROTOCOL:LINK][DESCRIPTION]] are recognized."
  (interactive)
  (when (and (called-interactively-p 'any)
         (use-region-p))
    (setq beg (region-beginning)
      end (region-end)))
  (when (display-graphic-p)
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (when-let ((image-data-link-parameters
     (cl-loop for link-par-entry in org-link-parameters
          with fun
          when (setq fun (plist-get (cdr link-par-entry) :image-data-fun))
          collect (cons (car link-par-entry) fun)))
    (image-data-link-re (regexp-opt (mapcar 'car image-data-link-parameters)))
    (re (format "\\[\\[\\(%s\\):\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
        image-data-link-re)))
       (while (re-search-forward re end t)
     (let* ((protocol (match-string-no-properties 1))
    (link (match-string-no-properties 2))
    (description (match-string-no-properties 3))
    (image-data-link (assoc-string protocol image-data-link-parameters))
    (el (save-excursion (goto-char (match-beginning 1)) (org-element-context)))
    image-data)
       (when (and el
              (eq (org-element-type el) 'link))
         (setq image-data
           (or (let ((old (get-char-property-and-overlay
                   (org-element-property :begin el)
                   'org-image-overlay)))
             (and old
                  (car-safe old)
                  (overlay-get (cdr old) 'display)))
           (funcall (cdr image-data-link) protocol link description)))
         (when image-data
           (let ((ol (org-image-update-overlay image-data el t t)))
         (when (and ol description)
           (overlay-put ol 'after-string description)))))))))))

(advice-add #'org-display-inline-images :after #'org-display-user-inline-images)

(defun org-inline-data-image (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

(org-link-set-parameters
 "img"
 :image-data-fun #'org-inline-data-image)

(require 'org-download)

(defun org-download-screenshot-img ()
  "Capture screenshot and insert img link with base64 encoded data."
  (interactive)
  (let ((file (expand-file-name org-download-screenshot-file)))
    (shell-command (format org-download-screenshot-method file))
    (insert "[[img:"
            (with-temp-buffer
              (let ((coding-system-for-read 'no-conversion))
                (insert-file-contents file)
                (base64-encode-region (point-min) (point-max) t)
                (buffer-string)))
            "]]"))
  (org-display-user-inline-images))

(defun org-activate-yank-img-links ()
  "Activate keybinding S-C-y for yanking [[img:...]] links in function `org-mode'.
Hook this function into `org-mode-hook'."
  (org-defkey org-mode-map (kbd "S-C-y") #'org-download-screenshot-img))

(add-hook 'org-mode-hook #'org-activate-yank-img-links)
