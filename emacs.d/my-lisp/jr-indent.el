(defun jr-indent-increase ()
  (interactive)
  (jr--indent 1)
  (setq deactivate-mark nil))

(defun jr-indent-decrease ()
  (interactive)
  (jr--indent -1)
  (setq deactivate-mark nil))

(defun jr--region-non-empty-lines-start ()
  "Returns the beginning of the first line of region, but only if
the region does not start in the middle of the line text."
  (let ((region-start (min (region-beginning) (region-end))))
    (save-mark-and-excursion
      (goto-char region-start)
      (if (string-match "^[ ]*$"
                        (buffer-substring (line-beginning-position) region-start))
          (line-beginning-position)
        (error "Partially selected line")))))

(defun jr--region-non-empty-lines-end ()
  "Returns the end of the last text line of region."
  (let ((region-end (max (region-beginning) (region-end))))
    (save-mark-and-excursion
      (goto-char region-end)
      (if (string-match "^[ ]*$"
                        (buffer-substring (line-beginning-position) region-end))
          (- region-end (+ (- region-end (line-beginning-position)) 1))
        region-end))))

(defun jr--indent (n)
  "Indent the current region or current lines n levels more or n
levels less"
  (save-mark-and-excursion
    (let ((offset (* n (jr--indent-offset)))
          (start (copy-marker (jr--region-non-empty-lines-start)))
          (end (copy-marker (jr--region-non-empty-lines-end))))
      (if (region-active-p)
          (if (> offset 0)
              (string-insert-rectangle start end (make-string offset ?\s))
            (progn
              (goto-char start)
              (while (<= (line-beginning-position) (marker-position end))
                (unless (jr--buffer-substring-match "^[ ]+$" (point) (+ (point) (- offset)))
                  (error "Block cannot be dedented any more"))
                (delete-char (- offset))
                (next-line)
                (beginning-of-line))))
        (indent-rigidly (line-beginning-position) (line-end-position) offset nil)))))

(defun jr--buffer-substring-match (regexp start end)
  (string-match regexp (buffer-substring start end)))

(defun jr--indent-offset ()
  (or (case major-mode
        (c-mode c-basic-offset)
        (python-mode python-indent-offset))
      (error "Unkown indent offset")))

(provide 'jr-indent)
