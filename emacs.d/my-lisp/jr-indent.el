(defun jr-indent-increase ()
  (interactive)
  (jr--indent 1)
  (setq deactivate-mark nil))

(defun jr-indent-decrease ()
  (interactive)
  (jr--indent -1)
  (setq deactivate-mark nil))

(defun jr--indent (levels)
  (let ((offset (* levels (jr--indent-offset))))
    (if (region-active-p)
        (let ((start (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
              (end (save-excursion
                     (goto-char (region-end))
                     (line-end-position))))
          (save-mark-and-excursion
            (indent-rigidly start end offset nil))))
    (indent-rigidly (line-beginning-position) (line-end-position) offset nil)))

(defun jr--indent-offset ()
  (or (case major-mode
        (c-mode c-basic-offset)
        (python-mode python-indent-offset))
      (error "Unkown indent offset")))

(provide 'jr-indent)
