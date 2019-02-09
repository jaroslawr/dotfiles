(defun jr/bind-keys (bindings)
  (if (eq (cdr bindings) nil)
      t
    (progn
      (let ((key (car bindings))
	    (function (cadr bindings)))
        (global-set-key (read-kbd-macro key) function)
	(jr/bind-keys (cddr bindings))))))

(provide 'jr-bind-keys)
