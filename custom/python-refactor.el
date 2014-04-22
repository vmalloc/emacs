
(defun --python-refactor(op)
  (shell-command-on-region (point) (mark) (format "%s %s" (executable-find "pyrefactor") op) nil t)
  (setq deactivate-mark nil)
  )

(defun python-toggle-dict-style ()
  (interactive)
  (--python-refactor "toggle_dict_style")
  )

(defun python-toggle-assert-style ()
  (interactive)
  (--python-refactor "toggle_assert_style")
  )


(provide 'python-refactor)
