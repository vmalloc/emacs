(defun python-toggle-dict-styles ()
  (interactive)
  (shell-command-on-region (point) (mark) "toggle_dict_style" nil t)
  (setq deactivate-mark nil)
  )

(provide 'python-dict-styles)
