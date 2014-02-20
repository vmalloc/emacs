(defun python-auto-pep8 ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (push-mark)
    (end-of-buffer)
    (shell-command-on-region (point) (mark) "autopep8 -" nil t)
    (pop-mark)))

