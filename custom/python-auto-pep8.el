(defun python-auto-pep8 ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (push-mark)
    (end-of-buffer)
    (shell-command-on-region (point) (mark) (format "%s -" (executable-find "autopep8")) nil t)
    (pop-mark)))

