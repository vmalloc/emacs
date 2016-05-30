
(defun --python-pyrefactor(op)
  (save-excursion
    (if (not (region-active-p))
        (er/mark-python-statement))
    (message (format "point: %s mark: %s" (point) (mark)))
    (shell-command-on-region (point) (mark) (format "%s %s" (executable-find "pyrefactor") op) nil t)
    (setq deactivate-mark nil)))

(defun python-toggle-dict-style ()
  (interactive)
  (--python-pyrefactor "toggle_dict_style")
  )

(defun python-toggle-assert-style ()
  (interactive)
  (--python-pyrefactor "toggle_assert_style")
  )


(defun python-extract-parameter ()
  (interactive)
  (let ((symbol (current-word)))
    (message (format "extracting parameter: %s" symbol))
    (save-excursion
      (back-to-indentation)
      (python-nav-beginning-of-defun)
      (search-forward "):")
      (backward-char 3)
      (if (not (looking-at "("))
          (progn
            (forward-char)
            (insert ", "))
        (forward-char))
      (insert symbol))))


(defun python-pytest-parametrize (&optional values-list)
  (interactive)
  (let ((symbol (current-word)))
    (back-to-indentation)
    (previous-line 1)
    (insert-line-after)
    (beginning-of-line)
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (insert (format "@pytest.mark.parametrize('%s', [%s])" symbol (if values-list values-list "")))
    (backward-char 2)

    ))

(defun python-pytest-extract-fixture()
  "Extracts the current variable (along with its creation) to a py.test fixture"
  (interactive)
  (let* ((name (symbol-at-point)))
    (call-interactively 'python-extract-parameter)
    (save-excursion
      (beginning-of-line)
      (let* ((line (thing-at-point 'line t)))
        (message "Line is %s" line)
        (kill-line)
        (goto-char (point-max))
        (insert "\n\n@pytest.fixture")
        (insert (format "\ndef %s():\n" name))
        (insert line)
        (beginning-of-line)
        (unless  (looking-at "[[:space:]]*$")
          (end-of-line)
          (insert "\n"))
        (indent-for-tab-command)
        (insert (format "return %s" name))))

    (indent-for-tab-command)
    ))



(defun python-pytest-parametrize-boolean ()
  (interactive)
  (save-excursion
    (python-pytest-parametrize "True, False")))

(provide 'python-refactor)
