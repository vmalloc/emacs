(defun python-auto-import (only-first-component)
  "automatically import the required symbol under the cursor. With prefix argument, only takes first module path component (so 'os.path.exists' will cause importing of 'os' alone)"
  (interactive "P")
  (python-insert-import-line-at-beginning-of-buffer (python-get-needed-import-string only-first-component))
  )

(defun python-get-needed-import-string (only-first-component)
  (let ((symbol (python-info-current-symbol)))
    (let ((parts (split-string symbol "\\.")))
      (if (<= (list-length parts) 1)
	  (format "from %s import %s" (read-string (format "import %s from? " symbol)) symbol)
          (format "import %s" (if only-first-component (car parts) (mapconcat 'identity (nbutlast parts 1) "."))))
      )
  ))

(defun python-insert-import-line-at-beginning-of-buffer (import-string)
  (save-excursion
    (beginning-of-buffer)
    (skip-comments-and-strings)
    (let ((beg (point)))
      (newline)
      (forward-line -1)
      (insert-string import-string)
      (forward-paragraph)
      (let ((end (point)))
	(sort-lines nil beg (point))
	(uniquify-all-lines-region beg (point))
	)
      )
  ))

(defun skip-comments-and-strings ()
  (while (looking-at-comment-or-string)
    (forward-line)))
(defun looking-at-comment-or-string ()
  (let ((face (get-text-property (point) 'face)))
    (message (format "face is %s" face))
    (or (eq face 'font-lock-comment-face)
	(eq face 'font-lock-comment-delimiter-face)
        (eq face 'font-lock-string-face))))

(defun uniquify-all-lines-region (start end)
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(global-set-key (kbd "C-c i") 'python-auto-import)

(provide 'python-auto-import)
