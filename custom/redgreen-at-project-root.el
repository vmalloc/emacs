(defun redgreen-at-project-root ()
  (interactive)
  (let ((root (napr/find-project-root)))
    (let ((compilation-scroll-output t)
	  (compilation-window-height 30)
	  )
      (compile (format "cd %s && %s -t \"%s\" -- --with-snort -w tests -x --nologcapture" root (get-redgreen-executable) (get-nosetests-executable)) t)
      )
    )
  )

(defun get-nosetests-executable ()
  (get-python-utility "nosetests"))

(defun get-redgreen-executable ()
  (get-python-utility "redgreen"))

(defun get-python-utility (name)
  (let ((dir (file-name-directory python-shell-interpreter)))
    (if dir
	(concat dir name)
	name
	)))

(defun napr/find-project-root (&optional root)
  "Determines the current project root by recursively searching for an indicator."
  (when (null root) (setq root default-directory))
  (cond
   ((napr/root-matches root (list ".git" ".hg" ".bzr"))
    (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t (napr/find-project-root (concat (file-name-as-directory root) "..")))))

(defun napr/root-match(root names)
  (member (car names) (directory-files root)))

(defun napr/root-matches(root names)
  (if (napr/root-match root names)
      (napr/root-match root names)
      (if (eq (length (cdr names)) 0)
          'nil
          (napr/root-matches root (cdr names))
          )))

(provide 'redgreen-at-project-root)
