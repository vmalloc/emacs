(use-package python
  :config (progn
	    (add-hook 'python-mode-hook
		      '(lambda ()
			 (setq python-indent-offset 4)
			 (local-set-key (kbd "C-c #") 'comment-or-uncomment-region)))))



(use-package python-refactor
  :bind (
	 :map python-mode-map
	      ("C-c r d" . python-toggle-dict-style)
	      ("C-c r a" . python-toggle-assert-style)
	      ("C-c r p" . python-extract-parameter)
	      ("C-c r P" . python-pytest-parametrize)
	      ("C-c r b" . python-pytest-parametrize-boolean)
	      ("C-c r i" . python-auto-import)
	      ))


(defun my/python-tidy-up()
  (interactive)
  (py-isort-buffer)
  (py-autopep8))


(defun my/python-auto-pep8 ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (push-mark)
    (end-of-buffer)
    (shell-command-on-region (point) (mark) (format "%s -" (executable-find "autopep8")) nil t)
    (pop-mark)))

(define-key python-mode-map (kbd "C-c t") 'my/python-auto-pep8)

(use-package py-isort
  :ensure t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; pylint
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun my/pylint-ignore-errors(global)
  "Ignores pylint warning at point locally. With a prefix argument, ignores them globally"
  (interactive "P")
  (let* ((errs (flycheck-overlay-errors-in (line-beginning-position) (line-end-position)))
         (ids (delete-dups (-map 'flycheck-error-id errs))))
    (when (> (length ids) 0)
      (save-excursion
	(if global
	    (progn
	      (beginning-of-buffer)
	      (insert "# ")
	      )
	  (comment-indent)
	  )
	(insert "pylint: disable="
		(s-join ", " ids))
	(if global
	    (insert "\n"))))))
