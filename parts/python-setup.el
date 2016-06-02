(add-to-list 'load-path (in-modes-d "python.el"))

(use-package python
  :config (progn
	    (add-to-list 'load-path (in-modes-d "python.el"))


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

(use-package py-autopep8
  :ensure t
  :bind (
	 :map python-mode-map
	      ("C-c t" . my/python-tidy-up)))

(use-package py-isort
  :ensure t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; pylint
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun my/pylint-ignore-errors-at-point()
  (interactive)
  (let* ((errs (flycheck-overlay-errors-in (line-beginning-position) (line-end-position)))
         (ids (delete-dups (-map 'flycheck-error-id errs))))
    (when (> (length ids) 0)
      (save-excursion
        (comment-indent)
        (insert "pylint: disable="
                (s-join ", " ids))))))
