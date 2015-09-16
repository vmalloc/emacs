(add-to-list 'load-path (in-modes-d "python.el"))

(require 'python)


(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup)
             (setq python-indent-offset 4)
             (local-set-key (kbd "C-c #") 'comment-or-uncomment-region)))

; run redgreen in current project
(autoload 'redgreen-at-project-root "redgreen-at-project-root.el" nil t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?r] 'redgreen-at-project-root)))

; python-auto-super
(autoload 'python-auto-super "python-auto-super.el" nil t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?s] 'python-auto-super)))

; python-auto-import
(autoload 'python-auto-import "python-auto-import.el" nil t)
(autoload 'python-isort-buffer "python-auto-import.el" nil t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?i] 'python-auto-import))
             (define-key python-mode-map [(control ?c) ?I] 'python-isort-buffer))


(dolist (symbol (list 'python-toggle-dict-style
                      'python-toggle-assert-style
                      'python-extract-parameter
                      'python-pytest-parametrize
                      'python-pytest-parametrize-boolean
                      ))
  (autoload symbol "python-refactor.el" nil t))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-c d") 'python-toggle-dict-style)
             (define-key python-mode-map (kbd "C-c a") 'python-toggle-assert-style)
             (define-key python-mode-map (kbd "C-c e p") 'python-extract-parameter)
             (define-key python-mode-map (kbd "C-c e P") 'python-pytest-parametrize)
             (define-key python-mode-map (kbd "C-c e b") 'python-pytest-parametrize-boolean)
             ))

(autoload 'python-auto-pep8 "python-auto-pep8.el" nil t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?8] 'python-auto-pep8)
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?t]
               '(lambda () (interactive)
                  (python-isort-buffer)
                  (python-auto-pep8)))))

(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))
