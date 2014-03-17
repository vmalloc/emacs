(add-to-list 'load-path (in-modes-d "python.el"))

(require 'python)

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup)
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
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?i] 'python-auto-import)))

(autoload 'python-toggle-dict-style "python-refactor.el" nil t)
(autoload 'python-toggle-assert-style "python-refactor.el" nil t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?d] 'python-toggle-dict-style)
             (define-key python-mode-map [(control ?c) ?a] 'python-toggle-assert-style)
             ))

(autoload 'python-auto-pep8 "python-auto-pep8.el" nil t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?8] 'python-auto-pep8)
             ))

