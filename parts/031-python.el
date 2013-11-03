(add-to-list 'load-path (in-modes-d "python.el"))

(require 'python)

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup)
             (local-set-key (kbd "C-c #") 'comment-or-uncomment-region)))

; run redgreen in current project
(require 'redgreen-at-project-root)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?r] 'redgreen-at-project-root)))

; python-auto-super
(require 'python-auto-super)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?s] 'python-auto-super)))

; python-auto-import
(require 'python-auto-import)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?i] 'python-auto-import)))

