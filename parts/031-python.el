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

; Jedi
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 200)
(setq jedi:get-in-function-call-timeout 5000)
(setq jedi:tooltip-method nil) ; Use the minibuffer to show signatures instead of the tooltip
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
