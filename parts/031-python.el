(cond
 ((>= emacs-major-version 24)
  (require-from-modes-d "python.el" 'python)))
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup)
             (local-set-key (kbd "C-c #") 'comment-or-uncomment-region)))

; pylookup
(setq pylookup-dir (in-modes-d "pylookup"))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(add-to-list 'load-path pylookup-dir)
(autoload 'pylookup-lookup "pylookup.el" nil t)


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
