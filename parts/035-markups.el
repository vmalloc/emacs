; YAML
(require-from-modes-d "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; Markdown
(autoload 'markdown-mode "markdown-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
