; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

; zencoding
(require 'zencoding-mode)
(setq zencoding-indentation 2)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-j") nil)
(define-key zencoding-mode-keymap (kbd "<C-return>") nil)
(define-key zencoding-mode-keymap (kbd "C-c i") 'zencoding-expand-line)
