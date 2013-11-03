; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

; zencoding
(require 'emmet-mode)
(setq emmet-indentation 2)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(define-key emmet-mode-keymap (kbd "C-j") nil)
(define-key emmet-mode-keymap (kbd "<C-return>") nil)
(define-key emmet-mode-keymap (kbd "C-c i") 'emmet-expand-line)

; js2-mode
(require 'js2-mode)
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'css-mode-hook '(lambda ()
                            (define-key css-mode-map (kbd "M-i") 'helm-css-scss)))
