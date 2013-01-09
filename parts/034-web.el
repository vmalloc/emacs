; nxml/nxhtml - a bloated mode for fancy XML/HTML editing with multiple-major-mode support.
; It is only autoloaded, not wired to any format, since it's very clumsy and requires getting
; used to...
(autoload 'nxhtml-mode (in-modes-d "nxhtml-mode/autostart.el") nil t)


; web-mode
(require-from-modes-d "web-mode")
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

; zencoding
(require-from-modes-d "zencoding" 'zencoding-mode)
(setq zencoding-indentation 2)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-j") nil)
(define-key zencoding-mode-keymap (kbd "<C-return>") nil)
(define-key zencoding-mode-keymap (kbd "C-c i") 'zencoding-expand-line)
