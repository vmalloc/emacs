;-------------- Web mode ---------------;

(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
         ("\\.hbs$" . web-mode))

  :config
  (add-hook 'web-mode-hook  '(lambda ()
                                 (setq web-mode-markup-indent-offset 2)
                                 (setq web-mode-css-indent-offset 2)
                                 (setq web-mode-code-indent-offset 2))))

(use-package emmet-mode
  :ensure t
  :config
  (progn
    (setq emmet-indentation 2)
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (define-key emmet-mode-keymap (kbd "<C-return>") nil)
    (define-key emmet-mode-keymap (kbd "C-c i") 'emmet-expand-line)))


(use-package css-mode
  :init
  (add-hook 'css-mode-hook '(lambda ()
                              (define-key css-mode-map (kbd "M-i") 'helm-css-scss))))




;-------------- Server mode ---------------;

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))
