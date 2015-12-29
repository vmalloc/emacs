
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package helm
  :ensure t
  :init
  (progn
    (helm-mode t)
    (setq helm-input-idle-delay 0)
    (setq helm-exit-idle-delay 0)
    )
  (require 'helm-config)
  :bind
   (("C-c h" . helm-mini)
     ("M-i" . helm-semantic-or-imenu)
     ("C-x y" . helm-show-kill-ring)
     ("M-x" . helm-M-x)))


