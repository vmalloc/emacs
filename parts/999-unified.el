
(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))
