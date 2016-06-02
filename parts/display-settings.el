;; show-paren
(setq show-paren-delay 0)
(show-paren-mode)

(use-package sublimity
  :ensure t
  :init (sublimity-mode 1)
  :config
  (setq sublimity-scroll-weight 10
	sublimity-scroll-drift-length 5))

(use-package popwin
  :ensure t
  :init (popwin-mode 1))

(use-package powerline
  :ensure t
  :init (powerline-default-theme))

;; Try to display battery info (only if applicable)
(condition-case ex
    (display-battery-mode t)
  ('error (message "Cannot display battery")))

;; Key assist
(use-package
  which-key
  :ensure t)
