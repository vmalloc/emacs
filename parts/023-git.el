(setq magit-last-seen-setup-instructions "1.4.0")


(defun my/magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun my/magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (my/magit-dont-ignore-whitespace)
    (my/magit-ignore-whitespace)))

(defun my/magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun my/magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))


(use-package magit
  :ensure t
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (setq magit-auto-revert-mode nil)
    (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)
    (define-key magit-status-mode-map (kbd "W") 'my/magit-toggle-whitespace)
    (add-hook 'magit-mode-hook 'magit-load-config-extensions))

  :bind (("C-c g" . magit-status)))
