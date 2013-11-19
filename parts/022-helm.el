(require 'projectile)
(projectile-global-mode)

(require 'helm)
(require 'helm-config)
(helm-mode t)
(setq helm-input-idle-delay 0)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)

(defun emacs-d-find-file ()
  (interactive)
  (let ((default-directory "~/.emacs.d/"))
    (helm-projectile)))

