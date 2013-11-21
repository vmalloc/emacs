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
  (helm-projectile-find-file-in-dir "~/.emacs.d"))

(global-set-key (kbd "C-c C-e") 'emacs-d-find-file)

(defun helm-projectile-find-file-in-dir (dir-name)
  (let ((default-directory dir-name))
    (helm-projectile)))

(defun make-quickdir (modifier dir-name)
  (lexical-let ((d dir-name))
    (let ((hook (lambda  () (interactive) (helm-projectile-find-file-in-dir d))))
      (global-set-key modifier hook))))

