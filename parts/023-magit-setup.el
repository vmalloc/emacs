(add-to-list 'load-path (in-modes-d "magit"))
(require 'magit)

; make magit window full screen, restore previous frame arrangement (http://whattheemacsd.com//setup-magit.el-01.html)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun my/magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

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

(define-key magit-status-mode-map (kbd "W") 'my/magit-toggle-whitespace)

(global-set-key [(f7)] 'magit-status)
(global-set-key [(control x) (f7)] 'magit-branch-manager)

