(ido-mode)
(setq ido-max-prospects 200)

(require 'ido-recentf-open)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
