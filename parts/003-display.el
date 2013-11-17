; Display settings
(setq inhibit-splash-screen t)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(display-time) ; useful for full-screen terminals

; Try to display battery info (only if applicable)
(condition-case ex
    (display-battery-mode t)
  ('error (message "Cannot display battery"))
  )

(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(setq ring-bell-function 'ignore)

; show-paren
(setq show-paren-delay 0)
(show-paren-mode)

; winner mode
(winner-mode 1)

; guid-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "%" "*" "C-c p"))
(guide-key-mode 1)

; diff-hl for highlighting differences in fringe
(add-hook 'prog-mode-hook '(lambda () (diff-hl-mode)))
