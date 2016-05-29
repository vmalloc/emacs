
; show-paren
(setq show-paren-delay 0)
(show-paren-mode)

; winner mode
(winner-mode 1)

(defun presentation-mode ()
  (interactive)
  (load-theme 'whiteboard t)
  (set-face-attribute 'default nil :height 120))

(add-to-list 'custom-theme-load-path (in-emacs-d "themes"))

(setq-default cursor-type 'bar)

(require 'sublimity-scroll)
(sublimity-mode 1)
(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)

(require 'popwin)
(popwin-mode 1)

(require 'powerline)
(powerline-default-theme)

