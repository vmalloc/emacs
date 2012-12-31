; rainbow-mode
(require 'rainbow-mode)
(add-to-list 'find-file-hook
	     (lambda () (unless (derived-mode-p 'web-mode) (rainbow-mode))))

; nyan-mode (no .emacs.d is whole without it)
(autoload 'nyan-mode (in-modes-d "nyan-mode/nyan-mode.el") nil t)
(unless in-terminal
  (nyan-mode t))

; pomodoro - time management technique
(autoload 'pomodoro "pomodoro.el" nil t)
