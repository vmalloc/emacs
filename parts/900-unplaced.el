(add-to-list 'load-path (in-modes-d "rainbow-mode"))
(require 'rainbow-mode)
(add-to-list 'find-file-hook
	     (lambda () (unless (derived-mode-p 'web-mode) (rainbow-mode))))

; nyan-mode (no .emacs.d is whole without it)
(unless in-terminal
  (require 'nyan-mode)
  (nyan-mode t))

; Turn on visual line mode in man pages
(add-hook 'Man-mode-hook
	  '(lambda ()
	     (visual-line-mode t)))

; Mongo DB
(require 'inf-mongo)
