;(require 'rainbow-mode)
;(add-to-list 'find-file-hook
;	     (lambda () (unless (derived-mode-p 'web-mode) (rainbow-mode))))

; nyan-mode (no .emacs.d is whole without it)
(unless in-terminal
  (require 'nyan-mode)
  (nyan-mode t))
