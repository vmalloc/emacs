; flycheck
(require 'flycheck)

; turn on flycheck-mode in various modes
(mapcar '(lambda (m) (add-hook m '(lambda () (flycheck-mode))))

        (list 'python-mode-hook 'js-mode-hook))

(global-set-key [(f5)] 'flycheck-previous-error)
(global-set-key [(f6)] 'flycheck-next-error)
