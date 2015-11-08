; flycheck
(require 'flycheck)

; turn on flycheck-mode in various modes
(mapcar '(lambda (m) (add-hook m '(lambda () (flycheck-mode))))

        (list 'python-mode-hook 'js-mode-hook))

(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(require 'flycheck-color-mode-line)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))



(global-set-key (kbd "C-c m f") 'flycheck-mode)
(global-set-key (kbd "C-c f r")
                '(lambda ()
                   (interactive)
                   (flycheck-mode t)))
(global-set-key [(f5)] 'flycheck-previous-error)
(global-set-key [(f6)] 'flycheck-next-error)
