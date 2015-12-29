; org-mode

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-mode-map [(meta left)]    nil)
                (define-key org-mode-map [(meta right)]   nil)
                (define-key org-mode-map [(meta down)]    nil)
              (define-key org-mode-map [(meta up)]   nil)
              (define-key org-mode-map (kbd "M-h")    nil))
              'append)
    (defadvice org-kill-line (after fix-cookies activate)
      (myorg-update-parent-cookie))

    (defadvice kill-whole-line (after fix-cookies activate)
      (myorg-update-parent-cookie))))
