; org-mode
(require 'org)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [(meta left)]    nil)
            (define-key org-mode-map [(meta right)]   nil)
            (define-key org-mode-map [(meta down)]    nil)
            (define-key org-mode-map [(meta up)]   nil))
          'append)
