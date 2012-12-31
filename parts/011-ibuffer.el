(require 'ibuffer)
(setq ibuffer-saved-filter-groups
  (quote (("default"
            ("Web"
             (or
              (mode . nxhtml-mode)
	      (mode . web-mode)
              (mode . javascript-mode)
              (mode . js-mode)
              (mode . css-mode)
              )
             )
            ("Programming"
              (or
                (mode . c++-mode)
                (mode . c-mode)
                (mode . emacs-lisp-mode)
                (mode . makefile-gmake-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . sh-mode)
                ;; etc
                ))
            ("Elisp"
              (mode . emacs-lisp-mode))
            ("Magit"
              (name . "\*magit.*\*"))
            ("Terminal"
              (mode . term-mode))
            ("Emacs"
              (name . "\*.*\*"))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)
