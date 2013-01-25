; Color theme
(cond
 ((>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
  (load-theme 'tomorrow-night-bright t))
 ((< emacs-major-version 24)
  (add-to-list 'load-path (in-emacs-d "legacy/themes/"))
  (load-library "color-theme")
  (require 'color-theme)
  (color-theme-initialize)
  (require 'color-theme-tomorrow)
  (color-theme-tomorrow-night-bright)
  (color-theme-install-faces '((region ((t (:background "white"))))))
))
