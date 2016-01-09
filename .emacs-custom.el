(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-executable "/usr/local/bin/ag")
 '(blink-cursor-mode nil)
 '(cua-rectangle-mark-key (kbd "C-x <C-return>"))
 '(custom-safe-themes
   (quote
    ("705f3f6154b4e8fac069849507fd8b660ece013b64a0a31846624ca18d6cf5e1" "4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" "8122f00211dbaf973fbe5831f808af92387c8fc1a44f0c6bcc9b22c16997c9dd" default)))
 '(flycheck-pylintrc "~/.pylintrc")
 '(helm-ff-transformer-show-only-basename nil)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(iflipb-ignore-buffers nil)
 '(minimap-window-location (quote right))
 '(ns-pop-up-frames nil)
 '(projectile-ack-function (quote ag))
 '(projectile-global-mode t)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "venv" "build" ".env")))
 '(set-mark-command-repeat-pop t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 0)
 '(web-mode-style-padding 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:background "black" :foreground "yellow" :weight bold))))
 '(flycheck-error-face ((t (:background "dark red"))) t)
 '(flycheck-warning-face ((t (:background "RoyalBlue4"))) t)
 '(helm-ff-directory ((t (:foreground "yellow"))))
 '(helm-selection ((t (:background "dark red" :underline t))))
 '(highlight ((t (:background "Yellow" :foreground "black"))))
 '(hl-line ((t (:background "#333"))))
 '(linum ((t (:inherit (shadow default) :background "#000" :foreground "#444"))))
 '(magit-diff-add ((t (:inherit diff-added :foreground "chartreuse"))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "orange red"))))
 '(magit-item-highlight ((t (:background "RoyalBlue4"))))
 '(mc/cursor-face ((t (:background "deep sky blue" :foreground "black"))))
 '(minimap-active-region-background ((t (:background "#333"))))
 '(mode-line ((t (:background "DarkGoldenrod2" :foreground "black"))))
 '(region ((t (:background "White" :foreground "black"))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(rst-level-2-face ((t (:background "grey78" :foreground "black"))) t)
 '(rst-level-3-face ((t (:background "grey71" :foreground "black"))) t)
 '(rst-level-4-face ((t (:background "grey64" :foreground "black"))) t)
 '(rst-level-5-face ((t (:background "grey57" :foreground "black"))) t)
 '(rst-level-6-face ((t (:background "#ddf" :foreground "black"))) t))
