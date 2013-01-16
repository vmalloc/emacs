;; Mac-specific
(setq ns-function-modifier 'hyper)

;; Home/end
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)

;; Return and indent on prog-mode variants
(defun my/set-newline-and-indent ()
  (message "newline and indent")
  (local-set-key [(return)] 'newline-and-indent))
(add-hook 'prog-mode-hook 'my/set-newline-and-indent)

;; F keys
(eval-after-load "c-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(eval-after-load "cc-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(global-set-key [(f7)] 'magit-status)
(global-set-key [(control x) (f7)] 'magit-branch-manager)
(global-set-key [(f9)] 'compile)
(global-set-key [(f12)] 'delete-trailing-whitespace)

;; Open shell with C-z
(global-set-key (kbd "C-z") 'shell)

;; Scroll without moving the cursor
(global-set-key "\C-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\C-p"  (lambda () (interactive) (scroll-down 4)) )
(global-set-key "\M-n"  (lambda () (interactive) (scroll-other-window   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-other-window-down 4)) )

;; Window moving
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-left>") 'windmove-left)

;; zooming
(global-set-key (kbd "C-}") 'my/zoom-in-font)
(global-set-key (kbd "C-{") 'my/zoom-out-font)

;; commenting/uncommenting
(defun my/comment-or-uncomment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-x ;") 'my/comment-or-uncomment-current-line)
