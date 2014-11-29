;; Mac-specific
(setq ns-function-modifier 'hyper)

;; Home/end
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)

;; Page Up/Down half screen scroll
(global-set-key (kbd "<prior>") 'View-scroll-half-page-backward)
(global-set-key (kbd "<next>") 'View-scroll-half-page-forward)

;; F keys
(eval-after-load "c-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(eval-after-load "cc-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(global-set-key [(f9)] 'projectile-compile-project)
(global-set-key [(f10)] 'previous-error)
(global-set-key [(f11)] 'next-error)
;; deleting trailing whitespaces
(global-set-key [(f12)] 'delete-trailing-whitespace)


;; Open shell with C-z
(global-set-key (kbd "C-z") 'shell)

;; Browse URLs with C-x /
(global-set-key (kbd "C-x /") 'browse-url)

;; Override news with man
(global-set-key (kbd "C-h n") 'man)

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
(global-set-key (kbd "C-}") 'text-scale-increase)
(global-set-key (kbd "C-{") 'text-scale-decrease)

;; commenting/uncommenting
(defun my/comment-or-uncomment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-x ;") 'my/comment-or-uncomment-current-line)

;; Error jumping
(global-set-key (kbd "C-x <C-down>") 'next-error)
(global-set-key (kbd "C-x <C-up>") 'previous-error)

;; Line joining
(add-hook 'prog-mode-hook '(lambda () (global-set-key (kbd "M-j")
                                           (lambda ()
                                             (interactive)
                                             (join-line -1)))))

;; override weird keys on js2-mode
(add-hook 'js2-mode-hook '(lambda ()
                            (define-key js2-mode-map (kbd "M-j") nil)
                            (define-key js2-mode-map (kbd "C-c C-e") nil)))




;; Kill/save the active region or the current line
(defun kill-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(global-set-key (kbd "C-w") 'kill-line-or-region)

(defun save-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(global-set-key (kbd "M-w") 'save-line-or-region)

;; Goto function definitions
(global-set-key (kbd "C-h C-f") 'find-function)

;; Remove C-c [num] from .rst mode

(add-hook 'rst-mode-hook '(lambda ()
                                  (mapcar (lambda (k) (define-key rst-mode-map (kbd (format "C-c %s" k)) nil))
                                          (list 1 2 3 4))))
