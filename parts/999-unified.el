;; Base settings ---------------------------------------------------------------

(use-package dash
  :ensure t
  :defer t)

;; File Management -------------------------------------------------------------

(use-package recentf
  :config (recentf-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package helm
  :ensure t
  :init
  (progn
    (helm-mode t)
    (setq helm-input-idle-delay 0)
    (setq helm-exit-idle-delay 0)
    (require 'helm-config)
    )
  :bind
   (("C-c h" . helm-recentf)
     ("M-i" . helm-semantic-or-imenu)
     ("C-x y" . helm-show-kill-ring)
     ("M-x" . helm-M-x)
     ("C-x C-f" . helm-find-files)
   ))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode 1)
  )

(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on))

(use-package helm-ag
  :ensure t)

(use-package flycheck
  :config
  (progn
    (mapcar '(lambda (m) (add-hook m '(lambda () (flycheck-mode))))
            (list 'python-mode-hook 'js-mode-hook))
    (add-hook prog-mode-hook '(lambda () (flycheck-mode)))
    (setq flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          (`not-checked nil)
          (`no-checker (propertize " -" 'face 'warning))
          (`running (propertize " ✷" 'face 'success))
          (`errored (propertize " !" 'face 'error))
          (`finished
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                  (no-errors (cdr (assq 'error error-counts)))
                  (no-warnings (cdr (assq 'warning error-counts)))
                  (face (cond (no-errors 'error)
                              (no-warnings 'warning)
                              (t 'success))))
             (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                         'face face)))
          (`interrupted " -")
          (`suspicious '(propertize " ?" 'face 'warning))))))
  :bind (([(f5)] . flycheck-previous-error)
         ([(f6)] . flycheck-next-error)))


;; Defaults --------------------------------------------------------------------

;; Make the super key work in Windows
(setq
 w32-pass-lwindow-to-system nil
 w32-pass-rwindow-to-system nil
 w32-pass-apps-to-system nil
 w32-lwindow-modifier 'super ; Left Windows key
 w32-rwindow-modifier 'super ; Right Windows key
 w32-apps-modifier 'hyper) ; Menu key


;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

; mark always active for selecting
(setq transient-mark-mode t)

; prevent dabbrev from replacing case
(setq dabbrev-case-replace nil)

; indentation/tabs
(setq default-tab-width 8)
(setq default-tab-indent 4)

; enable all disabled commands
(setq disabled-command-function nil)

; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)

; display trailing whitespaces in prog-mode variants
(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)
                            (subword-mode t)
                            (define-key prog-mode-map (kbd "<C-left>") 'backward-word)
                            (define-key prog-mode-map (kbd "<C-right>") 'forward-word)
))

;; yes/no turns to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; reasonable code offset
(setq c-basic-offset 4)

;; Don't require double escaping the re-builder
(setq reb-re-syntax 'string)

;; Scroll one line at a time without recentering the screen
(setq scroll-step 1
      scroll-conservatively 10000)


;; fix path for mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ido -------------------------------------------------------------------------

(use-package ido-mode
  :config
  (progn
    (ido-mode)
    (setq ido-max-prospects 200)

    (setq ido-auto-merge-work-directories-length -1)))


(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode))

(use-package ido-recentf-open
  :bind (("C-x C-r" . ido-recentf-open)))


;; Editing ---------------------------------------------------------------------

(use-package avy
  :ensure t
  :bind (("s-;" . avy-goto-char)))


;; Programming modes -----------------------------------------------------------

(use-package rust-mode
  :ensure t
  :mode (("\.rs$" . rust-mode)))


;; Web mode --------------------------------------------------------------------

(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
         ("\\.hbs$" . web-mode))

  :config
  (add-hook 'web-mode-hook  '(lambda ()
                                 (setq web-mode-markup-indent-offset 2)
                                 (setq web-mode-css-indent-offset 2)
                                 (setq web-mode-code-indent-offset 2))))

(use-package emmet-mode
  :ensure t
  :config
  (progn
    (setq emmet-indentation 2)
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (define-key emmet-mode-keymap (kbd "<C-return>") nil)
    (define-key emmet-mode-keymap (kbd "C-c i") 'emmet-expand-line)))


(use-package css-mode
  :init
  (add-hook 'css-mode-hook '(lambda ()
                              (define-key css-mode-map (kbd "M-i") 'helm-css-scss))))




;;-------------- Server mode ---------------

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))


;; Utilities -------------------------------------------------------------------

(use-package restclient
  :ensure t)

