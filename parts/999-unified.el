;;; 999-unified.el --- Unified configuration

;; Copyright (C) 2016 Rotem Yaari
;; Author: Rotem Yaari <rotemy@Rotems-MacBook-Pro.local>
;; Created: 2016-05-29
;; Version:
;; Keywords:
;; URL:
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Generic settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(require 'vmalloc-reset-emacs)
(vmalloc-reset-emacs)


; Try to display battery info (only if applicable)
(condition-case ex
    (display-battery-mode t)
  ('error (message "Cannot display battery"))
  )



(use-package dash
  :ensure t
  :defer t)

;; Buffer management

; prevent killing scratch
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Key assist
(use-package which-key
  :ensure t)


;; Window Movement -------------------------------------------------------------

;; Window moving
(use-package windmove
  :bind (
         ("<M-up>" . windmove-up)
         ("<M-down>" . windmove-down)
         ("<M-right>" . windmove-right)
         ("<M-left>" . windmove-left)))


;; File Management -------------------------------------------------------------

(use-package recentf
  :config (recentf-mode 1))


(use-package crux
  :defer t
  :ensure t)

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
   (
     ("M-i" . helm-semantic-or-imenu)
     ("C-x y" . helm-show-kill-ring)
   ))

(use-package swiper
  :ensure t
  :config (setq projectile-completion-system 'ivy)
  :bind (
         ("C-x b" . ivy-switch-buffer)
         ("C-c h" . ivy-recentf)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map projectile-command-map
         ("s s" . counsel-ag)
         )
  )


(use-package projectile
  :ensure t
  :init
  (projectile-global-mode 1)
  :bind (
         ("s-p" . projectile-find-file)
  ))


(use-package flycheck
  :config
  (progn
    (mapcar '(lambda (m) (add-hook m '(lambda () (flycheck-mode))))
            (list 'python-mode-hook 'js-mode-hook))
    (add-to-list 'prog-mode-hook '(lambda () (flycheck-mode)))
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
(add-to-list 'prog-mode-hook '(lambda ()
                            (subword-mode t)
                            (define-key prog-mode-map (kbd "<C-left>") 'backward-word)
                            (define-key prog-mode-map (kbd "<C-right>") 'forward-word)))


(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))



(-map (lambda (hook)
        (add-to-list hook '(lambda () (setq show-trailing-whitespace t))))
      (list 'prog-mode-hook))


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

;; Editing ---------------------------------------------------------------------

(use-package avy
  :ensure t
  :bind (("s-;" . avy-goto-char)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-symbol-like-this)
         ("C-." . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-symbol-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-;" . mc/mark-all-symbols-like-this)
         ("C-c m l" . mc/edit-ends-of-lines)))


(use-package vmalloc-multiple-cursors-utils
  :bind (("C-:" . vmalloc/mark-all-symbols-like-this-in-defun)))

(use-package vmalloc-powerline-settings
  :config (vmalloc-powerline-setup)
  )

(use-package vmalloc-editing-utils
  )

(use-package diff-hl
  :ensure t
  :config (add-hook 'prog-mode-hook '(lambda () (diff-hl-mode))))

(use-package undo-tree
  :ensure t
  :config (progn
            (global-undo-tree-mode t)
            ;; Keep region when undoing in region
            (defadvice undo-tree-undo (around keep-region activate)
              (if (use-region-p)
                  (let ((m (set-marker (make-marker) (mark)))
                        (p (set-marker (make-marker) (point))))
                    ad-do-it
                    (goto-char p)
                    (set-mark m)
                    (set-marker p nil)
                    (set-marker m nil))
                ad-do-it)))
  :diminish undo-tree-mode)

(defcustom python-snippet-debugger "pdb"
  "Which python debugger should be used in the pdb template"
  :type 'string
  :group 'yasnippet)


(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config (progn
            (setq yas-snippet-dirs (list (in-emacs-d "snippets") yas-installed-snippets-dir))

            (setq yas-indent-line 'fixed) ; for indented snippets
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            ;; Set Yasnippet's key binding to shift+tab
            (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
            ))




;; Autocomplete
; auto-complete

(use-package auto-complete
  :ensure t
  :config (progn
            (require 'auto-complete-config)
            (global-auto-complete-mode t)
            (ac-config-default)

            (defun ac-python-mode-setup ()
              (setq ac-sources (append '(ac-source-yasnippet ac-source-semantic) ac-sources)))

            (add-hook 'python-mode-hook 'ac-python-mode-setup)

            ))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Misc Development features
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package jira-commit
  :bind (("C-c J" . commit-jira-issue-interactive)))

(use-package iterm
  :config (progn
            (global-set-key (kbd "S-<f9>")  'shell-command-on-iterm)
            (global-set-key (kbd "C-S-<f9>")  'repeat-last-shell-command-on-iterm)
            (global-set-key (kbd "C-M-s-<f9>")  '(lambda ()
                                    (interactive)
                           (setq current-prefix-arg t)
                           (call-interactively 'shell-command-on-iterm)))))

;;
;; Programming modes
;; #############################################################################


;; misc ------------------------------------------------------------------------
(use-package company
  :ensure t)

;; Rust ------------------------------------------------------------------------
(use-package racer
  :ensure t
  :config (progn
            (setq racer-cmd "~/.cargo/bin/racer")
            (setq racer-rust-src-path "~/src/opensource/rust/rust/src")
            (add-hook 'racer-mode-hook #'company-mode)
  ))

(use-package rust-mode
  :ensure t
  :mode (("\.rs$" . rust-mode)))

;; Python ----------------------------------------------------------------------
(use-package python-extra
  :bind (
         :map python-mode-map
              ("C-c p I" . pylint-ignore-errors-at-point)))


;; Web
;; #############################################################################


(use-package nginx-mode
  :ensure t)


(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
         ("\\.hbs$" . web-mode))

  :config
  (add-hook 'web-mode-hook  '(lambda ()
                                 (emmet-mode)
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


(use-package scss-mode
  :ensure t
  :init
  (add-hook 'scss-mode-hook '(lambda ()
                              (setq show-trailing-whitespace t)
                              (define-key css-mode-map (kbd "M-i") 'helm-css-scss))))


(use-package ember-mode
  :ensure t
  :bind
  (("C-c e m" . ember-open-model)
   ("C-c e t" . ember-open-template)
   ("C-c e r" . ember-open-route)
   ("C-c e c" . ember-open-controller)))


;; Org mode ---------------------------------------------------------------------

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
    (setq org-src-fontify-natively t)
    (defadvice org-kill-line (after fix-cookies activate)
      (myorg-update-parent-cookie))

    (defadvice kill-whole-line (after fix-cookies activate)
      (myorg-update-parent-cookie))))


;; Server -----------------------------------------------------------------------

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))


;; Utilities -------------------------------------------------------------------

(use-package restclient
  :ensure t)


(provide '999-unified)
;;; 999-unified.el ends here
