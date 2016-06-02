;;; reset-emacs.el --- Reset emacs visual style and appearance

;; Copyright (C) 2016 Rotem Yaari
;; Author: Rotem Yaari <vmalloc@gmail.com>
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
;; Misc
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(menu-bar-mode -1)    ; get rid of the annoying menubars/toolbars etc.

(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))

(setq inhibit-splash-screen t)          ; get rid of splash screen
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(setq ring-bell-function 'ignore)

;; assume new files are already modified
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Autosave
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; lock files (pesky .# files) disabling
(setq create-lockfiles nil)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Sane keybindings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Mac-specific
(setq ns-function-modifier 'hyper)

;; Home/end
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)

;; pgup/pgdn
(global-set-key (kbd "<prior>") 'cua-scroll-down)
(global-set-key (kbd "<next>") 'cua-scroll-up)

;; line joining
(add-hook 'prog-mode-hook '(lambda () (global-set-key (kbd "s-j")
					   (lambda ()
					     (interactive)
					     (join-line -1)))))

;; Kill/save the active region or the current line
(defun my/kill-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(global-set-key (kbd "C-w") 'my/kill-line-or-region)

(defun my/save-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(global-set-key (kbd "M-w") 'my/save-line-or-region)

;; line manipulation
; Make C-o / C-S-o work like in VIM
(defun my/insert-line-before ()
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-for-tab-command))

(defun my/insert-line-after ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'my/insert-line-after)
(global-set-key (kbd "C-S-o") 'my/insert-line-before)


(provide 'vmalloc-reset-emacs)
;;; reset-emacs.el ends here
