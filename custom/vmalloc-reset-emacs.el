;;; vmalloc-reset-emacs.el --- Reset emacs visual style and appearance

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

(defun vmalloc-reset-emacs()
  (menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.

  (if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))

  (setq inhibit-splash-screen t) ; get rid of splash screen
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))

  (if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (setq ring-bell-function 'ignore)

  )

(provide 'vmalloc-reset-emacs)
;;; vmalloc-reset-emacs.el ends here
