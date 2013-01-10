(defcustom long-line-limit 120
  "Lines containing more characters than this limit will be considered long"
  :type 'integer
  :group 'long-lines-highlight)

(require 'column-marker)
(defun mark-long-lines ()
  (interactive)
  (column-marker-3 long-line-limit))

(defun unmark-long-lines ()
  (interactive)
  (column-marker-3 -1))

(set-face-attribute 'column-marker-3 nil
                    :background "dark red")

(add-hook 'prog-mode-hook 'mark-long-lines)
