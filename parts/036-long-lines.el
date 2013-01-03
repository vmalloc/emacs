(require 'column-marker)
(defun mark-long-lines ()
  (interactive)
  (column-marker-3 80))

(defun unmark-long-lines ()
  (interactive)
  (column-marker-3 -1))

(set-face-attribute 'column-marker-3 nil
                    :background "dark red")

(add-hook 'python-mode-hook 'mark-long-lines)
