(defun turtle-fwd()
  (interactive)
  (insert "forward(0)")
  )

(defun turtle-left()
  (interactive)
  (insert "left(0)")
  )

(defun turtle-right()
  (interactive)
  (insert "right(0)")
  )


(defun compile-turtle()
  (interactive)
  (let ((buffer (current-buffer))
	(start (point-min))
	(end (point-max)))
    (with-temp-buffer
      (progn
	(insert "
import os
from turtle import *
forward(0)
os.system('''/usr/bin/osascript -e 'tell app \"Finder\" to set frontmost of process \"python3\" to true' ''')
")
	(insert-buffer-substring buffer start end)
	(insert "\nexitonclick()\n")
	(shell-command-on-region (point-min) (point-max) "python3 -")))))


(defvar turtle-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap of command `turtle-mode'.")


(define-minor-mode turtle-mode
  "Mode for programming turtle"
  :keymap turtle-mode-map
  )

(add-hook 'turtle-mode-hook (lambda ()
			      (text-scale-increase 5)
			      (flycheck-mode -1)))

(define-key turtle-mode-map (kbd "<C-up>") 'turtle-fwd)
(define-key turtle-mode-map (kbd "<C-left>") 'turtle-left)
(define-key turtle-mode-map (kbd "<C-right>") 'turtle-right)
(define-key turtle-mode-map (kbd "<f9>") 'compile-turtle)

(provide 'turtle-mode)
