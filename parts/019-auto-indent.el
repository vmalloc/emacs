;; Move to the beginning of the text
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Return and indent on prog-mode variants
(defun my/set-newline-and-indent ()
  (message "newline and indent")
  (local-set-key [(return)] 'newline-and-indent))
(add-hook 'prog-mode-hook 'my/set-newline-and-indent)
