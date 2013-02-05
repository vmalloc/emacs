;; Auto indent when yanking
(setq auto-indent-on-yank-or-paste t)

;; Move to the beginning of the text rather then the beginning of the line
;; Don't use auto-indent to do that, since it causes problems in Python
(setq auto-indent-home-is-beginning-of-indent nil)
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

;; Prevent auto-indent from assigning its own indentation levels
(setq auto-indent-assign-indent-level-variables nil)

;; Setting this to t is annoything when displaying leading whitespaces
(setq auto-indent-blank-lines-on-move nil)

;; Delete any whitespace/tab until the previous character (Doesn't delete new lines)
(setq auto-indent-backward-delete-char-behavior 'hungry)

;; Don't reindent the previous line on newline
(setq auto-indent-newline-function 'newline-and-indent)

(require-from-modes-d "auto-indent-mode")
(auto-indent-global-mode)
