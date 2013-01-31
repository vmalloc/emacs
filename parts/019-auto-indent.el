;; Auto indent when yanking
(setq auto-indent-on-yank-or-paste t)

;; Move to the beginning of the text rather then the beginning of the line
(setq auto-indent-home-is-beginning-of-indent t)

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
