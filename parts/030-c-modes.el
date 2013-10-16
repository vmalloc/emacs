(defun my/c-mode-new-block (prefix) (interactive "P")
  "Opens a new C-style (curly-brace) block after the current line.
With prefix arg, forces opening brace to be in a new line"
  (beginning-of-line)
  (back-to-indentation)
  (let ((current-statement (current-word)))
    (end-of-line)
    (if (and (not prefix) (-contains? '("class" "do" "else" "for" "if" "struct" "switch" "while") current-statement))
        (insert " {")
      (newline)
      (insert "{")
      (c-indent-line-or-region))
    (newline)
    (insert "}")
    (if (-contains? '("class" "struct") current-statement)
        (insert ";"))
    (c-indent-line-or-region)
    (previous-line)
    (end-of-line)
    (newline-and-indent)))

(setq gdb-show-main t)
(add-hook 'c-mode-common-hook '(lambda ()
                                (define-key c-mode-base-map [(meta return)] 'my/c-mode-new-block)
				(local-set-key (kbd "C-c g") 'gdb-many-windows)))

(setq c-default-style "bsd"
      c-basic-offset 4)

(add-hook 'prog-mode-hook '(lambda ()
                             (unless (derived-mode-p 'makefile-mode) (setq indent-tabs-mode nil))))
