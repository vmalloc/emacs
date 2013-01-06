
(defun my/cancel-smartparens-if-active ()
  (condition-case ex ; we might not have smartparens at all...
      (let ((active-overlay (sp-get-active-overlay 'pair)))
        (when active-overlay
          (sp-remove-overlay active-overlay)))))

(defun my/c-mode-new-block () (interactive)
  (my/cancel-smartparens-if-active)
  (beginning-of-line)
  (back-to-indentation)
  (let ((current-statement (current-word)))
    (end-of-line)
    (if (-contains? '("class" "do" "else" "for" "if" "struct" "switch" "while") current-statement)
        (insert " {")
      (newline)
      (insert "{")
      (c-indent-line-or-region))
    (newline)
    (insert "}")
    (c-indent-line-or-region)
    (previous-line)
    (end-of-line)
    (newline-and-indent)))

(add-hook 'c-mode-common-hook '(lambda ()
                                (define-key c-mode-base-map [(meta return)] 'my/c-mode-new-block)))
