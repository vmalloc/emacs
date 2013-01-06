(c-add-style "my/c-style"
  '("bsd"  ; this must be defined elsewhere - it is in cc-modes.el
  (c-basic-offset . 4)
  (c-echo-syntactic-information-p . t)
  (c-comment-only-line-offset . (0 . 0))
  (c-offsets-alist . (
    (c                     . c-lineup-C-comments)
    (statement-case-open   . 0)
    (func-decl-cont        . 0)
    (case-label            . +)
    (substatement-open     . 0)
    ))
  ))
(setq c-default-style '((java-mode . "java") (other . "my/c-style")))

(defun my/cancel-smartparens-if-active ()
  (condition-case ex ; we might not have smartparens at all...
      (let ((active-overlay (sp-get-active-overlay 'pair)))
        (when active-overlay
          (sp-remove-overlay active-overlay)))))

(defun my/c-mode-new-block () (interactive)
  (my/cancel-smartparens-if-active)
  (end-of-line)
  (newline)
  (insert "{")
  (c-indent-line-or-region)
  (newline)
  (insert "}")
  (c-indent-line-or-region)
  (next-line -1)
  (newline-and-indent))

(define-key c-mode-base-map [(meta return)] 'my/c-mode-new-block)
