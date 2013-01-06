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
