(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "DOING" "|" "VERIFY" "DONE" "DELEGATED")))

(setq org-todo-keyword-faces
      '(("DOING" . "yellow")
        ("PENDING" . "gray")))

(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; for reveal.js exporting
(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  :init (load-library "ox-reveal")
  :config (progn
            (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
                  )
            ))


(provide 'vmalloc-org-setup)
