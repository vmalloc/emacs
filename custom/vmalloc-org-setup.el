(setq org-todo-keywords
       '((sequence "TODO" "DOING" "PENDING" "|" "VERIFY" "DONE" "DELEGATED")))

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

(provide 'vmalloc-org-setup)
