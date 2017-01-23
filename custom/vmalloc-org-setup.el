(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "DOING" "|" "VERIFY" "DONE" "DELEGATED")))

(setq org-todo-keyword-faces
      '(("DOING" . "yellow")
        ("PENDING" . "gray")))

(setq org-priority-faces '((?A . (:foreground "OrangeRed3" :weight 'bold))
                           (?B . (:foreground "Orange2"))
                           (?C . (:foreground "DarkOliveGreen3"))))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))


(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read))
  :config
  (progn
    (setq org-projectile:projects-file (concat org-directory "/projects.org"))
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))
  :ensure t)

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
