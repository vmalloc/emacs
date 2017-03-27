(setq org-directory "~/Dropbox/orgs")

(setq org-default-notes-file (concat org-directory "/todo.org"))

(setq org-agenda-files (list org-directory))
(setq org-agenda-span 1)

(add-to-list 'org-emphasis-alist
             '("*" (:foreground "yellow")
               ))
(add-to-list 'org-emphasis-alist
            '("~" (:foreground "white")
               ))



(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "NEXT" "DOING" "|" "VERIFY" "DONE" "DELEGATED")))

(setq org-enforce-todo-dependencies t)

(setq org-todo-keyword-faces
      '(("DOING" . "yellow")
        ("NEXT" . "orange3")
        ("PENDING" . "gray")))

(setq org-priority-faces '((?A . (:foreground "OrangeRed3" :weight 'bold))
                           (?B . (:foreground "Orange2"))
                           (?C . (:foreground "DarkOliveGreen3"))))


(setq org-capture-templates
      '(("t" "Todo" entry (file "todo.org")
         "* TODO %?\n")
        ("T" "Link TODO" entry (file "todo.org")
         "* TODO [[%c][%^{Title for link}]]
%?
")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("l" "Toolbox Link" entry (file+headline "toolbox.org" "Links")
         "* [[%c][%^{Title for link}]]       :toolbox:link:
%?
")
        ("m" "Merge request" entry (file "merge_requests.org")
          "* NEEDS-CR Merge request: %? %c")
        ))

(setq org-refile-targets
      '(("todo.org" :maxlevel . 1)
        ("planned-meetings.org" :maxlevel . 3)
        ("projects.org" :maxlevel . 1)))

(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read))
  :after projectile
  :config
  (progn
    (setq org-projectile:projects-file (concat org-directory "/projects.org"))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p" org-projectile:linked-capture-template)))
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
