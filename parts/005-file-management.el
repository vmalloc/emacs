; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; recentf - save history of recently visited files
(require 'recentf)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)

; saveplace - save position in files (http://whattheemacsd.com/init.el-03.html)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (in-emacs-d ".places"))
