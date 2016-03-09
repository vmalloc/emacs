;;; jira-commit.el --- commit by jira Issue interactively

;;; Commentary:

;;; Code:

(require 'magit)
(require 's)
(require 'json)
(require 'dash)


(defcustom jira-commit-jira-hostname ""
  "The hostname of the Jira server."
  :group 'jira-commit
  :type 'string
  :safe 'stringp)

(defcustom jira-commit-credentials-file "~/.config/emacs-jira-commit/credentials.el"
  "Location of the jira-commit credentials."
  :group 'jira-commit
  :type 'string
  :safe 'stringp)

(defcustom jira-commit-num-recent-issues 50
  "Number of recent issues to query."
  :group 'jira-commit
  :type 'int
  :safe 'intp)


(defun commit-jira-issue-interactive()
  (interactive)
  (let ((issues (--get-issues)))
    (let ((options (--map (format "%s: %s" (gethash "key" it) (gethash "summary" (gethash "fields" it))) issues)))
      (let ((selected (--get-choice options)))
        (if selected
            (magit-run-git-with-editor "commit" "-a" "-e" "-m" selected)
          (message "Aborted"))))))

(defun --get-choice (candidates)
  (helm :sources (helm-build-sync-source "Jira Issues"
                 :candidates options
                 :action (helm-make-actions "commit"  'identity
                                            "commit-and-fix" '--transform-to-fix
                                            "commit-no-message" '--transform-no-message
                                            )

                 :fuzzy-match t)
              :buffer "*Jira Commit*"))


(defun --get-issues ()
  (let ((url (concat (--get-jira-url) (--get-jira-query-string))))
    (let ((output (shell-command-to-string (concat "curl \"" url "\" 2>&/dev/null"))))
      (let ((json-object-type 'hash-table))
        (let ((issues-json (json-read-from-string output)))
          (gethash "issues" issues-json))))))


(defun --get-jira-url ()
  (concat "https://" (--get-jira-username) ":" (--get-jira-password) "@" jira-commit-jira-hostname "/rest/api/2/search")
  )

(defun --get-jira-username ()
  (car (--get-jira-auth))
  )

(defun --get-jira-password ()
  (cadr (--get-jira-auth))
  )

(defun --transform-to-fix (msg)
  (let ((parts (s-split-up-to ":" msg 1)))
    (let ((issue-key (car parts))
          (issue-summary (s-trim (cadr parts))))
      (format "%s (fix %s)" issue-summary issue-key))))

(defun --transform-no-message (msg)
  (let ((parts (s-split-up-to ":" msg 1)))
    (format "%s: " (car parts))))

(defun --get-jira-auth ()
  (let ((jira-commit-jira-username nil)
        (jira-commit-jira-password nil))
    (if (not (file-exists-p jira-commit-credentials-file))
        (progn
          (let ((dirname (file-name-directory jira-commit-credentials-file)))
            (if (not (file-exists-p dirname))
                (mkdir dirname)))
          (let ((username (read-string "Jira Username: "))
                (password (read-passwd "Jira Password: ")))
            (append-to-file
             (format "(setq jira-commit-jira-username \"%s\")\n(setq jira-commit-jira-password \"%s\")\n"
                     username password)
             nil
             jira-commit-credentials-file))))
    (load-file jira-commit-credentials-file)
    (list jira-commit-jira-username jira-commit-jira-password)))

(defun --get-jira-query-string ()
  (concat "?jql=assignee%20%3D%20currentUser%28%29%20and%20resolution%20is%20empty%20order%20by%20updated%20desc&fields=key%2Csummary&maxResults=" (format "%d" jira-commit-num-recent-issues)))

;;; jira-commit.el ends soon
(provide 'jira-commit)
;; Local Variables:
;; End:
;;; jira-commit.el ends here

