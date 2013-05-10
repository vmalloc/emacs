; Customizations beyond this configuration - separate to a different file
(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

; assume new files are always modified (useful for creating empty files)
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

; mark always active for selecting
(setq transient-mark-mode t)

; prevent dabbrev from replacing case
(setq dabbrev-case-replace nil)

; indentation/tabs
(setq default-tab-width 8)
(setq default-tab-indent 4)

; temporarily show line numbers when going-to-line (http://whattheemacsd.com//key-bindings.el-01.html)
(global-set-key (vector 'remap 'goto-line) 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

; enable all disabled commands
(setq disabled-command-function nil)

; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)

; display trailing whitespaces in prog-mode variants
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

; yes/no turns to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; don't ask about running (add-hook 'after-init-hook 
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

; reasonable code offset
(setq c-basic-offset 4)

;; Don't require double escaping the re-builder
(setq reb-re-syntax 'string)

;; Scroll one line at a time without recentering the screen
(setq scroll-step 1
      scroll-conservatively 10000)

; trigger prog-mode-hook on non-24 versions
(cond
 ((< emacs-major-version 24)
  (defun my/run-prog-mode-hooks ()
    (run-hooks 'prog-mode-hook))
  (add-hook 'c-mode-hook 'my/run-prog-mode-hooks)
  (add-hook 'c++-mode-hook 'my/run-prog-mode-hooks)
  (add-hook 'emacs-lisp-mode-hook 'my/run-prog-mode-hooks)
  (add-hook 'java-mode-hook 'my/run-prog-mode-hooks)
  (add-hook 'python-mode-hook 'my/run-prog-mode-hooks)))
