; --------  Preamble ---------
; init.el utils
(defun in-emacs-d (path)
  (concat "~/.emacs.d/" path))

(setq in-terminal (not window-system))

; Window settings
(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode nil))

; Customizations beyond this configuration - separate to a different file
(setq custom-file "~/.emacs-custom.el")

; Color theme
(add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
(load-theme 'manoj-dark t)

; Autosave/recentf
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; Usability
(fset 'yes-or-no-p 'y-or-n-p) ; yes/no turns to y/n

; Always-on modes
(ido-mode)

; --------  Basic editing facilities ---------

; assume new files are always modified (useful for creating empty files)
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

; mark always active for selecting
(setq transient-mark-mode t)

; prevent dabbrev from replacing case
(setq dabbrev-case-replace nil)

; indentation/tabs
(setq default-tab-width 8)
(setq default-tab-indent 4)

; -------- Languages --------
; C
(setq c-basic-offset 4)

; Python
(let ((python-mode-dir (in-emacs-d "modes/python-mode/")))
  (add-to-list 'load-path python-mode-dir)
  (autoload 'python-mode "python-mode" "python-mode" t)
  (setq py-install-directory python-mode-dir))

