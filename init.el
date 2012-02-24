; --------  Preamble ---------
; init.el utils
(defun in-emacs-d (path)
  (concat "~/.emacs.d/" path))
(setq mode-dir (in-emacs-d "modes/"))
(defun in-modes-d (path)
  (concat mode-dir path))
(setq utils-dir (in-emacs-d "utils/"))
(defun in-utils-d (path)
  (concat utils-dir path))
(add-to-list 'load-path mode-dir)
(setq in-terminal (not window-system))

(defun autoload-and-run (symbol file interactive callback)
  (autoload symbol file nil interactive)
  (eval-after-load (symbol-name symbol) callback)
  )

; Display settings
(setq inhibit-splash-screen t)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(display-time) ; useful for full-screen terminals
(display-battery-mode t)
(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(setq ring-bell-function 'ignore)

; Color theme
(cond 
 ((>= emacs-major-version 24)  
  (add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
  (load-theme 'vmalloc t))
 ((< emacs-major-version 24)  
  (add-to-list 'load-path (in-emacs-d "themes-legacy/"))
  (load-library "color-theme")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-dark-laptop)))

; recentf - save history of recently visited files
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)

; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; Usability
;    some sane keyboard bindings
;        home/end
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)
;    window moving
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
;    ibuffer key binding
(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)

;    yes/no turns to y/n
(fset 'yes-or-no-p 'y-or-n-p)
;    don't ask about running processes
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

; Terminal settings
; properly handle SHIFT+up for selection
(if in-terminal
    (define-key input-decode-map "\e[1;2A" [S-up]))

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
(global-linum-mode t)
; C
(setq c-basic-offset 4)

; Python
(let ((python-mode-dir (in-emacs-d "modes/python-mode/")))
  (add-to-list 'load-path python-mode-dir)
  (setq py-install-directory python-mode-dir)
  (autoload 'python-mode "python-mode.el" nil t))

; Haskell
(autoload 'haskell-mode (in-modes-d "haskell-mode/haskell-mode.el") nil t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'turn-on-haskell-ghci "haskell-ghci"
  "Turn on interaction with a GHCi interpreter." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

; HTML
(autoload 'nxhtml-mode (in-modes-d "nxhtml-mode/autostart.el") nil t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxhtml-mode))


; ------- Modes ---------
; ido
(ido-mode)
(require 'ido-recentf-open)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

; anything
(add-to-list 'load-path (in-modes-d "anything"))
(autoload-and-run 'anything "anything.el" t 
		  '(progn 
		    (require 'anything-config)
		    (add-to-list 'anything-sources 'anything-c-source-locate)
		    (add-to-list 'anything-sources 'anything-c-source-mac-spotlight)
		    (add-to-list 'anything-sources 'anything-c-source-kill-ring)
		    (add-to-list 'anything-sources 'anything-c-source-occur)
		    ))
(autoload 'anything-config "anything-config.el")
(global-set-key [(control x) (a)] 'anything)


; smex (ido for M-x commands)
(autoload 'smex-initialize (in-modes-d "smex/smex.el"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

; show-paren
(show-paren-mode)

; magit
(add-to-list 'load-path (in-modes-d "magit"))
(autoload 'magit-status "magit" nil t)

; uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; auto-complete
(add-to-list 'load-path (in-modes-d "auto-complete"))
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

; autopair
(require 'autopair)
(autopair-global-mode)
;    compatibility with delete-selection-mode
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

; YASnippet
(add-to-list 'load-path (in-modes-d "yasnippet"))
(require 'yasnippet)
(yas/global-mode 1)
(setq yas/indent-line 'fixed) ; for indented snippets

; ace-jump - quickly navigate to any character
(autoload 'ace-jump-char-mode (in-modes-d "ace-jump-mode/ace-jump-mode.el") nil t)
(setq ace-jump-mode-case-sensitive-search nil)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)

; drag stuff
(autoload 'drag-stuff-global-mode (in-modes-d "drag-stuff/drag-stuff.el") nil t)
(drag-stuff-global-mode t)
(define-key drag-stuff-mode-map (kbd "<C-M-up>") 'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "<C-M-down>") 'drag-stuff-down)

; org-mode
(add-to-list 'load-path (in-modes-d "org-mode/lisp"))
(autoload 'org-mode "org.el" nil t)

; nyan-mode (no .emacs.d is whole without it)
(autoload 'nyan-mode (in-modes-d "nyan-mode/nyan-mode.el") nil t)

; ------- Utilities -----
; browse-kill-ring
(autoload 'browse-kill-ring (in-utils-d "browse-kill-ring.el") nil t)

; rainbow-mode
(autoload 'rainbow-mode (in-utils-d "rainbow-mode.el") nil t)
(add-to-list 'find-file-hook 'rainbow-mode)

(autoload 'python-auto-super (in-utils-d "python-auto-super.el") nil t)
(eval-after-load "python-mode" '(define-key python-mode-map [(control ?x) ?p ?s] 'python-auto-super))
(eval-after-load "python-mode" '(load-file (in-modes-d "virtualenv/virtualenv.el")))

(setq pylookup-dir (in-utils-d "pylookup"))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(add-to-list 'load-path pylookup-dir)
(autoload 'pylookup-lookup "pylookup.el" nil t)
(eval-after-load "python-mode" '(define-key python-mode-map [(control ?x) ?p ?l] 'pylookup-lookup))

; ------- Keyboard shortcuts -----
; F keys
(eval-after-load "c-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(eval-after-load "cc-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(global-set-key [(f7)] 'magit-status)
(global-set-key [(f12)] 'delete-trailing-whitespace)

; Customizations beyond this configuration - separate to a different file
(setq custom-file "~/.emacs-custom.el")
(if (file-exists-p custom-file) 
    (load-file custom-file))
