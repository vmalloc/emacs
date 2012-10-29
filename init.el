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
(add-to-list 'load-path utils-dir)

(setq in-terminal (not window-system))

(if (not in-terminal)
    (server-start))

; add /usr/local/bin to exec-path
(add-to-list 'exec-path "/usr/local/bin")

; enable all disabled commands
(setq disabled-command-function nil)

(defun autoload-and-run (symbol file interactive callback)
  (autoload symbol file nil interactive)
  (eval-after-load (symbol-name symbol) callback)
  )

; Display settings
(setq inhibit-splash-screen t)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(display-time) ; useful for full-screen terminals

; Try to display battery info (only if applicable)
(condition-case ex
    (display-battery-mode t)
  ('error (message "Cannot display battery"))
  )

(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(setq ring-bell-function 'ignore)

; YASnippet - should appear before custom-set-variables
(add-to-list 'load-path (in-modes-d "yasnippet"))
(require 'yasnippet)
(yas/global-mode 1)
(setq yas/indent-line 'fixed) ; for indented snippets

; display trailing whitespaces
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

; Customizations beyond this configuration - separate to a different file
;(if (file-exists-p custom-file)
;    (load-file custom-file))

; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)

; Color theme
(cond
 ((>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
  (load-theme 'tomorrow-night-bright t))
 ((< emacs-major-version 24)
  (add-to-list 'load-path (in-emacs-d "legacy/themes/"))
  (load-library "color-theme")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-dark-laptop)))

; recentf - save history of recently visited files
(autoload 'recentf-mode "recentf.el" nil t)
(autoload 'recentf-save-list "recentf.el" nil t)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)


; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; Usability
;    some sane keyboard bindings
;        home/end
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)
(add-hook 'prog-mode-hook (lambda () (local-set-key [(return)] 'newline-and-indent)))
;    window moving
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-left>") 'windmove-left)
;    ibuffer key binding
(setq ibuffer-saved-filter-groups
  (quote (("default"
            ("Web"
             (or
              (mode . nxhtml-mode)
	      (mode . web-mode)
              (mode . javascript-mode)
              (mode . js-mode)
              (mode . css-mode)
              )
             )
            ("Programming"
              (or
                (mode . c++-mode)
                (mode . c-mode)
                (mode . emacs-lisp-mode)
                (mode . makefile-gmake-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . sh-mode)
                ;; etc
                ))
            ("Elisp"
              (mode . emacs-lisp-mode))
            ("Magit"
              (name . "\*magit.*\*"))
            ("Terminal"
              (mode . term-mode))
            ("Emacs"
              (name . "\*.*\*"))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;    yes/no turns to y/n
(fset 'yes-or-no-p 'y-or-n-p)
;    don't ask about running processes
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

; Terminal settings
; properly handle SHIFT+up for selection
(defadvice terminal-init-xterm (around map-keys-properly activate)
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;4A" [M-up])
  (define-key input-decode-map "\e[1;10A" [S-M-up])
  (define-key input-decode-map "\e[1;4B" [M-down])
  (define-key input-decode-map "\e[1;10B" [S-M-down])
  (define-key input-decode-map "\e[1;4D" [M-left])
  (define-key input-decode-map "\e[1;10D" [S-M-left])
  (define-key input-decode-map "\e[1;4C" [M-right])
  (define-key input-decode-map "\e[1;10C" [S-M-left])
  ad-do-it
)

; --------  Basic editing facilities ---------

; assume new files are always modified (useful for creating empty files)
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

; always revert files when they change on disk
(global-auto-revert-mode t)

; mark always active for selecting
(setq transient-mark-mode t)

; prevent dabbrev from replacing case
(setq dabbrev-case-replace nil)

; indentation/tabs
(setq default-tab-width 8)
(setq default-tab-indent 4)

; -------- Languages --------
(require 'linum)
(global-linum-mode t)
; C
(setq c-basic-offset 4)

; Python
(add-to-list 'load-path (in-modes-d "python.el"))
(require 'python)

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c #") 'comment-or-uncomment-region)))

; Haskell
(autoload 'haskell-mode (in-modes-d "haskell-mode/haskell-mode.el") nil t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'turn-on-haskell-ghci "haskell-ghci"

  "Turn on interaction with a GHCi interpreter." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-to-list 'load-path (in-modes-d "ghc-mod/elisp"))
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
(setq ghc-module-command "~/.cabal/bin/ghc-mod")

; lua
(add-to-list 'load-path (in-modes-d "lua-mode"))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

; nxml/nxhtml - a bloated mode for fancy XML/HTML editing with multiple-major-mode support.
; It is only autoloaded, not wired to any format, since it's very clumsy and requires getting
; used to...
(autoload 'nxhtml-mode (in-modes-d "nxhtml-mode/autostart.el") nil t)

; web

; web-mode
(add-to-list 'load-path (in-modes-d "web-mode"))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(add-to-list 'load-path (in-modes-d "zencoding"))
(require 'zencoding-mode)
(setq zencoding-indentation web-mode-html-offset)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)


; Markdown
(autoload 'markdown-mode "markdown-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

; ------- Modes ---------
; ido
(ido-mode)
(setq ido-max-prospects 200)

(require 'ido-recentf-open)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

; minimap
(add-to-list 'load-path (in-modes-d "minimap"))
(require 'minimap)
(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (null minimap-bufname)
      (minimap-create)
    (minimap-kill)))


; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

; iedit
(require 'iedit)
(global-set-key (kbd "C-x i") 'iedit-mode)

; helm mode
(setq helm-input-idle-delay 0)
(add-to-list 'load-path (in-modes-d "helm"))
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)

; show-paren
(show-paren-mode)

; magit
(add-to-list 'load-path (in-modes-d "magit"))
(autoload 'magit-status "magit" nil t)
(autoload 'magit-branch-manager "magit" nil t)

; uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; auto-complete
(add-to-list 'load-path (in-modes-d "auto-complete"))
(add-to-list 'load-path (in-modes-d "auto-complete/lib/ert"))
(add-to-list 'load-path (in-modes-d "auto-complete/lib/fuzzy"))
(add-to-list 'load-path (in-modes-d "auto-complete/lib/popup"))
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)

; autopair
(add-to-list 'load-path (in-modes-d "autopair"))
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t) ; wrap selected region with quotes/parens/etc.
(setq autpair-blink t)

; Disable the autopair mapping in term mode
(add-hook 'term-mode-hook
          '(lambda ()
	     (setq autopair-dont-activate t)))

; ace-jump - quickly navigate to any character
(autoload 'ace-jump-char-mode (in-modes-d "ace-jump-mode/ace-jump-mode.el") nil t)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)
;   only use lowercase letters for lookup
(setq ace-jump-mode-move-keys
  (nconc (loop for i from ?a to ?z collect i)))


(defun my/set-super-char-to-ace-jump-mode (c)
  (global-set-key
         (read-kbd-macro (concat "s-" (string c)))
         `(lambda () (interactive) (ace-jump-char-mode ,c))))

(defun my/set-super-char-to-register-jump (c)
  (global-set-key
         (read-kbd-macro (concat "C-s-" (string c)))
         `(lambda () (interactive) (point-to-register ,c)))
  (global-set-key
         (read-kbd-macro (concat "M-s-" (string c)))
         `(lambda () (interactive) (jump-to-register ,c))))

(unless (string-equal system-type "windows-nt")
  ;bind most printable characters to S-<character>
  (loop for c from ?\" to ?~ do (my/set-super-char-to-ace-jump-mode c))
  (loop for c in (list ?! ?@ ?# ?$ ?% ?^ ?& ?*) do (my/set-super-char-to-ace-jump-mode c))
  (loop for c from ?\" to ?~ do (my/set-super-char-to-register-jump c))
  )

; drag stuff
(add-to-list 'load-path (in-modes-d "drag-stuff"))
(setq drag-stuff-modifier '(meta control))
(require 'drag-stuff)
(drag-stuff-global-mode t)

; org-mode
(add-to-list 'load-path (in-modes-d "org-mode/lisp"))
(add-to-list 'load-path (in-modes-d "org-mode/contrib/lisp"))
(require 'org)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [(meta left)]    nil)
            (define-key org-mode-map [(meta right)]   nil)
            (define-key org-mode-map [(meta down)]    nil)
            (define-key org-mode-map [(meta up)]   nil))
          'append)

; nyan-mode (no .emacs.d is whole without it)
(autoload 'nyan-mode (in-modes-d "nyan-mode/nyan-mode.el") nil t)
(unless in-terminal
  (nyan-mode t))

; ------- Utilities -----

; expand-region
(add-to-list 'load-path (in-modes-d "expand-region"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(add-to-list 'load-path (in-modes-d "multiple-cursors.el"))
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C-c m l") 'mc/edit-ends-of-lines)

; better compilation window
;   make the compilation window always appear at the bottom
(defun organize-compilation-window ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h 10)))))))
;   automatically close the compilation frame if no errors occurred
(setq compilation-finish-function
      (lambda (buf str)
        (if (not (string-match "grep" (buffer-name buf)))
            (if (string-match "exited abnormally" str)

                ;;there were errors
                (message "compilation errors, press C-x ` to visit")
              ;;no errors, make the compilation window go away in 0.5 seconds
              (run-at-time 3 nil 'delete-windows-on buf)
              (message "NO COMPILATION ERRORS!")))))
(add-hook 'compilation-mode-hook 'organize-compilation-window)

; rainbow-mode
(autoload 'rainbow-mode (in-utils-d "rainbow-mode.el") nil t)
(add-to-list 'find-file-hook 'rainbow-mode)

; python-auto-super
(autoload 'python-auto-super (in-utils-d "python-auto-super.el") nil t)
(define-key python-mode-map [(control ?c) ?s] 'python-auto-super)

; pylookup
(setq pylookup-dir (in-utils-d "pylookup"))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(add-to-list 'load-path pylookup-dir)
(autoload 'pylookup-lookup "pylookup.el" nil t)
(eval-after-load "python-mode" '(define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup))

; flycheck
(add-to-list 'load-path (in-modes-d "flycheck"))
(require 'flycheck)

; epylint + flymake
(setq pycodechecker "epylint")
(if (executable-find pycodechecker)
    (when (load "flymake" t)
      (defun flymake-pycodecheck-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
			   'flymake-create-temp-inplace))
	       (local-file (file-relative-name
			    temp-file
			    (file-name-directory buffer-file-name))))
	  (list pycodechecker (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
		   '("\\.py\\'" flymake-pycodecheck-init)))
  (message (format "Cannot find executable %s. Flymake python code check will be disabled." pycodechecker))
  )

; pbcopy - use OS X's clipboard if we're in the terminal
(cond ((and in-terminal (string-equal system-type "darwin"))
    (require 'pbcopy)
    (turn-on-pbcopy)
    ))

; pomodoro - time management technique
(autoload 'pomodoro "pomodoro.el" nil t)

; ------- Keyboard shortcuts -----
; F keys
(eval-after-load "c-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(eval-after-load "cc-mode" '(define-key c-mode-map [(f6)] 'ff-find-other-file))
(global-set-key [(f7)] 'magit-status)
(global-set-key [(control x) (f7)] 'magit-branch-manager)
(global-set-key [(f9)] 'compile)
(global-set-key [(f12)] 'delete-trailing-whitespace)

; Customizations beyond this configuration - separate to a different file
(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

;; Don't require double escaping the re-builder
(setq reb-re-syntax 'string)

;; Scroll without moving the cursor
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )
(global-set-key "\C-\M-n"  (lambda () (interactive) (scroll-other-window   4)) )
(global-set-key "\C-\M-p"  (lambda () (interactive) (scroll-other-window-down 4)) )
