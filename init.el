; --------  Preamble ---------
; init.el utils
(defun in-emacs-d (path)
  (concat "~/.emacs.d/" path))
(setq mode-dir (in-emacs-d "modes/"))
(defun in-modes-d (path)
  (concat mode-dir path))
(setq custom-dir (in-emacs-d "custom/"))
(defun in-custom-d (path)
  (concat custom-dir path))
(add-to-list 'load-path mode-dir)
(add-to-list 'load-path custom-dir)

(defun require-from-modes-d (path &optional symbol)
  (add-to-list 'load-path (in-modes-d path))
  (if symbol
      (require symbol)
    (load-library path))
  )

(setq in-terminal (not window-system))

(if (not in-terminal)
    (server-start))

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
(require-from-modes-d "yasnippet")
(yas/global-mode 1)
(setq yas/indent-line 'fixed) ; for indented snippets
; rebind yasnippet-expand to C-c tab. This is because the new version of yasnippet
; has a wrong fallback to the default <tab>, breaking Python's indentation cycling feature,
; and possibly other things too.
;     - See:
;       - https://github.com/fgallina/python.el/issues/123
;       - https://github.com/capitaomorte/yasnippet/issues/332
(add-hook 'yas-minor-mode-hook
	  '(lambda ()
	     (define-key yas-minor-mode-map [(tab)] nil)
	     (define-key yas-minor-mode-map (kbd "TAB") nil)
	     (define-key yas-minor-mode-map [(control ?c) (tab)] 'yas/expand-from-trigger-key)))

; display trailing whitespaces
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

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

; rainbow-mode
(require 'rainbow-mode)
(add-to-list 'find-file-hook
	     (lambda () (unless (derived-mode-p 'web-mode) (rainbow-mode))))

; recentf - save history of recently visited files
(autoload 'recentf-mode "recentf.el" nil t)
(autoload 'recentf-save-list "recentf.el" nil t)
(run-with-idle-timer (* 5 60) t 'recentf-save-list)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)

; saveplace - save position in files (http://whattheemacsd.com/init.el-03.html)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (in-emacs-d ".places"))

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
(load-file (in-emacs-d "001-defaults.el"))

; -------- Languages --------
; C
(setq c-basic-offset 4)

; Python
(require-from-modes-d "python.el" 'python)
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
(require-from-modes-d "web-mode")
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(require-from-modes-d "zencoding" 'zencoding-mode)
(setq zencoding-indentation 2)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)

; YAML
(require-from-modes-d "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

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
(autoload 'minimap "minimap")
(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (require 'minimap)
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
(require-from-modes-d "helm" 'helm-config)
(helm-mode t)
(helm-dired-mode t)
(setq helm-input-idle-delay 0)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)

; show-paren
(show-paren-mode)

; magit
(add-to-list 'load-path (in-modes-d "magit"))
(require 'magit)

; make magit window full screen, restore previous frame arrangement (http://whattheemacsd.com//setup-magit.el-01.html)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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

; smart parens
(require-from-modes-d "dash") ; no relations to dash lookup below!
(require-from-modes-d "smartparens")
(smartparens-global-mode 1)

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
(require-from-modes-d "drag-stuff")
(setq drag-stuff-modifier '(meta control))
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
(require-from-modes-d "expand-region")
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(require-from-modes-d "multiple-cursors.el" 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)

(defun my/mark-all-like-this-in-defun ()
  "wrapper for mark-all-symbols-like-this-in-defun, automatically selecting symbols if unselected"
  (interactive)
  (if (region-active-p)
      (mc/mark-all-like-this-in-defun)
    (er/mark-symbol)
    (mc/mark-all-symbols-like-this-in-defun)))

(global-set-key (kbd "C-:") 'my/mark-all-like-this-in-defun)
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

; pylookup
(setq pylookup-dir (in-modes-d "pylookup"))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(add-to-list 'load-path pylookup-dir)
(autoload 'pylookup-lookup "pylookup.el" nil t)
(define-key python-mode-map [(control ?c) ?l] 'pylookup-lookup)

; dash support
(defun dash-lookup-current-word ()
  (interactive)
  (browse-url (format "dash://%s" (current-word)))
  )

(global-set-key (kbd "C-c d") 'dash-lookup-current-word)

; flymake
(require-from-modes-d "flymake")
(global-set-key [(f1)] 'flymake-display-err-menu-for-current-line)

; flycheck
(require-from-modes-d "flycheck")

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

; open shell with C-z
(global-set-key (kbd "C-z") 'shell)

; Customizations beyond this configuration - separate to a different file
(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

;; Don't require double escaping the re-builder
(setq reb-re-syntax 'string)

;; Scroll without moving the cursor
(global-set-key "\C-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\C-p"  (lambda () (interactive) (scroll-down 4)) )
(global-set-key "\M-n"  (lambda () (interactive) (scroll-other-window   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-other-window-down 4)) )

;; My stuff
; run redgreen in current project
(require 'redgreen-at-project-root)
(define-key python-mode-map [(control ?c) ?r] 'redgreen-at-project-root)

; python-auto-super
(require 'python-auto-super)
(define-key python-mode-map [(control ?c) ?s] 'python-auto-super)

; python-auto-import
(require 'python-auto-import)
(define-key python-mode-map [(control ?c) ?i] 'python-auto-import)

; Diminish modes from mode-line (http://whattheemacsd.com/init.el-04.html)
(require 'diminish)
(diminish 'auto-complete-mode)
(diminish 'drag-stuff-mode)
(diminish 'helm-mode)
(diminish 'rainbow-mode)
(diminish 'smartparens-mode)
(diminish 'undo-tree-mode)
(diminish 'yas-minor-mode)

; ------------------ Custom site-specific settings ------------------
(setq site-specific-filename (expand-file-name "~/.emacs-site.el"))
(if (file-exists-p site-specific-filename)
    (load site-specific-filename)
    )

