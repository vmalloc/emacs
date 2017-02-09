;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(package-initialize)

(setq emacs-dir "~/.emacs.d/")
(defun in-emacs-d (path)
  (concat emacs-dir path))
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

(require 'elisp-utils)
(load (in-emacs-d "setup-package.el"))

(if (getenv "http_proxy")
    (eval-after-load "url"
      '(progn
         (setq url-using-proxy(getenv "http_proxy")))))


; requirements
(defun --install-packages ()
  (package-install 'use-package))

(condition-case nil
    (--install-packages)
  (error
   (package-refresh-contents)
   (--install-packages)))

(require 'use-package)

(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))


(load-file (in-emacs-d "parts/reset-emacs.el"))
(load-file (in-emacs-d "parts/set-defaults.el"))
(load-file (in-emacs-d "parts/display-settings.el"))

; actually load all parts in order
(add-hook 'after-init-hook (lambda ()
                             (let* ((site-specific-filename (expand-file-name "~/.emacs-site.el")))
                               (if (file-exists-p site-specific-filename)
                                   (load site-specific-filename)
                                 ))))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Generic settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package
  dash
  :ensure t
  :defer t)

(use-package
  cl
  :ensure t)

(use-package savehist
  :config (progn
            (setq savehist-additional-variables '(iterm-send-history))
            (setq savehist-file "~/.emacs.d/savehist")
            (savehist-mode 1)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Buffer management
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package uniquify
  :config (progn ; uniquify
            (setq
             uniquify-buffer-name-style 'post-forward
             uniquify-separator ":")))

;; prevent killing scratch
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury *scratch* instead of killing it."
  (let* ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(use-package ibuffer
  :ensure t
  :config (progn
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
                             (mode . haskell-mode)
                             ;; etc
                             ))
                           ("Elisp"
                            (mode . emacs-lisp-mode))
                           ("Magit"
                            (or
                             (name . "\*magit.*\*")
                             (name . "COMMIT_EDITMSG")
                             ))
                           ("Terminal"
                            (mode . term-mode))
                           ("Emacs"
                            (name . "\*.*\*"))))))

            (add-hook 'ibuffer-mode-hook
                      (lambda ()
                        (ibuffer-switch-to-saved-filter-groups "default"))))
  :bind ("C-x C-b" . ibuffer))




;; Window Movement -------------------------------------------------------------

;; Window moving
(use-package
  windmove
  :bind (("<s-up>" . windmove-up)
         ("<s-down>" . windmove-down)
         ("<s-right>" . windmove-right)
         ("<s-left>" . windmove-left)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; File management
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package saveplace
  :ensure t
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (in-emacs-d ".places"))))

(use-package recentf
  :config (progn
            (run-with-idle-timer (* 5 60) t 'recentf-save-list)
            (setq recentf-auto-cleanup 'never)
            (setq recentf-max-saved-items 1000)))


(use-package
  recentf
  :config (recentf-mode 1))


(use-package
  crux
  :defer t
  :ensure t)

(use-package
  projectile
  :ensure t
  :config (projectile-global-mode))

(defun my/projectile-disable-remove-current-project (orig-fun &rest args)
  "ORIG-FUN ARGS."
  (cl-letf (((symbol-function 'projectile--remove-current-project) #'identity))
    (apply orig-fun args)))
(advice-add 'projectile-switch-project :around #'my/projectile-disable-remove-current-project)

(use-package
  helm
  :diminish helm-mode
  :ensure t
  :init (progn
          (helm-mode t)
          (setq helm-input-idle-delay 0)
          (setq helm-exit-idle-delay 0)
          (require 'helm-config))
  :bind (("M-i" . helm-semantic)
         ("C-x y" . helm-show-kill-ring)))

(use-package
  swiper
  :ensure t
  :init (progn
          (ivy-mode t)
          (setq projectile-completion-system 'ivy)
          (setq ivy-re-builders-alist
                '((t . ivy--regex-ignore-order)))
          )
  :bind (("C-s" . swiper)
         ("C-x b" . ivy-switch-buffer)
         ("C-c h" . ivy-recentf)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         :map projectile-command-map ("h" . projectile-find-file)
         :map ivy-minibuffer-map
            ("C-p" . my/ivy-projectile-find-file-on-selection)
            ("<next>" . ivy-scroll-up-command)
            ("<prior>" . ivy-scroll-down-command)
            ))

(use-package wgrep
  :ensure t)

;; projectile find file from within ivy
(defun my/ivy-projectile-find-file-on-selection()
  (interactive)
  (let* ((directory (if (s-starts-with? "/" ivy--current)
                        ivy--current
                      (concat ivy--directory ivy--current))))
    (run-with-timer 0 nil (lambda (d)
                            (let* ((default-directory  d))
                              (projectile-find-file))) directory)
    (minibuffer-keyboard-quit)))

(defun my/projectile-git-status ()
  (interactive)
  (let ((projectile-switch-project-action 'projectile-vc))
    (projectile-switch-project)))


(defun my/projectile-counsel-ag()
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(use-package
  counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         :map projectile-command-map
         ("s s" . my/projectile-counsel-ag)
         ("g" . my/projectile-git-status)
         ))

(use-package
  projectile
  :ensure t
  :init (projectile-global-mode 1)
  :bind (("s-p" . projectile-find-file)))

(use-package
  ag
  :ensure t)

(use-package
  flycheck
  :config (progn
            (mapcar
             '(lambda (m)
                (add-hook m
                          '(lambda ()
                             (flycheck-mode))))
             (list 'python-mode-hook 'js-mode-hook))
            (add-to-list 'prog-mode-hook
                         '(lambda ()
                            (flycheck-mode)))
            (setq flycheck-mode-line
                  '(:eval (pcase
                              flycheck-last-status-change
                            (`not-checked nil)
                            (`no-checker (propertize " -" 'face 'warning))
                            (`running (propertize " ✷" 'face 'success))
                            (`errored (propertize " !" 'face 'error))
                            (`finished (let* ((error-counts (flycheck-count-errors
                                                             flycheck-current-errors))
                                              (no-errors (cdr (assq 'error error-counts)))
                                              (no-warnings (cdr (assq 'warning error-counts)))
                                              (face
                                               (cond (no-errors 'error)
                                                     (no-warnings 'warning)
                                                     (t 'success))))
                                         (propertize (format " %s/%s" (or no-errors
                                                                          0)
                                                             (or no-warnings
                                                                 0)) 'face face)))
                            (`interrupted " -")
                            (`suspicious '(propertize " ?" 'face 'warning))))))
  :bind (([(f5)] . flycheck-previous-error)
         ([(f6)] . flycheck-next-error)))

;; use project-specific pylintrc when exists
(add-hook 'python-mode-hook
          (lambda () (let ((root-dir (projectile-project-root)))
                       (when root-dir
                         (let ((pylintrc (concat (file-name-as-directory root-dir) ".pylintrc")))
                           (when (file-exists-p pylintrc)
                             (make-local-variable 'flycheck-pylintrc)
                             (setq flycheck-pylintrc pylintrc)))))))

(use-package jedi
  :ensure t
  :config (add-hook 'python-mode-hook 'jedi:setup))


(use-package
  flyspell
  :ensure t)


;; Defaults --------------------------------------------------------------------

;; Make the super key work in Windows
(setq w32-pass-lwindow-to-system nil w32-pass-rwindow-to-system nil w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super       ; Left Windows key
      w32-rwindow-modifier 'super       ; Right Windows key
      w32-apps-modifier 'hyper)         ; Menu key


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
                                        ; use spaces instead of tabs when auto-indneting
(setq-default indent-tabs-mode nil)

                                        ; enable all disabled commands
(setq disabled-command-function nil)

                                        ; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)

                                        ; display trailing whitespaces in prog-mode variants
(add-to-list 'prog-mode-hook
             '(lambda ()
                (subword-mode t)
                (define-key prog-mode-map (kbd "<C-left>") 'backward-word)
                (define-key prog-mode-map (kbd "<C-right>") 'forward-word)))


(use-package
  whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :config (progn
            (add-hook 'prog-mode-hook (lambda () (wrap-region-mode t)))
            (add-hook 'markdown-mode-hook (lambda () (wrap-region-mode t)))
            (wrap-region-add-wrapper "*" "*")
            (wrap-region-add-wrapper "`" "`")))


(-map
 (lambda (hook)
   (add-to-list hook
                '(lambda ()
                   (setq show-trailing-whitespace t))))
 (list 'prog-mode-hook))


;; yes/no turns to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; reasonable code offset
(setq c-basic-offset 4)

;; Don't require double escaping the re-builder
(setq reb-re-syntax 'string)

;; Scroll one line at a time without recentering the screen
(setq scroll-step 1 scroll-conservatively 10000)


(use-package exec-path-from-shell
  :ensure t)

;; fix path for mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Editing ---------------------------------------------------------------------

(use-package
  avy
  :ensure t
  :bind (("s-;" . avy-goto-char)))

(use-package
  dumb-jump
  :ensure t
  :config (progn
            (dumb-jump-mode)
            (setq dumb-jump-selector 'ivy)
            ))

(use-package
  expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package
  multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-symbol-like-this)
         ("C-." . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-symbol-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-;" . mc/mark-all-symbols-like-this)
         ("C-c m l" . mc/edit-ends-of-lines)))


(use-package
  vmalloc-multiple-cursors-utils
  :bind (("C-:" . vmalloc/mark-all-symbols-like-this-in-defun)))

(use-package
  vmalloc-powerline-settings
  :config (vmalloc-powerline-setup))

(use-package
  vmalloc-editing-utils)

(use-package
  diff-hl
  :ensure t
  :config (add-hook 'prog-mode-hook
                    '(lambda ()
                       (diff-hl-mode))))

(use-package
  undo-tree
  :ensure t
  :config (progn
            (global-undo-tree-mode t)
            ;; Keep region when undoing in region
            (defadvice undo-tree-undo (around keep-region activate)
              (if (use-region-p)
                  (let ((m (set-marker (make-marker)
                                       (mark)))
                        (p (set-marker (make-marker)
                                       (point))))
                    ad-do-it
                    (goto-char p)
                    (set-mark m)
                    (set-marker p nil)
                    (set-marker m nil))
                ad-do-it)))
  :diminish undo-tree-mode)

(defcustom python-snippet-debugger "pdb"
  "Which python debugger should be used in the pdb template."
  :type 'string
  :group 'yasnippet)


(use-package
  yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config (progn
            (setq yas-snippet-dirs (list (in-emacs-d "snippets") yas-installed-snippets-dir))
            (setq yas-indent-line 'fixed) ; for indented snippets
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            ;; Set Yasnippet's key binding to shift+tab
            (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)))




;; Autocomplete
(use-package
  auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config (progn
            (require 'auto-complete-config)
            (global-auto-complete-mode t)
            (ac-config-default)
            (defun ac-python-mode-setup ()
              (setq ac-sources (append '(ac-source-yasnippet ac-source-semantic) ac-sources)))
            (add-hook 'python-mode-hook 'ac-python-mode-setup)))

;; company-mode
(use-package
  company
  :ensure t)


(use-package
  drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("<C-s-up>" . drag-stuff-up)
         ("<C-s-down>" . drag-stuff-down))
  :config (progn
            (drag-stuff-global-mode t)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Source control
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq magit-last-seen-setup-instructions "1.4.0")


(defun my/magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun my/magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (my/magit-dont-ignore-whitespace)
    (my/magit-ignore-whitespace)))

(defun my/magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun my/magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))


(use-package magit
  :ensure t
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (setq magit-auto-revert-mode nil)
    (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)
    (define-key magit-status-mode-map (kbd "W") 'my/magit-toggle-whitespace)
    (add-hook 'magit-mode-hook 'magit-load-config-extensions))

  :bind (("C-c g" . magit-status)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Misc Development features
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(load-file (in-emacs-d "parts/git-setup.el"))

(use-package gist
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package
  jira-commit
  :bind (("C-c J" . commit-jira-issue-interactive)))

(use-package emamux
  :ensure t
  :bind (("S-<f9>" . emamux:send-command))
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Compilation
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
          ;; then bury the *compilation* buffer, so that C-x b doesn't go there
          (bury-buffer "*compilation*")
          ;; and return to whatever were looking at before
          (replace-buffer-in-windows "*compilation*"))
        ;; Always return the anticipated result of compilation-exit-message-function
        (cons msg code)))

(setq compilation-scroll-output t)

(use-package compilation
  :bind (("<f9>" . compile)
         ("<M-f9>" . projectile-compile-project)
         ))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Programming
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Misc ------------------------------------------------------------------------
;; Move to the beginning of the text
(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'my/smart-beginning-of-line)
(global-set-key (kbd "C-a") 'my/smart-beginning-of-line)

;; Return and indent on prog-mode variants
(defun my/set-newline-and-indent ()
  (local-set-key [(return)] 'newline-and-indent))
(add-hook 'prog-mode-hook 'my/set-newline-and-indent)

;; Disable electric-indent-mode
(electric-indent-mode -1)


;; Semantic mode ---------------------------------------------------------------
(use-package semantic
  :init (semantic-mode t))


;; Rust ------------------------------------------------------------------------
(use-package
  racer
  :ensure t
  :config (progn
            (setq racer-cmd "~/.cargo/bin/racer")
            (setq racer-rust-src-path "~/src/opensource/rust/rust/src")
            (add-hook 'racer-mode-hook #'company-mode)))

(use-package
  rust-mode
  :ensure t
  :mode (("\.rs$" . rust-mode)))

(use-package
  d-mode
  :ensure t
  :mode (("\.d$" . d-mode)))

;; Python ----------------------------------------------------------------------
(load-file (in-emacs-d "parts/python-setup.el"))

(use-package
  nginx-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode (("\.yml$" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\.md$" . markdown-mode)))

(use-package
  web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
         ("\\.hbs$" . web-mode))
  :config (add-hook 'web-mode-hook
                    '(lambda ()
                       (emmet-mode)
                       (setq web-mode-markup-indent-offset 2)
                       (setq web-mode-css-indent-offset 2)
                       (setq web-mode-code-indent-offset 2))))

(eval-after-load 'flycheck '(flycheck-add-mode 'html-tidy 'web-mode))

(use-package
  emmet-mode
  :ensure t
  :config (progn
            (setq emmet-indentation 2)
            (define-key emmet-mode-keymap (kbd "C-j") nil)
            (define-key emmet-mode-keymap (kbd "<C-return>") nil)
            (define-key emmet-mode-keymap (kbd "C-c i") 'emmet-expand-line)))


(use-package
  scss-mode
  :ensure t
  :init (add-hook 'scss-mode-hook
                  '(lambda ()
                     (setq show-trailing-whitespace t)
                     (define-key css-mode-map (kbd "M-i") 'helm-css-scss))))


(use-package
  ember-mode
  :ensure t
  :bind (("C-c e m" . ember-open-model)
         ("C-c e t" . ember-open-template)
         ("C-c e r" . ember-open-route)
         ("C-c e c" . ember-open-controller)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Writing
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; ReStructured Text (rst)
(use-package rst-mode
  :config (progn
            ;; remove C-c <num> from map
            (add-hook 'rst-mode-hook '(lambda ()
                                  (mapcar (lambda (k) (define-key rst-mode-map (kbd (format "C-c %s" k)) nil))
                                          (list 1 2 3 4))))

            (setq rst-preferred-adornments
                  '((?= simple 0)
                    (?- simple 0)
                    (?~ simple 0)
                    (?+ simple 0)
                    (?` simple 0)
                    (?# simple 0)
                    (?@ simple 0)))))



;; Org mode ---------------------------------------------------------------------

(defun my/open-todo()
  (interactive)
  (find-file org-default-notes-file))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(use-package
  org-plus-contrib
  :ensure t)

(use-package
  org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-<f12>" . my/open-todo)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         )
  :config
  (progn
    (add-hook 'org-mode-hook
              (lambda ()
                (visual-line-mode 1)
                (define-key org-mode-map (kbd "M-h")    nil))
              'append)

    (setq org-src-fontify-natively t)

    (load-file (in-custom-d "vmalloc-org-setup.el"))

    (defadvice org-kill-line (after fix-cookies activate)
      (myorg-update-parent-cookie))

    (defadvice kill-whole-line (after fix-cookies activate)
      (myorg-update-parent-cookie))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; Server -----------------------------------------------------------------------

(use-package
  server
  :if window-system
  :init (add-hook 'after-init-hook 'server-start t))


;; Utilities -------------------------------------------------------------------

(use-package
  restclient
  :ensure t)


(use-package mode-icons
  :ensure t
  :config (mode-icons-mode 1))


;;; init.el ends here
