; --------  Preamble ---------
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
(defun init--install-packages ()
  (packages-install
   (cons 'powerline melpa)
   (cons 'flycheck-color-mode-line melpa)
   (cons 'let-alist melpa)
   (cons 'exec-path-from-shell melpa)
   (cons 'dash melpa)
   (cons 'flycheck melpa)
   (cons 'auto-complete melpa)
   (cons 'column-marker melpa)
   (cons 'ace-jump-mode melpa)
   (cons 'dired-details melpa)
   (cons 'wrap-region melpa)
   (cons 'org melpa)
   (cons 's melpa)
   (cons 'smex melpa)
   (cons 'web-mode melpa)
   (cons 'magit melpa)
   (cons 'yaml-mode melpa)
   (cons 'yasnippet melpa)
   (cons 'emmet-mode melpa)
   (cons 'undo-tree melpa)
   (cons 'multiple-cursors melpa)
   (cons 'markdown-mode melpa)
   (cons 'lua-mode melpa)
   (cons 'ido-ubiquitous melpa)
   (cons 'ido-vertical-mode melpa)
   (cons 'helm melpa)
   (cons 'helm-css-scss melpa)
   (cons 'projectile melpa)
   (cons 'ag melpa)
   (cons 'helm-projectile melpa)
   (cons 'js2-refactor melpa)
   (cons 'haskell-mode melpa)
   (cons 'find-file-in-project melpa)
   (cons 'expand-region melpa)
   (cons 'drag-stuff melpa)
   (cons 'inf-mongo melpa)
   (cons 'diminish melpa)
   (cons 'guide-key melpa)
   (cons 'diff-hl melpa)
   (cons 'free-keys melpa)
   (cons 'syslog-mode melpa)
   (cons 'iflipb melpa)
   (cons 'nxml melpa)
   (cons 'sublimity melpa)
   (cons 'sphinx-doc melpa)
   (cons 'restclient melpa)
   (cons 'gist melpa)
   (cons 'goto-chg melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(setq parts-dir (in-emacs-d "parts"))
(defun load-part (part-name)
  (load-file (concat parts-dir "/" part-name))
  )
(defun load-part-if-not-hidden (part-name)
  (if (not (elisp-utils/starts-with part-name "."))
      (load-part part-name)))

(defun autoload-and-run (symbol file interactive callback)
  (autoload symbol file nil interactive)
  (eval-after-load (symbol-name symbol) callback)
  )

; actually load all parts in order
(add-hook 'after-init-hook (lambda ()
			    (mapcar 'load-part-if-not-hidden (sort (directory-files parts-dir) 'string<))
					; ------------------ Custom site-specific settings ------------------
			    (setq site-specific-filename (expand-file-name "~/.emacs-site.el"))
			    (if (file-exists-p site-specific-filename)
				(load site-specific-filename)
			      )))
