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

(setq package-names '(
   ag
   atom-one-dark-theme
   auto-complete
   column-marker
   diff-hl
   diminish
   dired-details
   drag-stuff
   emmet-mode
   exec-path-from-shell
   expand-region
   find-file-in-project
   flycheck
   free-keys
   gist
   goto-chg
   guide-key
   helm-css-scss
   iflipb
   inf-mongo
   js2-refactor
   let-alist
   lua-mode
   markdown-mode
   multiple-cursors
   nxml
   powerline
   scss-mode
   smex
   sphinx-doc
   sublimity
   syslog-mode
   use-package
   wrap-region
   yaml-mode
   yasnippet
   ))

; requirements
(defun init--install-packages ()
  (dolist (pkg-name  package-names)
    (package-install pkg-name)))

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

(require 'use-package)

(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

; actually load all parts in order
(add-hook 'after-init-hook (lambda ()
			    (mapcar 'load-part-if-not-hidden (sort (directory-files parts-dir) 'string<))
					; ------------------ Custom site-specific settings ------------------
			    (setq site-specific-filename (expand-file-name "~/.emacs-site.el"))
			    (if (file-exists-p site-specific-filename)
				(load site-specific-filename)
			      )))
