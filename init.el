; --------  Preamble ---------
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

(require 'elisp-utils)

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
(mapcar 'load-part-if-not-hidden (sort (directory-files parts-dir) 'string<))

; ------------------ Custom site-specific settings ------------------
(setq site-specific-filename (expand-file-name "~/.emacs-site.el"))
(if (file-exists-p site-specific-filename)
    (load site-specific-filename)
    )
