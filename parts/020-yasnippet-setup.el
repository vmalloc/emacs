; YASnippet - should appear before custom-set-variables
(require 'yasnippet)
(defcustom python-snippet-debugger "pdb"
  "Which python debugger should be used in the pdb template"
  :type 'string
  :group 'yasnippet)

(setq yas-snippet-dirs (list (in-emacs-d "snippets") yas-installed-snippets-dir))

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
