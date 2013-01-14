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

; wrap-region
(require-from-modes-d "wrap-region")
(add-to-list 'prog-mode-hook (lambda () (wrap-region-mode t)))

; drag stuff
(require-from-modes-d "drag-stuff")
(setq drag-stuff-modifier '(super control))
(drag-stuff-global-mode t)

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

; dash support
(defun dash-lookup-current-word ()
  (interactive)
  (browse-url (format "dash://%s" (current-word)))
  )

(global-set-key (kbd "C-c d") 'dash-lookup-current-word)

; flymake
(require-from-modes-d "flymake")

; flycheck
(require-from-modes-d "flycheck")
(global-set-key (kbd "C-c m f") 'flycheck-mode)
(global-set-key (kbd "C-c f r")
                '(lambda ()
                   (interactive)
                   (flycheck-mode t)))


; Turn on by default in Python mode
(add-hook 'python-mode-hook
          '(lambda ()
             (flycheck-mode)))

; pbcopy - use OS X's clipboard if we're in the terminal
(cond ((and in-terminal (string-equal system-type "darwin"))
    (require 'pbcopy)
    (turn-on-pbcopy)
    ))
