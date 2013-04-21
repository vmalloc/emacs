; minimap
(require 'minimap)
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

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

; iedit
(require 'iedit)
(global-set-key (kbd "C-x i") 'iedit-mode)

; uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)

; wrap-region
(require 'wrap-region)
(add-hook 'prog-mode-hook (lambda () (wrap-region-mode t)))
(add-hook 'markdown-mode-hook (lambda () (wrap-region-mode t)))

(wrap-region-add-wrapper "*" "*")
(wrap-region-add-wrapper "`" "`")


; drag stuff
(require 'drag-stuff)
(setq drag-stuff-modifier '(super control))
(drag-stuff-global-mode t)

; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(require 'multiple-cursors)
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

; flycheck
(cond
 ((>= emacs-major-version 24)
  (require 'flycheck)
  ; turn on flycheck-mode in python-mode
  (add-hook 'python-mode-hook
        '(lambda ()
             (flycheck-mode)))
  (global-set-key (kbd "C-c m f") 'flycheck-mode)
  (global-set-key (kbd "C-c f r")
                  '(lambda ()
                     (interactive)
                     (flycheck-mode t)))))

;; pbcopy - use OS X's clipboard if we're in the terminal
(cond ((and in-terminal (string-equal system-type "darwin"))
    (require 'pbcopy)
    (turn-on-pbcopy)
    ))


;; Workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)
(setq wg-file (expand-file-name "~/.emacs-wg"))
(if (file-exists-p wg-file)
    (wg-load wg-file))

;; line manipulation
; Make C-o / C-S-o work like in VIM
(defun insert-line-before ()
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-for-tab-command))

(defun insert-line-after ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'insert-line-after)
(global-set-key (kbd "C-S-o") 'insert-line-before)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; TRAMP
; Use external scp instead of the build in
(set-default 'tramp-default-method "scp")

; Allow Sudo + SSH
(add-to-list 'tramp-default-proxies-alist
	     '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
	     '((regexp-quote (system-name)) nil nil))
