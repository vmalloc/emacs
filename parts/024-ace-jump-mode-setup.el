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

; Key to jump back to where we were before ace-jumping
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
