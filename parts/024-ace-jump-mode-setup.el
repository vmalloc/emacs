(require 'ace-jump-mode)

(global-set-key (kbd "s-;") 'ace-jump-char-mode)
(setq ace-jump-word-mode-use-query-char nil)
(global-set-key (kbd "s-'") 'ace-jump-word-mode)

; Key to jump back to where we were before ace-jumping
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
