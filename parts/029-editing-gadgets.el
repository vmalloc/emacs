

; uniquify
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")


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


; dash support
(defun dash-lookup-current-word ()
  (interactive)
  (browse-url (--get-dash-query-string)
  ))

(defun --get-dash-query-string ()
  (if (derived-mode-p 'python-mode)
      (format "dash-plugin://keys=flask,python2,requests,sqlalchemy,werkzeug&query=%s" (current-word))
    (if (derived-mode-p 'yaml-mode)
        (format "dash-plugin://keys=ansible&query=%s" (current-word))
      (concat "dash://" (current-word)))))


;; pbcopy - use OS X's clipboard if we're in the terminal
(cond ((and in-terminal (string-equal system-type "darwin"))
    (require 'pbcopy)
    (turn-on-pbcopy)
    ))

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

;; Semantic
(semantic-mode t)

;; Macros
(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)] 'toggle-kbd-macro-recording-on)

(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))


;; increment/decrement integers at point
;;   See http://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/
(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(global-set-key (kbd "M-=") 'increment-integer-at-point)
(global-set-key (kbd "M--") 'decrement-integer-at-point)

(setq electric-indent-mode nil)

; goto-chg
(require 'goto-chg)
(global-set-key (kbd "C-c b ,") 'goto-last-change)
(global-set-key (kbd "C-c b .") 'goto-last-change-reverse)
