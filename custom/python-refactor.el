;;; python-refactor.el --- Utilities for refactoring Python code

;; Copyright (C) 2016 Rotem Yaari
;; Author: Rotem Yaari <vmalloc@gmail.com>
;; Created: 2016-05-31
;; Version:
;; Keywords:
;; URL:
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; auto import
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun python-auto-import (only-first-component)
  "automatically import the required symbol under the cursor. With prefix argument, only takes first module path component (so 'os.path.exists' will cause importing of 'os' alone)"
  (interactive "P")
  (let ((temp-point (point))
        (prev-size (buffer-size)))
    (python-insert-import-line-at-beginning-of-buffer (--deduce-needed-import-string only-first-component))
    (goto-char (+ temp-point (- (buffer-size) prev-size)))))

(defun --deduce-needed-import-string (only-first-component)
  (let ((symbol (--python-info-current-symbol)))
    (let ((cached-result (gethash symbol --symbol-cache)))
      (if cached-result
          cached-result
        (--save-result symbol (--parse-import-string-from-symbol symbol))))))


(defun --save-result (symbol result)
  (if (not (s-starts-with? "from ." result))
      (progn
        (puthash symbol result --symbol-cache)
        (--dump-symbol-cache)))
  result)

(defvar python-auto-import-cache-filename "~/.emacs.d/.python-auto-import-cache")

(defun --dump-symbol-cache ()
  (with-temp-file python-auto-import-cache-filename
    (emacs-lisp-mode)
    (insert ";; this file was automatically generated by python-auto-import.el")
    (newline)
    (insert "(setq --symbol-cache (make-hash-table :test 'equal))")
    (newline)
    (maphash (lambda (key value)
               (insert (format "(puthash \"%s\" \"%s\" --symbol-cache)" key value))
               (newline))
             --symbol-cache)
     --symbol-cache))

(defun --load-symbol-cache ()
  (load python-auto-import-cache-filename t)
  (if (not (boundp '--symbol-cache))
      (setq --symbol-cache (make-hash-table :test 'equal))))

(--load-symbol-cache)

(defun --parse-import-string-from-symbol (symbol)
  (cond ((string= symbol "print") "from __future__ import print_function")
        (t (let ((parts (split-string symbol "\\.")))
             (if (<= (list-length parts) 1)
                 (format "from %s import %s" (read-string (format "import %s from? " symbol)) symbol)
               (format "import %s" (if (or
                                        only-first-component
                                        (python-autoimport--is-first-component-enough symbol))
                                       (car parts)
                                     (mapconcat 'identity (nbutlast parts 1) "."))))))))

(defun --python-info-current-symbol ()
  (if (< emacs-major-version 24)
      (with-syntax-table python-dotty-syntax-table
        (current-word))
    (python-info-current-symbol)))

(defun python-autoimport--is-first-component-enough (symbol)
  (or
   (s-starts-with? "os.path." symbol)
   ))

(defun python-insert-import-line-at-beginning-of-buffer (import-string)
  (save-excursion
    (beginning-of-buffer)
    (--skip-comments-and-strings)
    (--ensure-import-block)
    (newline)
    (forward-line -1)
    (insert-string import-string)
    (forward-paragraph)
    (py-isort-buffer)
    ))

(defun --ensure-import-block ()
  (if (not (or (looking-at "import ") (looking-at "from ")))
      (progn
        (newline-and-indent)
        (previous-line))))

(defun --skip-comments-and-strings ()
  (while (--looking-at-comment-or-string)
    (forward-line)))
(defun --looking-at-comment-or-string ()
  (let ((face (get-text-property (point) 'face)))
    (or (eq face 'font-lock-comment-face)
        (eq face 'font-lock-comment-delimiter-face)
        (eq face 'font-lock-string-face))))

(defun uniquify-all-lines-region (start end)
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun --end-of-chunk ()
  (goto-char (point-at-eol))
  (let ((limit (point-at-bol))
        temp
        expr-beg)
    (while (and (setq temp (nth 1 (syntax-ppss)))
                (<= limit temp))
      (goto-char temp)
      (setq expr-beg (point)))
    (when expr-beg
        (goto-char expr-beg)
      (forward-sexp))))

(defun my/sort-lines-as-exprs (reverse beg end) ; credit goes to http://bit.ly/VbW9AJ
  "sort lines, or whole expression if line ends mid-expression."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
                 'forward-line
                 '--end-of-chunk))))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; pyrefactor
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun --python-pyrefactor(op)
  (save-excursion
    (if (not (region-active-p))
        (er/mark-python-statement))
    (message (format "point: %s mark: %s" (point) (mark)))
    (shell-command-on-region (point) (mark) (format "%s %s" (executable-find "pyrefactor") op) nil t)
    (setq deactivate-mark nil)))

(defun python-toggle-dict-style ()
  (interactive)
  (--python-pyrefactor "toggle_dict_style")
  )

(defun python-toggle-assert-style ()
  (interactive)
  (--python-pyrefactor "toggle_assert_style")
  )

(defun --append-parameter (symbol)
  (search-forward "):")
  (backward-char 3)
  (if (not (looking-at "("))
    (progn
      (forward-char)
      (insert ", "))
    (forward-char))
  (insert symbol))

(defun --prepend-parameter (symbol)
  (search-forward "(")
  (insert symbol)
  (unless (looking-at ")")
    (insert ", ")))

(defun python-extract-parameter ()
  "Extracts the current symbol as a function paramater.  If prefix arg is specified, append at the beginning of the parameter list."
  (interactive)
  (let ((prefix-arg current-prefix-arg)
	(symbol (current-word)))
    (save-excursion
      (back-to-indentation)
      (python-nav-beginning-of-defun)
      (if prefix-arg
	  (--prepend-parameter symbol)
	(--append-parameter symbol)))))


(defun python-pytest-parametrize (&optional values-list)
  (interactive)
  (let ((symbol (current-word)))
    (back-to-indentation)
    (previous-line 1)
    (move-end-of-line nil)
    (newline-and-indent)
    (beginning-of-line)
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (insert (format "@pytest.mark.parametrize('%s', [%s])" symbol (if values-list values-list "")))
    (backward-char 2)

    ))

(defun python-pytest-extract-fixture()
  "Extracts the current variable (along with its creation) to a py.test fixture"
  (interactive)
  (let* ((name (symbol-at-point)))
    (call-interactively 'python-extract-parameter)
    (save-excursion
      (beginning-of-line)
      (let* ((line (thing-at-point 'line t)))
        (kill-line)
        (goto-char (point-max))
        (insert "\n\n@pytest.fixture")
        (insert (format "\ndef %s():\n" name))
        (insert line)
        (beginning-of-line)
        (unless  (looking-at "[[:space:]]*$")
          (end-of-line)
          (insert "\n"))
        (indent-for-tab-command)
        (insert (format "return %s" name))))

    (indent-for-tab-command)
    ))



(defun python-pytest-parametrize-boolean ()
  "When standing over a function argument, parametrize it as [True, False]."
  (interactive)
  (save-excursion
    (python-pytest-parametrize "True, False")))

(provide 'python-refactor)
;;; python-refactor.el ends here
