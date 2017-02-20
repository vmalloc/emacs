(defun vmalloc/mark-all-symbols-like-this-in-defun ()
  "wrapper for mark-all-symbols-like-this-in-defun, automatically selecting symbols if unselected"
  (interactive)
  (require 'er-basic-expansions)
  (if (not (region-active-p))
      (er/mark-symbol))
  (mc/mark-all-symbols-like-this-in-defun))

(provide 'vmalloc-multiple-cursors-utils)
