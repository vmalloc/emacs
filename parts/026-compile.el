(defun auto-close-compile-window (buffer string)
  "Auto-close a compilation buffer if succeeded without warnings/errors "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 5 nil
                      (lambda (buf)
                        (delete-window (get-buffer-window buf))
                        (bury-buffer buf))
                      buffer)))

(add-hook 'compilation-finish-functions 'auto-close-compile-window)
