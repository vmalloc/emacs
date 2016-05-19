;;; iterm.el --- iTerm integration for Emacs       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Rotem Yaari

;; Author: Rotem Yaari <rotemy@Rotems-MBP.office.il.infinidat.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defvar iterm-send-history nil)


(defun shell-command-on-iterm (command)
  "Execute COMMAND in the currently open iTerm session."
  (interactive (list (read-from-minibuffer "Command: " (car iterm-send-history) nil nil 'iterm-send-history)))
  (with-temp-buffer
    (insert "on run argv
        tell application \"iTerm\"
                tell current session of first window
                        write text (item 1 of argv)
                end tell
        end tell
end run")
    (shell-command-on-region (point-min) (point-max) (format "osascript - \"%s\" &" command))))

(provide 'iterm)
;;; iterm.el ends here
