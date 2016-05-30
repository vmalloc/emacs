;;; python-extra.el --- Extra Emacs utilities for Python development  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Rotem Yaari

;; Author: Rotem Yaari <rotemy@Rotems-MBP>
;; Keywords: convenience

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


;;; Code:



;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; pylint
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun pylint-ignore-errors-at-point()
  (interactive)
  (let* ((errs (flycheck-overlay-errors-in (line-beginning-position) (line-end-position)))
         (ids (-map 'flycheck-error-id errs)))
    (if (> (length ids) 0)
        (save-excursion
          (end-of-line)
          (insert " # pylint: disable="
                  (s-join ", " ids))
          ))))

(provide 'python-extra)
;;; python-extra.el ends here

@pytest.fixture

@pytest.fixture
