;;; helm-org-backlinks.el --- Helm interface for Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (helm "4.0") (org-backlinks "0.4.0"))

;; This file is NOT part of GNU Emacs.

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

;; Helm interface for Org backlinks.

;;; Code:

(require 'org-backlinks)
(require 'helm)

(defcustom helm-org-backlinks-actions
  (helm-make-actions "Go to heading" 'org-backlinks-goto)
  "Actions for `helm-org-backlinks' sources."
  :group 'org-backlinks
  :type 'sexp)

;;;###autoload
(defun helm-org-backlinks (&optional arg)
  "Select Org headings with `completing-read'.
With ARG, prompt for the depth of backlinks and forward links."
  (interactive "P" org-mode)
  (if-let* ((hdata (org-backlinks-get-heading-data))
            (depth (org-backlinks--depth arg))
            (ht (org-backlinks--collect hdata (car depth) (cdr depth)))
            (candidates (mapcar
                         (lambda (c)
                           (cons c (get-text-property 0 'org-marker c)))
                         (org-backlinks--candidates ht))))
      (helm :prompt "Go to heading: "
            :sources (helm-build-sync-source
                      "Backlinks"
                      :candidates candidates
                      :action helm-org-backlinks-actions))
    (message "There are no links to this heading.")))


;;; Provide

(provide 'helm-org-backlinks)

;;; helm-org-backlinks.el ends here
