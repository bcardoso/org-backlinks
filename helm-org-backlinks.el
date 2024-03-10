;;; helm-org-backlinks.el --- Helm interface for Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.2
;; Package-Requires: ((emacs "27.2"))

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

(require 'helm)
(require 'org-backlinks)


;;;; Custom variables

(defcustom helm-org-backlinks-actions
  (helm-make-actions
   "Go to heading" 'org-backlinks-goto-heading)
  "Actions for `helm-org-backlinks' sources."
  :group 'org-backlinks
  :type 'sexp)


;;;; Sources

(defmacro helm-org-backlinks-build-source (name candidates)
  "Building Helm source NAME with CANDIDATES."
  (declare (indent defun))
  `(when ,candidates
     (helm-build-sync-source ,name
       :action helm-org-backlinks-actions
       :candidates ,candidates)))

(defvar helm-org-backlinks-source
  (helm-org-backlinks-build-source "Backlinks"
    (lambda () org-backlinks-list))
  "Helm source for Org backlinks.")

(defvar helm-org-backlinks-second-order-source
  (helm-org-backlinks-build-source "2nd order Backlinks"
    (lambda () org-backlinks-second-list))
  "Helm source for second order Org backlinks.")

(defvar helm-org-backlinks-third-order-source
  (helm-org-backlinks-build-source "3rd order Backlinks"
    (lambda () org-backlinks-third-list))
  "Helm source for third order Org backlinks.")

(defvar helm-org-backlinks-direct-source
  (helm-org-backlinks-build-source "Direct links"
    (lambda () org-backlinks-direct-list))
  "Helm source for the direct links from current heading.")

(defvar helm-org-backlinks-indirect-source
  (helm-org-backlinks-build-source "Indirect links"
    (lambda () org-backlinks-indirect-list))
  "Helm source for the indirect links from current heading.")


;;;; Commands

;;;###autoload
(defun helm-org-backlinks ()
  "Helm interface for `org-backlinks'."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Not an Org buffer.")
    (org-backlinks-setup)
    (when (org-backlinks-all-list)
      (helm :prompt "Go to heading: "
            :truncate-lines nil
            :sources '(helm-org-backlinks-source
                       helm-org-backlinks-second-order-source
                       helm-org-backlinks-third-order-source
                       helm-org-backlinks-direct-source
                       helm-org-backlinks-indirect-source)))))


(provide 'helm-org-backlinks)

;;; helm-org-backlinks.el ends here
