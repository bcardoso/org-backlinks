;;; consult-org-backlinks.el --- Consult interface for Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))

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

;; Consult interface for Org backlinks.


;;; Code:

(require 'org-backlinks)
(require 'consult)

(defun consult-org-backlinks-goto-heading (heading)
  (org-backlinks-goto-heading
   (alist-get heading (org-backlinks-list-all) nil nil #'equal)))

(defmacro consult-org-backlinks-build-source (name narrow items)
  (declare (indent defun))
  `(list :name ,(propertize name 'face 'shadow)
         :prompt "Go to heading: "
         :narrow ,narrow
         :category 'org-heading
         :action #'org-backlinks-goto-heading
         :items (lambda () (mapcar #'car ,items))))

(defvar consult-org-backlinks-source
  (consult-org-backlinks-build-source
    "Backlinks"
    ?b org-backlinks-list)
  "Consult source for Org backlinks.")

(defvar consult-org-backlinks-second-order-source
  (consult-org-backlinks-build-source
    "2nd order Backlinks" ?2 org-backlinks-second-list)
  "Consult source for second order Org backlinks.")

(defvar consult-org-backlinks-third-order-source
  (consult-org-backlinks-build-source
    "3rd order Backlinks" ?3 org-backlinks-third-list)
  "Consult source for third order Org backlinks.")

(defvar consult-org-backlinks-direct-source
  (consult-org-backlinks-build-source
    "Direct links" ?d org-backlinks-direct-list)
  "Consult source for the direct links from current heading.")

(defvar consult-org-backlinks-indirect-source
  (consult-org-backlinks-build-source
    "Indirect links" ?i org-backlinks-indirect-list)
  "Consult source for the indirect links from current heading.")

(defcustom consult-org-backlinks-sources
  '(consult-org-backlinks-source
    consult-org-backlinks-second-order-source
    consult-org-backlinks-third-order-source
    consult-org-backlinks-direct-source
    consult-org-backlinks-indirect-source)
  "Source list for `consult-org-backlinks'."
  :group 'org-backlinks
  :type '(repeat (choice symbol)))

;;;###autoload
(defun consult-org-backlinks ()
  "Consult interface for `org-backlinks'."
  (interactive)
  (org-backlinks-setup)
  (when (org-backlinks-list-all)
    (consult--multi consult-org-backlinks-sources :sort nil)))


(provide 'consult-org-backlinks)

;;; consult-org-backlinks.el ends here
