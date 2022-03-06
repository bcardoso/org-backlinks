;;; org-backlinks.el --- Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.1
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

;; An interface for searching Org headings' backlinks: the Org headings
;; that have links to the Org heading at point.

;; Second order backlinks are the backlinks of each of the current
;; heading's backlinks.


;;; Code:

(require 'org-ql)


;;;; Custom variables

(defgroup org-backlinks nil
  "Group for `org-backlinks' customizations."
  :group 'org)

(defcustom org-backlinks-show-second-order-backlinks t
  "If non-nil, show second order backlinks.
Second order backlinks are the backlinks of each of the
current heading's backlinks."
  :type 'boolean)

(defcustom org-backlinks-id-prefix "id:"
  "Prefix for searching for ID."
  :type 'string)

(defcustom org-backlinks-custom-id-prefix "#"
  "Prefix for searching for CUSTOM_ID."
  :type 'string)

(defcustom org-backlinks-files 'org-files-list
  "Which Org files should be searched for backlinks.
Default values are:

  'agenda          list of Org agenda files
  'buffers         list of open Org buffers
  'org-files-list  list of Org agenda files + open Org buffers

Alternatively, this variable can be a custom list of Org files."
  :group 'org-hop
  :type 'sexp)


;;;; Variables

(defvar org-backlinks-list nil
  "List of Org headings with links to current heading.")

(defvar org-backlinks-second-list nil
  "List of Org headings with links to current heading's backlinks.")


;;;; Functions

(defun org-backlinks-files ()
  "List of Org files to search for headings."
  (cond ((eq org-backlinks-files 'agenda)
         (org-agenda-files))
        ((eq org-backlinks-files 'buffers)
         (org-buffer-list))
        ((eq org-backlinks-files 'org-files-list)
         (org-files-list))
        (t
         org-backlinks-files)))

(defun org-backlinks-get-heading-info ()
  "Return Org heading info at point: heading, buffer, point, id."
  `(,(org-format-outline-path (org-get-outline-path t t))
    ,(buffer-name)
    ,(point)
    ,(or (org-entry-get (point) "CUSTOM_ID") (org-id-get))))

(defun org-backlinks-query (id)
  "Return the headings that link to an ID."
  (org-ql-select (org-backlinks-files) id
    :action #'org-backlinks-get-heading-info))

(defun org-backlinks-goto-heading (buffer-point)
  (interactive)
  (org-mark-ring-push)
  (switch-to-buffer-other-window (car buffer-point))
  (goto-char (cadr buffer-point))
  (org-show-context)
  (org-show-entry)
  (org-show-children))

(defun org-backlinks-parse (id)
  (setq org-backlinks-list (org-backlinks-query id))

  ;; remove current heading from list
  (let ((point (point)))
    (re-search-backward org-heading-regexp nil t)
    (setq org-backlinks-query
          (delete (org-backlinks-get-heading-info) org-backlinks-list))
    (goto-char point))

  (when org-backlinks-show-second-order-backlinks
    (setq org-backlinks-second-list nil)

    ;; search for backlinks for headings with an ID
    (dolist (heading org-backlinks-list)
      (let ((heading-id (nth 3 heading)))
        (if heading-id
            (cl-pushnew
             (org-backlinks-query heading-id)
             org-backlinks-second-list :test #'equal))))

    ;; remove duplicates & headings already present in the first list
    (setq org-backlinks-second-list
          (cl-remove-duplicates (apply 'append org-backlinks-second-list)
                                :test #'equal :key #'car :from-end t))
    (dolist (heading org-backlinks-list)
      (setq org-backlinks-second-list
            (delete heading org-backlinks-second-list)))))

(defun org-backlinks-get-heading-id ()
  "Return Org entry CUSTOM_ID or ID."
  (interactive)
  (let ((id (org-id-get))
        (custom-id (org-entry-get (point) "CUSTOM_ID")))
    ;; custom_ids have priority over uuid
    (cond (custom-id (concat org-backlinks-custom-id-prefix custom-id))
          (id (concat org-backlinks-id-prefix id))
          (t nil))))

;;;###autoload
(defun org-backlinks ()
  (interactive)
  (let ((id (org-backlinks-get-heading-id)))
    (if (not id)
        (message "Entry has no ID.")
      (org-backlinks-parse id)
      (if (not org-backlinks-list)
          (message "There are no links to this entry.")
        (let* ((list (if org-backlinks-show-second-order-backlinks
                         (append org-backlinks-list
                                 org-backlinks-second-list)
                       org-backlinks-list))
               (heading (completing-read "Go to heading: " list)))
          (org-backlinks-goto-heading (cdr (assoc heading list))))))))


(provide 'org-backlinks)

;;; org-backlinks.el ends here
