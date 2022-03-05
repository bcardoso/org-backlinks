;;; org-backlinks.el --- Org backlinks interface -*- lexical-binding: t -*-

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

;; TODO: move helm functions to separate file
;;       use completing-read in org-backlinks function
(require 'helm)
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


;;;; Variables

;; TODO: move to another file

(defvar helm-org-backlinks-source nil
  "Helm source for Org backlinks")

(defvar helm-org-backlinks-2nd-source nil
  "Helm source for second order Org backlinks")


;;;; Functions

(defun org-backlinks-get-heading-info ()
  "Return Org heading info at point: heading, buffer, point, id."
  `(:heading ,(org-format-outline-path (org-get-outline-path t t))
    :buffer ,(buffer-name)
    :point ,(point)
    :id ,(or (org-entry-get (point) "CUSTOM_ID") (org-id-get))))

(defun org-backlinks-query (id)
  "Return the headings that link to an ID."
  (org-ql-select (org-buffer-list) id
    :action #'org-backlinks-get-heading-info))

(defun org-backlinks-headings (headings-list)
  "Return a list of Org headings locations."
  (let ((headings nil))
    (dolist (heading headings-list headings)
      (cl-pushnew `(,(plist-get heading :heading)
                    ,(plist-get heading :buffer)
                    ,(plist-get heading :point))
                  headings :test #'equal))))

(defun org-backlinks-goto-heading (buffer-point)
  (interactive)
  (org-mark-ring-push)
  (switch-to-buffer-other-window (car buffer-point))
  (goto-char (cadr buffer-point))
  (org-show-context)
  (org-show-entry)
  (org-show-children))

(defun org-backlinks-def-helm-sources (id)
  (let ((backlinks (org-backlinks-query id))
        (backlinks-2nd nil))
    ;; remove current heading from list
    (let ((point (point)))
      (re-search-backward org-heading-regexp nil t)
      (setq backlinks (delete (org-backlinks-get-heading-info) backlinks))
      (goto-char point))

    ;; parse backlinks
    (if backlinks ;; TODO: this must be global variable
        (setq helm-backlinks-source ;; TODO: move to separate file
              (helm-build-sync-source "Backlinks"
                :action '(("Go to" . org-backlinks-goto-heading))
                :candidates (org-backlinks-headings backlinks)))
      (setq helm-backlinks-source nil))

    ;; parse 2nd order backlinks
    (dolist (heading backlinks)
      (let ((heading-id (plist-get heading :id)))
        (if heading-id
            (cl-pushnew
             (org-backlinks-headings (org-backlinks-query heading-id))
             backlinks-2nd :test #'equal))))

    (setq backlinks-2nd
          (cl-remove-duplicates (apply 'append backlinks-2nd) ;; unnest
                                :test #'equal :key #'car :from-end t))

    (if backlinks-2nd ;; TODO: this must be global variable
        (setq helm-backlinks-2nd-source  ;; TODO: move to separate file
              (helm-build-sync-source "2nd order Backlinks"
                :action '(("Go to" . org-backlinks-goto-heading))
                :candidates backlinks-2nd))
      (setq helm-backlinks-2nd-source nil))))

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
      (org-backlinks-def-helm-sources id)
      (if (not helm-backlinks-source)
          (message "There are no links to this entry.")
        (helm :prompt "Go to heading: " ;; TODO: use completing-read, move helm to another file
              :sources '(helm-backlinks-source
                         helm-backlinks-2nd-source))))))


(provide 'org-backlinks)

;;; org-backlinks.el ends here
