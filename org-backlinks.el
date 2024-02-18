;;; org-backlinks.el --- Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Bruno Cardoso

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

;; An interface for searching backlinks to Org headings.

;; This package aims to provide a sense of context when searching for
;; links *to* and *from* the current note.

;; Backlinks are the Org headings that have links to the current
;; Org heading at point.  Second order backlinks are the backlinks to
;; each of the current heading's backlinks.  Third order backlinks
;; are the same but for the second order ones.

;; Optionally, it can also show the direct and indirect links of a note.

;; Direct links are the links to other headings present in the current
;; heading.  Indirect links are the direct links present in those headings.


;;; Code:

(require 'org-ql)


;;;; Custom variables

(defgroup org-backlinks nil
  "Group for `org-backlinks' customizations."
  :group 'org)

(defcustom org-backlinks-show-second-order-backlinks t
  "If non-nil, show second order backlinks.
Second order backlinks are the backlinks to each of the
current heading's backlinks."
  :group 'org-backlinks
  :type 'boolean)

(defcustom org-backlinks-show-third-order-backlinks t
  "If non-nil, show third order backlinks."
  :group 'org-backlinks
  :type 'boolean)

(defcustom org-backlinks-show-direct-links nil
  "If non-nil, show the direct links to other headings in current heading."
  :group 'org-backlinks
  :type 'boolean)

(defcustom org-backlinks-show-indirect-links nil
  "If non-nil, show the indirect links to other headings in current heading."
  :group 'org-backlinks
  :type 'boolean)

(defcustom org-backlinks-prefix-id "id:"
  "Prefix for ID search."
  :group 'org-backlinks
  :type 'string)

(defcustom org-backlinks-prefix-custom-id "#"
  "Prefix for CUSTOM_ID search."
  :group 'org-backlinks
  :type 'string)

(defcustom org-backlinks-files 'org-files-list
  "Which Org files should be searched for backlinks.
Default values are:

  'agenda          list of Org agenda files
  'buffers         list of open Org buffers
  'org-files-list  list of Org agenda files + open Org buffers

Alternatively, this variable can be a custom list of Org files."
  :group 'org-backlinks
  :type 'sexp)

(defcustom org-backlinks-width 78
  "Maximum number of characters available for the Org heading path."
  :group 'org-backlinks
  :type 'integer)

(defcustom org-backlinks-recenter nil
  "If nil, center point in selected window and maybe redisplay frame.
With a numeric value, recenter putting point on screen line
relative to the selected window. See `recenter'."
  :group 'org-backlinks
  :type 'integer)

;;;; Variables

(defvar org-backlinks-list nil
  "List of Org headings with links to current heading.")

(defvar org-backlinks-second-list nil
  "List of Org headings with links to current heading's backlinks.")

(defvar org-backlinks-third-list nil
  "List of Org headings of third order backlinks.")

(defvar org-backlinks-direct-list nil
  "List of Org headings direct linked from current heading.")

(defvar org-backlinks-indirect-list nil
  "List of Org headings indirect linked from current heading.")


;;;; Functions

(defun org-backlinks-files ()
  "List of Org files to search for headings."
  (delete-dups
   (mapcar #'file-truename
           (cond ((eq org-backlinks-files 'agenda)
                  (org-agenda-files))
                 ((eq org-backlinks-files 'org-files-list)
                  (org-files-list))
                 ((eq org-backlinks-files 'buffers)
                  (delete nil (mapcar #'buffer-file-name
                                      (org-buffer-list 'files t))))
                 (t
                  org-backlinks-files)))))

(defun org-backlinks-get-heading-info-by-id (id)
  "Return the heading info of ID."
  (org-ql-query
    :select #'org-backlinks-get-heading-info
    :from (org-backlinks-files)
    :where `(and (property "ID")
                 (string-match ,id
                               (org-entry-get (point) "ID")))))

(defun org-backlinks-get-heading-info-by-custom-id (custom-id)
  "Return the heading info of CUSTOM-ID."
  (org-ql-query
    :select #'org-backlinks-get-heading-info
    :from (org-backlinks-files)
    :where `(and (property "CUSTOM_ID")
                 (string-match ,custom-id
                               (org-entry-get (point) "CUSTOM_ID")))))

(defun org-backlinks-search-link (bound)
  "Return the ID or CUSTOM_ID of an Org link. BOUND is the end of heading."
  (let ((start (re-search-forward "\\[\\[\\(id:\\|.*::#\\)" bound t))
        (end (re-search-forward "\\(\\]\\[\\|\\]\\]\\)" nil t)))
    (goto-char (+ 1 (point)))
    (if start
        (buffer-substring-no-properties start (- end 2)))))

(defun org-backlinks-collect-heading-links (bound)
  "Collect links to Org IDs up until BOUND with `org-backlinks-search-link'."
  (save-excursion
    (org-back-to-heading)
    (let ((x (org-backlinks-search-link bound))
          (list nil))
      (while x
        (cl-pushnew x list :test #'equal)
        (setq x (org-backlinks-search-link bound)))
      (reverse list))))

(defun org-backlinks-get-heading-info ()
  "Return Org heading info.
This is a list whose CAR is the outline path of the current entry
and CDR is a plist containing `:tags', `:buffer', `:begin', `:end', `:id'
and `:custom_id'."
  (interactive)
  (let ((props (org-element-at-point-no-context)))
    `(,(org-format-outline-path
        (org-get-outline-path t t)
        org-backlinks-width
        (format "%s:" (file-name-nondirectory (buffer-file-name)))
        "/")
      (:tags      ,(org-get-tags)
       :buffer    ,(buffer-name)
       :begin     ,(org-element-property :begin props)
       :end       ,(org-element-property :end props)
       :id        ,(org-element-property :ID props)
       :custom_id ,(org-element-property :CUSTOM_ID props)))))

(defun org-backlinks-query (id)
  "Return the headings that link to an ID."
  (org-ql-select (org-backlinks-files) id
    :action #'org-backlinks-get-heading-info))

(defun org-backlinks-goto-heading (heading)
  "Go to HEADING."
  (interactive)
  (org-mark-ring-push)
  (switch-to-buffer-other-window (plist-get (car heading) :buffer))
  (goto-char (plist-get (car heading) :begin))
  (org-fold-show-context)
  (org-fold-show-entry)
  (org-fold-show-children)
  (recenter org-backlinks-recenter))

(defun org-backlinks-uniq (list)
  "Remove duplicates and normalize the LIST of headings."
  (cl-remove-duplicates (apply 'append list)
                        :test #'equal :key #'car :from-end t))

(defun org-backlinks-parse-backlinks (headings-list)
  "Parse HEADINGS-LIST for headings with IDs."
  (let ((backlinks nil))
    (dolist (heading headings-list)
      (let ((heading-id (or (plist-get (cadr heading) :custom_id)
                            (plist-get (cadr heading) :id))))
        (if heading-id
            (cl-pushnew (org-backlinks-query heading-id)
                        backlinks
                        :test #'equal))))
    ;; remove duplicates
    (setq backlinks (org-backlinks-uniq backlinks))

    ;; remove headings already present in the headings-list
    (setq backlinks (cl-set-difference backlinks headings-list
                                       :test #'equal))))

(defmacro org-backlinks-build-list (switch list prev del)
  "Macro for building the headings lists."
  `(if ,switch
       (setq ,list
             (cl-set-difference ,prev ,del :test #'equal))
     (setq ,list nil)))

(defun org-backlinks-parse (id)
  "Parse backlink lists for current ID."
  (let ((current-heading (save-excursion
                           (org-back-to-heading)
                           (org-backlinks-get-heading-info))))
    ;; backlinks
    (org-backlinks-build-list
     t
     org-backlinks-list
     (org-backlinks-query id)
     (list current-heading))

    ;; 2nd order backlinks
    (org-backlinks-build-list
     org-backlinks-show-second-order-backlinks
     org-backlinks-second-list
     (org-backlinks-parse-backlinks org-backlinks-list)
     (append (list current-heading) org-backlinks-list))

    ;; 3rd order backlinks
    (org-backlinks-build-list
     org-backlinks-show-third-order-backlinks
     org-backlinks-third-list
     (org-backlinks-parse-backlinks org-backlinks-second-list)
     (append (list current-heading) org-backlinks-list))))

(defun org-backlinks-direct-headings (bound)
  "Collect all Org links in current heading up until BOUND (end of heading)."
  (org-backlinks-uniq
   (append
    (mapcar #'org-backlinks-get-heading-info-by-id
            (org-backlinks-collect-heading-links bound))
    (mapcar #'org-backlinks-get-heading-info-by-custom-id
            (org-backlinks-collect-heading-links bound)))))

(defun org-backlinks-parse-direct-links ()
  "List of links from current heading to other headings."
  (let ((point (point))
        (current-heading (save-excursion
                           (org-back-to-heading)
                           (org-backlinks-get-heading-info))))
    (org-backlinks-build-list
     org-backlinks-show-direct-links
     org-backlinks-direct-list
     (org-backlinks-direct-headings (plist-get (cadr current-heading) :end))
     nil)

    (org-backlinks-build-list
     org-backlinks-show-indirect-links
     org-backlinks-indirect-list
     (org-backlinks-uniq
      (let ((indirect nil))
        (dolist (heading org-backlinks-direct-list (reverse indirect))
          (switch-to-buffer (plist-get (cadr heading) :buffer))
          (goto-char (plist-get (cadr heading) :begin))
          (cl-pushnew
           (org-backlinks-direct-headings (plist-get (cadr heading) :end))
           indirect :test #'equal))))
     (append (list current-heading) org-backlinks-direct-list))

    ;; XXX: ensure that we are always back to the current-heading
    (switch-to-buffer (plist-get (cadr current-heading) :buffer))
    (goto-char point)))


(defun org-backlinks-get-heading-id ()
  "Return Org entry CUSTOM_ID or ID."
  (interactive)
  (let ((id (org-id-get))
        (custom-id (org-entry-get (point) "CUSTOM_ID")))
    ;; custom_ids have priority over uuid
    (cond (custom-id (concat org-backlinks-prefix-custom-id custom-id))
          (id (concat org-backlinks-prefix-id id))
          (t nil))))

(defun org-backlinks-setup ()
  "Setup all links lists based on the ID of the current heading."
  (setq org-backlinks-list          nil
        org-backlinks-second-list   nil
        org-backlinks-third-list    nil
        org-backlinks-direct-list   nil
        org-backlinks-indirect-list nil)
  (let ((id (when (eq major-mode 'org-mode)
              (org-backlinks-get-heading-id))))
    (if id
        (org-backlinks-parse id)
      (message "Entry has no ID."))
    (if (not org-backlinks-list)
        (message "There are no links to this entry."))
    (if org-backlinks-show-direct-links
        (org-backlinks-parse-direct-links))))

(defun org-backlinks-all-list ()
  "Return a list with all possible links."
  (append org-backlinks-list
          (if org-backlinks-show-second-order-backlinks
              org-backlinks-second-list)
          (if org-backlinks-show-third-order-backlinks
              org-backlinks-third-list)
          (if org-backlinks-show-direct-links
              org-backlinks-direct-list)
          (if org-backlinks-show-indirect-links
              org-backlinks-indirect-list)))

;;;###autoload
(defun org-backlinks ()
  "Command for selection Org headings with `completing-read'."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Not an Org buffer.")
    (org-backlinks-setup)
    (let ((link-list (org-backlinks-all-list)))
      (when link-list
        (let ((heading (completing-read "Go to heading: " link-list)))
          (org-backlinks-goto-heading (cdr (assoc heading link-list))))))))


(provide 'org-backlinks)

;;; org-backlinks.el ends here
