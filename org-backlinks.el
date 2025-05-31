;;; org-backlinks.el --- Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.3
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

;; An interface for searching backlinks to Org headings.

;; This package aims to provide a sense of context when searching for
;; links *to* and *from* the current heading.

;; Backlinks are the Org headings that have links to the current
;; Org heading at point.  Second order backlinks are the backlinks to
;; each of the current heading's backlinks.  Third order backlinks
;; are the same but for the second order ones.

;; Optionally, it can also show the direct and indirect links of a note.

;; Direct links are the links to other headings present in the current
;; heading.  Indirect links are the direct links present in those headings.


;;; Code:

(require 'org-ql)

(declare-function org-ql--normalize-query "org-ql")


;;;; Custom variables

(defgroup org-backlinks nil
  "Group for `org-backlinks' customizations."
  :group 'org)

(defcustom org-backlinks-show-second-order-backlinks t
  "If non-nil, show second order backlinks.
Second order backlinks are the backlinks to each of the
current heading's backlinks."
  :type 'boolean)

(defcustom org-backlinks-show-third-order-backlinks t
  "If non-nil, show third order backlinks."
  :type 'boolean)

(defcustom org-backlinks-show-direct-links nil
  "If non-nil, show the direct links to other headings in current heading."
  :type 'boolean)

(defcustom org-backlinks-show-indirect-links nil
  "If non-nil, show the indirect links to other headings in current heading."
  :type 'boolean)

(defcustom org-backlinks-prefix-id "id:"
  "Prefix for ID search."
  :type 'string)

(defcustom org-backlinks-prefix-custom-id "#"
  "Prefix for CUSTOM_ID search."
  :type 'string)

(defcustom org-backlinks-files 'org-files-list
  "Which Org files should be searched for backlinks.
Default values are:

  \\='agenda          list of Org agenda files
  \\='buffers         list of open Org buffers
  \\='org-files-list  list of Org agenda files + open Org buffers

Alternatively, this variable can be a custom list of Org files."
  :type 'sexp)

(defcustom org-backlinks-width 78
  "Maximum number of characters available for the Org heading path."
  :type 'integer)

(defcustom org-backlinks-recenter nil
  "If nil, center point in selected window and maybe redisplay frame.
With a numeric value, recenter putting point on screen line
relative to the selected window. See `recenter'."
  :type 'integer)

(defface org-backlinks-file-face
  '((t (:inherit (shadow))))
  "Face for the file name part of the candidate.")


;;;; Variables

(defvar org-backlinks-link-types
  '(("id"   . "ID")
    ("file" . "CUSTOM_ID"))
  "Alist of Org link types and their related Org heading properties.

Arbitrary link parameters can be added to this list, as long as they are
defined with `org-link-set-parameters' and depend on unique values of an
specific Org heading property.")

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


;;;;; Heading data

(defun org-backlinks-candidate-format (heading marker)
  "Return the formatted candidate for HEADING at MARKER."
  (let ((candidate
         (concat (org-format-outline-path
                  (org-get-outline-path t t)
                  org-backlinks-width
                  (propertize (format "%s:" (marker-buffer marker))
                              'face 'org-backlinks-file-face))
                 " "
                 (propertize (org-make-tag-string
                              (org-element-property :tags heading))
                             'face 'org-tag))))
    (put-text-property 0 1 'org-marker marker candidate)
    candidate))

(defun org-backlinks-get-heading-ids (heading-at-point)
  "Return an alist of `org-backlinks-link-types' for HEADING-AT-POINT."
  (delete nil
          (let ((heading (or heading-at-point (org-element-at-point))))
            (mapcar
             (lambda (type)
               (when-let* ((prop (org-element-property
                                  (intern (concat ":" (upcase (cdr type))))
                                  heading)))
                 (cons (cdr type) prop)))
             org-backlinks-link-types))))

(defun org-backlinks-get-heading (&optional heading-at-point)
  "Return the relevant information for HEADING-AT-POINT.
This is a list whose CAR is the outline path of the current heading
and CDR is a plist of `:marker' and `:ids'."
  (let ((heading (or heading-at-point (org-element-at-point-no-context)))
        (marker (point-marker)))
    (cons (org-backlinks-candidate-format heading marker)
          (list :marker marker
                :ids (org-backlinks-get-heading-ids heading)))))


;;;;; Queries

(defun org-backlinks-find-backlinks (id-alist)
  "Return a list of headings with links to any ID on ID-ALIST."
  (when id-alist
    (org-ql-query
      :select #'org-backlinks-get-heading
      :from (org-backlinks-files)
      :where
      `(or ,@(mapcar
              (lambda (id)
                `(rifle
                  ,(pcase (car id)
                     ("ID"
                      (concat org-backlinks-prefix-id (cdr id)))
                     ("CUSTOM_ID"
                      (concat org-backlinks-prefix-custom-id (cdr id)))
                     (_
                      (concat
                       (car (rassoc (car id) org-backlinks-link-types))
                       ":" (cdr id))))))
              id-alist)))))

(defun org-backlinks-find-heading (id-alist)
  "Return the relevant information of the Org headings matching ID-ALIST."
  (when id-alist
    (org-ql-query
      :select #'org-backlinks-get-heading
      :from (org-backlinks-files)
      :where `(or ,@(mapcar (lambda (id)
                              `(property ,(car id) ,(cdr id)))
                            id-alist)))))


;;;;; Lists of headings

(defun org-backlinks-unique (headings-list)
  "Return a unique elements from HEADINGS-LIST."
  (delete nil
          (cl-remove-duplicates headings-list ;;(apply 'append list)
                                :test #'equal :key #'car :from-end t)))

(defun org-backlinks-build-list (headings-list remove-list &optional h)
  "Return a list from HEADINGS-LIST excluding entries from REMOVE-LIST.
When optional argument H is a heading, also remove it from final list."
  (let ((hlist (cl-set-difference headings-list remove-list :test #'equal)))
    (cl-remove h hlist :test #'equal)))

(defun org-backlinks-parse (headings-list)
  "Return a unique list of headings with links to headings in HEADINGS-LIST."
  (org-backlinks-unique
   (mapcar (lambda (heading)
             (org-backlinks-find-backlinks (plist-get (cdr heading) :ids)))
           headings-list)))


;;;;; Direct links

(defun org-backlinks-get-direct-links (&optional heading)
  "Return a list of `org-backlinks-link-types' present in HEADING."
  (let* ((heading (or heading (org-backlinks-get-heading)))
         (marker (plist-get (cdr heading) :marker)))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (save-restriction
        (org-narrow-to-subtree)
        (delete
         nil
         (org-element-map (org-element-parse-buffer)
             '(link)
           (lambda (link)
             (when-let* ((type (plist-get (cadr link) :type))
                         (type-prop (assoc type org-backlinks-link-types)))
               (let ((custom-id (plist-get (cadr link) :search-option))
                     (raw-link  (plist-get (cadr link) :raw-link)))
                 (cond ((and (equal type "file")
                             (stringp custom-id)
                             (string-match-p "::#" raw-link))
                        (cons (cdr type-prop)
                              (string-remove-prefix "#" custom-id)))
                       ((not (equal type "file"))
                        (cons (cdr type-prop)
                              (plist-get (cadr link) :path)))))))))))))


;;;; Setup

(defun org-backlinks-find-near-links (heading)
  "Find near links *in* and *to* HEADING. Return `org-backlinks-list'.
Near links are backlinks to HEADING or direct links present in HEADING."
  ;; Backlinks
  (if-let ((ids (plist-get (cdr heading) :ids)))
      (setq org-backlinks-list
            (org-backlinks-build-list (org-backlinks-find-backlinks ids)
                                      (list heading)))
    (message "Entry has no ID."))
  ;; Direct links
  (setq org-backlinks-direct-list
        (when org-backlinks-show-direct-links
          (org-backlinks-unique
           (org-backlinks-find-heading
            (org-backlinks-get-direct-links heading)))))
  org-backlinks-list)

(defun org-backlinks-find-distant-links (heading)
  "Find distant links related to HEADING.
Distant links are second and third order backlinks, and indirect links."
  ;; Second order links
  (setq org-backlinks-second-list
        (when (and org-backlinks-show-second-order-backlinks
                   org-backlinks-list)
          (org-backlinks-build-list
           (org-backlinks-parse org-backlinks-list)
           org-backlinks-list
           heading)))
  ;; Third order links
  (setq org-backlinks-third-list
        (when (and org-backlinks-show-third-order-backlinks
                   org-backlinks-second-list)
          (org-backlinks-build-list
           (org-backlinks-parse org-backlinks-second-list)
           (append org-backlinks-list org-backlinks-second-list)
           heading)))
  ;; Indirect links
  (setq org-backlinks-indirect-list
        (when (and org-backlinks-show-indirect-links
                   org-backlinks-direct-list)
          (org-backlinks-build-list
           (org-backlinks-unique
            (org-backlinks-find-heading
             (mapcan #'org-backlinks-get-direct-links
                     org-backlinks-direct-list)))
           org-backlinks-direct-list
           heading))))

(defun org-backlinks-setup ()
  "Setup `org-backlinks' lists."
  (setq org-backlinks-list          nil
        org-backlinks-second-list   nil
        org-backlinks-third-list    nil
        org-backlinks-direct-list   nil
        org-backlinks-indirect-list nil)
  (save-excursion
    (if (and (derived-mode-p 'org-mode)
             (ignore-errors (org-back-to-heading t)))
        (let ((heading (org-backlinks-get-heading)))
          (org-backlinks-find-near-links heading)
          (unless org-backlinks-list
            (message "There are no links to this entry."))
          (org-backlinks-find-distant-links heading))
      (message "Not an Org heading at point."))))

(defun org-backlinks-setup-simple (&rest _)
  "Simple and silient `org-backlinks-setup' for a backlink-only search."
  (let ((inhibit-message t)
        (message-log-max nil)
        (org-backlinks-show-second-order-backlinks nil)
        (org-backlinks-show-third-order-backlinks nil)
        (org-backlinks-show-direct-links nil)
        (org-backlinks-show-indirect-links nil))
    (org-backlinks-setup)))

(defun org-backlinks-list-all ()
  "Return a list with all possible links."
  (delete-dups
   (append org-backlinks-list
           org-backlinks-second-list
           org-backlinks-third-list
           org-backlinks-direct-list
           org-backlinks-indirect-list)))

(defun org-backlinks-goto-heading (heading)
  "Go to HEADING."
  (interactive)
  (let* ((marker (or (and (listp heading)
                          (plist-get heading :marker))
                     (get-text-property 0 'org-marker heading)))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (org-mark-ring-push)
    (switch-to-buffer-other-window buffer)
    (goto-char position)
    (org-fold-show-entry)
    (org-fold-show-children)
    (recenter org-backlinks-recenter)))

;;;###autoload
(defun org-backlinks ()
  "Command for selection Org headings with `completing-read'."
  (interactive)
  (org-backlinks-setup)
  (when-let ((link-list (org-backlinks-list-all)))
    (let ((heading (completing-read "Go to heading: " link-list)))
      (org-backlinks-goto-heading (cdr (assoc heading link-list))))))


(provide 'org-backlinks)

;;; org-backlinks.el ends here
