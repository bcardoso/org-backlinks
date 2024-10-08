;;; org-backlinks.el --- Org backlinks -*- lexical-binding: t -*-

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

  \\='agenda          list of Org agenda files
  \\='buffers         list of open Org buffers
  \\='org-files-list  list of Org agenda files + open Org buffers

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

(defface org-backlinks-file-face
  '((t (:inherit (shadow))))
  "Face for the file name part of the candidate."
  :group 'org-backlinks)


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

(defun org-backlinks-get-heading (&optional current-heading)
  "Return the relevant information about the current Org heading.
This is a list whose CAR is the outline path of the current entry
and CDR is a plist containing `:marker', `:buffer', `:begin', `:end',
`:id', and `:custom_id'."
  (interactive)
  (let* ((heading (or current-heading (org-element-at-point-no-context)))
         (marker (point-marker))
         (buffer (org-element-property :buffer heading))
         (candidate
          (concat (org-format-outline-path
                   (org-get-outline-path t t)
                   org-backlinks-width
                   (propertize (format "%s:" buffer)
                               'face 'org-backlinks-file-face))
                  " "
                  (propertize (org-make-tag-string
                               (org-element-property :tags heading))
                              'face 'org-tag))))
    (put-text-property 0 1 'org-marker marker candidate)
    (cons candidate
          (list :marker    marker
                :buffer    buffer
                :begin     (org-element-property :begin heading)
                :end       (org-element-property :end heading)
                :id        (org-element-property :ID heading)
                :custom_id (org-element-property :CUSTOM_ID heading)))))

(defun org-backlinks-get-heading-id (&optional heading)
  "Return a list of prefixed ID or CUSTOM_ID from HEADING."
  (interactive)
  (let ((custom-id (or (plist-get (cdr heading) :custom_id)
                       (org-entry-get (point) "CUSTOM_ID")))
        (id (or (plist-get (cdr heading) :id) (org-id-get))))
    (delete
     nil
     (list
      (and custom-id (concat org-backlinks-prefix-custom-id custom-id))
      (and id org-backlinks-prefix-id id)))))

(defun org-backlinks-find-links (id)
  "Return a list of headings with links to ID."
  (org-ql-query
    :select #'org-backlinks-get-heading
    :from (org-backlinks-files)
    :where (if (length> id 1)
               `(or (rifle ,(car id)) (rifle ,(cadr id)))
             `(rifle ,(car id)))))

(defun org-backlinks-find-heading (id)
  "Return the relevant information about the Org heading with ID."
  (org-ql-query
    :select #'org-backlinks-get-heading
    :from (org-backlinks-files)
    :where `(or (property "ID" ,id) (property "CUSTOM_ID" ,id))))

(defun org-backlinks-unique (list)
  "Return a unique list of elements from LIST."
  (cl-remove-duplicates (apply 'append list)
                        :test #'equal :key #'car :from-end t))

(defun org-backlinks-build-list (headings-list exclude-list &optional h)
  "Return a list from HEADINGS-LIST excluding entries from EXCLUDE-LIST.
When optional argument H is a heading, also exclude it from final list."
  (let ((hlist (cl-set-difference headings-list exclude-list :test #'equal)))
    (cl-remove h hlist :test #'equal)))

(defun org-backlinks-parse (headings-list)
  "Return a unique list of headings with links to headings in HEADINGS-LIST."
  (org-backlinks-unique
   (mapcar (lambda (heading)
             (org-backlinks-find-links
              (org-backlinks-get-heading-id heading)))
           headings-list)))


;;;;; Direct links

(defun org-backlinks-search-link (bound)
  "Return the ID or CUSTOM_ID in an Org link. BOUND is the end of heading."
  (let ((start (re-search-forward "\\[\\[\\(id:\\|.*::#\\)" bound t))
        (end (re-search-forward "\\(\\]\\[\\|\\]\\]\\)" bound t)))
    (goto-char (+ 1 (point)))
    (if start
        (buffer-substring-no-properties start (- end 2)))))

(defun org-backlinks-get-heading-links (heading)
  "Return a list of IDs or CUSTOM_IDs present in HEADING."
  (let* ((buffer (plist-get (cdr heading) :buffer))
         (begin (plist-get (cdr heading) :begin))
         (end (plist-get (cdr heading) :end))
         (links))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char begin)
       (while-let ((link (org-backlinks-search-link end)))
         (push link links))))
    (reverse links)))


;;;; Setup

(defun org-backlinks-find-near-links (heading)
  "Find near links *in* and *to* HEADING.
Near links are backlinks to HEADING or direct links present in HEADING.
Return `org-backlinks-list'."
  ;; backlinks
  (if-let ((id (org-backlinks-get-heading-id heading)))
      (setq org-backlinks-list
            (org-backlinks-build-list (org-backlinks-find-links id)
                                      (list heading)))
    (message "Entry has no ID."))
  ;; direct links
  (setq org-backlinks-direct-list
        (when org-backlinks-show-direct-links
          (org-backlinks-unique
           (mapcar #'org-backlinks-find-heading
                   (org-backlinks-get-heading-links heading)))))
  org-backlinks-list)

(defun org-backlinks-find-distant-links (heading)
  "Find distant links related to HEADING.
Distant links are second and third order backlinks, and indirect links."
  ;; second order links
  (setq org-backlinks-second-list
        (when (and org-backlinks-show-second-order-backlinks
                   org-backlinks-list)
          (org-backlinks-build-list
           (org-backlinks-parse org-backlinks-list)
           org-backlinks-list
           heading)))
  ;; third order links
  (setq org-backlinks-third-list
        (when (and org-backlinks-show-third-order-backlinks
                   org-backlinks-second-list)
          (org-backlinks-build-list
           (org-backlinks-parse org-backlinks-second-list)
           (append org-backlinks-list org-backlinks-second-list)
           heading)))
  ;; indirect links
  (setq org-backlinks-indirect-list
        (when (and org-backlinks-show-indirect-links
                   org-backlinks-direct-list)
          (org-backlinks-build-list
           (org-backlinks-unique
            (mapcar #'org-backlinks-find-heading
                    (flatten-tree (mapcar #'org-backlinks-get-heading-links
                                          org-backlinks-direct-list))))
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
         (buffer (or (and (listp heading)
                          (plist-get heading :buffer))
                     (marker-buffer marker)))
         (position (or (and (listp heading)
                            (plist-get heading :begin))
                       (marker-position marker))))
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
