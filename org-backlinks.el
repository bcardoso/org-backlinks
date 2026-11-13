;;; org-backlinks.el --- Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.4.0
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
(require 'org-ql-search)
(require 'org-super-agenda)

(declare-function org-ql--normalize-query "org-ql")


;;;; Custom variables

(defgroup org-backlinks nil
  "Group for `org-backlinks' customizations."
  :group 'org)

(defcustom org-backlinks-depth '(3 . 2)
  "A cons of (MAX-BACKWARD-DEPTH . MAX-FORWARD-DEPTH) to search for links."
  :type '(cons natnum natnum))

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
  :type
  '(choice (const :tag "Agenda files" agenda)
           (const :tag "Open Org buffers" buffers)
           (const :tag "Agenda files + Org buffers" org-files-list)
           (repeat :tag "Custom list of Org files" (file :must-match t))))

(defcustom org-backlinks-link-types
  '(("id"   . "ID")
    ("file" . "CUSTOM_ID"))
  "Alist of Org link types and their related Org heading properties.

Arbitrary link parameters can be added to this list, as long as they are
defined with `org-link-set-parameters' and depend on unique values of an
specific Org heading property."
  :type '(alist :key-type string :value-type string))

(defcustom org-backlinks-width 78
  "Maximum number of characters available for the Org heading path."
  :type 'natnum)

(defcustom org-backlinks-recenter nil
  "If nil, center point in selected window and maybe redisplay frame.
With a numeric value, recenter putting point on screen line
relative to the selected window. See `recenter'."
  :type 'integer)

;; TODO 2026-11-12: defcustom
(defvar org-backlinks-view-groups
  '((:auto-backlinks t)
    (:auto-near-links t)
    (:auto-distant-links t)))

(defface org-backlinks-file-face
  '((t (:inherit (shadow))))
  "Face for the file name part of the candidate.")


;;;; Functions

;;;;; Org files

(defun org-backlinks-files ()
  "List of Org files to search for headings."
  (delete
   nil
   (delete-dups
    (mapcar #'file-truename
            (cond ((eq org-backlinks-files 'agenda)
                   (org-agenda-files))
                  ((eq org-backlinks-files 'org-files-list)
                   (org-files-list))
                  ((eq org-backlinks-files 'buffers)
                   (mapcar #'buffer-file-name (org-buffer-list 'files t)))
                  (t
                   (ensure-list org-backlinks-files)))))))


;;;;; Heading data

(defun org-backlinks--key (marker)
  "Return the key (BUFFER . POS) for MARKER."
  ;; TODO 2026-11-13: when markerp
  (cons (marker-buffer marker) (marker-position marker)))

(defun org-backlinks--heading-ids (&optional heading-at-point)
  "Return an alist of `org-backlinks-link-types' for HEADING-AT-POINT."
  (delete
   nil
   (let ((heading (or heading-at-point (org-element-at-point-no-context))))
     (mapcar
      (lambda (type-prop)
        (when-let* ((prop (cdr type-prop))
                    (value (org-element-property
                            (intern (concat ":" (upcase prop)))
                            heading)))
          (cons prop value)))
      org-backlinks-link-types))))

(defun org-backlinks--heading-data (&optional heading-at-point)
  "Return the plist data for HEADING-AT-POINT."
  (let ((heading (or heading-at-point (org-element-at-point-no-context)))
        (marker (point-marker)))
    (list :marker marker
          :ids (org-backlinks--heading-ids heading)
          :olp (org-get-outline-path t t)
          :tags (org-element-property :tags heading))))

(defun org-backlinks--heading-format (marker olp &optional tags)
  "Return the formatted candidate for MARKER and OLP, with optional TAGS."
  (let ((candidate
         (concat (org-format-outline-path
                  olp
                  org-backlinks-width
                  (propertize (format "%s:" (marker-buffer marker))
                              'face 'org-backlinks-file-face))
                 (when tags
                   (concat " " (propertize (org-make-tag-string tags)
                                           'face 'org-tag))))))
    (put-text-property 0 1 'org-marker marker candidate)
    candidate))

(cl-defstruct (org-backlinks-heading
               (:constructor org-backlinks-heading--make)
               (:copier nil))
  marker ids olp tags direction depth candidate)

(defun org-backlinks-heading-new (hdata &optional direction depth)
  "Return a new heading from HDATA plist, with DIRECTION and DEPTH."
  (let ((marker (plist-get hdata :marker))
        (tags (plist-get hdata :tags))
        (olp (plist-get hdata :olp)))
    (org-backlinks-heading--make
     :marker marker
     :ids (plist-get hdata :ids)
     :olp olp
     :tags tags
     :direction direction
     :depth depth
     :candidate (org-backlinks--heading-format marker olp tags))))


;;;;; Queries

(defun org-backlinks--query-backlinks (id-alist)
  "Return the list of headings with links to ID-ALIST.
ID-ALIST is an alist of (LINK-TYPE . ID)."
  (when id-alist
    (org-ql-query
      :select #'org-backlinks--heading-data
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

(defun org-backlinks--query-ids (id-alist)
  "Return the list of Org headings' data whose properties match ID-ALIST.
ID-ALIST is an alist of (LINK-TYPE . ID)."
  (when id-alist
    (org-ql-query
      :select #'org-backlinks--heading-data
      :from (org-backlinks-files)
      :where `(or ,@(mapcar (lambda (id)
                              `(property ,(car id) ,(cdr id)))
                            id-alist)))))

;;;;; Forward links

(defun org-backlinks--heading-forward-links (marker)
  "Return the list of (LINK-TYPE . ID) present in heading at MARKER.
Link types are the ones defined in `org-backlinks-link-types'."
  (org-with-point-at marker
    (save-restriction
      (org-narrow-to-subtree)
      (delete
       nil
       (org-element-map (org-element-parse-buffer)
           '(link)
         (lambda (link)
           (when-let* ((plist (cadr link))
                       (type (plist-get plist :type))
                       (prop (cdr (assoc type org-backlinks-link-types))))
             (let ((custom-id (plist-get plist :search-option))
                   (raw-link  (plist-get plist :raw-link)))
               (cond ((and (equal type "file")
                           (stringp custom-id)
                           (string-match-p "::#" raw-link))
                      (cons prop (string-remove-prefix "#" custom-id)))
                     ((not (equal type "file"))
                      (cons prop (plist-get plist :path))))))))))))


;;;;; Collect links relative to a heading

(defun org-backlinks--add-heading (hdata direction depth ht)
  "Add a new heading, created from the HDATA plist, into the HT hash-table.
DIRECTION is a symbol, either \\='backward or \\='forward.
DEPTH is a number that indicates the level of the reference.
Return the new heading or nil."
  (declare (indent 1))
  (if-let* ((marker (plist-get hdata :marker))
            (key (org-backlinks--key marker))
            (heading (gethash key ht)))
      nil
    (setq heading (org-backlinks-heading-new hdata direction depth))
    (puthash key heading ht)
    heading))

(defun org-backlinks--add-headings (hdata-list direction depth ht)
  "Add new headings from HDATA-LIST into the HT hash-table and return them.
Arguments DIRECTION and DEPTH are the same for every new heading
in the list, see `org-backlinks--add-heading' for more details."
  (declare (indent 1))
  (let (result)
    (dolist (hdata hdata-list)
      (when-let* ((heading (org-backlinks--add-heading hdata
                             direction depth ht)))
        (push heading result)))
    (nreverse result)))

(defun org-backlinks--collect-backward (headings depth ht)
  "Collect backlinks for HEADINGS at DEPTH into the HT hash-table.
Return the list of new headings."
  (let* ((ids (delete-dups
               (remove nil (mapcan #'org-backlinks-heading-ids headings))))
         (hdata-list (org-backlinks--query-backlinks ids)))
    (org-backlinks--add-headings hdata-list 'backward depth ht)))

(defun org-backlinks--collect-forward (headings depth ht)
  "Collect forward links for HEADINGS at DEPTH into the HT hash-table.
Return the list of new headings."
  (let* ((markers (delete-dups (mapcar #'org-backlinks-heading-marker
                                       headings)))
         (ids (delete nil (mapcan #'org-backlinks--heading-forward-links
                                  markers)))
         (hdata-list (org-backlinks--query-ids ids)))
    (org-backlinks--add-headings hdata-list 'forward depth ht)))

(cl-defun org-backlinks-collect (hdata &optional (backward 3) (forward 2))
  "Collect the links for HDATA plist and return a cons (ROOT . HASH-TABLE).
BACKWARD is maximum depth for backlinks.
FORWARD is maximum depth for forward links."
  (let* ((ht (make-hash-table :test #'equal))
         (depth 0)
         (root (org-backlinks-heading-new hdata nil depth))
         (h-back (list root))
         (h-fwd (list root)))
    (puthash (org-backlinks--key (org-backlinks-heading-marker root))
             root ht)
    (while (or (and h-back (< depth backward))
               (and h-fwd (< depth forward)))
      (incf depth)
      (when (and h-back (<= depth backward))
        (setq h-back (org-backlinks--collect-backward h-back depth ht)))
      (when (and h-fwd (<= depth forward))
        (setq h-fwd (org-backlinks--collect-forward h-fwd depth ht))))
    (cons root ht)))


;;;;; Lists of headings

(defun org-backlinks--unique (headings)
  "Return the unique elements from HEADINGS."
  (delete
   nil
   (cl-remove-duplicates headings :test #'equal :key #'car :from-end t)))

(defun org-backlinks-build-list (headings-list remove-list &optional h)
  "Return a list from HEADINGS-LIST excluding entries from REMOVE-LIST.
When optional argument H is a heading, also remove it from final list."
  (let ((hlist (cl-set-difference headings-list remove-list :test #'equal)))
    (cl-remove h hlist :test #'equal)))

(defun org-backlinks-parse (headings-list)
  "Return a unique list of headings with links to headings in HEADINGS-LIST."
  (org-backlinks--unique
   (mapcar (lambda (heading)
             (org-backlinks--query-backlinks
              (plist-get (cdr heading) :ids)))
           headings-list)))



;;;; Setup

(defun org-backlinks-find-near-links (heading)
  "Find near links *in* and *to* HEADING. Return `org-backlinks-list'.
Near links are backlinks to HEADING or direct links present in HEADING."
  ;; Backlinks
  (if-let* ((ids (plist-get (cdr heading) :ids)))
      (setq org-backlinks-list
            (org-backlinks-build-list (org-backlinks--query-backlinks ids)
                                      (list heading)))
    (message "Entry has no ID."))
  ;; Direct links
  (setq org-backlinks-direct-list
        (when org-backlinks-show-direct-links
          (org-backlinks--unique
           (org-backlinks--query-ids
            (org-backlinks--heading-forward-links heading)))))
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
           (org-backlinks--unique
            (org-backlinks--query-ids
             (mapcan #'org-backlinks--heading-forward-links
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
        (let ((heading (org-backlinks--heading-data)))
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
  "Select Org headings with `completing-read'."
  (interactive)
  (org-backlinks-setup)
  (when-let* ((link-list (org-backlinks-list-all)))
    (let ((heading (completing-read "Go to heading: " link-list)))
      (org-backlinks-goto-heading (cdr (assoc heading link-list))))))


;;;; View

(defvar org-backlinks-view-buffer "*org-backlinks-view*")

(defun org-backlinks-view--markers (headings)
  "Return the markers from the list of HEADINGS."
  (delete nil
          (cl-remove-duplicates
           (mapcar (lambda (e)
                     (let ((m (plist-get (cdr e) :marker)))
                       (and (markerp m) m)))
                   headings)
           :test #'equal)))

(defun org-backlinks-view--buffers (headings)
  "Return the buffers from the list of HEADINGS."
  (delete nil
          (cl-remove-duplicates
           (mapcar #'marker-buffer
                   (org-backlinks-view--markers headings))
           :test #'equal)))

(org-ql-defpred org-backlinks-view-marker-p (&rest markers)
  "Return non-nil if current heading's marker is in MARKERS"
  :body (let ((e (org-element-at-point-no-context))
              (buf (current-buffer)))
          (cl-some (lambda (m)
                     (and (eq (marker-buffer m) buf)
                          (= (marker-position m)
                             (org-element-property :begin e))))
                   markers)))

(defun org-backlinks-view--marker-member (marker headings-list)
  (member marker (org-backlinks-view--markers headings-list)))

(org-super-agenda--def-auto-group near-links "near links"
  :key-form (and (org-backlinks-view--marker-member
                  (org-super-agenda--get-marker item)
                  (append org-backlinks-list
                          org-backlinks-direct-list))
                 t)
  :header-form "Near links")

(org-super-agenda--def-auto-group distant-links "distant links"
  :key-form (and (org-backlinks-view--marker-member
                  (org-super-agenda--get-marker item)
                  (append org-backlinks-second-list
                          org-backlinks-third-list
                          org-backlinks-indirect-list))
                 t)
  :header-form "Distant links")

(org-super-agenda--def-auto-group backlinks "backlinks"
  :key-form (let ((m (org-super-agenda--get-marker item)))
              (cond ((org-backlinks-view--marker-member
                      m org-backlinks-list)
                     "Back")
                    ((org-backlinks-view--marker-member
                      m org-backlinks-second-list)
                     "2nd order back")
                    ((org-backlinks-view--marker-member
                      m org-backlinks-third-list)
                     "3rd order back")
                    ((org-backlinks-view--marker-member
                      m org-backlinks-direct-list)
                     "Direct ")
                    ((org-backlinks-view--marker-member
                      m org-backlinks-indirect-list)
                     "Indirect ")
                    (t "Other ")))
  :header-form (concat key "links"))

;;;###autoload
(defun org-backlinks-view ()
  "View backlinks and direct links for current Org heading ."
  (interactive nil org-mode)
  (let ((inhibit-message t)
        (message-log-max nil))
    (org-backlinks-setup))
  (if-let* ((headings (org-backlinks-list-all))
            (buffers (org-backlinks-view--buffers headings))
            (markers (org-backlinks-view--markers headings))
            (buf (get-buffer-create org-backlinks-view-buffer)))
      (let ((org-ql-default-predicate 'heading))
        (org-ql-search
          buffers
          `(org-backlinks-view-marker-p ,@markers)
          :title "org-backlinks view"
          :buffer org-backlinks-view-buffer
          :super-groups org-backlinks-view-groups))
    (user-error "No links were found")))


;;; Provide

(provide 'org-backlinks)

;;; org-backlinks.el ends here
