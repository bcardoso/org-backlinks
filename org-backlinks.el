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

(require 'org-element)
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

(defun org-backlinks--heading-ids (&optional epom)
  "Return an alist of `org-backlinks-link-types' for EPOM."
  (when-let* ((heading (org-element-at-point-no-context epom)))
    (delete nil
            (mapcar
             (lambda (type-prop)
               (when-let* ((prop (cdr type-prop))
                           (value (org-element-property
                                   (intern (concat ":" (upcase prop)))
                                   heading)))
                 `(,prop . ,value)))
             org-backlinks-link-types))))

(defun org-backlinks--heading-data (&optional epom)
  "Return the plist data for EPOM."
  (when-let* ((heading (org-element-at-point-no-context epom))
              (marker (point-marker)))
    `( :marker ,marker
       :ids ,(copy-tree (org-backlinks--heading-ids heading))
       :olp ,(org-get-outline-path t t)
       :tags ,(org-element-property :tags heading))))

(cl-defstruct (org-backlinks-heading
               (:constructor org-backlinks-heading--make)
               (:copier nil))
  marker ids olp tags direction depth)

;; TODO 2026-11-14: maybe rename to org-backlinks-heading-make
(defun org-backlinks-heading-new (hdata &optional direction depth)
  "Return a new heading from HDATA plist, with DIRECTION and DEPTH."
  (when hdata
    (let ((marker (plist-get hdata :marker))
          (tags (plist-get hdata :tags))
          (olp (plist-get hdata :olp)))
      (org-backlinks-heading--make
       :marker marker
       :ids (copy-tree (plist-get hdata :ids))
       :olp olp
       :tags tags
       :direction direction
       :depth depth))))

(defun org-backlinks-get-current-heading-data ()
  "Return the `org-backlinks--heading-data' for the current Org heading."
  (if (and (derived-mode-p 'org-mode)
           (not (org-before-first-heading-p)))
      (save-excursion
        (org-back-to-heading t)
        (org-backlinks--heading-data))
    (user-error "No Org heading at point")))


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
                      `(,prop . ,(string-remove-prefix "#" custom-id)))
                     ((not (equal type "file"))
                      `(,prop . ,(plist-get plist :path))))))))))))


;;;;; Collect links relative to a heading

(defun org-backlinks--add-heading (hdata direction depth ht)
  "Add a new heading, created from the HDATA plist, into the HT hash-table.
DIRECTION is a symbol, either \\='backward or \\='forward.
DEPTH is a number that indicates the level of the reference.
Return the new heading or nil."
  (if-let* ((key (plist-get hdata :marker))
            (heading (gethash key ht)))
      nil
    (setq heading (org-backlinks-heading-new hdata direction depth))
    (puthash key heading ht)
    heading))

(defun org-backlinks--remove-heading (heading ht)
  "Remove HEADING from the HT hash-table."
  (remhash (org-backlinks-heading-marker heading) ht))

(defun org-backlinks--add-headings (hdata-list direction depth ht)
  "Add new headings from HDATA-LIST into the HT hash-table and return them.
Arguments DIRECTION and DEPTH are the same for every new heading
in the list, see `org-backlinks--add-heading' for more details."
  (let (result)
    (dolist (hdata hdata-list)
      (when-let* ((h (org-backlinks--add-heading hdata direction depth ht)))
        (push h result)))
    (nreverse result)))

(defun org-backlinks--collect-backward (headings depth ht)
  "Collect backlinks for HEADINGS at DEPTH into the HT hash-table.
Return the list of new headings."
  (let* ((ids (delete-dups (mapcan #'org-backlinks-heading-ids headings)))
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

(defvar org-backlinks-collect nil
  "Hash table from the last `org-backlinks--collect' call.")

(cl-defun org-backlinks--collect (hdata &optional (backward 3) (forward 2))
  "Collect the links for HDATA plist a hash table and return it.
BACKWARD is maximum depth for backlinks.
FORWARD is maximum depth for forward links."
  (declare (indent 1))
  (let* ((ht (make-hash-table :test #'equal :weakness 'key))
         (depth 0)
         (root (org-backlinks--add-heading hdata nil depth ht))
         (key (org-backlinks-heading-marker root))
         (h-back (list root))
         (h-fwd (list root)))
    (while (or (and h-back (< depth backward))
               (and h-fwd (< depth forward)))
      (incf depth)
      (when (and h-back (<= depth backward))
        (setq h-back (org-backlinks--collect-backward h-back depth ht)))
      (when (and h-fwd (<= depth forward))
        (setq h-fwd (org-backlinks--collect-forward h-fwd depth ht))))
    (org-backlinks--remove-heading root ht)
    (setq org-backlinks-collect ht)))

(defun org-backlinks--filter (ht depth &optional direction)
  "Filter HT by DEPTH and DIRECTION."
  (when (hash-table-p ht)
    (seq-filter (lambda (h)
                  (and (eq (org-backlinks-heading-depth h) depth)
                       (or (eq (org-backlinks-heading-direction h) direction)
                           (not direction))))
                (hash-table-values ht))))


;;;; Completion interface

(defun org-backlinks--format-group (direction depth)
  "Return the group indicator string for DIRECTION and DEPTH."
  (format "%s (L%s)"
          (if (eq direction 'backward) "Backlinks" "Forward links")
          depth))

(defun org-backlinks--format (heading)
  "Return the candidate string for HEADING."
  (let* ((marker (org-backlinks-heading-marker heading))
         (tags (org-backlinks-heading-tags heading))
         (group (org-backlinks--format-group
                 (org-backlinks-heading-direction heading)
                 (org-backlinks-heading-depth heading)))
         (candidate
          (concat (org-format-outline-path
                   (org-backlinks-heading-olp heading)
                   org-backlinks-width
                   (propertize (format "%s:" (marker-buffer marker))
                               'face 'org-backlinks-file-face))
                  (when tags
                    (concat " " (propertize (org-make-tag-string tags)
                                            'face 'org-tag))))))
    (put-text-property 0 1 'org-marker marker candidate)
    (put-text-property 0 1 'org-backlinks-group group candidate)
    candidate))

(defun org-backlinks--group-function (candidate transform)
  "Return the group title for CANDIDATE or TRANSFORM it."
  (if transform candidate
    (get-text-property 0 'org-backlinks-group candidate)))

(defun org-backlinks--display-sort (candidates)
  "Sort CANDIDATES by group."
  (sort candidates
        :key (lambda (s) (get-text-property 0 'org-backlinks-group s))))

(defun org-backlinks--completion-table (candidates)
  "Create a completion table for CANDIDATES with group metadata."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . org-heading)
                   (group-function . org-backlinks--group-function)
                   (display-sort-function . org-backlinks--display-sort))
      (complete-with-action action candidates string pred))))

(defun org-backlinks--candidates (ht)
  "Return the list of heading candidates from HT."
  (mapcar #'org-backlinks--format (hash-table-values ht)))

(defun org-backlinks--depth (&optional read-number)
  "Return the depth values from `org-backlinks-depth' or READ-NUMBER."
  (let* ((bl-depth (car org-backlinks-depth))
         (fl-depth (cdr org-backlinks-depth)))
    (cons (if read-number
              (read-number "Backlinks depth: "  bl-depth)
            (or bl-depth 3))
          (if read-number
              (read-number "Forward links depth: "  fl-depth)
            (or fl-depth 2)))))

(defun org-backlinks-goto-marker (marker)
  "Go to MARKER in other window."
  (org-mark-ring-push)
  (switch-to-buffer-other-window (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-fold-show-entry)
  (org-fold-show-children)
  (recenter org-backlinks-recenter))

;;;###autoload
(defun org-backlinks (&optional arg)
  "Select Org headings with `completing-read'.
With ARG, prompt for the depth of backlinks and forward links."
  (interactive "P" org-mode)
  (if-let* ((minibuffer-allow-text-properties t)
            (hdata (org-backlinks-get-current-heading-data))
            (depth (org-backlinks--depth arg))
            (ht (org-backlinks--collect hdata (car depth) (cdr depth)))
            (candidates (org-backlinks--candidates ht))
            (candidate (completing-read
                        "Go to heading: "
                        (org-backlinks--completion-table candidates) nil t)))
      (org-backlinks-goto-marker (get-text-property 0 'org-marker candidate))
    ;; FIXME 2026-11-14: this message shows only when there are no
    ;; backwards nor forward links; but a heading might have only forward
    ;; links and no backlinks, or no ID property.
    (message "There are no links to this heading.")))

;; TODO 2026-11-13: if no :ids -- (message "Entry has no ID.")

;; TODO 2026-11-14: alternative to (defun org-backlinks-setup-simple (&rest _))


;;;; View

(defvar org-backlinks-view-buffer "*org-backlinks-view*")

(org-ql-defpred org-backlinks-view-marker-p (&rest markers)
  "Return non-nil if a heading's marker is in MARKERS"
  :body (let* ((e (org-element-at-point))
               (buf (current-buffer))
               (pos (org-element-property :begin e)))
          (cl-some (lambda (m)
                     (and (eq (marker-buffer m) buf)
                          (eq (marker-position m) pos)))
                   markers)))

(org-super-agenda--def-auto-group org-backlinks "backlinks"
  :keyword :org-backlinks
  :key-form (let* ((marker (org-super-agenda--get-marker item))
                   (heading (gethash marker org-backlinks-collect)))
              (org-backlinks--format-group
               (org-backlinks-heading-direction heading)
               (org-backlinks-heading-depth heading))))

;;;###autoload
(defun org-backlinks-view (&optional arg)
  "View backlinks and direct links for current Org heading.
With ARG, prompt for the depth of backlinks and forward links."
  (interactive "P" org-mode)
  (if-let* ((minibuffer-allow-text-properties t)
            (hdata (org-backlinks-get-current-heading-data))
            (depth (org-backlinks--depth arg))
            (ht (org-backlinks--collect hdata (car depth) (cdr depth)))
            (markers (hash-table-keys ht))
            (buffers (delete-dups (mapcar #'marker-buffer markers)))
            (buf (get-buffer-create org-backlinks-view-buffer)))
      (let ((org-ql-default-predicate 'heading))
        (org-ql-search buffers
          `(org-backlinks-view-marker-p ,@markers)
          :title "Org Backlinks View"
          :buffer org-backlinks-view-buffer
          :super-groups '((:org-backlinks t))))
    (user-error "No links were found")))


;;; Provide

(provide 'org-backlinks)

;;; org-backlinks.el ends here
