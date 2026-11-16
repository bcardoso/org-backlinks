;;; org-backlinks.el --- Org backlinks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/org-backlinks
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (org "9.7") (org-ql "0.8") (org-super-agenda "1.3"))

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

(defcustom org-backlinks-scope 'org-files-list
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
  '(("id" "ID")
    ("file" "CUSTOM_ID" "#")
    ("cite" "CUSTOM_ID" "@"))
  "List of link specs used to search and extract heading ids.
Each element is a list of (LINK-TYPE PROPERTY PREFIX):

  LINK-TYPE  an Org link type, as in `org-link-parameters';
  PROPERTY   the Org heading property that stores the id, used to
             query forward links;
  PREFIX     nil, or the string that precedes the id in a search,
             used to query backlinks.

Additional link types can be added, as long as their links depend on
unique values of a heading property. Note that PREFIX is matched
literally. Each spec should define a distinct PROPERTY."
  :type '(repeat (list (string :tag "Link type")
                       (string :tag "Heading property")
                       (choice :tag "Search prefix"
                               (string :tag "Prefix")
                               (const :tag "None" nil)))))

(defcustom org-backlinks-width 78
  "Maximum number of characters available for the Org heading path."
  :type 'natnum)

(defcustom org-backlinks-switch-to-buffer-fn #'switch-to-buffer-other-window
  "Switch to buffer function used by `org-backlinks-goto'."
  :type 'function)

(defcustom org-backlinks-recenter nil
  "If nil, center point in selected window and maybe redisplay frame.
With a numeric value, recenter putting point on screen line
relative to the selected window. See `recenter'."
  :type '(choice (const nil) integer))

(defcustom org-backlinks-view-buffer "*org-backlinks-view*"
  "Name of the `org-backlinks-view' buffer."
  :type 'string)

(defface org-backlinks-file-face
  '((t (:inherit shadow)))
  "Face for the file name part of the candidate.")


;;;; Helper functions

(defun org-backlinks--unique (fn collection &optional append)
  "Apply FN on COLLECTION and return the result without duplicates.
When APPEND is non-nil, apply `append' before the deduplication."
  (delete nil (delete-dups (if append
                               (apply #'append (mapcar fn collection))
                             (mapcar fn collection)))))

(defun org-backlinks--key (marker)
  "Return a cons of (BUFFER . POS) for MARKER."
  (cons (marker-buffer marker) (marker-position marker)))


;;;; Org files

(defun org-backlinks-files ()
  "List of Org files to search for headings."
  (org-backlinks--unique
   #'file-truename
   (cond ((eq org-backlinks-scope 'agenda)
          (org-agenda-files))
         ((eq org-backlinks-scope 'org-files-list)
          (org-files-list))
         ((eq org-backlinks-scope 'buffers)
          (mapcar #'buffer-file-name (org-buffer-list 'files t)))
         (t
          (ensure-list org-backlinks-scope)))))


;;;; Org heading data

(defun org-backlinks--heading-ids (heading)
  "Return the alist of (PROPERTY . VALUE) links of the Org HEADING element.
PROPERTY must be defined in `org-backlinks-link-types', which see."
  (delq nil (mapcar (lambda (prop)
                      (when-let* ((value (org-element-property
                                          (intern (concat ":" (upcase prop)))
                                          heading)))
                        `(,prop . ,value)))
                    (delete-dups (mapcar #'cadr org-backlinks-link-types)))))

(defun org-backlinks--heading-data (&optional pom)
  "Return the plist data of the Org heading at POM or point."
  (when-let* ((heading (org-element-at-point-no-context pom))
              (marker (point-marker)))
    `( :marker ,marker
       :ids ,(org-backlinks--heading-ids heading)
       :olp ,(org-get-outline-path t t)
       :tags ,(org-element-property :tags heading))))

(defun org-backlinks-get-heading-data (&optional noerror)
  "Return the `org-backlinks--heading-data' for the current Org heading.
When NOERROR is non-nil, don't signal an error."
  (if (and (derived-mode-p 'org-mode) (not (org-before-first-heading-p)))
      (save-excursion
        (org-back-to-heading t)
        (org-backlinks--heading-data))
    (unless noerror (user-error "No Org heading at point"))))

(cl-defstruct (org-backlinks-heading
               (:constructor org-backlinks-heading--make)
               (:copier nil))
  marker ids olp tags direction depth group)

(defun org-backlinks--format-group (direction depth)
  "Return the group indicator string for DIRECTION and DEPTH."
  (format "%s (L%s)"
          (if (eq direction 'backward) "Backlinks" "Forward links")
          depth))

(defun org-backlinks-heading-new (hdata direction depth)
  "Return a new heading from HDATA plist, with DIRECTION and DEPTH."
  (when hdata
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
       :group (org-backlinks--format-group direction depth)))))


;;;; Backlinks & Forward links

(defun org-backlinks--search-strings (id-alist)
  "Return the list of search strings for ID-ALIST.
ID-ALIST is an alist of (PROPERTY . VALUE).
PROPERTY must be defined in `org-backlinks-link-types', which see.
The search strings are used by `org-backlinks--query-backlinks'."
  (mapcan (lambda (prop-value)
            (let* ((specs (seq-filter (lambda (s)
                                        (equal (nth 1 s) (car prop-value)))
                                      org-backlinks-link-types)))
              (mapcar (lambda (spec)
                        (format "%s%s"
                                (or (nth 2 spec) (concat (nth 0 spec) ":"))
                                (cdr prop-value)))
                      specs)))
          id-alist))

(defun org-backlinks--query-backlinks (id-alist files)
  "Return the headings' data from FILES that contain links to ID-ALIST.
ID-ALIST is an alist of (PROPERTY . VALUE)."
  (when id-alist
    (let ((strings (org-backlinks--search-strings id-alist)))
      (org-ql-query
        :select #'org-backlinks--heading-data
        :from files
        :where `(or ,@(mapcar (lambda (s) `(rifle ,s)) strings))))))

(defun org-backlinks--query-ids (id-alist files)
  "Return the Org headings' data from FILES whose properties match ID-ALIST.
ID-ALIST is an alist of (PROPERTY . VALUE)."
  (when id-alist
    (org-ql-query
      :select #'org-backlinks--heading-data
      :from files
      :where `(or ,@(mapcar (lambda (id) `(property ,(car id) ,(cdr id)))
                            id-alist)))))

(defun org-backlinks--forward-links (marker)
  "Return the list of (PROPERTY . VALUE) links present in heading at MARKER.
Link types are the ones defined in `org-backlinks-link-types'."
  (org-with-point-at marker
    (save-restriction
      (org-narrow-to-subtree)
      (delq
       nil
       (org-element-map (org-element-parse-buffer)
           '(link citation-reference)
         (lambda (e)
           (when-let* ((spec (assoc (or (org-element-property :type e)
                                        (and (org-element-property :key e)
                                             "cite"))
                                    org-backlinks-link-types))
                       (type (nth 0 spec))
                       (prop (nth 1 spec)))
             (cond ((equal type "file")
                    (let ((option (org-element-property :search-option e)))
                      (when (and option (string-prefix-p "#" option))
                        `(,prop . ,(substring option 1)))))
                   ((equal type "cite")
                    `(,prop . ,(org-element-property :key e)))
                   (t
                    `(,prop . ,(org-element-property :path e)))))))))))


;;;; Collect links relative to a heading

(defun org-backlinks--add-heading (hdata direction depth ht)
  "Add a new heading, created from the HDATA plist, into the HT hash-table.
DIRECTION is a symbol, either \\='backward or \\='forward.
DEPTH is a number that indicates the level of the reference.
Return the new heading or nil."
  (if-let* ((key (org-backlinks--key (plist-get hdata :marker)))
            (heading (gethash key ht)))
      nil
    (setq heading (org-backlinks-heading-new hdata direction depth))
    (puthash key heading ht)
    heading))

(defun org-backlinks--remove-heading (heading ht)
  "Remove HEADING from the HT hash-table."
  (remhash (org-backlinks--key (org-backlinks-heading-marker heading)) ht))

(defun org-backlinks--add-headings (hdata-list direction depth ht)
  "Add new headings from HDATA-LIST into the HT hash-table and return them.
Arguments DIRECTION and DEPTH are the same for every new heading
in the list, see `org-backlinks--add-heading' for more details."
  (let (result)
    (dolist (hdata hdata-list)
      (when-let* ((h (org-backlinks--add-heading hdata direction depth ht)))
        (push h result)))
    (nreverse result)))

(defun org-backlinks--collect-backward (headings depth files ht)
  "Collect backlinks for HEADINGS from FILES at DEPTH into HT hash-table.
Return the list of new headings."
  (let* ((ids (org-backlinks--unique #'org-backlinks-heading-ids headings t))
         (hdata-list (org-backlinks--query-backlinks ids files)))
    (org-backlinks--add-headings hdata-list 'backward depth ht)))

(defun org-backlinks--collect-forward (headings depth files ht)
  "Collect forward links for HEADINGS from FILES at DEPTH into HT hash-table.
Return the list of new headings."
  (let* ((markers (org-backlinks--unique #'org-backlinks-heading-marker
                                         headings))
         (ids (org-backlinks--unique #'org-backlinks--forward-links
                                     markers t))
         (hdata-list (org-backlinks--query-ids ids files)))
    (org-backlinks--add-headings hdata-list 'forward depth ht)))

(cl-defun org-backlinks--collect (hdata backward forward)
  "Collect headings linked to and from the heading described by HDATA.
HDATA is the plist returned by `org-backlinks--heading-data'.
BACKWARD and FORWARD are the maximum depths for backlinks and for
forward links, respectively.

The backward tree is expanded first, up to BACKWARD levels; the forward
tree is then expanded from the root, up to FORWARD levels. Headings
reachable in both directions are therefore collected as backlinks, and
their forward expansion is not followed.

Return the hash-table of headings, keyed by `org-backlinks--key', which
is cons of (BUFFER . POSITION)."
  (let* ((ht (make-hash-table :test #'equal))
         (files (org-backlinks-files))
         (root (org-backlinks--add-heading hdata nil 0 ht)))
    (let ((ref (list root)))
      (dotimes (depth backward)
        (setq ref (and ref (org-backlinks--collect-backward
                            ref (1+ depth) files ht)))))
    (let ((ref (list root)))
      (dotimes (depth forward)
        (setq ref (and ref (org-backlinks--collect-forward
                            ref (1+ depth) files ht)))))
    (org-backlinks--remove-heading root ht)
    ht))


;;;; Completion interface

(defun org-backlinks--format (heading)
  "Return the candidate string for HEADING."
  (let* ((marker (org-backlinks-heading-marker heading))
         (tags (org-backlinks-heading-tags heading))
         (group (org-backlinks-heading-group heading))
         (sort (cons (pcase (org-backlinks-heading-direction heading)
                       ('backward 0) (_ 1))
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
    (put-text-property 0 1 'org-backlinks-sort sort candidate)
    candidate))

(defun org-backlinks--group-function (candidate transform)
  "Return the group title for CANDIDATE or TRANSFORM it."
  (if transform candidate
    (get-text-property 0 'org-backlinks-group candidate)))

(defun org-backlinks--display-sort (candidates)
  "Sort CANDIDATES by group."
  (sort candidates
        :key (lambda (s) (get-text-property 0 'org-backlinks-sort s))))

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
  (let (candidates)
    (maphash (lambda (_ h) (push (org-backlinks--format h) candidates)) ht)
    (nreverse candidates)))

(defun org-backlinks--depth (&optional read-number)
  "Return the depth values from `org-backlinks-depth' or READ-NUMBER."
  (let* ((bl-depth (car org-backlinks-depth))
         (fl-depth (cdr org-backlinks-depth)))
    (cons (if read-number (read-number "Backlinks depth: "  bl-depth)
            bl-depth)
          (if read-number (read-number "Forward links depth: "  fl-depth)
            fl-depth))))

(defun org-backlinks-goto (candidate)
  "Go to CANDIDATE localtion. See `org-backlinks-switch-to-buffer-fn'."
  (if-let* ((marker (if (markerp candidate) candidate
                      (get-text-property 0 'org-marker candidate))))
      (progn
        (org-mark-ring-push)
        (funcall org-backlinks-switch-to-buffer-fn (marker-buffer marker))
        (goto-char (marker-position marker))
        (org-fold-show-entry)
        (org-fold-show-children)
        (recenter org-backlinks-recenter))
    (user-error "No marker found for %S" candidate)))

;;;###autoload
(defun org-backlinks (&optional arg)
  "Select Org headings with `completing-read'.
With ARG, prompt for the depth of backlinks and forward links."
  (interactive "P" org-mode)
  (let ((minibuffer-allow-text-properties t))
    (if-let* ((hdata (org-backlinks-get-heading-data))
              (depth (org-backlinks--depth arg))
              (ht (org-backlinks--collect hdata (car depth) (cdr depth)))
              (candidates (org-backlinks--candidates ht))
              (candidate (completing-read
                          (if (plist-get hdata :ids)
                              "Go to heading: "
                            "Go to heading (no ID; forward links only): ")
                          (org-backlinks--completion-table candidates)
                          nil t)))
        (org-backlinks-goto candidate)
      (message "There are no links to this heading."))))


;;;; Query

(defun org-backlinks--filter (ht &optional direction depth)
  "Filter HT by DIRECTION and DEPTH."
  (when (hash-table-p ht)
    (let (result)
      (maphash (lambda (_ h)
                 (and (or (eq (org-backlinks-heading-direction h) direction)
                          (not direction))
                      (or (eq (org-backlinks-heading-depth h) depth)
                          (not depth))
                      (push h result)))
               ht)
      (nreverse result))))

(defmacro org-backlinks--with-orig-buffer (&rest body)
  "Evaluate BODY in the buffer from which the current command was invoked.
When the minibuffer is active, evaluate body in the original buffer.
This allows calling `org-backlinks-query' from a custom `consult'
source, since its `:items' function is called from the minibuffer. When
the minibuffer is not active, use the current buffer instead."
  `(let ((buf (if (minibuffer-window-active-p (minibuffer-window))
                  (window-buffer (minibuffer-selected-window))
                (current-buffer))))
     (with-current-buffer buf ,@body)))

;;;###autoload
(defun org-backlinks-query (&optional hdata direction depth formatted)
  "Return the list of headings relative to the current heading or HDATA.
HDATA is the heading plist returned by `org-backlinks-get-heading-data'.
When DIRECTION is \\='backward or \\='forward, return only the headings
from that direction; when nil, return headings from both directions.
When DEPTH is a number, return only the headings at that depth.
When FORMATTED is non-nil, format headings with `org-backlinks--format'."
  (unless (memq direction '(nil backward forward))
    (user-error "Invalid direction: %S" direction))
  (when-let* ((hdata (or hdata
                         (org-backlinks--with-orig-buffer
                          (org-backlinks-get-heading-data 'noerror))))
              (depth (or depth 1))
              (backward (if (or (null direction) (eq direction 'backward))
                            depth 0))
              (forward (if (or (null direction) (eq direction 'forward))
                           depth 0))
              (ht (org-backlinks--collect hdata backward forward))
              (headings (org-backlinks--filter ht direction depth)))
    (if formatted
        (mapcar #'org-backlinks--format headings)
      headings)))


;;;; View

(defvar org-backlinks-view-ht nil
  "Hash table of headings from the last call of `org-backlinks--collect'.")

(org-ql-defpred org-backlinks-view-p (ht)
  "Return non-nil if the heading at point is in hash-table HT-NAME.
HT is keyed by `org-backlinks--key'."
  :body (gethash (cons (current-buffer)
                       (org-element-property
                        :begin (org-element-at-point-no-context)))
                 ht))

(org-super-agenda--def-auto-group org-backlinks "backlinks"
  :keyword :org-backlinks
  :key-form (or (when-let* ((marker (get-text-property 0 'org-marker item))
                            (key (org-backlinks--key marker))
                            (heading (gethash key org-backlinks-view-ht)))
                  (org-backlinks-heading-group heading))
                "Other"))

;;;###autoload
(defun org-backlinks-view (&optional arg)
  "View backlinks and direct links for current Org heading.
With ARG, prompt for the depth of backlinks and forward links."
  (interactive "P" org-mode)
  (if-let* ((hdata (org-backlinks-get-heading-data))
            (depth (org-backlinks--depth arg))
            (ht (org-backlinks--collect hdata (car depth) (cdr depth)))
            (buffers (org-backlinks--unique #'car (hash-table-keys ht))))
      (progn
        (setq org-backlinks-view-ht ht)
        (org-ql-search
          buffers
          `(org-backlinks-view-p ,org-backlinks-view-ht)
          :title (concat "Org Backlinks for "
                         (car (last (plist-get hdata :olp))))
          :buffer org-backlinks-view-buffer
          :super-groups '((:org-backlinks t))))
    (user-error "No links were found")))


;;;; Consult source

(defvar org-backlinks-consult-source
  (list :name      "Backlinks"
        :prompt    "Go to heading: "
        :narrow    ?l
        :category  'org-heading
        :items     (lambda () (org-backlinks-query nil 'backward 1 t))
        :action    #'org-backlinks-goto)
  "Consult source for the level-1 backlinks of the Org heading at point.")


;;; Provide

(provide 'org-backlinks)

;;; org-backlinks.el ends here
