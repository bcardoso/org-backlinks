;;; helm-org-backlinks.el --- Helm interface for Org backlinks -*- lexical-binding: t -*-

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

;; Helm interface for Org backlinks.


;;; Code:

(require 'helm)
(require 'org-backlinks)


;;;; Variables

(defvar helm-org-backlinks-source nil
  "Helm source for Org backlinks.")

(defvar helm-org-backlinks-second-order-source nil
  "Helm source for second order Org backlinks.")

(defvar helm-org-backlinks-third-order-source nil
  "Helm source for third order Org backlinks.")

(defvar helm-org-backlinks-direct-source nil
  "Helm source for the direct links from current heading.")

(defvar helm-org-backlinks-indirect-source nil
  "Helm source for the indirect links from current heading.")


;;;; Functions

(defmacro helm-org-backlinks-build-source (switch source name candidates)
  "Macro for building `helm-org-backlinks' sources."
  `(if ,switch
       (setq ,source
             (if ,candidates
                 (helm-build-sync-source ,name
                   :action '(("Go to heading" . org-backlinks-goto-heading))
                   :candidates ,candidates)))
     (setq ,source nil)))

(defun helm-org-backlinks-build-sources ()
  "Build Helm sources for Org backlinks."
  (helm-org-backlinks-build-source t
                                   helm-org-backlinks-source
                                   "Backlinks"
                                   org-backlinks-list)

  (helm-org-backlinks-build-source org-backlinks-show-second-order-backlinks
                                   helm-org-backlinks-second-order-source
                                   "2nd order Backlinks"
                                   org-backlinks-second-list)

  (helm-org-backlinks-build-source org-backlinks-show-third-order-backlinks
                                   helm-org-backlinks-third-order-source
                                   "3rd order Backlinks"
                                   org-backlinks-third-list)

  (helm-org-backlinks-build-source org-backlinks-show-direct-links
                                   helm-org-backlinks-direct-source
                                   "Direct links"
                                   org-backlinks-direct-list)

  (helm-org-backlinks-build-source org-backlinks-show-direct-links
                                   helm-org-backlinks-indirect-source
                                   "Indirect links"
                                   org-backlinks-indirect-list))

;;;###autoload
(defun helm-org-backlinks ()
  "Helm interface for `org-backlinks'."
  (interactive)
  (org-backlinks-setup)
  (when (org-backlinks-all-list)
    (helm-org-backlinks-build-sources)
    (helm :prompt "Go to heading: "
          :truncate-lines nil
          :sources `(helm-org-backlinks-source
                     ,(if org-backlinks-show-second-order-backlinks
                          helm-org-backlinks-second-order-source)
                     ,(if org-backlinks-show-third-order-backlinks
                          helm-org-backlinks-third-order-source)
                     ,(if org-backlinks-show-direct-links
                          helm-org-backlinks-direct-source)
                     ,(if org-backlinks-show-direct-links
                          helm-org-backlinks-indirect-source)))))


(provide 'helm-org-backlinks)

;;; helm-org-backlinks.el ends here
