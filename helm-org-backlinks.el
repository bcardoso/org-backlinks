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

;; Helm interface for Org backlinks


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


;;;; Functions

(defun helm-org-backlinks-build-sources ()
  "Build the Helm sources for Org backlinks."
  (if org-backlinks-list
      (setq helm-org-backlinks-source
            (helm-build-sync-source "Backlinks"
              :action '(("Go to" . org-backlinks-goto-heading))
              :candidates org-backlinks-list))
    (setq helm-org-backlinks-source nil))

  (if (not org-backlinks-show-second-order-backlinks)
      (setq helm-org-backlinks-second-order-source nil))
    (if (not org-backlinks-second-list)
        (setq helm-org-backlinks-second-order-source nil)
      (setq helm-org-backlinks-second-order-source
            (helm-build-sync-source "2nd order Backlinks"
              :action '(("Go to" . org-backlinks-goto-heading))
              :candidates org-backlinks-second-list))

      (if (not org-backlinks-show-third-order-backlinks)
          (setq helm-org-backlinks-third-order-source nil)
        (if (not org-backlinks-third-list)
            (setq helm-org-backlinks-third-order-source nil)
          (setq helm-org-backlinks-third-order-source
                (helm-build-sync-source "3rd order Backlinks"
                  :action '(("Go to" . org-backlinks-goto-heading))
                  :candidates org-backlinks-third-list))))))


;;;###autoload
(defun helm-org-backlinks ()
  (interactive)
  (let ((id (org-backlinks-get-heading-id)))
    (if (not id)
        (message "Entry has no ID.")
      (org-backlinks-parse id)
      (helm-org-backlinks-build-sources)
      (if (not helm-org-backlinks-source)
          (message "There are no links to this entry.")
        (helm :prompt "Go to heading: "
              :sources `(helm-org-backlinks-source
                         ,(if org-backlinks-show-second-order-backlinks
                              helm-org-backlinks-second-order-source)
                         ,(if org-backlinks-show-third-order-backlinks
                              helm-org-backlinks-third-order-source)))))))


(provide 'helm-org-backlinks)

;;; helm-org-backlinks.el ends here
