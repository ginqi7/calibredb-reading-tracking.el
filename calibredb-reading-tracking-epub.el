;;; calibredb-reading-tracking-epub.el ---           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'calibredb-reading-tracking-obj)
(require 'nov)

(defclass crt:book-epub (crt:book)
  '()
  "EPUB-specific book class.
Extends `crt:book' with EPUB-specific page tracking functionality.
")

;; convert epub file by https://github.com/tthkbw/epub_pager
(defcustom crt:book-epub-page-info-rexp "<\\([0-9]+\\)/\\([0-9]+\\)> \\([0-9]+\\)/\\([0-9]+\\)"
  "Regex pattern to extract page information from EPUB buffer.

Group 1: current page number
Group 2: total pages
Used by `crt:book-epub--page-info' to parse the mode line or buffer.")

(defun crt:book-epub--page-info (search-function)
  "Extract page information using SEARCH-FUNCTION.

SEARCH-FUNCTION should be a function like `re-search-forward' or
`re-search-backward' that searches for `crt:book-epub-page-info-rexp'.

Returns a plist with :page and :total-pages, or nil if not found."
  (save-excursion
    (when (funcall search-function crt:book-epub-page-info-rexp nil t)
      (list :page (string-to-number (match-string 1))
            :total-pages (string-to-number (match-string 2))))))

(defun crt:book-epub--previous-page-info ()
  "Search backward for EPUB page information in the buffer.

Returns a plist with :page and :total-pages."
  (interactive)
  (crt:book-epub--page-info #'re-search-backward))

(defun crt:book-epub--next-page-info ()
  "Search forward for EPUB page information in the buffer.

Returns a plist with :page and :total-pages."
  (interactive)
  (crt:book-epub--page-info #'re-search-forward))

(defun crt:book-epub--total-page ()
  "Get the current and total page count for the EPUB.

First tries to find page info by searching backward, then forward.
If neither finds the pattern, falls back to nov-documents-index and
the length of nov-documents (epub_pager variables).

Returns a plist with :page and :total-pages."
  (let ((page-info (or (crt:book-epub--previous-page-info)
                       (crt:book-epub--next-page-info))))
    (unless page-info
      (setq page-info (list
                       :page nov-documents-index
                       :total-pages (length nov-documents))))
    page-info))

(cl-defmethod crt:book-new ((obj crt:book-epub))
  "Initialize a new EPUB book object for tracking.

Creates or updates a tracking record for the EPUB book:
1. Gets the book-id from the object UUID
2. Extracts current page information from the buffer
3. Creates a new tracking record if one doesn't exist
4. Updates both tracking and book objects with page info
5. Persists to database

Returns the initialized OBJ with tracking information attached."
  (let* ((book-id (crt:obj-uuid obj))
         (page-info (crt:book-epub--total-page))
         (tracking (crt:tracking-get :book-id book-id)))
    ;; If tracking not exists, create one.
    (unless tracking
      (setq tracking (crt:tracking
                      :book-id book-id
                      :status 'reading
                      :started-at (crt:current-time))))
    ;; Update tracking page information.
    (crt:tracking-page-writer tracking (plist-get page-info :page))
    (crt:tracking-total-pages-writer tracking (plist-get page-info :total-pages))
    ;; Save to DB.
    (setq tracking (crt:obj-add-or-update tracking))
    ;; (print tracking)
    ;; Update Book's information.
    (crt:book-page-writer obj (plist-get page-info :page))
    (crt:book-total-pages-writer obj (plist-get page-info :total-pages))
    (crt:book-tracking-writer obj tracking)
    obj))

;; (fmakunbound #'crt:book-new)

(provide 'calibredb-reading-tracking-epub)
;;; calibredb-reading-tracking-epub.el ends here
