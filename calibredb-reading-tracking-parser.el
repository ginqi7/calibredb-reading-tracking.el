;;; calibredb-reading-tracking-parser.el ---         -*- lexical-binding: t; -*-

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

(defcustom crt:epub-page-rexp "<\\([0-9]+\\)/\\([0-9]+\\)> \\([0-9]+\\)/\\([0-9]+\\)"
  "")

;;; Class Definitions
(defclass crt:parser ()
  ((buffer :initarg :buffer :initform (current-buffer))
   (file-path :initarg :file-path :initform (buffer-file-name))))

(defclass crt:epub-parser (crt:parser)
  ())

;;; Internal Functions

(defun crt:epub--page-info (search-function)
  ""
  (save-excursion
    (when (funcall search-function crt:epub-page-rexp nil t)
      (list :page (string-to-number (match-string 1))
            :total-pages (string-to-number (match-string 2))))))

(defun crt:epub--previous-page-info ()
  "Search backward for EPUB page information in the buffer.

Returns a plist with :page and :total-pages."
  (interactive)
  (crt:epub--page-info #'re-search-backward))

(defun crt:epub--next-page-info ()
  "Search forward for EPUB page information in the buffer.

Returns a plist with :page and :total-pages."
  (interactive)
  (crt:epub--page-info #'re-search-forward))

;;; API Functions
(defun crt:parser-build ()
  (let* ((path (buffer-file-name))
         (extension (file-name-extension path)))
    (pcase extension
      ("epub" (crt:epub-parser)))))

(cl-defmethod crt:parse-page-info ((obj crt:epub-parser))
  ""
  (with-current-buffer (eieio-oref obj 'buffer)
    (let ((page-info (or (crt:epub--previous-page-info)
                         (crt:epub--next-page-info))))
      (unless page-info
        (setq page-info (list
                         :page nov-documents-index
                         :total-pages (length nov-documents))))
      page-info)))

(cl-defmethod crt:parse-book-id ((obj crt:parser))
  (let* ((file-path (eieio-oref obj 'file-path))
         (dir-name (file-name-directory file-path))
         (id))
    (when (string-match "(\\([0-9]+\\))/$" dir-name)
     (setq id (string-to-number (match-string 1 dir-name)))
     id)))

(cl-defmethod crt:parse ((obj crt:epub-parser))
  (let* ((book-id (crt:parse-book-id obj))
         (page-info (crt:parse-page-info obj)))
    (plist-put page-info :id book-id)))

(provide 'calibredb-reading-tracking-parser)
;;; calibredb-reading-tracking-parser.el ends here
