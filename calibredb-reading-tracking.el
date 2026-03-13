;;; calibredb-reading-tracking.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

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
(require 'calibredb-reading-tracking-epub)

(defcustom crt:time-format "%Y-%m-%d %H:%M"
  "Format string for displaying timestamps.

Passed to `format-time-string' for generating time strings
stored in the database.")

(defun crt:current-time ()
  "Return the current time formatted with `crt:time-format'."
  (format-time-string crt:time-format))

(defun crt:parse-book-buffer ()
  "Parse the current buffer to create a book tracking object.

Extracts the book ID from the directory name (expects pattern like
\"(123)/\" for CalibreDB structure) and creates an appropriate book
object based on file extension.

Currently supports EPUB files. Returns a `crt:book' object or nil
if the file is not in a recognized CalibreDB format."
  (let* ((dir-name (file-name-directory (buffer-file-name)))
         (extension (file-name-extension (buffer-file-name)))
         (id))
    (when (string-match "(\\([0-9]+\\))/$" dir-name)
      (setq id (string-to-number (match-string 1 dir-name)))
      (pcase (intern extension)
        ('epub (crt:book-new (crt:book-epub
                              :uuid id
                              :file (buffer-file-name))))))))

(defun crt:start-log ()
  "Start a new reading session log for the current book.

Creates a new log entry with the current timestamp and page number.
If there's an existing unfinished log, displays a message with the
duration elapsed since it started instead of creating a new log.

Requires the buffer to be a book file in CalibreDB format."
  (interactive)
  (when-let* ((book (or (crt:parse-book-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name)))))
              (tracking (crt:book-tracking book)))
    ;; (print book)
    (let ((latest-log (crt:log-latest (crt:obj-uuid tracking)))
          (log (crt:log
                :tracking-uuid (crt:obj-uuid tracking)
                :started-at (crt:current-time)
                :finished-at nil
                :page-from (crt:book-page book)
                :page-to nil)))
      (if (and latest-log (not (crt:log-finished-at latest-log)))
          ;; Latest log is unfinished.
          (message (format "Your latest log not finished. [Time: %s - %s] [Duration: %s minutes] [Page: %s - %s]"
                           (crt:log-started-at latest-log)
                           (crt:current-time)
                           (crt:duration-min (crt:log-started-at latest-log) (crt:current-time))
                           (crt:log-page-from latest-log)
                           (crt:book-page book)))
        (crt:obj-message (crt:obj-add-or-update log))))))

(defun crt:finish-log ()
  "Finish the current reading session log.

Updates the latest unfinished log with the current timestamp and
page number, then saves it to the database.

Displays a message if there is no unfinished log to finish."
  (interactive)
  (when-let* ((book (or (crt:parse-book-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name)))))
              (tracking (crt:book-tracking book))
              (log (crt:log-latest (crt:obj-uuid tracking))))
    ;; (print book)
    (if (and log (not (crt:log-finished-at log)))
        (progn
          (crt:log-finished-at-writer log (crt:current-time))
          (crt:log-page-to-writer log (crt:book-page book))
          (setq log (crt:obj-add-or-update log))
          (crt:tracking-duration-min-writer
           tracking
           (+ (crt:tracking-duration-min tracking)
              (crt:duration-min
               (crt:log-finished-at log)
               (crt:log-started-at log))))
          (crt:obj-add-or-update tracking)
          (crt:obj-message log))
      ;; There is not a unfinished log.
      (message "There is not an unfinished log."))))

(defun crt:toggle-log ()
  "Toggle between starting and finishing a reading session.

Checks if there's an unfinished log for the current book:
- If yes, calls `crt:finish-log' to complete it
- If no, calls `crt:start-log' to begin a new session

This provides a convenient single command for tracking reading
sessions."
  (interactive)
  (when-let* ((book (or (crt:parse-book-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name)))))
              (tracking (crt:book-tracking book)))
    ;; (print book)
    (let ((latest-log (crt:log-latest (crt:obj-uuid tracking))))
      (if (and latest-log (not (crt:log-finished-at latest-log)))
          ;; Latest log is unfinished.
          (crt:finish-log)
        (crt:start-log)))))

(provide 'calibredb-reading-tracking)
;;; calibredb-reading-tracking.el ends here
