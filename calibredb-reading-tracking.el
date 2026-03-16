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
(require 'calibredb-reading-tracking-ctable)
(require 'calibredb-reading-tracking-db)

(defcustom crt:time-format "%Y-%m-%d %H:%M"
  "Format string for displaying timestamps.

Passed to `format-time-string' for generating time strings
stored in the database.")

;;; API Functions
(cl-defmethod crt:add-or-update ((entity crt:entity))
  (when (< 0 (caar (crt:db-run-sql (list (crt:entity-insert-or-update-sql entity)
                                         (crt:db-total-changes-sql)))))
    entity))

(cl-defmethod crt:query ((entity crt:entity))
  (let* ((result))
    (setq result (crt:db-run-sql (list (crt:entity-query-sql entity))))
    ;; (print result)
    (crt:entity-build-list entity result)))

(defun crt:get-log (tracking-uuid)
  (let* ((log (crt:entity-substitute-columns
               (crt:entity-log)
               (list (crt:column-tracking-uuid
                      :where '=
                      :value tracking-uuid))))
         (car (crt:query log)))))

(defun crt:parse-buffer ()
  ""
  (let* ((parser (crt:parser-build))
         (page-info (crt:parse parser))
         (tracking (crt:entity-substitute-columns
                    (crt:entity-tracking)
                    (list (crt:column-book-id
                           :value (plist-get page-info :id)
                           :where '=))))
         (exist-trackings (crt:query tracking)))
    ;; (print (plist-get page-info :id))
    (setq tracking (or (car exist-trackings) tracking))
    (crt:entity-substitute-columns
     tracking
     (list (crt:column-page :value (plist-get page-info :page))
           (crt:column-total-pages :value (plist-get page-info :total-pages))))
    ;; (print tracking)
    (crt:add-or-update tracking)))

(defun crt:start-log (&optional tracking exist-log)
  "Start a new reading session log for the current book.

Creates a new log entry with the current timestamp and page number.
If there's an existing unfinished log, displays a message with the
duration elapsed since it started instead of creating a new log.

Requires the buffer to be a book file in CalibreDB format."
  (interactive)
  (when-let ((tracking (or tracking (crt:parse-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name))))))
    (let* ((log (crt:entity-substitute-columns
                 (crt:entity-log)
                 (list (crt:column-tracking-uuid
                        :where '=
                        :value (crt:entity-column-value tracking crt:column-uuid)))))
           (exist-log (or exist-log (car (crt:query log)))))
      (if (and exist-log (not (crt:entity-column-value exist-log crt:column-finished-at)))
          ;; Latest log is unfinished.
          (message (format "Your latest log not finished. %s" (crt:entity-message exist-log)))
        (message
         (format "Starting a new log: %s"
                 (crt:entity-message
                  (crt:add-or-update
                   (crt:entity-substitute-columns
                    log
                    (list (crt:column-page-from
                           :value (crt:entity-column-value tracking crt:column-page))))))))))))

(defun crt:finish-log (&optional tracking exist-log)
  "Finish the current reading session log.

Updates the latest unfinished log with the current timestamp and
page number, then saves it to the database.

Displays a message if there is no unfinished log to finish."
  (interactive)
  (when-let ((tracking (or tracking (crt:parse-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name))))))
    (let* ((log (crt:entity-substitute-columns
                 (crt:entity-log)
                 (list (crt:column-tracking-uuid
                        :where '=
                        :value (crt:entity-column-value tracking crt:column-uuid)))))
           (exist-log (or exist-log (car (crt:query log)))))
      (if (and exist-log (not (crt:entity-column-value exist-log crt:column-finished-at)))
          (if (< (- (crt:current-time) (crt:entity-column-value exist-log crt:column-started-at)) 60)
              (message (format "Your reading duration is under 1 minute[%ss] and cannot be finished: %s"
                               (- (crt:current-time) (crt:entity-column-value exist-log crt:column-started-at))
                               (crt:entity-message exist-log)))
            (message (format "Finished a log: %s"
                             (crt:entity-message
                              (crt:add-or-update
                               (crt:entity-substitute-columns
                                exist-log
                                (list (crt:column-page-to :value (crt:entity-column-value tracking crt:column-page))
                                      (crt:column-finished-at :value (crt:current-time)))))))))
        (message "There is not an unfinished log.")))))

(defun crt:toggle-log ()
  "Toggle between starting and finishing a reading session.

Checks if there's an unfinished log for the current book:
- If yes, calls `crt:finish-log' to complete it
- If no, calls `crt:start-log' to begin a new session

This provides a convenient single command for tracking reading
sessions."
  (interactive)
  (when-let ((tracking (or (crt:parse-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name))))))
    (let* ((log (crt:entity-substitute-columns
                  (crt:entity-log)
                  (list (crt:column-tracking-uuid
                         :where '=
                         :value (crt:entity-column-value tracking crt:column-uuid)))))
           (exist-log (car (crt:query log))))
      (if (and exist-log (not (crt:entity-column-value exist-log crt:column-finished-at)))
          ;; Latest log is unfinished.
          (crt:finish-log tracking exist-log)
        (crt:start-log tracking exist-log)))))

(defun crt:list-tracking ()
  ""
  (interactive)
  (crt:ctable-render-list (crt:query (crt:entity-tracking))))

(defun crt:list-logs ()
  ""
  (interactive)
  (when-let* ((tracking (or (crt:parse-buffer) (crt:message-return-nil (format "This book file is not in calibredb. [%s]" (buffer-file-name))))))
    (crt:ctable-render-list
     (crt:query
      (crt:entity-substitute-columns
       (crt:entity-log)
       (list (crt:column-tracking-uuid
              :where '=
              :value (crt:entity-column-value tracking crt:column-uuid))))))))

(crt:query (crt:entity-tracking))

(defun crt:init ()
  "Initialize the reading tracking database.

Creates the necessary SQLite tables (`reading-tracking' and
`reading-logs') if they do not exist. Run this once before using
the reading tracking features."
  (interactive)
  (crt:db-init-tables))

(provide 'calibredb-reading-tracking)
;;; calibredb-reading-tracking.el ends here
