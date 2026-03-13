;;; calibredb-reading-tracking-obj.el ---                         -*- lexical-binding: t; -*-

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
(require 'calibredb-reading-tracking-db)

;;; Classes
(defclass crt:obj ()
  ((uuid :initarg :uuid :initform nil :reader crt:obj-uuid))
  "Base class for all reading tracking objects.

Provides a UUID identifier for database persistence.")

(defclass crt:log (crt:obj)
  ((started-at :initarg :started-at :reader crt:log-started-at)
   (finished-at :initarg :finished-at :reader crt:log-finished-at :writer crt:log-finished-at-writer)
   (page-from :initarg :page-from :reader crt:log-page-from)
   (page-to :initarg :page-to :reader crt:log-page-to :writer crt:log-page-to-writer)
   (tracking-uuid :initarg :tracking-uuid :reader crt:log-tracking-uuid))
  "Represents a single reading session log.

Tracks when a reading session started and finished, and which pages
were read during the session.")

(defclass crt:tracking (crt:obj)
  ((book-id :initarg :book-id :reader crt:tracking-book-id)
   (started-at :initarg :started-at :reader crt:tracking-started-at)
   (finished-at :initarg :finished-at :initform nil :reader crt:tracking-finished-at)
   (page :initarg :page :reader crt:tracking-page :writer crt:tracking-page-writer)
   (total-pages :initarg :total-pages :reader crt:tracking-total-pages :writer crt:tracking-total-pages-writer)
   (status :initarg :status :reader crt:tracking-status)
   (logs :initarg :logs :reader crt:tracking-logs)
   (duration-min :initarg :duration-min :initform 0 :reader crt:tracking-duration-min :writer crt:tracking-duration-min-writer))
  "Represents reading progress tracking for a book.

Tracks overall progress including current page, total pages, status,
and timestamps for when reading started and finished.")

(defclass crt:book (crt:obj)
  ((file :initarg :file :reader crt:book-file :writer crt:book-file-writer)
   (page :initarg :page :reader crt:book-page :writer crt:book-page-writer)
   (total-pages :initarg :total-pages :reader crt:book-total-pages :writer crt:book-total-pages-writer)
   (tracking :initarg :tracking :reader crt:book-tracking :writer crt:book-tracking-writer))
  "Represents a book being tracked.

Contains file path, current page information, and associated tracking data.")

(cl-defmethod crt:obj-build-one ((obj crt:obj) db-data)
  "Build OBJ from DB-DATA retrieved from database.

DB-DATA should be a plist with :columns (list of slot names) and
:row (list of values). Sets each slot on OBJ from the corresponding
row value and returns the modified OBJ."
  (when-let ((columns (plist-get db-data :columns))
             (row (plist-get db-data :row)))
    (dolist (idx (number-sequence 0 (1- (length columns))))
      (eieio-oset obj (nth idx columns) (nth idx row)))
    obj))

(defun crt:obj-build-list (class-func db-data)
  "Build a list of CLASS-FUNC objects from DB-DATA.

CLASS-FUNC is a constructor function for the object class.
DB-DATA should be a plist with :columns (list of slot names) and
:rows (list of row values). Returns a list of instantiated objects
with slots populated from database rows."
  (when-let ((columns (plist-get db-data :columns))
             (rows (plist-get db-data :rows)))
    (mapcar
     (lambda (row)
       (let ((obj (funcall class-func)))
         (dolist (idx (number-sequence 0 (1- (length columns))))
           (eieio-oset obj (nth idx columns) (nth idx row)))
         obj))
     rows)))

(cl-defmethod crt:obj-add-or-update ((obj crt:log))
  "Save or update LOG object to the database.

Inserts a new log entry or updates an existing one based on UUID.
Returns the LOG object with database-assigned values."
  (crt:obj-build-one obj
   (crt:db-insert-or-update-log
    :uuid (crt:obj-uuid obj)
    :started-at (crt:log-started-at obj)
    :finished-at (crt:log-finished-at obj)
    :page-from (crt:log-page-from obj)
    :page-to (crt:log-page-to obj)
    :tracking-uuid (crt:log-tracking-uuid obj))))

(cl-defmethod crt:obj-add-or-update ((obj crt:tracking))
  "Save or update TRACKING object to the database.

Inserts a new tracking entry or updates an existing one based on UUID.
Returns the TRACKING object with database-assigned values."
  (crt:obj-build-one obj
   (crt:db-insert-or-update-tracking
    :uuid (crt:obj-uuid obj)
    :started-at (crt:tracking-started-at obj)
    :finished-at (crt:tracking-finished-at obj)
    :status (crt:tracking-status obj)
    :page (crt:tracking-page obj)
    :total-pages (crt:tracking-total-pages obj)
    :book-id (crt:tracking-book-id obj)
    :duration-min (crt:tracking-duration-min obj))))

(cl-defun crt:tracking-get (&key uuid book-id)
  "Get tracking by UUID or BOOK-ID.

Returns a `crt:tracking' object populated from the database, or nil
if not found."
  (let ((columns (remove 'logs (crt:class-properties crt:tracking))))
    (crt:obj-build-one
     (crt:tracking)
     (crt:db-get-tracking :uuid uuid :book-id book-id :columns columns))))

(defun crt:log-latest (tracking-uuid)
  "Get the most recent log for TRACKING-UUID.

Returns a `crt:log' object for the latest reading session, or nil
if no logs exist."
  (when-let ((columns (crt:class-properties crt:log)))
    (crt:obj-build-one
     (crt:log)
     (crt:db-latest-log tracking-uuid columns))))

(cl-defmethod crt:obj-message ((obj crt:log))
  "Display LOG object information as a message.

Shows start/finish time, duration in minutes, and page range.
Different output depending on whether the log is finished or ongoing."
  (let ((started-at (crt:log-started-at obj))
        (finished-at (crt:log-finished-at obj))
        (page-to (crt:log-page-to obj))
        (page-from (crt:log-page-from obj)))
    (if finished-at
        (print (format "Finish a log. [Time: %s - %s] [Duration: %s minutes] [Page: %s - %s]"
                       started-at finished-at
                       (crt:duration-min started-at finished-at)
                       page-from page-to))
      (print (format "Start a new log. [Time: %s] [Page: %s]" started-at page-from)))))

(defun crt:trackings ()
  "Get all reading tracking records.

Returns a list of `crt:tracking' objects for all books being tracked,
ordered by started-at descending (most recent first)."
  (let ((columns (remove 'logs (crt:class-properties crt:tracking))))
    (crt:obj-build-list
     #'crt:tracking
     (crt:db-trackings columns))))

(defun crt:logs (tracking-uuid)
  "Get all reading logs for TRACKING-UUID.

Returns a list of `crt:log' objects for the specified tracking record,
ordered by started-at descending (most recent first)."
  (let ((columns (crt:class-properties crt:log)))
    (crt:obj-build-list
     #'crt:log
     (crt:db-logs tracking-uuid columns))))

(provide 'calibredb-reading-tracking-obj)
;;; calibredb-reading-tracking-obj.el ends here
