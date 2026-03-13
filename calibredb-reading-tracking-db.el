;;; calibredb-reading-tracking-db.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
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

(require 'calibredb)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'cl-lib)
(require 'calibredb-reading-tracking-utils)

;;; Custom Variables

(defcustom calibredb-reading-tracking-db-limit 100
  "Default limit for database queries.")

;;; Internal Variables

;;; Internal Functions
(defun crt:db--check (name value)
  "Signal an error if VALUE is nil.

NAME is used in the error message to identify the missing argument."
  (unless value
    (error (format "%s must be provided." name))))

(cl-defun crt:db--select-sql-exp (&key columns table clauses offset limit order desc)
  "Build a SELECT SQL expression for EmacSQL.

COLUMNS - list of columns to select (default: *)
TABLE - table name to select from (required)
CLAUSES - WHERE conditions
OFFSET - number of rows to skip (default: 0)
LIMIT - max rows to return (default: `calibredb-reading-tracking-db-limit')
ORDER - column to order by
DESC - if non-nil, order in descending order

Returns a vector suitable for `emacsql'."
  (crt:db--check ":table" table)
  (let ((columns (or columns '(*)))
        (offset (or offset 0))
        (limit (or limit calibredb-reading-tracking-db-limit)))
    (vconcat [:select]
             (list (vconcat columns))
             (list :from table)
             (when clauses (list :where clauses))
             (when order (list :order :by order (when desc :desc)))
             (list :limit (vector offset limit)))))

(cl-defun crt:db--insert-or-update-sql-exp (&key columns table values)
  "Build an INSERT OR UPDATE SQL expression for EmacSQL with upsert.

COLUMNS - list of column names (required)
TABLE - table name to insert into (required)
VALUES - list of value lists corresponding to columns (required)

Uses ON CONFLICT DO UPDATE for upsert behavior on the first column.
Returns a vector suitable for `emacsql'."
  (crt:db--check ":table" table)
  (crt:db--check ":columns" columns)
  (crt:db--check ":values" values)
  (vconcat (vector :insert :into table)
           (list (vconcat columns))
           (list :values (mapcar #'vconcat values))
           (list :on :conflict (vector (intern (format ":%s"(car columns)))))
           (list :do :update :set)
           (list (vconcat (mapcar
                           (lambda (column) (list '= column (intern (format "excluded:%s" column))))
                           (cdr columns))))))

(defun crt:db--create-table-tracking ()
  "Create the `reading-tracking' table if it does not exist.

The table stores reading progress for books with columns:
- uuid: primary key
- book-id: unique reference to books.id
- status, started-at, finished-at, page, total-pages"
  (let ((db (emacsql-sqlite-open calibredb-db-dir)))
    (emacsql db [:create-table
                 :if :not :exists reading-tracking
                 ([(uuid :primary-key) (book-id :unique) status started-at finished-at page total-pages]
                  (:foreign-key [book-id] :references books [id] :on-delete :cascade))])))

(defun crt:db--create-table-logs ()
  "Create the `reading-logs' table if it does not exist.

The table stores individual reading session logs with columns:
- uuid: primary key
- tracking-uuid: foreign key to reading-tracking.uuid
- started-at, finished-at, page-from, page-to"
  (let ((db (emacsql-sqlite-open calibredb-db-dir)))
    (emacsql db [:create-table
                 :if :not :exists reading-logs
                 ([(uuid :primary-key) tracking-uuid started-at finished-at page-from page-to]
                  (:foreign-key [tracking-uuid] :references reading-tracking [uuid] :on-delete :cascade))])))

;;; API Functions
(defun crt:db-init ()
  "Initialize the database schema for reading tracking.

Creates the `reading-tracking' and `reading-logs' tables if they
do not already exist."
  (crt:db--create-table-tracking)
  (crt:db--create-table-logs))

;; (crt:db-init)

(cl-defun crt:db-insert-or-update-log (&key uuid started-at finished-at page-from page-to tracking-uuid)
  "Insert or update a reading log entry.

UUID - unique identifier (generated if not provided)
STARTED-AT - when the reading session started
FINISHED-AT - when the reading session ended (can be nil)
PAGE-FROM - starting page number
PAGE-TO - ending page number
TRACKING-UUID - parent tracking record identifier (required)

Returns a plist with :columns and :row of the saved log."
  (crt:db--check ":tracking-uuid" tracking-uuid)
  (let* ((uuid (or uuid (crt:uuid)))
         (db (emacsql-sqlite-open calibredb-db-dir))
         (columns '(uuid started-at finished-at page-from page-to tracking-uuid))
         (sql-exp (crt:db--insert-or-update-sql-exp
                   :table 'reading-logs
                   :columns columns
                   :values (list (list uuid started-at finished-at page-from page-to tracking-uuid))))
         (select-sql-exp (crt:db--select-sql-exp
                          :table 'reading-logs
                          :columns columns
                          :clauses `(= uuid ,uuid))))
    (emacsql db sql-exp)
    (list :columns columns
          :row (car (emacsql db select-sql-exp)))))

(cl-defun crt:db-insert-or-update-tracking (&key uuid book-id started-at finished-at status page total-pages)
  "Insert or update a reading tracking entry.

UUID - unique identifier (generated if not provided)
BOOK-ID - CalibreDB book identifier (required)
STARTED-AT - when reading started
FINISHED-AT - when reading finished
STATUS - current reading status
PAGE - current page number
TOTAL-PAGES - total pages in the book

Returns a plist with :columns and :row of the saved tracking record."
  (crt:db--check ":book-id" book-id)
  (let* ((uuid (or uuid (crt:uuid)))
         (db (emacsql-sqlite-open calibredb-db-dir))
         (columns '(uuid book-id started-at finished-at status page total-pages))
         (sql-exp (crt:db--insert-or-update-sql-exp
                   :table 'reading-tracking
                   :columns columns
                   :values (list (list uuid book-id started-at finished-at status page total-pages))))
         (select-sql-exp (crt:db--select-sql-exp
                          :table 'reading-tracking
                          :columns columns
                          :clauses `(= uuid ,uuid))))
    (emacsql db sql-exp)
    (crt:db-get-tracking :db db :uuid uuid :book-id book-id :columns columns)))

(cl-defun crt:db-get-tracking (&key db uuid book-id columns)
  "Get a tracking record by UUID or BOOK-ID.

DB - database connection (defaults to opening calibredb-db-dir)
UUID - tracking record UUID
BOOK-ID - CalibreDB book identifier
COLUMNS - list of columns to select

Returns a plist with :columns and :row, or nil if not found."
  (let ((db (or db (emacsql-sqlite-open calibredb-db-dir)))
        (clauses '(= 1 1))
        (sql)
        (row))
    (when uuid
      (setq clauses `(and ,clauses (= uuid ,uuid))))
    (when book-id
      (setq clauses `(and ,clauses (= book-id ,book-id))))
    (setq sql (daily-db--select-sql-exp
               :columns columns
               :table 'reading-tracking
               :clauses clauses
               :limit 1))
    (setq row (car (emacsql db sql)))
    (when row (list :columns columns :row row))))

(cl-defun crt:db-get-log (&key db uuid tracking-uuid columns)
  "Get a log record by UUID or TRACKING-UUID.

DB - database connection (defaults to opening calibredb-db-dir)
UUID - log record UUID
TRACKING-UUID - parent tracking record identifier
COLUMNS - list of columns to select

Returns a plist with :columns and :row, or nil if not found."
  (let ((db (or db (emacsql-sqlite-open calibredb-db-dir)))
        (clauses '(= 1 1))
        (sql)
        (row))
    (when uuid
      (setq clauses `(and ,clauses (= uuid ,uuid))))
    (when tracking-uuid
      (setq clauses `(and ,clauses (= tracking-uuid ,tracking-uuid))))
    (setq sql (daily-db--select-sql-exp
               :columns columns
               :table 'reading-logs
               :clauses clauses
               :limit 1))
    (setq row (car (emacsql db sql)))
    (when row (list :columns columns :row row))))

(defun crt:db-latest-log (tracking-uuid columns)
  "Get the most recent log for TRACKING-UUID.

TRACKING-UUID - parent tracking record identifier
COLUMNS - list of columns to select

Returns a plist with :columns and :row, ordered by started-at descending."
  (let ((db (emacsql-sqlite-open calibredb-db-dir))
        (sql))
    (setq sql (daily-db--select-sql-exp
               :columns columns
               :table 'reading-logs
               :clauses `(= tracking-uuid ,tracking-uuid)
               :limit 1
               :order 'started-at
               :desc t))
    (list :columns columns
          :row (car (emacsql db sql)))))

(provide 'calibredb-reading-tracking-db)
;;; calibredb-reading-tracking-db.el ends here
