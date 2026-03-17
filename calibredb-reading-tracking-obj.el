;;; calibredb-reading-tracking-obj.el ---            -*- lexical-binding: t; -*-

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
(require 'eieio)

(require 'calibredb-reading-tracking-utils)
;;; Class Definitions
(defclass crt:obj ()
  (()))

(defclass crt:entity (crt:obj)
  ((table-name :initform)
   (columns :initform (make-hash-table) :initarg :columns)
   (limit :initform 1000)
   (offset :initform 0)))

(defclass crt:db-column (crt:obj)
  ((column :initarg :column :initform nil :reader crt:db-column-name)
   (type :initarg :type :initform nil :reader crt:db-column-type)
   (reference-table :initarg :reference-table :initform nil :reader crt:db-column-reference-table)
   (reference-column :initarg :reference-column :initform nil :reader crt:db-column-reference-column)
   (virtual :initarg :virtual :initform nil :reader crt:db-column-virtual)
   (not-null :initarg :not-null :initform nil :reader crt:db-column-not-null-p)
   (foreign-key :initarg :foreign-key :initform nil :reader crt:db-column-foreign-key-p)
   (primary-key :initarg :primary-key :initform nil :reader crt:db-column-primary-key-p)
   (unique :initarg :unique :initform nil :reader crt:db-column-unique-p)
   (external :initarg :external :initform nil :reader crt:db-column-external-p)))

(defclass crt:ctable-column (crt:obj)
  ((title :initarg :title :initform nil)
   (align :initarg :align :initform 'left)))

(defclass crt:column (crt:obj)
  ((db-column :initarg :db-column :initform (crt:db-column) :reader crt:column-db-column)
   (ctable-column :initarg :ctable-column :initform nil)
   (selected :initarg :selected :initform t :reader crt:column-selected)
   (where :initarg :where :initform nil :reader crt:column-where)
   (order-by :initarg :order-by :initform nil :reader crt:column-order-by)
   (title :initarg :title :initform nil)
   (value :initarg :value :initform nil)))

(defclass crt:column-uuid (crt:column)
  ((value :initform (crt:uuid))
   (db-column :initform (crt:db-column
                         :column 'uuid
                         :primary-key t))))

(defclass crt:column-started-at (crt:column)
  ((order-by :initform 'desc)
   (ctable-column :initform (crt:ctable-column :title "Started"))
   (value :initform (crt:current-time))
   (db-column :initform (crt:db-column
                         :column 'started-at
                         :unique t
                         :type 'integer))))

(defclass crt:column-finished-at (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Finished"))
   (db-column :initform (crt:db-column
                          :column 'finished-at
                          :type 'integer))))

(defclass crt:column-page-from (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Page From"))
   (value :initform 0)
   (db-column :initform (crt:db-column
                          :column 'page-from
                          :type 'integer))))

(defclass crt:column-page-to (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Page To"))
   (db-column :initform (crt:db-column
                          :column 'page-to
                          :type 'integer))))

(defclass crt:column-tracking-uuid (crt:column)
  ((db-column :initform (crt:db-column
                          :column 'tracking-uuid
                          :type 'integer
                          :not-null t
                          :foreign-key t
                          :reference-table 'reading-tracking
                          :reference-column 'uuid))))

(defclass crt:column-book-id (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Book ID"))
   (db-column :initform (crt:db-column
                          :column 'book-id
                          :type 'integer
                          :not-null t
                          :unique t
                          :foreign-key t
                          :reference-table 'books
                          :reference-column 'id))))

(defclass crt:column-page (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Page"))
   (value :initform 0)
   (db-column :initform (crt:db-column
                         :column 'page
                         :type 'integer))))

(defclass crt:column-total-pages (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Total Pages"))
   (value :initform 0)
   (db-column :initform (crt:db-column
                         :column 'total-pages
                         :type 'integer))))

(defclass crt:column-status (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Status"))
   (value :initform 0)
   (db-column :initform (crt:db-column
                         :column 'status
                         :type 'integer))))

(defclass crt:column-duration (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Duration"))
   (db-column  :initform (crt:db-column
                          :column 'duration
                          :type 'integer
                          :virtual '(- finished-at started-at)))))

(defclass crt:column-book-title (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Title"))
   (db-column :initform (crt:db-column
                         :column '(funcall printf "%s" books:title)
                         :external t))))

(defclass crt:column-book-author (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Author"))
   (db-column :initform (crt:db-column
                         :column '(funcall printf "%s" books:author-sort)
                         :external t))))

(defclass crt:column-logs-count (crt:column)
  ((ctable-column :initform (crt:ctable-column :title "Logs Count"))
   (db-column  :initform (crt:db-column
                          :column '(funcall count reading-logs:uuid)
                          :type 'integer
                          :external t))))

(defclass crt:entity-log (crt:entity)
  ((table-name :initform reading-logs)
   (columns :initform
            (list
             (crt:column-uuid)
             (crt:column-started-at)
             (crt:column-finished-at)
             (crt:column-page-from)
             (crt:column-page-to)
             (crt:column-tracking-uuid)
             (crt:column-duration)))))

(defclass crt:entity-tracking (crt:entity)
  ((table-name :initform reading-tracking)
   (columns :initform (list
                       (crt:column-uuid
                        :db-column
                        (let ((db-column (eieio-oref (crt:column-uuid) 'db-column)))
                          (eieio-oset db-column 'reference-table 'reading-logs)
                          (eieio-oset db-column 'reference-column 'tracking-uuid)
                          db-column))
                       (crt:column-book-id)
                       (crt:column-book-title)
                       (crt:column-book-author)
                       (crt:column-status)
                       (crt:column-page)
                       (crt:column-total-pages)
                       (crt:column-started-at)
                       (crt:column-finished-at)
                       (crt:column-logs-count)
                       (crt:column-duration
                        :db-column
                        (let ((db-column (eieio-oref (crt:column-uuid) 'db-column)))
                          (eieio-oset db-column 'column '(funcall sum reading-logs:duration))
                          (eieio-oset db-column 'external t)
                          db-column))))))

(cl-defmethod crt:column-format ((obj crt:column))
  (when (eieio-oref obj 'value)
    (format "%s" (eieio-oref obj 'value))))

(cl-defmethod crt:column-format ((obj crt:column-duration))
  (when (eieio-oref obj 'value)
    (format "%s" (/ (eieio-oref obj 'value) 60))))

(cl-defmethod crt:column-format ((obj crt:column-started-at))
  (when (eieio-oref obj 'value)
    (format "%s" (crt:format-time (eieio-oref obj 'value)))))

(cl-defmethod crt:column-format ((obj crt:column-finished-at))
  (when (eieio-oref obj 'value)
    (format "%s" (crt:format-time (eieio-oref obj 'value)))))

(cl-defmethod crt:column-format ((obj crt:column-status))
  (when (eieio-oref obj 'value)
    (format "%s" (pcase (eieio-oref obj 'value)
                   (0 "Reading")
                   (1 "Finished")
                   (2 "Aborted")))))

;; API Functions
(cl-defmethod crt:column-db-column-name ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) 'column))

(cl-defmethod crt:column-virtual ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) 'virtual))

(cl-defmethod crt:column-external-p ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) 'external))

(cl-defmethod crt:column-foreign-key-p ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) 'foreign-key))

(cl-defmethod crt:column-primary-key-p ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) 'primary-key))

(cl-defmethod crt:column-reference-table  ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) ':reference-table))

(cl-defmethod crt:column-reference-column  ((obj crt:column))
  (eieio-oref (eieio-oref obj 'db-column) ':reference-column))

(cl-defmethod crt:entity-column-value ((obj crt:entity) column-class)
  (eieio-oref (find-if (lambda (column) (same-class-p column column-class)) (eieio-oref obj 'columns)) 'value))

(cl-defmethod crt:entity-substitute-columns ((obj crt:entity) &optional new-columns)
  (let ((columns (eieio-oref obj 'columns)))
    (dolist (new-column new-columns)
      (setq columns (cl-substitute-if new-column
                                      (lambda (column) (same-class-p column (type-of new-column)))
                                      columns)))
    (eieio-oset obj 'columns columns)
    obj))

(cl-defmethod crt:entity-build ((obj crt:entity) row)
  (let* ((new-obj (clone obj))
         (columns (eieio-oref new-obj 'columns))
         (idx 0))
    (dolist (column columns)
      (when (eieio-oref column 'selected)
        (eieio-oset column 'value (nth idx row))
        (setq idx (1+ idx))))
    (eieio-oset new-obj 'columns columns)
    new-obj))

(cl-defmethod clone ((obj crt:obj) &rest params)
  (let* ((class (eieio-object-class obj))
         (new-obj (funcall (eieio-object-class obj)))
         (slot-names (remove 'nil (mapcar #'eieio-slot-descriptor-name (eieio-class-slots class))))
         (value))
    ;; (print slot-names)
    (dolist (slot-name slot-names)
      (setq value (eieio-oref obj slot-name))
      (cond ((class-p value) (eieio-oset new-obj slot-name (clone value)))
            ((listp value) (eieio-oset new-obj slot-name (mapcar #'clone value)))
            (t (eieio-oset new-obj slot-name value))))
    new-obj))

(cl-defmethod crt:entity-build-list ((obj crt:entity) data)
  (mapcar (lambda (row) (crt:entity-build obj row)) data))

(cl-defmethod crt:entity-print ((obj crt:entity))
  (let ((cls (eieio-class-name (eieio-object-class obj)))
        (columns (eieio-oref obj 'columns)))
    (print (format "Class: %s\n Columns: \n"cls))
    (mapcar (lambda (column) (crt:column-print column)) columns)))

(cl-defmethod crt:column-print ((obj crt:column))
  (print (format "%s: %s" (eieio-class-name (eieio-object-class obj)) (eieio-oref obj 'value))))

(cl-defmethod crt:entity-message ((obj crt:entity))
  (let ((columns (remove nil (remove-if-not (lambda (column)
                                              (and (eieio-oref column 'value)
                                                   (eieio-oref column 'ctable-column)))

                                            (eieio-oref obj 'columns)))))
    (mapcar (lambda (column) (format "[%s: %s]"
                                (eieio-oref (eieio-oref column 'ctable-column) 'title)
                                (crt:column-format column)))
            columns)))

;; (crt:entity-message (crt:entity-tracking))

(provide 'calibredb-reading-tracking-obj)
;;; calibredb-reading-tracking-obj.el ends here
