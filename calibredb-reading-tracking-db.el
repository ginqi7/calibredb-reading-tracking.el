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
(require 'calibredb-reading-tracking-obj)

;;; Internal Functions
(cl-defmethod crt:db--schema ((obj crt:column))
  (let* ((db-column (crt:column-db-column obj))
         (column (crt:db-column-name db-column))
         (type (crt:db-column-type db-column))
         (virtual (crt:db-column-virtual db-column))
         (not-null-p (crt:db-column-not-null-p db-column))
         (primary-key-p (crt:db-column-primary-key-p db-column))
         (unique-p (crt:db-column-unique-p db-column))
         (external-p (crt:db-column-external-p db-column)))
    (unless external-p
      (remove nil (append (list column
                                (when type type)
                                (when primary-key-p :primary-key)
                                (when unique-p :unique)
                                (when not-null-p :not-null))
                          (when virtual (list :generated :always :as virtual :virtual)))))))

;; (crt:db--schema (crt:column-uuid))

(cl-defmethod crt:db--constraint ((obj crt:column))
  (let* ((db-column (crt:column-db-column obj))
         (column (crt:db-column-name db-column))
         (foreign-key-p (crt:db-column-foreign-key-p db-column))
         (reference-table (crt:db-column-reference-table db-column))
         (reference-column (crt:db-column-reference-column db-column)))
    (when foreign-key-p
      `(:foreign-key [,column] :references ,reference-table [,reference-column] :on-delete :cascade))))

;; (crt:db--constraint (crt:column-tracking-uuid))

(cl-defmethod crt:db--column-full-name ((obj crt:column) table-name)
  (let* ((column (crt:column-db-column-name obj))
         (external (crt:column-external-p obj)))
    ;; TODO: Refactor to generalize the full name logic.
    (if external
        column
      (crt:full-name table-name column))))

;; (crt:db--column-full-name (crt:column-uuid) 'reading-logs)

(cl-defmethod crt:db--column-where ((obj crt:column))
  (let ((where (eieio-oref obj 'where))
        (value (eieio-oref obj 'value))
        (column (crt:column-db-column-name obj)))
    (list where column value)))

;; (crt:db--column-where (crt:column-uuid :value "6483c7c1-6d97-484b-887f-67ad2cc63d55" :where '=))

(cl-defmethod crt:db--order-by (table-name (obj crt:column))
  (let ((order-by (eieio-oref obj 'order-by))
        (column (crt:column-db-column-name obj)))
    (list order-by (crt:full-name table-name column))))

;; (crt:db--order-by (crt:column-uuid :order-by 'desc) 'reading-log)

(cl-defmethod crt:db--selects ((obj crt:entity))
  (when-let* ((columns (remove-if-not #'crt:column-selected (eieio-oref obj 'columns)))
              (primary-table (eieio-oref obj 'table-name)))
    (vconcat (mapcar (lambda (column)
                       (if (crt:column-external-p column)
                           (crt:column-db-column-name column)
                         (crt:full-name primary-table (crt:column-db-column-name column))))
                     columns))))

;; (crt:db--selects (crt:entity-log))
;; (crt:db--selects (crt:entity-tracking))

(cl-defmethod crt:db--join-on ((obj crt:column) table-name)
  (when-let* ((reference-table (crt:column-reference-table obj))
              (reference-column (crt:column-reference-column obj)))
    `(:left-join ,reference-table :on
                 (= ,(crt:db--column-full-name obj table-name)
                    ,(crt:full-name reference-table reference-column)))))

(cl-defmethod crt:entity--join-ons ((obj crt:entity))
  (when-let* ((external-columns (remove-if-not (lambda (column) (eieio-oref column 'selected)) (remove-if-not #'crt:column-external-p (eieio-oref obj 'columns))))           (reference-columns (remove-if-not #'crt:column-reference-table (eieio-oref obj 'columns)))
              (main-table (eieio-oref obj 'table-name))
              (left-joins (mapcar (lambda (column) (crt:db--join-on column main-table)) reference-columns)))
    (when external-columns
      (apply #'append left-joins))))

(cl-defmethod crt:db--wheres ((obj crt:entity))
  (when-let ((wheres (mapcar #'crt:db--column-where (remove-if-not #'crt:column-where (eieio-oref obj 'columns)))))
    (list :where (append '(and (= 1 1)) wheres))))

(cl-defmethod crt:db--group-by ((obj crt:entity))
  (when-let ((main-table (eieio-oref obj 'table-name))
             (primary-key (find-if #'crt:column-primary-key-p (eieio-oref obj 'columns))))
    (list :group-by (crt:db--column-full-name primary-key main-table))))

(cl-defmethod crt:db--order-bys ((obj crt:entity))
  (when-let ((order-bys (mapcar (apply-partially #'crt:db--order-by (eieio-oref obj 'table-name))
                                (remove-if-not #'crt:column-order-by (eieio-oref obj 'columns)))))
    `(:order-by ,(vconcat order-bys))))

(cl-defmethod crt:db--stored-columns ((obj crt:entity))
  (let* ((columns (eieio-oref obj 'columns)))
    (remove-if #'crt:column-external-p (remove-if #'crt:column-virtual columns))))

(cl-defmethod crt:db--conflicts ((obj crt:entity))
  (let* ((columns (crt:db--stored-columns obj))
         (db-columns (mapcar (lambda (column) (eieio-oref column 'db-column)) columns))
         (conflict-columns (remove-if-not (lambda (column) (eieio-oref column 'primary-key)) db-columns)))
    `(:on-conflict ,(vconcat (mapcar (lambda (column) (intern (format ":%s" (eieio-oref column 'column)))) conflict-columns)))))

(cl-defmethod crt:db--update-sets ((obj crt:entity))
  (let* ((columns (crt:db--stored-columns obj))
         (db-columns (mapcar (lambda (column) (eieio-oref column 'db-column)) columns))
         (update-columns (remove-if (lambda (column) (eieio-oref column 'primary-key)) db-columns))
         (update-columns-names (mapcar (lambda (column) (eieio-oref column 'column)) update-columns)))
    `(:do-update-set ,(vconcat (mapcar (lambda (column) (list '= column (intern (format "excluded:%s" column)))) update-columns-names)))))

;;; API Functions

(cl-defmethod crt:db-create-table-sql ((obj crt:entity))
  (let* ((db-table-name (eieio-oref obj 'table-name))
         (columns (eieio-oref obj 'columns))
         (db-columns (vconcat (remove nil (mapcar #'crt:db--schema columns))))
         (db-constraints (car (remove nil (mapcar #'crt:db--constraint columns)))))
    `[:create-table
      :if-not-exists ,db-table-name
      (,db-columns
       ,db-constraints)]))

(cl-defmethod crt:db-query-sql ((obj crt:entity))
  (let* ((db-table-name (eieio-oref obj 'table-name))
         (selects (crt:db--selects obj))
         (join-on (crt:entity--join-ons obj))
         (wheres (crt:db--wheres obj))
         (group-by (crt:db--group-by obj))
         (order-bys (crt:db--order-bys obj))
         (offset (or (eieio-oref obj 'offset) 0))
         (limit (or (eieio-oref obj 'limit) 1000)))
    (vconcat `[:select ,selects :from ,db-table-name]
              join-on
              wheres
              group-by
              order-bys
              `[:limit ,limit :offset ,offset])))

(cl-defmethod crt:db-insert-or-update-sql ((obj crt:entity))
  (let* ((db-table-name (eieio-oref obj 'table-name))
         (stored-columns (crt:db--stored-columns obj))
         (values (vconcat (mapcar (lambda (column) (eieio-oref column 'value)) stored-columns)))
         (selects (vconcat (mapcar (lambda (column) (eieio-oref (eieio-oref column 'db-column) 'column)) stored-columns)))
         (conflicts (crt:db--conflicts obj))
         (update-sets (crt:db--update-sets obj)))
    (vconcat `[:insert-into ,db-table-name ,selects :values ,values]
             conflicts
             update-sets)))

(cl-defmethod crt:entity-delete-sql ((obj crt:entity))
  (when-let* ((db-table-name (eieio-oref obj 'table-name))
              (primary-key (find-if #'crt:column-primary-key-p (eieio-oref obj 'columns)))
              (value (eieio-oref primary-key 'value))
              (full-name (crt:full-name db-table-name (crt:column-db-column-name primary-key))))
    `[:delete-from ,db-table-name :where (= ,full-name ,value)]))

(defun crt:db-run-sql (sqls)
  ""
  (let ((db (emacsql-sqlite-open calibredb-db-dir))
        (result))
    (emacsql-with-transaction db
      (dolist (sql sqls)
        ;; (print sql)
        (setq result (emacsql db sql))))
    result))

(defun crt:db-total-changes-sql ()
  [:select (funcall total_changes)])

(defun crt:db-init-tables ()
  (interactive)
  (crt:db-run-sql
   (list (crt:db-create-table-sql (crt:entity-tracking))
         (crt:db-create-table-sql (crt:entity-log)))))

(provide 'calibredb-reading-tracking-db)
;;; calibredb-reading-tracking-db.el ends here
