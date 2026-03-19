;;; calibredb-reading-tracking-ctable.el ---         -*- lexical-binding: t; -*-

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

(require 'calibredb-reading-tracking-obj)
(require 'transient)
;; Internal Functions

(cl-defmethod crt:ctable--column-model ((obj crt:ctable-column))
  (let ((title (eieio-oref obj 'title))
        (align (eieio-oref obj 'align)))
   (make-ctbl:cmodel :title title :align align)))

(defun crt:ctable--column-models (columns)
  ""
  (mapcar #'crt:ctable--column-model columns))

(defun crt:ctable--model-data (lst)
  ""
  (mapcar (lambda (entity)
            (let ((columns (remove-if-not (lambda (column) (eieio-oref column 'ctable-column))
                            (eieio-oref entity 'columns))))
              (append (mapcar (lambda (column) (crt:column-format column)) columns) (list entity))))
          lst))

;;; Classes Functions

(cl-defmethod crt:ctable-list-buffer ((obj crt:entity-log))
  "Return or create the buffer for displaying LOG list.

Returns the \"*reading-logs*\" buffer."
  (get-buffer-create "*reading-logs*"))

(cl-defmethod crt:ctable-list-buffer ((obj crt:entity-tracking))
  "Return or create the buffer for displaying TRACKING list.

Returns the \"*reading-tracking*\" buffer."
  (get-buffer-create "*reading-tracking*"))

(cl-defmethod crt:ctable-actions ((obj crt:entity-log))
  "Return the transient menu for LOG actions.

Returns a transient prefix command with actions available for log
objects in the table."
  (crt:ctable-log-actions))

(cl-defmethod crt:ctable-actions ((obj crt:entity-tracking))
  "Return the transient menu for TRACKING actions.

Returns a transient prefix command with actions available for
tracking objects in the table."
  (crt:ctable-tracking-actions))

;; API Functions

(defun crt:ctable-render-list (lst &optional header-line)
  "Render LST as an interactive table in a dedicated buffer.

LST should be a list of EIEIO objects of the same type.
Creates column models, extracts data, and displays in a read-only
buffer with click hooks for interactive actions."
  (when-let* ((obj (car lst))
              (column-model (crt:ctable--column-models (remove nil (mapcar (lambda (column) (eieio-oref column 'ctable-column)) (eieio-oref obj 'columns)))))
              (data (crt:ctable--model-data lst))
              (model (make-ctbl:model :column-model column-model :data data)))
    (with-current-buffer (crt:ctable-list-buffer obj)
      (when header-line
        (setq-local header-line-format header-line))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (switch-to-buffer (current-buffer))
        (setq component (ctbl:create-table-component-region :model model))
        (ctbl:cp-add-click-hook component (lambda () (crt:ctable-actions obj)))
        (setq-local buffer-read-only t)))))

(defun crt:ctable-list-logs (tracking-uuid &optional header-line)
  "Display logs for TRACKING-UUID.

Renders a table of all reading logs associated with the specified
tracking record."
  (let ((log (crt:entity-log
              :columns
              (list
               (crt:column-uuid)
               (crt:column-started-at)
               (crt:column-finished-at)
               (crt:column-page-from)
               (crt:column-page-to)
               (crt:column-tracking-uuid
                :where '=
                :value tracking-uuid)
               (crt:column-duration)))))
   (crt:ctable-render-list (crt:query log) (when header-line (format "Book: %s" header-line)))))

;;; Ctable Actions
(defun crt:ctable-tracking-action-list-logs ()
  "Display logs for the selected tracking row.

Should be called from within a tracking table buffer with a row
selected. Extracts the tracking object from the selected row and
renders its logs in a new table buffer."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (tracking (car (last row)))
         (tracking-uuid (crt:entity-column-value tracking crt:column-uuid)))
    (crt:ctable-list-logs tracking-uuid (crt:entity-column-value tracking crt:column-book-title))))

(defun crt:ctable-log-action-refresh ()
  "Refresh the log table by re-fetching logs for the selected log's tracking.

Should be called from within a log table buffer with a row selected.
Extracts the tracking UUID from the selected log and re-renders the
log list."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (log (car (last row))))
    (crt:ctable-list-logs (crt:entity-column-value log crt:column-tracking-uuid))))

(defun crt:ctable-log-action-add ()
  "Add a new reading log for the selected tracking record.

Prompts for started-at, finished-at, page-from, and page-to values.
Should be called from within a log table buffer with a row selected
to get the tracking UUID."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (log (car (last row)))
         (started-at (crt:parse-time-string (read-string "started at: " (format-time-string crt:time-format))))
         (finished-at (crt:parse-time-string (read-string "finished at: " (format-time-string crt:time-format))))
         (page-from (string-to-number (read-string "Page from: ")))
         (page-to (string-to-number (read-string "Page to: "))))
    (crt:add-or-update (crt:entity-substitute-columns
                        (crt:entity-log)
                        (list (crt:column-tracking-uuid :value (crt:entity-column-value log crt:column-tracking-uuid))
                              (crt:column-started-at :value started-at)
                              (crt:column-finished-at :value finished-at)
                              (crt:column-page-from :value page-from)
                              (crt:column-page-to :value page-to))))
    (crt:ctable-log-action-refresh)))

(defun crt:ctable-log-action-delete ()
  ""
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (log (car (last row))))
    (when (yes-or-no-p (format "Are you sure you want to delete: %s?" (crt:entity-message log)))
        (crt:delete log))
    (crt:ctable-log-action-refresh)))

(transient-define-prefix crt:ctable-tracking-actions ()
  "Transient menu for tracking table actions.

Provides commands available when selecting a tracking row."
  ["Tracking Actions"
   ("RET" "List Logs" crt:ctable-tracking-action-list-logs)])

(transient-define-prefix crt:ctable-log-actions ()
  "Transient menu for log table actions.

Provides commands available when selecting a log row."
  ["Log Actions"
   ("a" "Add" crt:ctable-log-action-add)
   ("d" "Delete" crt:ctable-log-action-delete)
   ("r" "Refresh" crt:ctable-log-action-refresh)])

(provide 'calibredb-reading-tracking-ctable)
;;; calibredb-reading-tracking-ctable.el ends here
