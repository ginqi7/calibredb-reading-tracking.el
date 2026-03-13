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
(defun crt:ctable--column-model (columns)
  "Create a list of `ctbl:cmodel' objects for COLUMNS.

COLUMNS is a list of symbols representing slot names.
Returns a list of column model objects with titles derived from
the slot names, all left-aligned."
  (mapcar (lambda (col) (make-ctbl:cmodel :title (symbol-name col) :align 'left)) columns))

(defun crt:ctable--model-data (columns lst)
  "Extract model data from LST for the given COLUMNS.

COLUMNS is a list of slot symbols to extract.
LST is a list of EIEIO objects.
Returns a list where each element contains the slot values followed
by the original object (for use in click handlers)."
  (mapcar (lambda (row) (append (mapcar (lambda (col) (eieio-oref row col)) columns) (list row))) lst))

;;; Classes Functions
(cl-defmethod crt:ctable-visible-columns ((obj crt:tracking))
  "Return visible columns for TRACKING object.

Returns all slot names except 'logs' and 'uuid' for table display."
  (let ((columns (crt:obj-properties obj)))
    (remove 'logs (remove 'uuid columns))))

(cl-defmethod crt:ctable-visible-columns ((obj crt:log))
  "Return visible columns for LOG object.

Returns all slot names except 'logs' and 'uuid' for table display."
  (let ((columns (crt:obj-properties obj)))
    (remove 'logs (remove 'uuid columns))))

(cl-defmethod crt:ctable-list-buffer ((obj crt:log))
  "Return or create the buffer for displaying LOG list.

Returns the \"*reading-logs*\" buffer."
  (get-buffer-create "*reading-logs*"))

(cl-defmethod crt:ctable-list-buffer ((obj crt:tracking))
  "Return or create the buffer for displaying TRACKING list.

Returns the \"*reading-tracking*\" buffer."
  (get-buffer-create "*reading-tracking*"))

(cl-defmethod crt:ctable-actions ((obj crt:log))
  "Return the transient menu for LOG actions.

Returns a transient prefix command with actions available for log
objects in the table."
  (crt:ctable-log-actions))

(cl-defmethod crt:ctable-actions ((obj crt:tracking))
  "Return the transient menu for TRACKING actions.

Returns a transient prefix command with actions available for
tracking objects in the table."
  (crt:ctable-tracking-actions))

;; API Functions

(defun crt:ctable-render-list (lst)
  "Render LST as an interactive table in a dedicated buffer.

LST should be a list of EIEIO objects of the same type.
Creates column models, extracts data, and displays in a read-only
buffer with click hooks for interactive actions."
  (when-let* ((obj (car lst))
              (visible-columns (crt:ctable-visible-columns obj))
              (column-model (crt:ctable--column-model visible-columns))
              (data (crt:ctable--model-data visible-columns lst))
              (model (make-ctbl:model :column-model column-model :data data)))
    (with-current-buffer (crt:ctable-list-buffer obj)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq component (ctbl:create-table-component-region :model model))
        (ctbl:cp-add-click-hook component (lambda () (crt:ctable-actions obj))))
     (setq-local buffer-read-only t)
     (switch-to-buffer (current-buffer)))))

(defun crt:ctable-list-logs (tracking-uuid)
  "Display logs for TRACKING-UUID.

Renders a table of all reading logs associated with the specified
tracking record."
  (crt:ctable-render-list (crt:logs tracking-uuid)))

;;; Ctable Actions
(defun crt:ctable-tracking-action-list-logs ()
  "Display logs for the selected tracking row.

Should be called from within a tracking table buffer with a row
selected. Extracts the tracking object from the selected row and
renders its logs in a new table buffer."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (tracking (car (last row))))
    (crt:ctable-list-logs (crt:obj-uuid tracking))))

(defun crt:ctable-log-action-refresh ()
  "Refresh the log table by re-fetching logs for the selected log's tracking.

Should be called from within a log table buffer with a row selected.
Extracts the tracking UUID from the selected log and re-renders the
log list."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (log (car (last row))))
    (crt:ctable-list-logs (crt:log-tracking-uuid log))))

(defun crt:ctable-log-action-add ()
  "Add a new reading log for the selected tracking record.

Prompts for started-at, finished-at, page-from, and page-to values.
Should be called from within a log table buffer with a row selected
to get the tracking UUID."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (log (car (last row)))
         (started-at (read-string "started at: " (crt:current-time)))
         (finished-at (read-string "finished at: " (crt:current-time)))
         (page-from (read-string "Page from: "))
         (page-to (read-string "Page to: ")))
    (crt:obj-add-or-update (crt:log
                            :tracking-uuid (crt:log-tracking-uuid log)
                            :started-at started-at
                            :finished-at finished-at
                            :page-from page-from
                            :page-to page-to))))

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
   ("r" "Refresh" crt:ctable-log-action-refresh)])

(provide 'calibredb-reading-tracking-ctable)
;;; calibredb-reading-tracking-ctable.el ends here
