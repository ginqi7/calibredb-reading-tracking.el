;;; calibredb-reading-tracking-calendar.el ---       -*- lexical-binding: t; -*-

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

(require 'calendar)

;;; Custom Defined Faces

(defface crt:calendar-marker-0 '((t :background "#ebedf0"))
  "Calendar face for days with 0 reading logs.")
(defface crt:calendar-marker-1 '((t :background "#c6e48b"))
  "Calendar face for days with 1-2 reading logs.")
(defface crt:calendar-marker-2 '((t :background "#7bc96f"))
  "Calendar face for days with 3-5 reading logs.")
(defface crt:calendar-marker-3 '((t :background "#239a3b"))
  "Calendar face for days with 6-8 reading logs.")
(defface crt:calendar-marker-4 '((t :background "#196127"))
  "Calendar face for days with 9+ reading logs.")

;;; Internal Functions
(defun crt:calendar--parse-date (s)
  "Parse date string S into calendar format.

S is expected to be in \"YYYY-MM-DD\" format.
Returns a list (MONTH DAY YEAR) as expected by `calendar-mark-visible-date`."
  (pcase-let ((`(,y ,m ,d)
               (mapcar #'string-to-number (split-string s "-"))))
    (list m d y))) ;; calendar uses (month day year)

(defun crt:calendar-marker (count)
  "Return the appropriate face for a calendar day with COUNT reading logs.

Uses a GitHub-style heatmap color scheme with 5 intensity levels."
  (cond ((<= count 0) 'crt:calendar-marker-0)
        ((<= count 2) 'crt:calendar-marker-1)
        ((<= count 5) 'crt:calendar-marker-2)
        ((<= count 8) 'crt:calendar-marker-3)
        (t            'crt:calendar-marker-4)))

(defun crt:calendar-logs-group-by-day ()
  "Query all reading logs and group them by the date they started.

Returns a hash table where:
- Keys are date strings in \"YYYY-MM-DD\" format
- Values are lists of `crt:entity-log` objects that started on that date."
  (let ((logs (crt:query (crt:entity-log)))
        (table (make-hash-table :test 'equal)))
    (dolist (log logs)
      (let ((date-str (format-time-string "%Y-%m-%d"
                       (crt:entity-column-value log crt:column-started-at))))
        (puthash date-str (cons log (gethash date-str table)) table)))
    table))

;;; Interactive Commands
(defun crt:calendar ()
  "Display a calendar with reading activity heatmap.

Opens the standard Emacs calendar and marks each day with a colored
face based on the number of reading logs started on that day.
The heatmap uses 5 intensity levels from light gray (no activity)
to dark green (9+ logs)."
  (interactive)
  (calendar)
  (use-local-map (make-sparse-keymap))
  (let ((table (crt:calendar-logs-group-by-day)))
   (dolist (date (hash-table-keys table))
     (let* ((count (length (gethash date table)))
            (face (crt:calendar-marker count)))
       (calendar-mark-visible-date (crt:calendar--parse-date date) face)))
   (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\n")
      (crt:ctable-render-list (crt:query (crt:entity-tracking))
                              :buffer (current-buffer)
                              :append-p t))))
  (delete-other-windows))

(provide 'calibredb-reading-tracking-calendar)
;;; calibredb-reading-tracking-calendar.el ends here
