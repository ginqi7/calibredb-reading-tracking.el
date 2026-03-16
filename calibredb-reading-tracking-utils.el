;;; calibredb-reading-tracking-utils.el ---          -*- lexical-binding: t; -*-

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

(defun crt:uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                   (random)
                   (current-time)
                   (user-uid)
                   (emacs-pid)
                   (user-full-name)
                   user-mail-address
                   (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
        (substring rnd 0 8)
        (substring rnd 8 12)
        (substring rnd 13 16)
        (format "%x"
            (logior
                 #b10000000
                 (logand
                      #b10111111
                      (string-to-number
                           (substring rnd 16 18) 16))))
        (substring rnd 18 20)
        (substring rnd 20 32))))

(defun crt:class-properties (clss)
  "Return a list of property names (slot names) for EIEIO class CLSS."
  (mapcar #'cl--slot-descriptor-name (eieio-class-slots clss)))

(defun crt:obj-properties (obj)
  "Return a list of property names (slot names) for EIEIO object OBJ.

Convenience wrapper around `crt:class-properties' that extracts
the class from OBJ automatically."
  (crt:class-properties (eieio-object-class obj)))

(defun crt:message-return-nil (msg)
  "Display MSG with `message' and return nil.

Useful for `when-let' bindings where you want to signal failure
with a message when a condition is not met."
  (message msg)
  nil)

(defun crt:duration (start finish)
  ""
  (time-subtract finish start))

(defun crt:current-time ()
  ""
  (floor (float-time)))

(defun crt:duration-min (start finish)
  ""
  (let* ((duration (crt:duration start finish)))
    (/ duration 60)))

(defun crt:format-time (time)
  (when time
   (substring (format-time-string crt:time-format time) 2)))

(defun crt:parse-time-string (time-str)
  (floor (float-time (date-to-time time-str))))

(defun crt:full-name (table column)
  (intern (format "%s:%s" table column)))

(provide 'calibredb-reading-tracking-utils)
;;; calibredb-reading-tracking-utils.el ends here
