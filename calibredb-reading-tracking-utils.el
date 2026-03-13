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
                   (org-time-convert-to-list nil)
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
  "Compute the duration between START and FINISH time strings.

START and FINISH are time strings parseable by `parse-time-string'.
Returns a time value suitable for `time-subtract' results."
  (let* ((start-time (parse-time-string start))
         (finish-time (parse-time-string finish)))
    (time-subtract finish-time start-time)))

(defun crt:duration-min (start finish)
  "Compute the duration in minutes between START and FINISH time strings.

START and FINISH are time strings parseable by `parse-time-string'.
Returns the total duration as an integer number of minutes."
  (let* ((duration (crt:duration start finish)))
    (+ (nth 1 duration)
       (* 60 (nth 2 duration)))))

(provide 'calibredb-reading-tracking-utils)
;;; calibredb-reading-tracking-utils.el ends here
