;;; liquorice-math.el -- mathematic utilites for Liquorice library
;;; Copyright (C) 2015 Matti Hänninen <matti@mjhanninen.com>
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun liquorice-linspace (from to steps)
  (when (> steps 1)
    (let ((result ())
          (n (float (1- steps))))
      (dotimes (i steps)
        (setq result
              (cons (+ (float from)
                       (* (/ (- n i) n)
                          (float (- to from))))
                    result)))
      result)))

(defun liquorice-lin-interp (from to alpha)
  (+ from (* alpha (- to from))))

(defun liquorice-ang-interp (from to alpha)
  (let* ((mod-from (mod from 360.0))
         (mod-to (mod to 360.0))
         (alt-to (+ mod-to (if (<= mod-to mod-from)
                               360.0
                             -360.0)))
         (act-to (if (< (abs (- mod-to mod-from))
                        (abs (- alt-to mod-from)))
                     mod-to
                   alt-to)))
    (mod (liquorice-lin-interp mod-from act-to alpha) 360.0)))

(provide 'liquorice-math)
