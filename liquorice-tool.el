;;; liquorice-tool.el -- an utility package for Liquorice library
;;; Copyright (C) 2015 Matti Hänninen <matti@mjhanninen.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; WORK IN PROGRESS

;;; Please do note that this files is in work-in-progress condition and in its
;;; current state should be considered entirely in as unstable of internal to
;;; the library.

(require 'faces)

(defun color-strip (colors)
  (apply #'concat
         (mapc (lambda (color)
                 (let ((s "   "))
                   (put-text-property 0 3
                                      'face `(:background ,color)
                                      s)
                   (insert s)))
               colors))
  (insert "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Random testing stuff

(defun color-interp (from to steps)
  (let* ((from-lch (to-lch-uv from))
         (to-lch (to-lch-uv to)))
    (mapcar (lambda (alpha)
              (color-to-string
               (lch-interp from-lch
                           to-lch
                           alpha)))
            (linspace 0.0 1.0 steps))))

(provide 'liquorice-tool)
