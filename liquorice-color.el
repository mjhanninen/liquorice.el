;;; liquorice-color.el -- color space manipulations for Liquorice library
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

(require 'color)

;;; References:
;;;
;;; [lindbloom]: http://www.brucelindbloom.com

;;;; Converting strings to colors

(defun liquorice-string-to-color (color)
  "Converts the string COLOR into a color. COLOR can be any
string representation that is also recognized by
COLOR-NAME-TO-RGB."
  (let ((triplet (color-name-to-rgb color)))
    (when triplet
      ;; TODO: We just assume that the color triplet is in the sRGB space;
      ;; investigate.
      (cons :srgb triplet))))

;;;; Primary conversions between color spaces

;;; Conversions between sRGB and XYZ spaces

(defun liquorice-xyz-to-srgb (xyz-color)
  "Converts XYZ-COLOR from the XYZ color space to the sRGB color space."
  (assert (eq (car xyz-color) :xyz) "Expected a XYZ triplet")
  (cons :srgb (apply #'color-xyz-to-srgb (cdr xyz-color))))

(defun liquorice-srgb-to-xyz (srgb-color)
  "Converts SRGB-COLOR from the sRGB color space to the XYZ color space."
  (assert (eq (car srgb-color) :srgb) "Expected an sRGB triplet")
  (cons :xyz (apply #'color-srgb-to-xyz (cdr srgb-color))))

;;; Conversions between XYZ and Lab spaces

(defun liquorice-xyz-to-lab (xyz-color)
  "Converts XYZ-COLOR from the XYZ color space to the Lab color space."
  (assert (eq (car xyz-color) :xyz) "Expected a XYZ triplet")
  (cons :lab (apply #'color-xyz-to-lab (cdr xyz-color))))

(defun liquorice-lab-to-xyz (lab-color)
  "Converts LAB-COLOR from the lab color space to the XYZ color space."
  (assert (eq (car lab-color) :lab) "Expected a Lab triplet")
  (cons :xyz (apply #'color-lab-to-xyz (cdr lab-color))))

;;;; Conversions between XYZ and Luv spaces

(defun xyz-to-luv (xyz)
  (if (eq (car xyz) :xyz)
      (cl-destructuring-bind (x y z) (cdr xyz)
        (let* ((y-rel (/ y d65-ref-y))
               (L (if (> y-rel cie-epsilon)
                      (- (* 116.0
                            (expt y-rel (/ 1.0 3.0)))
                         16.0)
                    (* cie-kappa y-rel)))
               (denom-val (+ x
                             (* 15.0 y)
                             (* 3.0 z)))
               (denom-ref (+ d65-ref-x
                             (* 15.0 d65-ref-y)
                             (* 3.0 d65-ref-z)))
               (u (* L 13.0 4.0
                     (- (/ x denom-val)
                        (/ d65-ref-x denom-ref))))
               (v (* L 13.0 9.0
                     (- (/ y denom-val)
                        (/ d65-ref-y denom-ref)))))
          (list :luv L u v)))
        (error "Expected a XYZ triplet")))

(defun luv-to-xyz (luv)
  (if (eq (car luv) :luv)
      (cl-destructuring-bind (L u v) (cdr luv)
        (let* ((inv-denom-ref (/ 1.0
                                 (+ d65-ref-x
                                    (* 15.0 d65-ref-y)
                                    (* 3.0 d65-ref-z))))
               (u-ref (* 4.0 d65-ref-x inv-denom-ref))
               (v-ref (* 9.0 d65-ref-y inv-denom-ref))
               (y (if (> L (* cie-kappa cie-epsilon))
                      (expt (/ (+ L 16.0) 116.0) 3.0)
                    (/ L cie-kappa)))
               (a (/ (- (/ (* 52.0 L)
                           (+ u (* 13.0 L u-ref)))
                        1.0)
                     3.0))
               (b (* -5.0 y))
               (c (/ -1.0 3.0))
               (d (* y
                     (- (/ (* 39.0 L)
                           (+ v (* 13.0 L v-ref)))
                        5.0)))
               (x (/ (- d b)
                     (- a c)))
               (z (+ (* a x) b)))
          (list :xyz x y z)))
    (error "Expected a Luv triplet")))

;;;; Conversions between Luv and LCH(uv) spaces

(defun luv-to-lch-uv (luv)
  (if (eq (car luv) :luv)
      (cl-destructuring-bind (L u v) (cdr luv)
        (let* ((C (sqrt (+ (* u u) (* v v))))
               (H (mod (* 180.0 (/ (atan v u) pi)) 360.0)))
          (list :lch-uv L C H)))
    (error "Expected a Luc triplet")))

(defun lch-uv-to-luv (lch)
  (if (eq (car lch) :lch-uv)
      (cl-destructuring-bind (L C H) (cdr lch)
        (let* ((h (/ (* pi H) 180.0))
               (u (* C (cos h)))
               (v (* C (sin h))))
          (list :luv L u v)))
    (error "Expected a LCH(uv) triplet")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TRANSITIVE CONVERSIONS

(defun liquorice-to-xyz (color)
  "Converts COLOR into its CIE XYZ representation. COLOR should
be either a list of color co-ordinates or a string that is
readable by X-COLOR-VALUES. The list of color co-ordinates is
expected to be of the form

    (COLOR-SPACE C-1 C-2 ... C-N)

where COLOR-SPACE is a keyword indicating the color space and C-I
is the I-th color co-ordinate. The function returns a list of the
form

    (:XYZ X Y Z)"
  (if (stringp color)
      (srgb-to-xyz (liquorice-string-to-color color))
    (case (car color)
      (:xyz color)
      (:rgb (srgb-to-xyz color))
      (:luv (luv-to-xyz color))
      (:lch-uv (luv-to-xyz (lch-uv-to-luv color))))))

(defmacro liquorice-unless-in-space (color space &rest body)
  "An *internal* macro for performing some color space conversion
calculations only if the color space of COLOR is not SPACE.
Otherwise the macro just returns (the evaluated value of) COLOR."
  (declare (indent 2))
  (let ((color-sym (cl-gensym "color")))
    `(let ((,color-sym ,color))
       (or (and (consp ,color-sym)
                (eq (car ,color-sym) ,space)
                ,color-sym)
           ,@body))))

(defun to-rgb (color)
  (liquorice-unless-in-space color :rgb
    (xyz-to-srgb (liquorice-to-xyz color))))

(defun to-luv (color)
  (liquorice-unless-in-space color :luv
    (xyz-to-luv (liquorice-to-xyz color))))

(defun to-lch-uv (color)
  (liquorice-unless-in-space color :lch-uv
    (luv-to-lch-uv (to-luv color))))

;;;; Converting back to strings

(defun color-to-string (color)
  (if (stringp color)
      color
    (apply #'concat
           "#"
           (mapcar (lambda (v)
                     (let ((i (max 0 (min 255 (round (* 255.0 v))))))
                       (format "%02X" i)))
                   (cdr (to-rgb color))))))
