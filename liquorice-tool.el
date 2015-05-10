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

;;; References:
;;;
;;; [lindbloom]: http://www.brucelindbloom.com

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

;;; D65 reference white:
;;; D65 | 0.95047 | 1.00000 | 1.08883

;;;; String conversions

(defun string-to-rgb (color)
  (let ((triplet (x-color-values color)))
    (when triplet
      (let ((denom (if (and (string-prefix-p "#" color)
                            (= (length color) 10))
                       17821440.0
                     65280.0)))
        (cons :rgb
              (mapcar (lambda (x)
                             (/ (float x) denom))
                           triplet))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Primitive conversions between color spaces

;;;; Conversions between sRGB and XYZ spaces

(defconst d65-ref-x 0.95047)

(defconst d65-ref-y 1.00000)

(defconst d65-ref-z 1.08883)

(defconst cie-epsilon 0.008856)

(defconst cie-kappa 903.3)

(defun srgb-compand (v)
  (if (> v 0.04045)
      (expt (/ (+ v 0.055) 1.055) 2.4)
    (/ v 12.92)))

(defun srgb-expand (v)
  (if (> v 0.0031308)
      (- (* 1.055 (expt v (/ 1.0 2.4))) 0.055)
    (* 12.92 v)))

(defun srgb-to-xyz (rgb)
  (if (eq (car rgb) :rgb)
      (cl-destructuring-bind (r g b) (mapcar #'srgb-compand
                                             (cdr rgb))
        (let* ((x (+ (* r 0.4124564)
                     (* g 0.3575761)
                     (* b 0.1804375)))
               (y (+ (* r 0.2126729)
                     (* g 0.7151522)
                     (* b 0.0721750)))
               (z (+ (* r 0.0193339)
                     (* g 0.1191920)
                     (* b 0.9503041))))
          (list :xyz x y z)))
    (error "Expected a RGB triplet")))

(defun xyz-to-srgb (xyz)
  (if (eq (car xyz) :xyz)
      (cl-destructuring-bind (x y z) (cdr xyz)
        (let* ((r (srgb-expand
                   (+ (* x  3.2404542)
                      (* y -1.5371385)
                      (* z -0.4985314))))
               (g (srgb-expand
                   (+ (* x -0.9692660)
                      (* y  1.8760108)
                      (* z  0.0415560))))
               (b (srgb-expand
                   (+ (* x  0.0556434)
                      (* y -0.2040259)
                      (* z  1.0572252)))))
          (list :rgb r g b)))
    (error "Expected a XYZ triplet")))

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

;;;; Transitive conversions between spaces

(defun to-xyz (color)
  (if (stringp color)
      (srgb-to-xyz (string-to-rgb color))
    (case (car color)
      (:xyz color)
      (:rgb (srgb-to-xyz color))
      (:luv (luv-to-xyz color))
      (:lch-uv (luv-to-xyz (lch-uv-to-luv color))))))

(defmacro unless-in-space (color space &rest body)
  (declare (indent 2))
  (let ((color-sym (cl-gensym "color")))
    `(let ((,color-sym ,color))
       (or (and (consp ,color-sym)
                (eq (car ,color-sym) ,space)
                ,color-sym)
           ,@body))))

(defun to-rgb (color)
  (unless-in-space color :rgb
    (xyz-to-srgb (to-xyz color))))

(defun to-luv (color)
  (unless-in-space color :luv
    (xyz-to-luv (to-xyz color))))

(defun to-lch-uv (color)
  (unless-in-space color :lch-uv
    (luv-to-lch-uv (to-luv color))))

(defun color-to-string (color)
  (if (stringp color)
      color
    (apply #'concat
           "#"
           (mapcar (lambda (v)
                     (let ((i (max 0 (min 255 (round (* 255.0 v))))))
                       (format "%02X" i)))
                   (cdr (to-rgb color))))))

;;;; Interpolating inside color spaces

(defun lin-interp (from to alpha)
  (+ from (* alpha (- to from))))

(defun ang-interp (from to alpha)
  (let* ((to-alt (if (< to from)
                     (+ to 360.0)
                   (- to 360.0)))
         (to-act (if (< (abs (- to from))
                        (abs (- to-alt from)))
                     to
                   to-alt)))
    (mod (lin-interp from to-act alpha) 360.0)))

(defun lch-interp (from to alpha)
  (if (and (eq (car from) :lch-uv)
           (eq (car to) :lch-uv))
      (cl-destructuring-bind (from-L from-C from-H) (cdr from)
        (cl-destructuring-bind (to-L to-C to-H) (cdr to)
          (list :lch-uv
                (lin-interp from-L to-L alpha)
                (lin-interp from-C to-C alpha)
                (ang-interp from-H to-H alpha))))
    (error "Expected LCH(uv) triplets")))

(defun linspace (from to steps)
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
