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

(provide 'liquorice-tool)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; String conversions

(defun rgb-to-string (rgb)
  (cl-destructuring-bind (r g b) rgb
    (liquorice-rgb r g b)))

(defun hex-to-float (h)
  (/ (float (string-to-number h 16))
     (1- (expt 16.0 (length h)))))

(defun string-to-rgb (h)
  (let ((l (length h)))
    (unless (and (or (= l 4) (= l 7))
                 (string= (substring h 0 1) "#"))
      (error "Expecting a string of the form \"#RGB\" or \"#RRGGBB\"."))
    (cond
     ((= l 4)
      (list (hex-to-float (substring h 1 2))
            (hex-to-float (substring h 2 3))
            (hex-to-float (substring h 3 4))))
     ((= l 7)
      (list (hex-to-float (substring h 1 3))
            (hex-to-float (substring h 3 5))
            (hex-to-float (substring h 5 7)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Primitive conversions between color spaces

;;; sRGB-tp-XYZ

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
  (cl-destructuring-bind (r g b) (mapcar #'srgb-compand rgb)
    (let* ((x (+ (* r 0.4124564)
                 (* g 0.3575761)
                 (* b 0.1804375)))
           (y (+ (* r 0.2126729)
                 (* g 0.7151522)
                 (* b 0.0721750)))
           (z (+ (* r 0.0193339)
                 (* g 0.1191920)
                 (* b 0.9503041))))
      (list x y z))))

(defun xyz-to-srgb (xyz)
  (cl-destructuring-bind (x y z) xyz
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
      (list r g b))))

(defun xyz-to-luv (xyz)
  (cl-destructuring-bind (x y z) xyz
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
      (list L u v))))

(defun luv-to-xyz (luv)
  (cl-destructuring-bind (L u v) luv
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
      (list x y z))))

(defun luv-to-lch-uv (luv)
  (cl-destructuring-bind (L u v) luv
    (let* ((C (sqrt (+ (* u u) (* v v))))
           (H (mod (* 180.0 (/ (atan v u) pi)) 3)))
      (list L C H))))

(defun lch-uv-to-luv (lch)
  (cl-destructuring-bind (L C H) lch
    (let* ((h (/ (* pi H) 180.0))
           (u (* C (cos h)))
           (v (* C (sin h))))
      (list L u v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Derived conversions between color spaces

(defun srgb-to-lch-uv (rgb)
  (luv-to-lch-uv
   (xyz-to-luv
    (srgb-to-xyz rgb))))

(defun lch-uv-to-srgb (lch)
  (xyz-to-srgb
   (luv-to-xyz
    (lch-uv-to-luv lch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (cl-destructuring-bind (from-L from-C from-H) from
    (cl-destructuring-bind (to-L to-C to-H) to
      (list (lin-interp from-L to-L alpha)
            (lin-interp from-C to-C alpha)
            (ang-interp from-H to-H alpha)))))

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
  (let* ((from-lch (srgb-to-lch-uv (string-to-rgb from)))
         (to-lch (srgb-to-lch-uv (string-to-rgb to))))
    (mapcar (lambda (alpha)
              (rgb-to-string
               (lch-uv-to-srgb
                (lch-interp from-lch
                            to-lch
                            alpha))))
            (linspace 0.0 1.0 steps))))
