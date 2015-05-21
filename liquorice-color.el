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

(require 'cl-lib)
(require 'color)
(require 'pcase)

(require 'liquorice-math)

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

;;;; Explicit conversions between color spaces

;;; Conversions between sRGB and HSL spaces

;;; TODO: Test that `color-hsl-to-rgb' and `color-rgb-to-hsl' really return
;;; correct values. Sometimes I get somewhat unbelievable results but haven't
;;; rendered them to actual colors so don't know for sure.

(defun liquorice-srgb-to-hsl (srgb-color)
  "Converts SRGB-COLOR from the sRGB color space to the HSL color
space."
  (assert (eq (car srgb-color) :srgb) "Expected an sRGB triplet")
  (cons :hsl (apply #'color-hsl-to-rgb (cdr srgb-color))))

(defun liquorice-hsl-to-srgb (hsl-color)
  "Converts HSL-COLOR from the HSL color space to the sRGB color
space."
  (assert (eq (car hsl-color) :hsl) "Expected an HSL triplet")
  (cons :srgb (apply #'color-rgb-to-hsl (cdr hsl-color))))

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

;;; Conversions between Lab and LCH(ab) spaces

(defun liquorice-lab-to-lch-ab (lab-color)
  "Converts LAB-COLOR from the CIE-Lab color space to the LCH(ab)
color space."
  (assert (eq (car lab-color) :lab) "Expected a Lab triplet")
  (pcase-let* ((`(,L ,a ,b) (cdr lab-color))
               (C (sqrt (+ (* a a) (* b b))))
               (H (mod (* 180.0 (/ (atan b a) pi)) 360.0)))
    (list :lch-ab L C H)))

(defun liquorice-lch-ab-to-lab (lch-color)
  "Converts LCH-COLOR from the LCH(ab) color space to the CIE-Lab
color space."
  (assert (eq (car lch-color) :lch-ab) "Expected a LCH(ab) triplet")
  (pcase-let* ((`(,L ,C ,H) (cdr lch-color))
               (h (/ (* pi H) 180.0))
               (a (* C (cos h)))
               (b (* C (sin h))))
    (list :lab L a b)))

;;; Conversions between XYZ and Luv spaces

(defun liquorice-xyz-to-luv (xyz-color)
  "Converts XYZ-COLOR from the XYZ color space to the CIE-Luv
color space. Assumes CIE D65 white point."
  (assert (eq (car xyz-color) :xyz) "Expected a XYZ triplet")
  (pcase-let* ((`(,x ,y ,z) (cdr xyz-color))
               (`(,ref-x ,ref-y ,ref-z) color-d65-xyz)
               (y-rel (/ y ref-y))
               (L (if (> y-rel color-cie-ε)
                      (- (* 116.0
                            (expt y-rel (/ 1.0 3.0)))
                         16.0)
                    (* color-cie-κ y-rel)))
               (denom-val (+ x
                             (* 15.0 y)
                             (* 3.0 z)))
               (denom-ref (+ ref-x
                             (* 15.0 ref-y)
                             (* 3.0 ref-z)))
               (u (* L 13.0 4.0
                     (- (/ x denom-val)
                        (/ ref-x denom-ref))))
               (v (* L 13.0 9.0
                     (- (/ y denom-val)
                        (/ ref-y denom-ref)))))
    (list :luv L u v)))

(defun liquorice-luv-to-xyz (luv-color)
  "Converts LUV-COLOR from the CIE-Luv color space to the XYZ
color space. Assumes CIE D65 white point."
  (assert (eq (car luv-color) :luv) "Expected a Luv triplet")
  (pcase-let* ((`(,L ,u ,v) (cdr luv-color))
               (`(,ref-x ,ref-y ,ref-z) color-d65-xyz)
               (inv-denom-ref (/ 1.0
                                 (+ ref-x
                                    (* 15.0 ref-y)
                                    (* 3.0 ref-z))))
               (u-ref (* 4.0 ref-x inv-denom-ref))
               (v-ref (* 9.0 ref-y inv-denom-ref))
               (y (if (> L (* color-cie-κ color-cie-ε))
                      (expt (/ (+ L 16.0) 116.0) 3.0)
                    (/ L color-cie-κ)))
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

;;; Conversions between Luv and LCH(uv) spaces

(defun liquorice-luv-to-lch-uv (luv-color)
  "Converts LUV-COLOR from the CIE-Luv color space to the LCH(uv)
color space."
  (assert (eq (car luv-color) :luv) "Expected a Luv triplet")
  (pcase-let* ((`(,L ,u ,v) (cdr luv-color))
               (C (sqrt (+ (* u u) (* v v))))
               (H (mod (* 180.0 (/ (atan v u) pi)) 360.0)))
    (list :lch-uv L C H)))

(defun liquorice-lch-uv-to-luv (lch-color)
  "Converts LCH-COLOR from the LCH(uv) color space to the CIE-Luv
color space."
  (assert (eq (car lch-color) :lch-uv) "Expected a LCH(uv) triplet")
  (pcase-let* ((`(,L ,C ,H) (cdr lch-color))
               (h (/ (* pi H) 180.0))
               (u (* C (cos h)))
               (v (* C (sin h))))
    (list :luv L u v)))

;;;; Implicit conversions

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
      (liquorice-srgb-to-xyz
       (liquorice-string-to-color color))
    (pcase (car color)
      (:xyz color)
      (:srgb (liquorice-srgb-to-xyz color))
      (:hsl (liquorice-srgb-to-xyz
             (liquorice-hsl-to-srgb color)))
      (:lab (liquorice-lab-to-xyz color))
      (:lch-ab (liquorice-lab-to-xyz
                (liquorice-lch-ab-to-lab color)))
      (:luv (liquorice-luv-to-xyz color))
      (:lch-uv (liquorice-luv-to-xyz
                (liquorice-lch-uv-to-luv color))))))

(defmacro liquorice-unless-already (color space &rest body)
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

(defun liquorice-to-srgb (color)
  (liquorice-unless-already color :srgb
    (liquorice-xyz-to-srgb
     (liquorice-to-xyz color))))

(defun liquorice-to-hsl (color)
  (liquorice-unless-already color :hsl
    (liquorice-srgb-to-hsl
     (liquorice-to-srgb color))))

(defun liquorice-to-lab (color)
  (liquorice-unless-already color :lab
    (liquorice-xyz-to-lab
     (liquorice-to-xyz color))))

(defun liquorice-to-lch-ab (color)
  (liquorice-unless-already color :lch-ab
    (liquorice-lab-to-lch-ab
     (liquorice-to-lab color))))

(defun liquorice-to-luv (color)
  (liquorice-unless-already color :luv
    (liquorice-xyz-to-luv
     (liquorice-to-xyz color))))

(defun liquorice-to-lch-uv (color)
  (liquorice-unless-already color :lch-uv
    (liquorice-luv-to-lch-uv
     (liquorice-to-luv color))))

;;;; Converting back to strings

(defun liquorice-to-string (color)
  (if (stringp color)
      color
    (pcase-let ((`(,r ,g ,b) (cdr (liquorice-to-srgb color))))
      (color-rgb-to-hex (color-clamp r)
                        (color-clamp g)
                        (color-clamp b)))))

;;;; Altering existing colors

(defun liquorice-saturate (color percent)
  (pcase-let ((`(:hsl ,H ,S ,L) (liquorice-to-hsl color)))
    (cons :hsl (color-saturate-hsl H S L percent))))

(defun liquorice-blend (from-color to-color alpha)
  (pcase-let* ((`(:lch-uv ,from-L ,from-C ,from-H)
                (liquorice-to-lch-uv from-color))
               (`(:lch-uv ,to-L ,to-C ,to-H)
                (liquorice-to-lch-uv to-color)))
    (list :lch-uv
          (liquorice-lin-interp from-L to-L alpha)
          (liquorice-lin-interp from-C to-C alpha)
          (liquorice-ang-interp from-H to-H alpha))))

(defun liquorice-ramp (from-color to-color steps)
  (let* ((from-lch (liquorice-to-lch-uv from-color))
         (to-lch (liquorice-to-lch-uv to-color)))
    (mapcar (lambda (alpha)
              (liquorice-blend from-lch
                               to-lch
                               alpha))
            (liquorice-linspace 0.0 1.0 steps))))

(defun liquorice-set-lightness (color new-L)
  (pcase-let* ((`(:lch-uv ,L ,C ,H)
                (liquorice-to-lch-uv color)))
    (list :lch-uv (min 100.0 (max 0.0 new-L)) C H)))

(defun liquorice-alter-lightness (color inc-L)
  (pcase-let* ((`(:lch-uv ,L ,C ,H)
                (liquorice-to-lch-uv color)))
    (list :lch-uv (min 100.0 (max 0.0 (+ L inc-L))) C H)))

(defun liquorice-set-chroma (color new-C)
  (pcase-let* ((`(:lch-uv ,L ,C ,H) (liquorice-to-lch-uv color)))
    (list :lch-uv
          L
          (min 100.0 (max 0.0 new-C))
          H)))

(defun liquorice-alter-chroma (color inc-C)
  (pcase-let* ((`(:lch-uv ,L ,C ,H) (liquorice-to-lch-uv color)))
    (list :lch-uv
          L
          (min 100.0 (max 0.0 (+ C inc-C)))
          H)))

(defun liquorice-set-hue (color new-H)
  (pcase-let* ((`(:lch-uv ,L ,C ,H)
                (liquorice-to-lch-uv color)))
    (list :lch-uv L C (mod new-H 360.0))))

(defun liquorice-alter-hue (color angle)
  (pcase-let* ((`(:lch-uv ,L ,C ,H)
                (liquorice-to-lch-uv color)))
    (list :lch-uv L C (mod (+ H angle) 360.0))))

(defun liquorice-copy-hue (ref-color hue-color)
  (pcase-let* ((`(:lch-uv ,ref-L ,ref-C ,ref-H)
                (liquorice-to-lch-uv ref-color))
               (`(:lch-uv ,hue-L ,hue-C ,hue-H)
                (liquorice-to-lch-uv hue-color)))
    (list :lch-uv ref-L ref-C hue-H)))

(defmacro liquorice-alter-color (init-color &rest body)
  (declare (indent 1))
  (let ((result-sym (cl-gensym "result-")))
    ;; FIXME: Replace `copy-list' with something that also ensures that
    ;; `init-color' is a color (and not a name of a color).
    `(let ((,result-sym (copy-list (liquorice-to-lch-uv ,init-color))))
       (cl-flet ((set-lightness (L)
                   (setq ,result-sym (liquorice-set-lightness ,result-sym L)))
                 (alter-lightness (L)
                   (setq ,result-sym (liquorice-alter-lightness ,result-sym L)))
                 (set-chroma (C)
                   (setq ,result-sym (liquorice-set-chroma ,result-sym C)))
                 (alter-chroma (C)
                   (setq ,result-sym (liquorice-alter-chroma ,result-sym C)))
                 (set-hue (H)
                   (setq ,result-sym (liquorice-set-hue ,result-sym H)))
                 (alter-hue (H)
                   (setq ,result-sym (liquorice-alter-hue ,result-sym H))))
         (progn ,@body)
         ,result-sym))))

;;;; Predicates

(defun liquorice-color-p (val)
  "Tests whether VAL is a color."
  (and (consp val)
       (case (car val)
         (:srgb t)
         (:xyz t)
         (:lab t)
         (:lch-ab t)
         (:luv t)
         (:lch-uv t)
         (:hsl t) )))

;;;; Instantiate colors

(defun liquorice-srgb (r g b)
  "Construct an sRGB color.  If *all* arguments are integers then
their value range is assumed to be from 0 to 255. Otherwise,
i.e. when at least one of the arguments is a float, the value
range is assumed to be from 0.0 to 1.0. Arguments values are
clamped to the assumed value range.

This function is internal to the library."
  (if (or (floatp r) (floatp g) (floatp b))
      (list :srgb
            (color-clamp (float r))
            (color-clamp (float g))
            (color-clamp (float b)))
    (list :srgb
          (color-clamp (/ r 255.0))
          (color-clamp (/ g 255.0))
          (color-clamp (/ b 255.0)))))

(provide 'liquorice-color)
