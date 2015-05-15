;;; liquorice.el -- Library for simplifying theme definitions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities for specifying colors

(defun liquorice-int-to-hex (x)
  "Converts X to a string of two digit hexadecimal number.  X is
clamped between 0 and 255.

This function is internal to the library."
  (format "%02x" (min (max 0 (truncate x)) 255)))

(defun liquorice-float-to-hex (x)
  "Converts a floating point number X to a string of two digit
hexadecimal number with 0.0 corresponding to hexadecimal 00 and
1.0 to hexadecimal FF.

This function is internal to the library."
  (liquorice-int-to-hex (round (* x 255.0))))

(defun liquorice-rgb (r g b)
  "Returns the hexadecimal specification of the color defined by
the color triplet (R, G, B).  If all arguments are integers then
their value range is assumed to be from 0 to 255. Otherwise,
i.e. when at least one of the arguments is a float, the value
range is assumed to be from 0.0 to 1.0. Arguments values are
clamped to the assuemd value range.

This function is internal to the library."
  (if (or (floatp r) (floatp g) (floatp b))
      (concat "#"
              (liquorice-float-to-hex r)
              (liquorice-float-to-hex g)
              (liquorice-float-to-hex b))
    (concat "#"
            (liquorice-int-to-hex r)
            (liquorice-int-to-hex g)
            (liquorice-int-to-hex b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities for handling plists

(defun liquorice-plist-merge (left-plist right-plist)
  "Merges two property lists LEFT-PLIST and RIGHT-PLIST together
with entries from RIGHT-PLIST overriding those from LEFT-PLIST.

This function is internal to the library."
  (if left-plist
      (let ((result (copy-list left-plist))
            (right-head right-plist))
        (while right-head
          (plist-put result
                     (car right-head)
                     (cadr right-head))
          (setq right-head (cddr right-head)))
        result)
    right-plist))

(defun liquorice-plist-sort (plist)
  "Sorts the property list PLIST according to the propoerty
keys.

This function is internal to the library."
  (let ((head plist)
        (keys ())
        (result ()))
    (while head
      (setq keys (cons (car head) keys))
      (setq head (cddr head)))
    (dolist (key (nreverse (sort keys #'string<)))
      (setq result
            (cons key
                  (cons (plist-get plist key)
                        result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities for inspecting existing faces

(defun liquorice-existing-faces (faces)
  "Takes a list of FACES and filters out those that are not
currently among (FACE-LIST)."
  (let ((all-faces (make-hash-table)))
    (dolist (face (face-list))
      (puthash face t all-faces))
    (cl-remove-if-not (lambda (f)
                        (gethash f all-faces nil))
                      faces)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The DSL

;;; A *description* contains property descriptions for multiple faces and is a
;;; monoid under merge operation.
;;;
;;; The merge operation is right leaning in the sense that if two descriptions
;;; L and R are being merged (L m R) and describe the same property for the
;;; same face then the property in the right description R is given
;;; precedence.

(defun liquorice-build-desc (attrs faces)
  (let ((desc (make-hash-table)))
    (dolist (face faces)
      (puthash face attrs desc))
    desc))

(defun liquorice-merge-descs (left right)
  (let ((res (copy-hash-table left)))
    (maphash (lambda (face right-attrs)
               (let* ((left-attrs (gethash face left ()))
                      (new-attrs (liquorice-plist-merge left-attrs
                                                        right-attrs)))
                 (puthash face new-attrs res)))
             right)
    res))

(defun liquorice-merge-many-descs (descs)
  (cl-loop for desc in descs
           with res = (make-hash-table)
           do (setq res (liquorice-merge-descs res desc))
           finally return res))

(defun liquorice-set-attrs (env attrs faces)
  "Updates the environment ENV destructively so that for all
faces in FACES the face attributes are amended to include
ATTRS. Many of the DSL statements get directly translated to
calls of this funcion.

This function is internal to the library."
  (dolist (face faces)
    (let* ((old-attrs (gethash face env ()))
           (new-attrs (liquorice-plist-merge old-attrs attrs)))
      (puthash face new-attrs env))))

(defun liquorice-env-to-internal-desc (env)
  "Converts the environment ENV (normally after the statements of
the DSL have been \"executed\") into an internal description of
faces that is the end result of \"executing\" the DSL.

This function is internal to the library."
  (let (result)
    (maphash (lambda (face plist)
               (setq result
                     (cons (cons face
                                 (liquorice-plist-sort plist))
                           result)))
             env)
    (sort result
          (lambda (l r)
            (string< (car l)
                     (car r))))))

(defmacro liquorice-base-dsl (&rest body)
  "Minimal version of the DSL that defines only the ATTRS
primitive. The ATTRS primitices is an imperative statement of the
form

    (ATTRS ATTRIBUTES FACE...)

where attributes is a property list (KEY VALUE KEY VALUE...)
that specifies the properties that should be applied to one or
more FACE that follow the property list.

The elements of ATTRIBUTES get evaluated individually.

Each FACE should be an unquoted symbol naming a face.

This macro is internal to the library."
  (let ((env (gensym)))
    `(let ((,env (make-hash-table)))
       (cl-macrolet ((attrs (attributes &rest faces)
                       `(liquorice-set-attrs ,',env
                                             (list ,@attributes)
                                             ',faces)))
         ,@body)
       (liquorice-env-to-internal-desc ,env))))

(defmacro liquorice-dsl (&rest body)
  "Extends the DSL by defining the following syntactic sugar:

    (FG COLOR FACE...)
    (BG COLOR FACE...)
    (RGB RED GREEN BLUE)
    (GRAY VALUE)

The (FG COLOR FACE...) and (BG COLOR FACE...) forms correspond
to (ATTRS (:FOREGROUND COLOR) FACE...) and (ATTRS (:BACKGROUND
COLOR) FACE...) forms, correspondingly.

The (RGB RED GREEN BLUE) and (GRAY VALUE) forms are functions
defined within the DSL that can be used to build hexadecimal
specifications for colors. In case all arguments are integers the
value range is taken to be from 0 to 255. Otherwise (i.e. when at
least one of the arguments is a float) the value range from 0.0
to 1.0.

This macro is internal to the library."
  `(liquorice-base-dsl
    (cl-macrolet ((fg (color &rest faces)
                    `(attrs (:foreground ,color) ,@faces))
                  (bg (color &rest faces)
                    `(attrs (:background ,color) ,@faces)))
      (cl-flet ((rgb (r g b)
                  (liquorice-rgb r g b))
                (gray (i)
                  (liquorice-rgb i i i)))
        ,@body))))

(defun liquorice-theme-args-from-internal-desc (desc)
  "Transforms the internal face description DESC into form that
can be fed directly to Emacs' face setting functions such as
CUSTOM-SET-FACES.

This function is internal to the library."
  (mapcar (lambda (face-desc)
            `(,(car face-desc) ((t ,@(cdr face-desc)))))
          desc))

(defmacro liquorice-build-set-faces-args (&rest body)
  "A domain specific language for specifying font faces. Should
be used in conjunction with some of the Emacs font specifying
functions like CUSTOM-SET-FACES or CUSTOM-THEME-SET-FACES.

Usage example:

    (apply #'custom-set-faces
           (liquorice-build-set-faces-args
             (let ((funny-red (rgb 0.5 0.1 0.1))
                   (radiant-green (rgb 0.0 1.0 0.7))
                   (dull-gray (gray 0.8)))
               (bg funny-red defaul fringe)
               (fg dull-gray default)
               (fg radiant-green fringe))))"
  (declare (indent 0))
  `(liquorice-theme-args-from-internal-desc
    (liquorice-dsl
     ,@body)))

(provide 'liquorice)
