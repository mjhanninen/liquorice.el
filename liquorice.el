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
(require 'liquorice-color)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities for handling plists

(defun liquorice-plist-merge (left-plist right-plist)
  "Merges two property lists LEFT-PLIST and RIGHT-PLIST together
with entries from RIGHT-PLIST overriding those from LEFT-PLIST.

This function is internal to the library."
  (if left-plist
      (let ((result (cl-copy-list left-plist))
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
;;; monoid under *merge* operation.
;;;
;;; The merge operation is right leaning in the sense that if two descriptions
;;; L and R are being merged (L m R) and describe the same property for the
;;; same face then the property in the right description R is given
;;; precedence.

(defun liquorice-new-desc (attrs faces)
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

;;;

(defmacro liquorice-proto-dsl (&rest body)
  "This macro is internal to the library."
  (declare (indent 0))
  `(cl-macrolet ((desc (&rest body)
                   `(liquorice-merge-many-descs
                     (list ,@body))))
     (desc ,@body)))

(defmacro liquorice-build-desc (&rest body)
  "

    (LIQUORICE-BUILD-DESC
      EXPR-1
      ...
      EXPR-N)

Each of the expressions should evaluate to a description that are
then merged together to a resultant description that is returned
by the form.

    (ATTRS PLIST FACE-1 ... FACE-N)
    (BG COLOR FACE-1 ... FACE-N)
    (FG COLOR FACE-1 ... FACE-N)
    ()

Note that many forms like `let' and `progn' return only one of
the body forms. To enable combining expressions inside their
bodies the following macro can be used:

    (DESC
      EXPR-1
      ...
      EXPR-N)

Also:

    (RGB RED GREEN BLUE)
    (GRAY INTENSITY)

"
  (declare (indent 0))
  `(cl-flet ((rgb (r g b)
               (liquorice-srgb r g b))
             (gray (i)
               (liquorice-srgb i i i)))
     (cl-macrolet ((alter-color (&rest forms)
                     `(liquorice-alter-color ,@forms))
                   (attrs (attributes &rest faces)
                     `(liquorice-new-desc (list ,@attributes) ',faces))
                   (fg (color &rest faces)
                     `(attrs (:foreground ,color) ,@faces))
                   (bg (color &rest faces)
                     `(attrs (:background ,color) ,@faces)))
       (liquorice-proto-dsl ,@body))))

(defun liquorice-eval-colors (elem)
  "In case ELEM is a color converts it to its string representation.
Otherwise return ELEM unchanged.

This function is internal to the library."
  (if (liquorice-color-p elem)
      (liquorice-to-string elem)
    elem))

(defun liquorice-prepare-face (face spec)
  "Builds a face description for a single face from FACE and
SPEC.  All colors in SPEC are converted to the corresponding
string representations.  See, for instance, `custom-set-faces'
for the form of FACE and SPEC."
  `(,face ((t ,@(mapcar #'liquorice-eval-colors spec)))))

(defun liquorice-desc-to-face-specs (desc)
  "Converts the given description into a form that is directly
embeddable as an argument to functions `custom-set-faces' and
`custom-theme-set-faces'.

This function is internal to the library."
  (let (face-specs)
    (maphash (lambda (face attrs)
               (push (liquorice-prepare-face face attrs)
                     face-specs))
             desc)
    (sort face-specs
          (lambda (l r)
            (string< (car l)
                     (car r))))))

(defmacro liquorice-build-face-specs (&rest body)
  "A domain specific language for specifying font faces. Should
be used in conjunction with some of the Emacs font specifying
functions like CUSTOM-SET-FACES or CUSTOM-THEME-SET-FACES.

Usage example:

    (apply #'custom-set-faces
           (liquorice-build-face-specs
             (let ((funny-red (rgb 0.5 0.1 0.1))
                   (radiant-green (rgb 0.0 1.0 0.7))
                   (dull-gray (gray 0.8)))
               (bg funny-red defaul fringe)
               (fg dull-gray default)
               (fg radiant-green fringe))))"
  (declare (indent 0))
  `(liquorice-desc-to-face-specs
    (liquorice-build-desc
     ,@body)))

(provide 'liquorice)
