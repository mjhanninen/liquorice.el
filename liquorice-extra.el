;;; liquorice-extra.el
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
(require 'liquorice)

;;; Face descriptions are monoid under combination

(defun liquorice-mode-line-desc
    (highlight
     active-foreground
     active-background
     inactive-foreground
     inactive-background)
  "Builds the partial theme description for mode lines and
vertical window borders (which are regarded as extensions of
inactice mode lines)."
  (liquorice-build-desc
    (attrs (:foreground active-foreground
            :background active-background)
           mode-line)
    (attrs (:foreground inactive-foreground
            :background inactive-background)
           mode-line-inactive)
    (attrs (:background nil
            :foreground highlight)
           mode-line-buffer-id)
    (attrs (:background nil
            :foreground inactive-background)
           vertical-border)))

(provide 'liquorice-extra)
