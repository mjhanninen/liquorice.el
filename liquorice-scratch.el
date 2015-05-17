(require 'liquorice-color)
(require 'liquorice-math)

(defun insert-color (color)
  (let ((color-str (liquorice-to-string color)))
    (insert color-str
            " ("
            (let ((sample " f "))
              (put-text-property 0 3
                                 'face `(:background ,color-str)
                                 sample)
              sample)
            ") "
            (format "(%s%s)"
                    (car color)
                    (apply #'concat
                           (mapcar (lambda (x)
                                     (format " %.3f" x))
                                   (cdr color)))))))

(defun insert-color-strip (&rest colors)
  (dolist (color colors)
    (insert-color color)
    (insert "\n")))

(defun insert-color-ramp (from-color to-color steps)
  (apply #'insert-color-strip
         (liquorice-ramp from-color to-color steps)))

(provide 'liquorice-scratch)
