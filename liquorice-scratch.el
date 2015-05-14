(require 'liquorice-color)
(require 'liquorice-math)

(defun insert-color-strip (&rest colors)
  (mapc (lambda (color)
          (let ((s "   "))
            (put-text-property 0 3
                               'face `(:background
                                       ,(liquorice-to-string color))
                               s)
            (insert s)))
        colors)
  (insert "\n"))

(defun insert-color-ramp (from-color to-color steps)
  (apply #'insert-color-strip
         (liquorice-ramp from-color to-color steps)))

(provide 'liquorice-scratch)
