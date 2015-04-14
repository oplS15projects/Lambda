
(require plot)

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff))

(add-keyword 'plot (lambda (x)
                     (display-message (list "plot" x))))

(add-keyword 'simplify (lambda (x)
                         (display-message (list "simplify" x))))