
(require plot)

(define ens (make-base-namespace))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff))

(add-keyword 'plot (lambda (x)
                     (display-message (list "plot" x))))

(add-keyword 'simplify (lambda (x)
                         (display-message (list "simplify" x))))

(add-keyword 'eval (lambda (x)
                         (display-message (list "evaluated to:" (eval x ens)))))

(add-keyword 'err (lambda (x)
                         (display-message (list "cannot evaluate:" x))))