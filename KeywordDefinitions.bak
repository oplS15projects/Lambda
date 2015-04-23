
(require plot)

(define ens (make-base-namespace))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff))

(add-keyword 'plot (lambda (x)
                     (display-message (list "plot" x))))

(add-keyword 'simplify (lambda (x)
                         (display-message (list "simplify" (infix->prefix x)))))

(add-keyword 'eval (lambda (x)
                     (string-append "evaluated to: " (number->string (eval (infix->prefix x) ens)))))

(add-keyword 'err (lambda (x)
                    (string-append "cannot evaluate: " x)))