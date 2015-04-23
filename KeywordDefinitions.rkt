
(require plot)

(define-namespace-anchor anch)
(define ns (namespace-anchor->namespace anch))

(define var 0)

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff))

(add-keyword 'simplify (lambda (x)
                         (display-message (list "simplify" (infix->prefix x)))))

(add-keyword 'eval (lambda (x)
                     (string-append "evaluated to: " (number->string (eval `(let ([^ expt]), (infix->prefix x)) ens)))))

(add-keyword 'err (lambda (x)
                    (string-append "cannot evaluate: " x)))

(add-keyword 'plot (lambda (x1)
                     ; Plot helper
                     (define (help-plot in) 
                       (begin 
                         (set! var in )
                         (eval `(let ([x var]), (infix->prefix x1)) ns)
                         )
                       ) 
                     
                     ; Plot function
                     (begin
                       ; Send plot to canvas
                       (send pb insert (plot-snip (function help-plot -10 10))0 0) 
                       ; Return Equation
                       (string-append "plotted: " x1)
                       )))