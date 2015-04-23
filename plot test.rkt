#lang racket

(require plot)
(include "Parser.rkt")

(define-namespace-anchor anch)
(define ns (namespace-anchor->namespace anch))

(define l1 (infix->prefix "x*x"))

(define var 0)

(define (help-plot in) 
  (begin 
    (set! var in )
    (eval `(let ([x var]), l1) ns)
    )
  ) 

(plot (function help-plot -10 10)) 



;(define x x)
;(define 'x x)

;(plot (function (lambda  (var) 
;                 (eval `(let ([x var])
;                        , '(+ 2 x) ) ens))) #:x-min -10 #:x-max 10 #:y-min -10 #:y-max 10 )

;(define (a in)

;(eval l2 ens)

#|
(define (a x1)
  
  (define ens (namespace-anchor->namespace anch))
  (define x 3)
  
  (eval l2 ens))


(define (b x1)
  (let ([x x1]) x))
|#

;(a '(* x x))

;(define (test var) (eval `(let ([x var]), '(+ x 4)) ens))

;(define (a x) (list + 2 x))