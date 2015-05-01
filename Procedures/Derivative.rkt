;; Code for Prefix Derivative function of Lambda
;; Used code provided in the Hw assignment
;; Exercise 2.57 on pp. 151 of SICP, the differentiator.

;; Don't remove the equation1 definition.
(define equation1 '(* x y (+ x 3)))  ; i.e., ((x^2)y + 3xy) dx

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;(define (accumulate op initial sequence)
; (if (null? sequence)
;     initial
;     (op (car sequence)
;         (accumulate op initial (cdr sequence)))))

;; per dotted-tail procedure notation, second and subsequent args
;; will get made into a list and provided in "augend"
;;
;; the constructor should deal with three cases:
;; [augend empty]      (make-sum 'x) is 'x
;; [augend length 1]   (make-sum 'x 3) is '(+ x 3)
;;                     (make-sum 'x 0) is 'x
;;                     (make-sum 1 2) is 3
;; [augend is 2+]      (make-sum 'x 'y 'x) is '(+ x y z)
;;
;; the code for the length 1 case is quite similar to the original
;; implementation; you should bring it in and modify it
(define (make-sum a1 . augend)
  (cond ((= 0 (length augend )) a1 )
        ((= 1 (length augend))
         (let (( a2 (car augend )))
           (cond ((=number? a1 0 ) a2)
                 ((=number? a2 0 ) a1 ) 
                 (( and (number? a1 )(number? a2 )) (+ a1 a2 ))
                 ;(else (list a1 a2 '+  ))))) ;ok postfix out put two errors
                 ;(else (list a1 '+ a2 ))))) ;ok infix out put
                 (else (or(list '+ a1 a2 ) (list a1 '+ a2 ))))));ok Prefix output
        (else 
         (append (list '+ a1 ) augend ))))

(define (sum? x)
  (and (pair? x) (or(eq? (car x) '+) (eq? (cadr x) '+)))) ;ok

(define (addend s) (or(cadr s) (car s ))) ;ok

;; you're allowed to have augend also be a constructor
;; you will need to test for the length of the augend, and do
;; something different the length=1 case and length is 2+ case. 
(define (augend s)
  (apply make-sum (cddr s)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Divid::::::::::::::::::::::::::::::::::::::::::::::
(define (make-div d1 . augend)
  (cond ((= 0 (length augend )) d1 )
        ((= 1 (length augend))
         (let (( d2 (car augend )))
           (cond ((=number? d1 0 ) d2)
                 ((=number? d2 0 ) d1 ) 
                 (( and (number? d1 )(number? d2 )) (/ d1 d2 ))
                 ;(else (list d1 d2 '+  ))))) ;ok postfix out put two errors
                 ;(else (list d1 '+ d2 ))))) ;ok infix out put
                 (else (or(list '/ d1 d2 ) (list d1 '/ d2 ))))));ok Prefix output
        (else 
         (append (list '/ d1 ) augend ))))

(define (div? x)
  (and (pair? x) (or(eq? (car x) '/) (eq? (cadr x) '/)))) ;ok

(define (addend-div s) (or(cadr s) (car s ))) ;ok

;; you're allowed to have augend also be a constructor
;; you will need to test for the length of the augend, and do
;; something different the length=1 case and length is 2+ case. 
(define (augend-div s)
  (apply make-div (cddr s)))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; like make-sum, this should work with 1, 2, or 3+ args
;; and perform reductions on 1 and 2 arg cases
(define (make-product m1 . multiplicand)
  (cond ((= 0 (length multiplicand)) m1)
        ((= 1 (length multiplicand))
         (let ((m2 (car multiplicand )))
           (cond ((or (=number? m1 0 ) (=number? m2 0 )) 0 )
                 ((=number? m1 1 ) m2 )
                 ((=number? m2 1 ) m1 )
                 ((and (number? m1 ) (number? m2 )) (* m1 m2 ))
                 (else (or(list '* m1 m2 ) (list m1 '* m2 ))))))
        (else 
         (append ( list '* m1 ) multiplicand ))))



(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;; may also construct a product expression
(define (multiplicand p)
  (apply make-product(cddr p)))

;;; differentiation for exponents
(define (make-exponentiation x y)
  (cond ((= y 0) 1)
        ((= y 1) x)
        (else (list '** x y))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

;;; deriv including exponentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ;;;;;;;;;;;;;;;;;;;;;;;;     ;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
        ((div? exp)
         (make-div (deriv (addend-div exp) var)
                   (deriv (augend-div exp) var)))
        ;;;;;;;;;;;;;;;;;;;;;;;    ;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) 
                                                          (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))


;(define test (deriv  '(+ (* x x) (* x x)) 'x))
;; String Pretty Print for Deriv output
(define (deriv-print l1)
  
  ; Helper - adjusted from parser
  (define (fix-emb-list item)
    
    (if (list? item)
        ; If list contains another list, go in and fix it
        (string-append* "" (append (list "(") (map fix-emb-list item) (list ")")))
        ; else convert the symbol to a string
        (cond
          ((number? item) (string-append (number->string item) " "))
          ((symbol? item) (string-append (symbol->string item) " "))
          )
        )
    ) 
  
  (string-append (string-append* "(" (map fix-emb-list l1)) ")" )
  
  )


#| Tests for Deriv

;;http://courses.cs.washington.edu/courses/cse341/12au/racket/deriv.rkt
;; Unit tests.  See
;; http://docs.racket-lang.org/rackunit/quick-start.html
;; for documentation on Racket's unit testing library.
(require rackunit)

(define deriv-testing 
 (test-suite 
   "tests for deriv program"
   (check-equal? (deriv 'x 'x) 1 "deriv of x wrt x")
   (check-equal? (deriv 'y 'x) 0 "deriv of y wrt x")
   (check-equal? (deriv '(+ x 3) 'x) 1 "deriv of (+ x 3) wrt x")
   (check-equal? (deriv 'x 3 ) 0 "deriv of 3 wrt x") ;ok
  
   ;(check-equal? (deriv '(* (+ 2 3) x) 'x) 5 "deriv of unsimplified expression")
   (check-equal? (deriv '(+ x y) 'x) 1 "deriv of (+ x y) wrt x")
;   ;; simplification is not as clever as it could be in the following case:
   (check-equal? (deriv '(* (+ x 1) (+ x -1)) 'x) '(+ (+ x 1) (+ x -1)) "deriv of (* (+ x 1) (+ x -1)) wrt x")
   (check-equal? (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))) "complex deriv")
   ))
;
(require rackunit/text-ui)
;;; this line runs the tests ....
(run-tests deriv-testing)
;;more test 
(make-sum 'x 3) ; '(+ x 3)
(sum? (make-sum 'x 3)) ;#t
(make-div 9 3) ;3
(make-div 9 'x) ; '(/ 9 x)
(make-div 3 9) ; 1/3
(addend-div (make-div 'x 3 )) ;'x
(augend-div (make-div 'x 3 )) ;3
(addend (make-sum 'x 3)) ; 'x
(augend (make-sum 'x 3)) ;3
(make-product 'x 3) ;'(* x 3)
(product? (make-product 'x 3)) ;#t
(multiplier (make-product 'x 3)) ;'x
(multiplicand (make-product 'x 3)) ;3
(make-exponentiation 'x 3) ;'(** x 3)
(exponentiation? (make-exponentiation 'x 3)) ;#t
(base (make-exponentiation 'x 3)) ;'x
(exponent (make-exponentiation 'x 3)) ;3
;(deriv '(x + 3) 'x)         ;1
;(deriv '(x * (y * (x + 3))) 'x)     ; '((x * y) + (y * (x + 3))) 
;(deriv '((x * y) * (x + 3)) 'x)     ; '((x * y) + (y * (x + 3)))
;(deriv '((x ** 3) + (x ** 2)) 'x)   ; '((3 * (x ** 2)) + (2 * x))
(make-product 'x 3) ;'(* x 3)
(make-product 1 2) ;2
(make-product 'x 1) ; 'x
(make-product 1 'x) ; 'x
(make-product 'x) ; 'x
(make-product 'x 'y 'z) ;'(* x y z)
(multiplier '(* x 3)) ; 'x
(multiplicand '(* x 3)) ;3
(multiplier '(* x y z)) ;'x
(multiplicand '(* x y z)) ;'(* y z)
(multiplicand equation1) ;'(* y (+ x 3))
(deriv equation1 'x) ;'(+ (* x y) (* y (+ x 3)))
|#

(provide (all-defined-out))
