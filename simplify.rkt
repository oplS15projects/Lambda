#lang racket

(define (variable? x) (symbol? x))

(define operator-list '(* + -))

(define (operator? x)
  (if (null? (filter (lambda (y) (eq? y x)) operator-list))
      #f
      #t))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (add-variables lst)
  (if (null? lst)
      lst
      (let ((count 1))
        (if (variable? (car lst))
            (begin
              (accumulate (lambda (x y) 
                           (if (same-variable? x (car lst))
                               (set! count (+ 1 count))
                               (set! count count)))0 (cdr lst))
              (if (< count 2)
                  (cons (car lst) (add-variables (filter (lambda (x) (not(same-variable? x (car lst)))) (cdr lst))))
                  (cons (list '* count (car lst)) (add-variables (filter (lambda (x) (not(same-variable? x (car lst)))) (cdr lst))))))                                                                 
            (cons (car lst) (add-variables (cdr lst)))))))

  
(define test '(x '(y y) x))

(define (simplify exp)
  (add-variables (simplify-helper exp)))
          
(define (simplify-helper exp)
  (if (null? exp)
      '()
      (if (list? (car exp))
          (cons (add-variables (car exp)) (simplify-helper (cdr exp)))
          (cons (car exp) (simplify-helper (cdr exp))))))
          
