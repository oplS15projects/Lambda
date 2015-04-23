

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
  (define (add-variables-helper lst)
    (if (null? lst)
        lst
        (let ((count 1))
          (if (variable? (car lst))
              (if (not (operator? (car lst)))
                  (begin
                    (accumulate (lambda (x y) 
                                  (if (same-variable? x (car lst))
                                      (set! count (+ 1 count))
                                      (set! count count)))0 (cdr lst))
                    (display count)
                    (if (< count 2)
                        (cons (car lst) (add-variables-helper (filter (lambda (x) (not(same-variable? x (car lst)))) (cdr lst))))
                        (cons (list '* count (car lst)) (add-variables-helper (filter (lambda (x) (not(same-variable? x (car lst)))) (cdr lst))))))                                                                 
                  (add-variables-helper (cdr lst)))
              (add-variables-helper (cdr lst))))))
  (car (add-variables-helper lst)))
(define (testing x)
  (match x
    [(list '+ 'x 'x) (list '* 2 'x)]
    [(list ?s (list ?n)) (+ ?s ?n)]
    [(list '+ 'x (list '* ?n 'x)) (list '* (+ 1 ?n) 'x)]
    [(list '+ ?s ?n) (+ ?s ?n)]))

(define current-op 'none)

(define (work exp)
  (define (work-helper exp op)
    (if (null? exp)
        '()
        (if (list? (car exp))
            (if (
            
            

(define (simplify exp)
  (display exp))
          
(define (simplify-helper exp)
  (if (null? exp)
      '()
      (if (list? (car exp))
          (cons (add-variables (car exp)) (simplify-helper (cdr exp)))
          (cons (car exp) (simplify-helper (cdr exp))))))
          
