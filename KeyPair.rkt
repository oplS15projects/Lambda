
(require plot)

(require racket/include)

;Key pair object. 
(define (key-pair keyword procedure)
  (define (dispatch m)
    (cond ((eq? m 'get-keyword) keyword)
          ((eq? m 'get-procedure) procedure)))
  dispatch)

;Key pair getter for keyword.
(define (get-keyword key-pair)
  (key-pair 'get-keyword))

;Key pair getter for Procedure.
(define (get-procedure key-pair)
  (key-pair 'get-procedure))

;List that holds all key-pairs.
(define key-pair-list '())

;Adds key-pair to the key-pair-list.
(define (add-key-pair key-pair)
  (set! key-pair-list (cons key-pair key-pair-list)))

;Removes key-pair from the key-pair-list.
(define (remove-key-pair keyword)
  keyword)

;Finds the key-pair object from the given keyword.
(define (find-key-pair keyword)
  (filter (lambda (x)
            (if (eq? keyword (get-keyword x))
                #t
                #f)) key-pair-list))

;Creates a keyword in the system.
(define (create-keyword keyword procedure)
  (add-key-pair (key-pair keyword procedure)))

;----Low Level-----
;----------------------------------------------Abstraction Level---------------------------------------------------------------
;----High Level----  

;Adds a new keyword to the system.
(define (add-keyword keyword procedure)
  (create-keyword keyword procedure))

(define (get-definition keyword)
  (get-procedure (car (find-key-pair keyword))))

(include "KeywordDefinitions.rkt")