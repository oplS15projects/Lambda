(include "KeyPair.rkt")

(define (evaluate keyword expression)
  ((get-definition keyword) expression))