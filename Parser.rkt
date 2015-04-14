#lang racket

; Code taken/altered from http://matt.might.net/articles/lexers-in-racket/ to get us started/get ideas

; Get parser tools from Racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(include "Evaluator.rkt")

(define (string->char s)
  (car (string->list s)))


(define test-exp1 (open-input-string "simplify x+2x+3"))
(define test-exp2 (open-input-string "plot x - 2 * x / 3 ^"))
(define exp "simplify x+2x+3")


; Basic Parser Example
(define lex-char
  (lexer
   ; skip spaces:
   (#\space     (lex-char input-port))
   
   ; skip newline:
   (#\newline   (lex-char input-port))
   
   ; an actual character:
   (any-char    (list 'CHAR (string->char lexeme)))))


; Modified Calculator Expression Lexer example

; identifiers:  [a-zA-Z]+
; delimiters:   "("|")"
; operators:    "+"|"*"|"/"|"-"|"^"
; integers:     -?[0-9]+
; whitespace:   [ \n]+

(define exp-lexer
  (lexer
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    (cons `(ID ,(string->symbol lexeme))
          (exp-lexer input-port))]
   
   [#\( 
    (cons '(LPAR)
          (exp-lexer input-port))]
   
   [#\)
    (cons '(RPAR) 
          (exp-lexer input-port))]
   
   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    (cons `(INT ,(string->number lexeme))
          (exp-lexer input-port))]
   
   [(:or #\+ #\* #\/ #\- #\^)
    (cons `(OP ,(string->symbol lexeme))
          (exp-lexer input-port))]
   
   [whitespace 
    (exp-lexer input-port)]
   
   [(eof)
    '()]))


;; TO RUN LEXER
;; (exp-lexer test-exp1)
;; output: '((ID simplify) (ID x) (OP +) (INT 2) (ID x) (OP +) (INT 3))
;; lexers take an (open-input-string) port, not a straight up string.

;; -------------------------------------------------------------------- 
;; Parser pseudocode

;; Get input string -> string port
;; Lex expression as local list
;;
;; Filter keywords out (if ID & length > 1) (keyword: build/add to k-list)
;;
;; Filter expression as-is into e-list
;;
;; push k-list and e-list to backend.
;;
;; -------------------------------------------------------------------- 

;; Main parser for removing keywords from in-expression, takes in a string
(define (main-parser in-exp) 
  (let (
        [exp (exp-lexer (open-input-string in-exp))] ; lex the in-exp and store it locally
        [k-list '() ] ; list for keywords
        [e-list '() ] ; list for expression
        )
    
    ; Predicate for finding keywords from input expression
    (define (is-keyword? item)
      (and (equal? 'ID (car item)) ( > (string-length (symbol->string (car (cdr item)))) 1))
      )
    
    ; Predicate for finding equation items from input expression
    (define (is-equation? item)
      (or    (equal? 'OP (car item)) ; OP - operation
             (equal? 'INT (car item)) ; INT - numbers
             (and (equal? 'ID (car item)) ( = (string-length (symbol->string (car (cdr item)))) 1)) ; ID - variables
             )
      )
    
    ; Helper function to extract data without tags 
    (define (rem-tags item)
      (car (cdr item)) ; returns the symbol of the data 
      )
    
    ; Filter in-exp for keywords, append all keywords to k-list
    (set! k-list (map rem-tags (filter is-keyword? exp)))
    
    ; Filter in-exp for equation, append equation to e-list
    (set! e-list (map rem-tags (filter is-equation? exp)))
    
    ; Now input is separated into two lists:
    ; k-list has only keywords in it
    ; e-list has the full, un altered equation
    
    ; Verification print
    ;(begin 
    ;(display k-list)
    ;(newline)
    ;(display e-list)
    ;)
    
    ; Evaluate call to backend
    ; only call for one keyword for now, pass e-list as string
    
    (evaluate (car k-list) (string-append* "" (map (lambda(item)
                                                     (cond
                                                       ((number? item) (number->string item))
                                                       ((symbol? item) (symbol->string item))
                                                       )
                                                     ) e-list)))
    
    )
  )


;; Decided that infix -> prefix should be separate from the main keyword parser, since passing to plot and
;; other functions, we wont want it in racket format. Maybe we should add keyword "evaluate" or if there is no
;; keyword, we will just infix -> prefix and try to compute the function. If the function fails, we just return the
;; original expression as an output or "error evaluating"

;; Gunnaa beee funnn too writee thisssss

;; -------------------------------------------------------------------- 
;; Infix --> Prefix pseudocode
;;
;; Filter human expression and build Racket expression (e-list)
;;  
;;  (if ID & length < 1) (variable: add variable to e-list)
;;  (if INT) (number: add number to e-list)
;;
;;  Human expression is in infix, convert to prefix
;;  (if OP)
;;        (if op is same as previous (current op after l-paren), dont add)
;;        (else op is new, add left paren and op before last id/num, inc p-cnt))
;;  (add p-count right parens to e-list)
;;
;; return e-list
;;
;; -------------------------------------------------------------------- 

; Provide all definitions in this file
(provide (all-defined-out))
