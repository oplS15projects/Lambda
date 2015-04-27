; Code taken/altered from http://matt.might.net/articles/lexers-in-racket/ to get us started/get ideas

; Get parser tools from Racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(include "Evaluator.rkt")

(define (string->char s)
  (car (string->list s)))

;; Test expressions
(define test-exp1 (open-input-string "simplify x+2x+3"))
(define test-exp2 (open-input-string "plot x - 2 * x / 3 ^"))
(define exp-simp "simplify x+2x+3")
(define exp-num (open-input-string "1+2*3"))
(define test-eval "1+2*3")
(define exp-parse '())

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
    (cons `(LPAR ,(string->symbol lexeme))
          (exp-lexer input-port))]
   
   [#\)
    (cons `(RPAR ,(string->symbol lexeme))
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

;; ----------------------------Infix->Prefix-------------------------- 
;; Module from: http://docs.racket-lang.org/guide/hash-reader.html
;; 
;; usage: (syntax->datum (read-arith #f expression)) <- expression must be in open-input-string form
;;

(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))

(define (read-arith src in)
  (define-values (line col pos) (port-next-location in))
  (define expr-match
    (regexp-match
     ; Match an operand followed by any number of 
     ; operator–operand sequences, and prohibit an
     ; additional operator from following immediately:
     #px"^([a-z]|[0-9]+)(?:[-+*/^]([a-z]|[0-9]+))*(?![-+*/^])"
     in))
  
  (define (to-syntax v delta span-str)
    (datum->syntax #f v (make-srcloc delta span-str)))
  (define (make-srcloc delta span-str)
    (and line
         (vector src line (+ col delta) (+ pos delta)
                 (string-length span-str))))
  
  (define (parse-expr s delta)
    (match (or (regexp-match #rx"^(.*?)([+-])(.*)$" s)
               (regexp-match #rx"^(.*?)([*/])(.*)$" s)
               (regexp-match #rx"^(.*?)(\\^)(.*)$" s))
      [(list _ a-str op-str b-str)
       (define a-len (string-length a-str))
       (define a (parse-expr a-str delta))
       (define b (parse-expr b-str (+ delta 1 a-len)))
       (define op (to-syntax (string->symbol op-str)
                             (+ delta a-len) op-str))
       (to-syntax (list op a b) delta s)]
      [else (to-syntax (or (string->number s)
                           (string->symbol s))
                       delta s)]))
  
  (parse-expr (bytes->string/utf-8 (car expr-match)) 0))

;; Function to convert a string infix equation to a list prefix equation
(define (infix->prefix str)
  (syntax->datum (read-arith #f (open-input-string str))))

;; -------------------------------------------------------------------- 

; Convert list with many items to string 
(define (lst-to-str l1)
  (string-append* "" (map (lambda(item)
                            (cond
                              ((number? item) (number->string item))
                              ((symbol? item) (symbol->string item))
                              )
                            ) l1)))

;; -------------------------------Parser------------------------------- 
;; pseudocode

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
    
    ;; -- Helper Functions --
    ; Predicate for finding keywords from input expression
    (define (is-keyword? item)
      (and (equal? 'ID (car item)) ( > (string-length (symbol->string (car (cdr item)))) 1))
      )
    
    ; Predicate for finding equation items from input expression
    (define (is-equation? item)
      (or    (equal? 'OP (car item)) ; OP - operation
             (equal? 'INT (car item)) ; INT - numbers
             ;(equal? 'LPAR (car item)) ; LPAR - Left Paren
             ;(equal? 'RPAR (car item)) ; RPAR - Right Paren
             (and (equal? 'ID (car item)) ( = (string-length (symbol->string (car (cdr item)))) 1)) ; ID - variables
             )
      )
    
    ; Predicate for finding variables from equation
    (define (is-var? item)
      (equal? 'ID (car item))
      )
    
    ; Function to add * operator to variable coefficients
    (define (add-mult l1)
      (define (iter in out)
        
        (cond 
          ; If in list is empty
          ((empty? (cdr in)) (append out (list (car in))))
          
          ; If current item is a coefficient and next item is a variable, insert *
          ((and 
            (equal? 'INT (car (car in))) 
            (equal? 'ID (car (car (cdr in)))))  (iter (cdr in) (append out (list (car in)) (list (list 'OP  '*)))) )
          
          ; Otherwise continue normally
          (else (iter (cdr in) (append out (list (car in)))) )
          )
        )
      
      (iter l1 '())
      )
    
    ; Extract data without tags 
    (define (rem-tags item)
      (car (cdr item)) ; returns the symbol of the data 
      )
    
    
    ; --- Parse in-exp ---
    
    ; Filter in-exp for keywords, append all keywords to k-list
    (set! k-list (map rem-tags (filter is-keyword? exp)))
    
    ; Filter in-exp for equation, append equation to e-list
    (set! e-list (add-mult (filter is-equation? exp)))
    
    ;(set! exp-parse (filter is-equation? exp))
    ;(set! exp-parse (add-mult exp-parse))
    
    ; Test Prints 
    ;(begin (display k-list)(newline)(display e-list)(newline))  
    
    ; Now input is separated into two lists:
    ; k-list has only keywords in it (no tags)
    ; e-list has the full, un altered equation (with tags)
    
    
    ;; --- Evaluate call to backend ---
    
    ; Here we are going to check if the user just wants to evaluate a generic, numbers only math equation
    (cond
      ; If k-list is empty and no IDs in equation, append eval to list
      ((and (empty? k-list) (empty? (filter is-var? e-list))) (set! k-list (list 'eval)))
      ; If k-list is empty and IDs (variables) in equation, append err and tell backend to return expression (cannot eval with variables)
      ((and (empty? k-list) (not (empty? (filter is-var? e-list)))) (set! k-list (list 'err)))
      ; If k-list is eval and IDs (variables) in equation, append err and tell backend to return expression (cannot eval with variables)
      ((and (equal? 'eval (car k-list)) (not (empty? (filter is-var? e-list)))) (set! k-list (list 'err)))
      )
    
    ; Remove tags from e-list
    (set! e-list (map rem-tags e-list))
    
    ; Test Prints
    ;(begin (display k-list)(newline)(display e-list)(newline)(display (car k-list)))  
    
    ; --- Call Backend with k-list and e-list ---
    ; only calls with one keyword for now, passes e-list as string
    (evaluate (car k-list) (lst-to-str e-list))
    
    )
  )
;; -------------------------------------------------------------------- 


; Provide all definitions in this file
(provide (all-defined-out))
