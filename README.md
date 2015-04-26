#Lambda
##ShuddaWuddaCdr

##Authors
- **Brian Carlson**
- **Joshua Caravetta**
- **Norman Mutunga**


##Overview
The goal is to have core mathematical engine that is simple , powerful and easy to build upon. Simple in terms that it allows the user to enter their equation to evaluate in infix notation with english keywords to further define operations on the equation. The power of the Lambda will come from the ease at which the mathematical engine backend can be expanded on to add new math functionality.


##Screenshots
*place some screenshots of initial ui, and inputs using eval/plot/deriv*
![screenshot showing initial ui](images/ui.png)
![screenshot showing plot input](plot.png)
![screenshot showing eval input](eval.png)
![screenshot showing deriv input](deriv.png)

##Concepts Demonstrated
*will need input from everyone here*
- Backend High/Low level abstraction barriers - Josh
- Main Parser keyword/equation extraction and fixing - Brian
- GUI strategies - Norm
- Functions - All

##External Technology and Libraries
- Plot - http://docs.racket-lang.org/plot/
- GUI - http://docs.racket-lang.org/gui/
- Parser - http://docs.racket-lang.org/parser-tools/index.html?q=~a

##Favorite Lines of Code
####Brian
The `main-parser` procedure takes the complete english keyword and infix notation expression from the frontend GUI and breaks it up into a keyword list and equation string with the help of a lexical parser. It then ensures that the keyword list is not empty, if it is, then it inserts the `'eval` keyword in. It will then check to make sure proper notation/operators are used in the equation. Finally, the `main-parser` strips the lexical parser tags from the equation string and calls the backend `evaluator` with the keyword and the equation string. 

This procedure uses many helper functions, along with many `map` and `filter` calls.

```
(define (main-parser in-exp) 
  (let (
        [exp (exp-lexer (open-input-string in-exp))] ; lex the in-exp and store it locally
        [k-list '() ] ; list for keywords
        [e-list '() ] ; list for expression
        )
    
    ;; -- Helper Functions --
    (define (is-keyword? item)
      (and (equal? 'ID (car item)) ( > (string-length (symbol->string (car (cdr item)))) 1))
      )
    
    (define (is-equation? item)
      (or    (equal? 'OP (car item)) ; OP - operation
             (equal? 'INT (car item)) ; INT - numbers
             ;(equal? 'LPAR (car item)) ; LPAR - Left Paren
             ;(equal? 'RPAR (car item)) ; RPAR - Right Paren
             (and (equal? 'ID (car item)) ( = (string-length (symbol->string (car (cdr item)))) 1)) ; ID - variables
             )
      )
    
    (define (is-var? item)
      (equal? 'ID (car item))
      )
    
    (define (add-mult l1)
      (define (iter in out)
        (cond 
          ((empty? (cdr in)) (append out (list (car in))))
          ((and 
            (equal? 'INT (car (car in))) 
            (equal? 'ID (car (car (cdr in)))))  (iter (cdr in) (append out (list (car in)) (list (list 'OP  '*)))) )
          (else (iter (cdr in) (append out (list (car in)))) )
          )
        )
      
      (iter l1 '())
      )
    
    (define (rem-tags item)
      (car (cdr item)) ; returns the symbol of the data 
      )
  
    ; --- Parse in-exp ---
    
    ; Filter in-exp for keywords, append all keywords to k-list
    (set! k-list (map rem-tags (filter is-keyword? exp)))
    
    ; Filter in-exp for equation, append equation to e-list
    (set! e-list (add-mult (filter is-equation? exp)))
    
    ; Now input is separated into two lists:
    ; k-list has only keywords in it (no tags)
    ; e-list has the full, unaltered equation (with tags)
    
    ;; --- Evaluate call to backend ---
    
    ; Here we are going to check if the user just wants to evaluate a generic, numbers only math equation
    (cond
      ((and (empty? k-list) (empty? (filter is-var? e-list))) (set! k-list (list 'eval)))
      ((and (empty? k-list) (not (empty? (filter is-var? e-list)))) (set! k-list (list 'err)))
      )
    
    ; Remove tags from e-list
    (set! e-list (map rem-tags e-list))
    
    ; --- Call Backend with k-list and e-list ---
    ; only calls with one keyword for now, passes e-list as string
    (evaluate (car k-list) (lst-to-str e-list))
    
    )
  )
```

####Josh

####Norman
This procedure reads in a string from the `Input` field of the GUI, hands it over to the `main-parser` procedure that parses the expression. The evaluated result is then returned from the backend `evaluator` and placed into the `outputString`. The `outputString` is sent to the `Output` field of the GUI so the user can see the result. If the `plot` keyword is used the user can also see the plot on the canvas of the GUI.

```
;(init-value "Expression")
      (callback (λ (input-field event)
            (cond
             ; If a user hits enter to compute an equation
            ((equal? (send event get-event-type) 'text-field-enter) 
             (begin
             ; Clear canvas
             (send pb erase)
             ; Set outputString to solved equation
             (set! outputString (main-parser (send input-field get-value)))
             ; Send outputString to output-field
            (send output-field set-value outputString))))))
```

##Additional Remarks / Project Status
- Working frontend GUI with input, output and plot canvas fields.
- Working general expression parser that handles initial keyword and equation parsing.
- Working infix->prefix parser that handles transforming input equation to prefix for evaluation (handles operator precedence).
- High and Low level abstracted backend that forms a dynamic database of key pairs for additional mathematical procedures based on keywords.
- Evaluation of basic equations with any combination of the following operators: **-,+,/,\*,^**.
- Plot of basic equations with any combination of the following operators: **-,+,/,\*,^** and variable **x**. 
- Working keywords: eval, plot, deriv. *(deriv currently only works with **+,\*,^**. and its output isn't formatted correctly)*
- Other planned keywords: simplify.

#How to Download and Run

1. Download the latest release from here: *release link*
2. Open and run Lambda.rkt
3. Input expression is typed into the `Input`, using syntax: `keyword equation`
4. Output is seen in the `Output` or lower canvas depending on keywords used.

**Example 1:** `plot x+2*x+3` - support for only one variable **x** is currently available.

This will plot `x+2*x+3` in the lower canvas field

**Example 2:** `eval 1+2*3^4` or `1+2*3^4` will both evaluate to: 163. `eval` is the default keyword if no keyword is entered.

If `eval` is used on an equation with a variable, such as `x+2`, it will return `cannot evaluate: x+2` since there is a variable.
