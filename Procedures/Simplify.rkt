

(define operator-list '(* + -))

(define (operator? x)
  (if (null? (filter (lambda (y) (eq? y x)) operator-list))
      #f
      #t))

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (matching x)
  (begin (match x
           [(list ?n 'x '+ ?s) (list ?n 'x '+ ?s)]
           [(list ?s '+ ?n 'x) (list ?n 'x '+ ?s)]
           [(list 'x '+ ?n 'x) (list (+ ?n 1) 'x)]
           [(list ?n 'x '+ 'x) (list (+ ?n 1) 'x)]
           [(list 'x '+ 'x) (list 2 'x)]
           [(list 'x ?n '+ 'x) (list (+ ?n 1) 'x)]
           [(list ?n 'x '+ 'x) (list (+ ?n 1) 'x)]
           [(list 'x ?n) (list 'x ?n)]
           [(list ?n 'x) (list ?n 'x)]
           [(list ?s '+ ?n) (list (+ ?s ?n))]
           [(list 'x '+ ?n '+ 'x) (list 2 'x '+ ?n)]
           [(list 'x '+ 'x '+ ?n) (list 2 'x '+ ?n)]
           [(list ?n '+ 'x '+ 'x) (list 2 'x '+ ?n)]
           [(list 'x '+ ?s '+ ?n) (list 'x '+ (+ ?s ?n))]
           [(list ?s '+ 'x '+ ?n) (list 'x '+ (+ ?s ?n))]
           [(list ?s '+ ?n '+ 'x) (list 'x '+ (+ ?s ?n))]
           [(list ?m 'x '+ ?s '+ ?n) (list ?m 'x '+ (+ ?s ?n))]
           [(list ?s '+ ?m 'x '+ ?n) (list ?m 'x '+ (+ ?s ?n))]
           [(list ?s '+ ?n '+ ?m 'x) (list ?m 'x '+ (+ ?s ?n))]
           [(list 'x ?m '+ ?s '+ ?n) (list ?m 'x '+ (+ ?s ?n))]
           [(list ?s '+ 'x ?m '+ ?n) (list ?m 'x '+ (+ ?s ?n))]
           [(list ?s '+ ?n '+ 'x ?m) (list ?m 'x '+ (+ ?s ?n))]
           [(list ?s 'x '+ ?n '+ ?m 'x) (list (+ ?s ?m) 'x '+ ?n)]
           [(list ?s '+ ?n '+ ?a) (list (+ (+ ?s ?n) ?a))]
           [(list ?n 'x '- ?s) (list ?n 'x '- ?s)]
           [(list ?s '- ?n 'x) (list ?n 'x '- ?s)]
           [(list ?n 'x '- 'x) (list (- ?n 1) 'x)]
           [(list 'x '- ?n 'x) (list (- ?n 1) 'x)]
           [(list 'x '- 'x) (list 0)]
           [(list 'x ?n '- 'x) (list (- ?n 1) 'x)]
           [(list ?n 'x '- 'x) (list (- ?n 1) 'x)]
           [(list 'x ?n) (list 'x ?n)]
           [(list ?n 'x) (list ?n 'x)]
           [(list ?s '- ?n) (list (- ?s ?n))]
           [(list 'x '- ?n '- 'x) (list '- ?n)]
           [(list 'x '- 'x '- ?n) (list '- ?n)]
           [(list ?n '- 'x '- 'x) (list '- ?n)]
           [(list 'x '- ?s '- ?n) (list 'x '- (- ?s ?n))]
           [(list ?s '- 'x '- ?n) (list 'x '- (- ?s ?n))]
           [(list ?s '- ?n '- 'x) (list 'x '- (- ?s ?n))]
           [(list ?m 'x '- ?s '- ?n) (list ?m 'x '- (- ?s ?n))]
           [(list ?s '+ ?m 'x '+ ?n) (list ?m 'x '- (- ?s ?n))]
           [(list ?s '+ ?n '+ ?m 'x) (list ?m 'x '- (- ?s ?n))]
           [(list 'x ?m '+ ?s '+ ?n) (list ?m 'x '- (- ?s ?n))]
           [(list ?s '+ 'x ?m '+ ?n) (list ?m 'x '- (- ?s ?n))]
           [(list ?s '+ ?n '+ 'x ?m) (list ?m 'x '- (- ?s ?n))]
           [(list ?s 'x '- ?n '- ?m 'x) (list (- ?s ?m) 'x '- ?n)]
           [(list ?s '- ?n '- ?a) (list (- (- ?s ?n) ?a))]
           [(list ?s '/ ?n) (list (/ ?s ?n))]
           [(list 'x '* ?n) (list ?n 'x)]
           [(list ?n '* 'x) (list ?n 'x)]
           [(list ?s '* ?n) (list (* ?s ?n))]
           [(list 'x '- ?n '+ ?s) (list 'x '+ (- ?s ?n))]
           [(list ?n '- 'x '+ ?s) (list (+ ?n ?s) '- 'x)]
           [(list ?n '- ?s '+ 'x) (list (- ?n ?s) '+ 'x)]
           [(list 'x '+ ?n '- ?s) (list 'x '+ (- ?n ?s))]
           [(list ?n '+ 'x '- ?s) (list 'x '+ (- ?n ?s))]
           [(list ?n '+ ?s '- 'x) (list (+ ?n ?s) '- 'x)]
           [(list ?m 'x '- ?n '+ ?s) (list ?m 'x '+ (- ?s ?n))]
           [(list ?n '- ?m 'x '+ ?s) (list (+ ?n ?s) '- ?m 'x)]
           [(list ?n '- ?s '+ ?m 'x) (list (- ?n ?s) '+ ?m 'x)]
           [(list ?m 'x '+ ?n '- ?s) (list ?m 'x '+ (- ?n ?s))]
           [(list ?n '+ ?m 'x '- ?s) (list ?m 'x '+ (- ?n ?s))]
           [(list ?n '+ ?s '- ?m 'x) (list (+ ?n ?s) '- ?m 'x)]
           [(list 'x '+ ?n '- 'x) (list ?n)]
           [(list 'x '+ 'x '- '?n) (list 2 'x '- ?n)]
           [(list ?s 'x '+ ?n '- 'x) (list (- ?s 1) 'x '+ ?n)]
           [(list 'x '+ ?n '- ?s 'x) (list (- ?s 1) 'x '+ ?n)]
           [(list ?n '+ ?s 'x '- 'x) (list (- ?s 1) 'x '+ ?n)]
           [(list ?n '+ 'x '- ?s 'x) (list (- ?s 1) 'x '+ ?n)])))

(define current-op 'none)

(define (simplify exp)
  (mlist->mstring (simp (mstring->mlist exp))))

(define (simp exp)
  (define (simp-helper exp prvcar currentexp ops-seen)
    (begin
      (if (null? exp)
          (my-flatten (cons (matching currentexp) exp))
          (if (operator? (car exp))
              (if (eq? '* (car exp))
                  (simp (append (append (get-last currentexp) (my-flatten (matching (list prvcar (car exp) (car (cdr exp)))))) (cddr exp)))
                  (if (eq? 3 (+ ops-seen 1))
                      (simp-helper (my-flatten (cons (matching currentexp) exp)) '() '() 0)
                      (simp-helper (cdr exp) (car exp) (append currentexp (list (car exp))) (+ ops-seen 1))))
              (simp-helper (cdr exp) (car exp) (append currentexp (list (car exp))) ops-seen)))))
  (simp-helper  exp '() '() 0))

(define (get-last lst)
  (define (get-last-helper lst last)
    (if (null? (cdr lst))
         last
        (get-last-helper (cdr lst) (append last (list (car lst))))))
  (get-last-helper lst '()))
          
(define (my-flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (my-flatten (car lst)) (my-flatten (cdr lst))))
        (else (list lst))))

(define (mstring->mlist string)
  (define (mstring->mlist-helper string number-string lst)
    (begin
    (if (= (string-length string) 0)
        (if (= (string-length number-string) 0)
            lst
            (append lst (list (string->number number-string))))
        (if (string->number (substring string 0 1))
            (mstring->mlist-helper (substring string 1 (string-length string)) (string-append number-string (substring string 0 1)) lst)
            (if (= (string-length number-string) 0)
                (mstring->mlist-helper (substring string 1 (string-length string)) "" (append lst (list (string->symbol (substring string 0 1)))))
                (mstring->mlist-helper (substring string 1 (string-length string)) "" (append lst (list (string->number number-string)) (list (string->symbol (substring string 0 1)))))))))) 
  (mstring->mlist-helper string "" '()))

(define (mlist->mstring lst)
  (define (mlist->mstring-helper lst string)
    (if (null? lst)
        string
    (if (symbol? (car lst))
        (mlist->mstring-helper (cdr lst)(string-append string (symbol->string (car lst))))
        (mlist->mstring-helper (cdr lst)(string-append string (number->string (car lst)))))))
  (mlist->mstring-helper lst ""))
