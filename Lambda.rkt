#lang racket

; OPL Final Project - Lambda (λ

; Date: 4/21/15

; Programmers:
;  Norman Mutunga
;  Brian Carlson
;  Joshua Caravetta


(require racket/gui/base)
(require plot)
(include "Parser.rkt")

;A nice way to include /Import procedures
;(require rackunit
;         rackunit/log
;         "GUI-sin.rkt")

;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
(define outputString "")
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;main frame for Lambda (λ)
(define frameG (new frame% [label "Lambda (λ)"]
                    [width 600]
                    [height 600]
                    [stretchable-height #t]
                    [stretchable-width #t]
                    (x 400) ( y 100)))

;,,,,,,,,,,,,,,,,,,,,,Input Test Field,,,,,,,,,,,,,,
(define input-field (new text-field%
                         (label "Input")
                         ;[min-width 50]
                         ;(horiz-margin 0)
                         [min-height 30]
                         (parent frameG)
                         ;(init-value "Expression")
                         ;this should return the current text of the editor
                         (callback (λ (input-field event)
                                     ; (send input-window get-text) ;this should work ??                                      
                                     ;(send frameG on-traverse-char #f)
                                     (cond
                                       ; If a user hits enter to compute an equation
                                       ((equal? (send event get-event-type) 'text-field-enter) 
                                        (begin
                                          ; Set outputString to solved equation
                                          (set! outputString (main-parser (send input-field get-value)))
                                          ;(set! outputString (send input-field get-value))
                                          ; Send outputString to output-field
                                          (send output-field set-value outputString)
                                          ))
                                       ;(else (display 2))
                                       )
                                     ;(send input-window erase)
                                     ;(send event get-text)
                                     
                                     ))
                         ))

;,,,,,,,,,,,,,,,,,,,Output Text Filed from Input,,,,,,,,,,,,,,,,,,,,
(define output-field (new text-field%
                          [label "Output"]
                          [min-height 30]
                          (parent frameG)
                          (callback (λ (output-field event)
                                      (cond
                                        ; If a user tries to modify the output text, return it to original
                                        ((equal? (send event get-event-type) 'text-field) (send output-field set-value outputString))
                                        ;(else (display 2))
                                        )
                                      ))))


;; Basic label to display instructions under in/output fields
(define msgG (new message% [parent frameG]
                  [label "Press Enter to compute your input"]))
   
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,>>>
;,,,,,,,,,,,,,,,,,,,,, The graph display canvas,,,,,,,,,,,,,,,,

(define graph-display (new editor-canvas% 
                           [parent frameG]
                           [min-width 400]
                           [min-height 500]))

(define pb (new pasteboard%))
(send graph-display set-editor pb) 

;,,,,,,,,,,,,,,,,,,,,,Vertical Panel,,,,,,,,,,,,,,,,,,,,,,,,
(define main-panel (new vertical-panel% 
                        (parent frameG)
                        (border 0)
                        (alignment '(center center))))


;;,,,,,,,,,,,,,,,,,,,,,Horizontal Panel,,,,,,,,,,,,,,,,,,,,,,,,
;(define main-panel (new horizontal-panel% 
;                        (parent frameG)
;                        (border 0)
;                        (alignment '(center center))))



;; Show Lambda,,,,,,,,
(send frameG show #t)
