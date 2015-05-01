#lang racket

; OPL Final Project - Lambda (λ)

; Date: 4/21/15

; Programmers:
;  Norman Mutunga
;  Brian Carlson
;  Joshua Caravetta


(require racket/gui/base)
(require plot)
(require racket/include)
(require  "Images/images.rkt")
(include "Backend/Parser.rkt")

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
                         [min-width 600]
                         [min-height 30]
                         [stretchable-width #f]
                         (parent frameG)
                         ;(init-value "Expression")
                         ;this should return the current text of the editor
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
                                          (send output-field set-value outputString))))))))

;,,,,,,,,,,,,,,,,,,,Output Text Filed from Input,,,,,,,,,,,,,,,,,,,,
(define output-field (new text-field%
                          [label "Output"]
                          [min-width 600]
                          [min-height 30]
                          [stretchable-width #f]
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
                           [min-width 900]
                           [min-height 500]
                           [stretchable-width #f]
                           [stretchable-height #f]
                           [style '(transparent auto-hscroll auto-vscroll)]))

(define pb (new pasteboard%))
(send graph-display set-editor pb) 

;,,,,,,,,,,,,,,,,,,,,,Vertical Panel,,,,,,,,,,,,,,,,,,,,,,,,
(define main-panel (new vertical-panel% 
                        (parent frameG)
                        (border 0)
                        (alignment '(center center))))


;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,GoatLambda borrowed logo on REP.
;http://docs.racket-lang.org/images/Embedding_Bitmaps_in_Compiled_Files.html?q=images

the-logo
(display "Lambda 
;Norman Mutunga
;Brian Carlson
;Joshua Caravetta
;OPL Final Project © 2015")

;; Show Lambda GUI
(send frameG show #t)
