#lang racket
;Norman Mutunga
;Brian Carlson
;Joshua Caravetta
;FP4
(require racket/gui/base)
(require plot)
;A nice way to include /Import procedures
;(require rackunit
;         rackunit/log
;         "GUI-sin.rkt")
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
(define stringInput "" )
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;main frame for Goat Lambda (λ)
(define frameG (new frame% [label "Goat Lambda (λ)"]
                    [width 500]
                    [height 500]
                    [stretchable-height #t]
                    [stretchable-width #t]
                    (x 400) ( y 100)))

;,,,,,,,,,,,,,,,,,,,,,InPut Test Field,,,,,,,,,,,,,,
(define input-window (new text-field%
                          (label "UserInPut")
                          ;[min-width 50]
                          ;(horiz-margin 0)
                          [min-height 30]
                          (parent frameG)
                          ;(init-value "Expression")
                          ;this should return the current text of the editor
                          (callback (λ (input-window event)
                                      (send event get-event-type)
                                      ; (send event get-text)
                                      ;(send event get-text)
                                      ))
                          ))
;,,,,,,,,,,,,,,,,,,,OutPut Text Filed from Input,,,,,,,,,,,,,,,,,,,,
(define out-put (new text-field%
                     [label "InPutExpr"]
                     [min-height 30]
                     (parent frameG)
                     (callback (λ ( out-put text-field )
                                 (send  (send text-field get-editor) get-text )))))
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;(define out-put2 (new text-field%
;                     [label "UserInPut2"]
;                     (parent frameG)
;                     (callback (λ ( out-put event)
;                                  ;; Make the editor have a maximum width:
;                                 (send  (send out-put2 get-editor) auto-wrap #f)
;                                  ;; Keep the caret visible:
;                                 (send (send out-put2 get-editor ) set-padding 0 0 2 0)
;                                  ;; Right-align the first paragraph:
;                                 (send (send out-put2 get-editor) set-paragraph-alignment 500 'right)))))
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;basic label to display on top of Compute Button....
(define msgG (new message% [parent frameG]
                  [label "Click the Compute Button"]))

;; Button to be clicked to triger an event.
;it creates this on REP : (object:button% ...)
(new button% [parent frameG]
     [label "Compute"]
     [callback (λ (button event)
                 (send msgG set-label "Graphical OutPut")
                 ;(send text-field get-text)
                 (send pb set-dragable #f)
                 (send pb erase)
                 (send pb insert (plot3d-snip
                                  (surface3d (λ
                                                 (x y) (* 1 (sin x)))
                                             (- 10) 10 (- 10) 10)
                                  #:title "sin(x)"
                                  #:x-label "x" #:y-label "y" #:z-label "sin(x)") 0 0))])
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,>>>
;,,,,,,,,,,,,,,,,,,,,, The graph display canvas,,,,,,,,,,,,,,,,

(define graph-display (new editor-canvas% [parent frameG]))
(define pb (new pasteboard%)) ;from joshua
(define t (new text%)) ;from joshua
(send graph-display set-editor pb) ;from joshua

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



;;show Goat Lambda,,,,,,,,
(send frameG show #t)
