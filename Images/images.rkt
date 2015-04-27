
#lang racket
;Norman Mutunga
;Brian Carlson
;Joshua Caravetta
;OPL Final Project © 2015
(require images/compile-time
         (for-syntax images/logos))
(provide the-logo)
;the logo print out on REP
(define the-logo (compiled-bitmap (plt-logo #:height 100)))
