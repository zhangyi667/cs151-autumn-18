#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
; This is a very simple line that can auto-twist.

(define-struct Line
 (
	[curv : Real]	
	[direction : (U 'incr 'decr)]
 )
)

(define WIDTH 100)
(define MAX 2)

(: draw (Line -> Image))
(define (draw w)
 (match w
 [(Line c d) (add-curve (rectangle WIDTH WIDTH "solid" "black")
             (* 0.2 WIDTH) (* 0.2 WIDTH) 0 c
             (* 0.8 WIDTH) (* 0.8 WIDTH) 0 c
             "white")]))

(: tick (Line -> Line))
(define (tick l)
	(match l
		[(Line c 'incr) (if (> c MAX) (Line MAX 'decr) (Line (+ 0.01 c) 'incr))]
		[(Line c 'decr) (if (< c 0) (Line 0 'incr) (Line (- c 0.01) 'decr))]
	)
)


(: run (Integer -> Line))
(define (run c)
 (big-bang (Line c 'incr) : Line
	[to-draw draw]
	[on-tick tick 1/20]
 )
)

(run 0)