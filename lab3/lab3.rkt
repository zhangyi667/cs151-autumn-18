#lang typed/racket
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(: dt Positive-Real)
(define dt 1/30)

(define-struct Ball
  ([y-position : Real] ;; the height of the ball's center in meters, ground y=0
   [radius     : Integer] ;; in meters
   [velocity   : Real] ;; in meters per second
   [elasticity : Real] ;; a number between 0 and 1
   [color      : Image-Color]))

(define-struct World
  ([ball1 : Ball]
   [ball2 : Ball]
   [ball3 : Ball]
   [background : Image]
   [time-elapsed : Real]))

;; make-circle: helper function to draw circle given Ball
(: make-circle (Ball -> Image))
(define (make-circle b)
  (circle (Ball-radius b) "solid" (Ball-color b)))

"eyeball test"
(make-circle (Ball 100 10 5 1 "pink"))

;; y-position: returns corrected y-value so y=0 is the ground
(: y-position (Image Ball -> Real))
(define (y-position image b)
  (- (image-height image) (Ball-y-position b) (Ball-radius b)))

(check-expect (y-position (square 200 "solid" "silver")
                          (Ball 190 10 5 1 "red")) 0)

;; draw: turns the world into an image with three balls, the background
;; image, the timer in seconds at top, and heights in meters below the timer
(: draw (World -> Image))
(define (draw w)
  (match w
    [(World b1 b2 b3 image time)
     (above
      (text (number->string (exact-floor time)) 30 "black")
      (text (string-append
             (number->string (exact-ceiling (Ball-y-position b1)))
             "          "
             (number->string (exact-ceiling (Ball-y-position b2)))
             "          "
             (number->string (exact-ceiling (Ball-y-position b3)))) 20 "black")
      (place-image
       (make-circle b1)
       (exact-ceiling (* 1/4 (image-width image)))
       (y-position image b1)
       (place-image
        (make-circle b2)
        (exact-ceiling (* 1/2 (image-width image)))
        (y-position image b2)
        (place-image
         (make-circle b3)
         (exact-ceiling (* 3/4 (image-width image)))
         (y-position image b3)
         image))))]))

"another eyeball test"
(draw (World (Ball 100 10 0 0.9 "red")
             (Ball 100 15 0 0.8 "yellow")
             (Ball 100 20 0 0.7 "green")
             (rectangle 300 200 "solid" "silver") 0))

;; new-velocity: calculates the ball's new velocity after a bounce
(: new-velocity (Ball -> Real))
(define (new-velocity b)
  (max 0 (- (* (- (Ball-elasticity b)) (Ball-velocity b)) 0.5)))

(check-within (new-velocity (Ball 200 10 -10 1 "red")) 9.5 0.00001)

;; next-location: calculates the ball's next y-position
(: next-location (Ball -> Real))
(define (next-location b)
  (+ (Ball-y-position b) (* (Ball-velocity b) dt)))

(check-within (next-location (Ball 10 10 30 1 "red")) 11 0.0001)

;; velocity-change: ball's new velocity while accelerating
(: velocity-change (Ball -> Real))
(define (velocity-change b)
  (+ (Ball-velocity b) (* dt (- 9.8))))

(check-within (velocity-change (Ball 90 10 30 1 "red")) 29.67 0.1)

;; tick-ball: for every tick, calculates a new position and velocity for
;; all three balls
(: tick-ball (Ball -> Ball))
(define (tick-ball b)
  (match b
    [(Ball y r v e c)
     (cond
       ;; ball is below ground
       [(<= y 0)
        (Ball (* dt (new-velocity b)) r (new-velocity b) e c)] 
       ;; if ball is still below ground, set y-value to 0
       [(< (next-location b) 0)
        (Ball 0 r (velocity-change b) e c)] 
       ;; ball is falling or rising normally due to gravity
       [else
        (Ball (next-location b) r (velocity-change b) e c)])]))

(check-within (tick-ball (Ball -3 10 30 1 "red")) (Ball 0 10 0 1 "red") 0.0001)
(check-within (tick-ball (Ball -1 10 -5 1 "red")) (Ball 0.15 10 4.5 1 "red") 0.001)
(check-within (tick-ball (Ball 300 10 -30 1 "red")) (Ball 299 10 -30.33 1 "red") 0.01)

;; tick: increases the time by dt seconds after each tick and calls tick-ball
;; on each ball
(: tick (World -> World))
(define (tick w)
  (match w
    [(World b1 b2 b3 image time)
     (World (tick-ball b1) (tick-ball b2) (tick-ball b3) image (+ dt time))]))

(check-within (tick (World (Ball 100 20 0 0.9 "red")
                           (Ball 100 20 0 0.8 "yellow")
                           (Ball 100 20 0 0.7 "green")
                           (rectangle 600 500 "solid" "black") 0))
              (World (Ball 100 20 -0.33 0.9 "red")
                     (Ball 100 20 -0.33 0.8 "yellow")
                     (Ball 100 20 -0.33 0.7 "green")
                     (rectangle 600 500 "solid" "black") 1/30) 0.1)

;; run-simulation: runs program using big-bang given ball inputs and image
;; for the world
(: run-simulation (Ball Ball Ball Image -> World))
(define (run-simulation b1 b2 b3 image)
  (big-bang (World b1 b2 b3 image 0) : World
    [to-draw draw]
    [on-tick tick]))

(run-simulation
 (Ball 200 10 0 0.7 "darkred")
 (Ball 200 10 0 0.8 "orange")
 (Ball 200 10 0 0.6 "darkblue")
 (above (rectangle 200 300 "solid" "dodgerblue")
        (rectangle 200  40 "solid" "green")))

(test)