#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

;; nm->km: converts a measurement in nautical miles to kilometers
(: nm->km (Exact-Rational -> Exact-Rational))
(define (nm->km x)
  (* 1852/1000 x))

(check-within (nm->km 10) 18.52 0.00001)
(check-within (nm->km 99) (* 99 1852/1000) 0.00001)

;; km->nm: converts a measurement in kilometers to nautical miles
(: km->nm (Exact-Rational -> Exact-Rational))
(define (km->nm x)
  (* 1000/1852 x))

(check-within (km->nm 10) (/ 10000 1852) 0.0001)
(check-within (km->nm 1852) 1000 0.00001)

;; coins->cents: computes total number of cents given quarters,
;; dimes, nickels, and pennies
(: coins->cents (Integer Integer Integer Integer -> Integer))
(define (coins->cents q d n p)
  (+ (* 25 q) (* 10 d) (* 5 n) p))

(check-expect (coins->cents 1 1 1 1) 41)
(check-expect (coins->cents 100 0 0 0) 2500)

;; eval-quad: given the 3 coefficients of a quadratic equation and
;; the x value, compute the corresponding y value on the parabola
(: eval-quad (Real Real Real Real -> Real))
(define (eval-quad a b c x)
  (+ (* a x x) (* b x) c))

(check-within (eval-quad 3 2 1 8) 209 0.00001)
(check-within (eval-quad 0.1 0.1 0.1 0.1) 0.111 0.00001)

;; distance-fallen: given the number of seconds that an object has been in
;; free-fall, calculate how far the object has traveled in meters
(: distance-fallen (Real -> Real))
(define (distance-fallen t)
  (* 1/2 98/10 t t))

(check-within (distance-fallen 10) 490 0.00001)
(check-within (distance-fallen 9.5) 442.225 0.00001)

;; seconds-to-fall: given how far an object has fallen in meters,
;; calculate how long the object has been in free-fall
(: seconds-to-fall (Real -> Real))
(define (seconds-to-fall d)
  (sqrt (* d 10/49)))

(check-within (seconds-to-fall 490) 10 0.00001)
(check-within (seconds-to-fall 442.225) 9.5 0.00001)

;; majority?: takes a fraction between 0 and 1 and returns true
;; if that fraction is a majority, or more than half
(: majority? (Real -> Boolean))
(define (majority? x)
  (cond
    [(<= x 1/2) #f]
    [else #t]))

(check-expect (majority? 0.5) #f)
(check-expect (majority? 0.8) #t)
(check-expect (majority? 0.3) #f)

;; liquid-water?: takes in a Fahrenheit temperature and returns
;; true if the water is liquid. Note: substance at transition point
;; is not considered in the liquid phase
(: liquid-water? (Real -> Boolean))
(define (liquid-water? x)
  (cond
    [(or (<= x 32) (>= x 212)) #f]
    [else #t]))

(check-expect (liquid-water? 32) #f)
(check-expect (liquid-water? 90) #t)
(check-expect (liquid-water? 213) #f)

;; above-quad?: takes five inputs from user, three coefficients
;; of a quadratic equation and the values of x and y of a coordinate
;; and returns true if the point lies strictly above the parabola
(: above-quad? (Real Real Real Real Real -> Boolean))
(define (above-quad? a b c x y)
  (cond
    [(> y (eval-quad a b c x)) #t]
    [else #f]))

(check-expect (above-quad? 0.1 0.1 0.1 0.1 0.111) #f)
(check-expect (above-quad? 0.1 0.1 0.1 0.1 0.1111) #t)

;; midpt-x: computes the x value of the midpoint of two points
;; midpt-y: computes the y value of the midpoint of two points
(: midpt-x (Real Real Real Real -> Real))
(define (midpt-x x1 y1 x2 y2)
  (/ (+ x1 x2) 2))
(: midpt-y (Real Real Real Real -> Real))
(define (midpt-y x1 y1 x2 y2)
  (/ (+ y1 y2) 2))

(check-within (midpt-x 5 1 -1 -5) 2 0.0001)
(check-within (midpt-y 5 1 -1 -5) -2 0.0001)

;; cars-needed: given a number of people and bikes, with each car holding
;; a maximum of 4 people and 2 bikes, calculate min number of cars needed
(: cars-needed : Integer Integer -> Integer)
(define (cars-needed people bikes)
  (exact-ceiling (max (/ people 4) (/ bikes 2))))
    
(check-expect (cars-needed 20 17) 9)
(check-expect (cars-needed 15 3) 4)

;; cars-pairs: rounds up to the next smallest even number of cars needed
(: cars-pairs : Integer Integer -> Integer)
(define (cars-pairs people bikes)
  (cond
    [(= 1 (remainder (cars-needed people bikes) 2))
     (+ 1 (cars-needed people bikes))]
    [else (cars-needed people bikes)]))

(check-expect (cars-pairs 20 17) 10)
(check-expect (cars-pairs 15 3) 4)

(test)