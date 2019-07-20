#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct FlightHistory
  ([segments : Integer]
   [miles    : Integer]))

(define-struct Seats
  ([first    : Integer]
   [business : Integer]
   [coach    : Integer]))

;; negative?: helper function returns true if Seats contains negative number
(: negative? : Seats -> Boolean)
(define (negative? seats)
  (or (< (Seats-first seats) 0)
      (< (Seats-business seats) 0)
      (< (Seats-coach seats) 0)))

(check-expect (negative? (Seats 0 9 -1)) #t)
(check-expect (negative? (Seats 20 20 20)) #f)

;; assign-plane: generates recommended plane to fit given number of passengers
(: assign-plane : Integer -> String)
(define (assign-plane people)
  (string-append
   "Recommendation: "
   (cond
     [(< people 0) (error "assign-plane: cannot have negative passengers")]
     [(= people 0) "cancel flight"]
     [(<= people 128) "319"]
     [(<= people 150) "320"]
     [(<= people 166) "737-800"]
     [(<= people 179) "737-900"]
     [(<= people 213) "757"]
     [(<= people 242) "767"]
     [(<= people 366) "777"]
     [else "split flights"])))
 
(check-error (assign-plane -10) "assign-plane: cannot have negative passengers")
(check-expect (assign-plane 0) "Recommendation: cancel flight")
(check-expect (assign-plane 200) "Recommendation: 757")
(check-expect (assign-plane 400) "Recommendation: split flights")

;; miles-earned: given number of flights and miles in the past year,
;; determine membership and calculate number of miles to be credited
(: miles-earned : FlightHistory -> Integer)
(define (miles-earned p)
  (cond
    [(or (< (FlightHistory-segments p) 0)
         (< (FlightHistory-miles p) 0))
     (error "miles-earned: negative input given")]
    [(or (>= (FlightHistory-segments p) 50)
         (>= (FlightHistory-miles p) 70000))
     (* 2 (FlightHistory-miles p))]
    [(or (>= (FlightHistory-segments p) 25)
         (>= (FlightHistory-miles p) 35000))
     (exact-ceiling (* 5/4 (FlightHistory-miles p)))]
    [else (FlightHistory-miles p)]))

(check-error (miles-earned (FlightHistory -10 0))
             "miles-earned: negative input given")
(check-expect (miles-earned (FlightHistory 20 10000)) 10000)
(check-expect (miles-earned (FlightHistory 25 10000)) 12500)
(check-expect (miles-earned (FlightHistory 20 75000)) 150000)

;; passengers: counts the total number of passengers on a flight
(: passengers : Seats -> Integer)
(define (passengers pl)
  (cond
    [(negative? pl) (error "passengers: negative input given")]
    [else (+ (Seats-first pl)
             (Seats-business pl)
             (Seats-coach pl))]))

(check-error (passengers (Seats -9 30 20))
              "passengers: negative input given")
(check-expect (passengers (Seats 30 90 20)) 140)

;; combine-flights: combine number of passengers in respective classes
(: combine-flights : Seats Seats -> Seats)
(define (combine-flights p1 p2)
  (cond
    [(or (negative? p1) (negative? p2))
     (error "combine-flights: negative input given")]
    [else (Seats (+ (Seats-first p1) (Seats-first p2))
                 (+ (Seats-business p1) (Seats-business p2))
                 (+ (Seats-coach p1) (Seats-coach p2)))]))

(check-error (combine-flights (Seats 30 20 10) (Seats 40 50 -1))
             "combine-flights: negative input given")
(check-expect (combine-flights (Seats 30 90 20)(Seats 20 110 30))
              (Seats 50 200 50))

;; can-fit?: given the capacity of an aircraft and number of tickets sold
;; in each class, determines if the specified aircraft can seat all the
;; passengers in their desired class of seats
(: can-fit? : Seats Seats -> Boolean)
(define (can-fit? cap sold)
  (cond
    [(or (negative? cap) (negative? sold))
     (error "can-fit?: negative input given")]
    [else (and (<= (Seats-first sold) (Seats-first cap))
               (<= (Seats-business sold) (Seats-business cap))
               (<= (Seats-coach sold) (Seats-coach cap)))]))

(check-error (can-fit? (Seats -20 150 20) (Seats 19 150 10))
             "can-fit?: negative input given")
(check-expect (can-fit? (Seats 20 150 20) (Seats 19 150 10)) #t)
(check-expect (can-fit? (Seats 20 150 20) (Seats 30 200 20)) #f)

;; max-premium?: true if all premium seats have been sold on a given aircraft
(: max-premium? : Seats Seats -> Boolean)
(define (max-premium? cap sold)
  (cond
    [(or (negative? cap) (negative? sold))
     (error "max-premium?: negative input given")]
    [else (and (= (Seats-first sold) (Seats-first cap))
               (= (Seats-business sold) (Seats-business cap)))]))

(check-error (max-premium? (Seats 20 150 20) (Seats 20 -1 10))
             "max-premium?: negative input given")
(check-expect (max-premium? (Seats 20 150 20) (Seats 20 150 10)) #t)
(check-expect (max-premium? (Seats 20 150 20) (Seats 10 150 20)) #f)

;; upgrade: if first class is not full, move business class passengers to full
;; if first class is still not full, move coach passengers to first class
;; if business class is not full, move coach to business
(: upgrade : Seats Seats -> Seats)
(define (upgrade cap sold)
  (cond
    [(or (negative? cap) (negative? sold))
     (error "upgrade: negative input given")]
    ;; no seat changes
    [(max-premium? cap sold) sold]
    ;; moves everyone to first class, other classes empty
    [(<= (passengers sold) (Seats-first cap)) (Seats (passengers sold) 0 0)]
    ;; fills first and business class and puts leftover in coach
    [(>= (- (passengers sold) (Seats-first cap) (Seats-business cap)) 0)
     (Seats (Seats-first cap)
            (Seats-business cap)
            (- (passengers sold) (Seats-first cap) (Seats-business cap)))]
    ;; fills first class and puts leftover in business, coach is empty
    [else (Seats (Seats-first cap) (- (passengers sold) (Seats-first cap)) 0)]))

(check-error (upgrade (Seats 100 100 100) (Seats 100 0 -9))
             "upgrade: negative input given")
(check-expect (upgrade (Seats 100 100 100) (Seats 0 90 60)) (Seats 100 50 0))
(check-expect (upgrade (Seats 100 100 100) (Seats 80 70 60)) (Seats 100 100 10))
(check-expect (upgrade (Seats 100 100 100) (Seats 50 40 30)) (Seats 100 20 0))
(check-expect (upgrade (Seats 100 100 100) (Seats 30 20 10)) (Seats 60 0 0))

;; seats->letters: prints out a string of Fs Bs and Cs, each letter
;; corresponding to a passenger in the respective class
(: seats->letters : Seats -> String)
(define (seats->letters roster)
  (cond
    [(negative? roster)
     (error "seats->letters: negative input given")]
    [else (string-append
           (make-string (Seats-first roster) #\F)
           (make-string (Seats-business roster) #\B)
           (make-string (Seats-coach roster) #\C))]))

(check-error (seats->letters (Seats 8 -9 12))
             "seats->letters: negative input given")
(check-expect (seats->letters (Seats 1 2 3)) "FBBCCC")
(check-expect (seats->letters (Seats 7 6 5)) "FFFFFFFBBBBBBCCCCC")
   
(test)