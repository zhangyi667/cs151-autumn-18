#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct Finish
  ([lname : String]
   [fname : String]
   [time-millis : Integer]
   [school : String]
   [grade : Integer]))

(define-struct (Ranked a)
  ([rank : Integer]
   [item : a]))

;; list of top 12 runners is used for some check-expects
(define test-list
  (list
   (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
   (Finish "Kumar" "Jennifer" 552342 "Central" 8)
   (Finish "Jones" "Zelda" 552521 "Pepper" 5)
   (Finish "Rosen" "Marci" 552961 "Pepper" 7)
   (Finish "Anderson" "Pauline" 553188 "West" 7)
   (Finish "Green" "Peggy" 553486 "Central" 5)
   (Finish "Jameson" "Beyonce" 553510 "Mercy" 5)
   (Finish "Jackson" "Patricia" 554045 "West" 5)
   (Finish "Green" "Alexandra" 555005 "Ridgemont" 5)
   (Finish "Williams" "Beyonce" 555783 "Harper" 8)
   (Finish "Brown" "Cecilia" 555877 "West" 5)
   (Finish "Cohen" "Sophia" 557265 "Pepper" 7)))

;; two-digits: adds a 0 if necessary to front of integer to make it 2 digits
(: two-digits (Integer -> String))
(define (two-digits i)
  (if (< i 10)
      (string-append "0" (number->string i))
      (number->string i)))

(check-expect (two-digits 0) "00")
(check-expect (two-digits 5) "05")

;; three-digits: adds 0s if necessary to make integer 3 digits
(: three-digits (Integer -> String))
(define (three-digits i)
  (cond
    [(< i 10) (string-append "00" (number->string i))]
    [(< i 100) (string-append "0" (number->string i))]
    [else (number->string i)]))
  
(check-expect (three-digits 0) "000")
(check-expect (three-digits 5) "005")
(check-expect (three-digits 30) "030")

; Display the time as minutes, seconds, and milliseconds, in the form "9:03.223"
; You should always display three digits past the decimal point.
(: format-time (Integer -> String))
(define (format-time t)
  (string-append (number->string (quotient t 60000))
                 ":"
                 (two-digits (quotient (remainder t 60000) 1000))
                 "."
                 (three-digits (remainder t 1000))))

(check-expect (format-time 360000) "6:00.000")
(check-expect (format-time 600000) "10:00.000")
(check-expect (format-time 550588) "9:10.588")

;;; Check that the list of finishing times is strictly ascending.
(: strictly-ascending-times? ((Listof Finish) -> Boolean))
(define (strictly-ascending-times? list)
  (match list
    ['() #f]
    [(cons head '()) #t]
    [(cons head (cons head1 tail))
     (and (< (Finish-time-millis head) (Finish-time-millis head1))
          (strictly-ascending-times? (cons head1 tail)))]))

(check-expect (strictly-ascending-times? sample-data) #t)
(check-expect (strictly-ascending-times?
               (list
                (Finish "a" "b" 1 "c" 5)
                (Finish "a" "b" 2 "c" 5)
                (Finish "a" "b" 3 "c" 5)
                (Finish "a" "b" 3 "c" 5))) #f)

;; Find the finish for the named person, given last name and first name.
;; Assume no name appears more than once.
;; Return the symbol 'NotFound if the named person is not in the list.
(: find-finish (String String (Listof Finish) -> (U Finish 'NotFound)))
(define (find-finish last first l)
  (match l
    ['() 'NotFound]
    [(cons head tail)
     (if (and (string=? last (Finish-lname head))
              (string=? first (Finish-fname head)))
         head
         (find-finish last first tail))]))

(check-expect (find-finish "Bingham" "Elizabeth" sample-data)
              (Finish "Bingham" "Elizabeth" 572615 "Mercy" 7))
(check-expect (find-finish "random" "name" sample-data) 'NotFound)

;; Return all the finishes from the specified grade.
(: from-grade (Integer (Listof Finish) -> (Listof Finish)))
(define (from-grade i l)
  (match l
    ['() '()]
    [(cons f r) (if (= (Finish-grade f) i)
                    (cons f (from-grade i r))
                    (from-grade i r))]))

(check-expect (from-grade 4 sample-data) '())

;; Return all the finishes from the specified school.
(: from-school (String (Listof Finish) -> (Listof Finish)))
(define (from-school s l)
  (match l
    ['() '()]
    [(cons f r) (if (string=? s (Finish-school f))
                    (cons f (from-school s r))
                    (from-school s r))]))

(check-expect (from-school "Mercy" test-list)
              (list (Finish "Jameson" "Beyonce" 553510 "Mercy" 5)))

;; Return the top n finishes ("top" meaning fastest).
;; Assume the list is in order from fastest to slowest.
;; If there are not that many finishes in the list, return as many as there are.
(: top-n (Integer (Listof Finish) -> (Listof Finish)))
(define (top-n n l)
  (match l
    ['() '()]
    [(cons f r) (if (> n 0)
                    (cons f (top-n (- n 1) r))
                    '())]))

(check-expect (top-n 5 sample-data)
              (list
               (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
               (Finish "Kumar" "Jennifer" 552342 "Central" 8)
               (Finish "Jones" "Zelda" 552521 "Pepper" 5)
               (Finish "Rosen" "Marci" 552961 "Pepper" 7)
               (Finish "Anderson" "Pauline" 553188 "West" 7)))

;; Return the top n finishes from the named school.
;; Assume the list is in order from fastest to slowest.
;; If there are not that many finishes in the list, return as many as there are.
(: top-n-from-school (Integer String (Listof Finish) -> (Listof Finish)))
(define (top-n-from-school n school list)
  (top-n n (from-school school list)))

(check-expect (top-n-from-school 3 "Pepper" sample-data)
              (list
               (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
               (Finish "Jones" "Zelda" 552521 "Pepper" 5)
               (Finish "Rosen" "Marci" 552961 "Pepper" 7)))

;; Return the top n finishes from the specified grade.
;; Assume the list is in order from fastest to slowest.
;; The grade is the second of the two integer arguments.
;; If there are not that many finishes in the list, return as many as there are.
(: top-n-from-grade (Integer Integer (Listof Finish) -> (Listof Finish)))
(define (top-n-from-grade n grade list)
  (top-n n (from-grade grade list)))

(check-expect (top-n-from-grade 3 8 sample-data)
              (list
               (Finish "Kumar" "Jennifer" 552342 "Central" 8)
               (Finish "Williams" "Beyonce" 555783 "Harper" 8)
               (Finish "McBride" "Paula" 557313 "Mercy" 8)))

;; finds the sum of all the times
(: find-sum ((Listof Finish) -> Integer))
(define (find-sum list)
  (match list
    ['() 0]
    [(cons head tail) (+ (Finish-time-millis head) (find-sum tail))]))

(check-expect (find-sum test-list) 6646571)

;; Compute the mean finishing time.
(: mean-time ((Listof Finish) -> Integer))
(define (mean-time list)
  (round (/ (find-sum list) (length list))))

(check-expect (mean-time sample-data) 581952)

;; give-rank : takes an integer for the rank and a list of Finish and returns
;; a list of Ranked Finish
(: give-rank : Integer (Listof Finish) -> (Listof (Ranked Finish)))
(define (give-rank n list)
  (match list
    ['() '()]
    [(cons h t) (cons (Ranked n h) (give-rank (add1 n) t))]))

(check-expect (give-rank 1 (top-n 3 test-list))
              (list
               (Ranked 1 (Finish "Petersen" "Rhea" 550588 "Pepper" 5))
               (Ranked 2 (Finish "Kumar" "Jennifer" 552342 "Central" 8))
               (Ranked 3 (Finish "Jones" "Zelda" 552521 "Pepper" 5))))

;; Assign ranks to every finish in the list, starting at 1.
;; Assume the list is in order from fastest to slowest.
;; Hint: write a helper function that consumes two arguments.
(: rank-race ((Listof Finish) -> (Listof (Ranked Finish))))
(define (rank-race list)
  (give-rank 1 list))
    
(check-expect (rank-race (top-n 3 test-list))
              (list
               (Ranked 1 (Finish "Petersen" "Rhea" 550588 "Pepper" 5))
               (Ranked 2 (Finish "Kumar" "Jennifer" 552342 "Central" 8))
               (Ranked 3 (Finish "Jones" "Zelda" 552521 "Pepper" 5))))

;; same-finish? : compares a finish and a ranked finish and returns true if
;; they refer to the same person
(: same-finish? : Finish (Ranked Finish) -> Boolean)
(define (same-finish? x y)
  (match* (x y)
    [((Finish ln fn _ _ _) (Ranked _ (Finish last first _ _ _)))
                           (and (string=? ln last)
                                (string=? fn first))]))

(check-expect (same-finish?
               (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
               (Ranked 1 (Finish "Petersen" "Rhea" 550588 "Pepper" 5))) #t)

;; get-rank : returns the rank of the runner given the finish
(: get-rank : Finish (Listof (Ranked Finish)) -> Integer)
(define (get-rank finish list)
  (match list
    [(cons h t) (if (same-finish? finish h)
                    (Ranked-rank h)
                    (get-rank finish t))]))

(check-expect (get-rank (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
                        (rank-race sample-data)) 1)
(check-expect (get-rank (Finish "Mattison" "Rhea" 611204 "Laurel" 7)
                        (rank-race sample-data)) 100)

;; sum-ranks: returns the sum of the ranks
(: sum-ranks : (Listof Finish) (Listof (Ranked Finish)) -> Integer)
(define (sum-ranks top3 list)
  (match top3
    ['() 0]
    [(cons h t) (+ (get-rank h list) (sum-ranks t list))]))

(check-expect (sum-ranks test-list (rank-race sample-data)) 78)

;; The team score is the sum of the ranks of the top three finishes.
;; Assume the list is in order from fastest to slowest.
;; If the team has fewer than three finishes in the list, return 'NoScore.
(: team-score (String (Listof Finish) -> (U Integer 'NoScore)))
(define (team-score sch list)
  (match list
    ['() 0]
    [(cons head tail)
     (if (< (length (top-n-from-school 3 sch list)) 3)
         'NoScore
         (sum-ranks (top-n-from-school 3 sch list) (rank-race list)))]))

(check-expect (team-score "Pepper" sample-data) 8)
(check-expect (team-score "Central" sample-data) 22)
(check-expect (team-score "random" sample-data) 'NoScore)

(: sample-data (Listof Finish))
(define sample-data
  (list
   (Finish "Petersen" "Rhea" 550588 "Pepper" 5)
   (Finish "Kumar" "Jennifer" 552342 "Central" 8)
   (Finish "Jones" "Zelda" 552521 "Pepper" 5)
   (Finish "Rosen" "Marci" 552961 "Pepper" 7)
   (Finish "Anderson" "Pauline" 553188 "West" 7)
   (Finish "Green" "Peggy" 553486 "Central" 5)
   (Finish "Jameson" "Beyonce" 553510 "Mercy" 5)
   (Finish "Jackson" "Patricia" 554045 "West" 5)
   (Finish "Green" "Alexandra" 555005 "Ridgemont" 5)
   (Finish "Williams" "Beyonce" 555783 "Harper" 8)
   (Finish "Brown" "Cecilia" 555877 "West" 5)
   (Finish "Cohen" "Sophia" 557265 "Pepper" 7)
   (Finish "McBride" "Paula" 557313 "Mercy" 8)
   (Finish "Rosen" "Pavithra" 557916 "Central" 7)
   (Finish "Bingham" "Paula" 558498 "Laurel" 7)
   (Finish "McBride" "Margaret" 559266 "Central" 5)
   (Finish "Kumar" "Solange" 559525 "Harper" 5)
   (Finish "Strickland" "Cecilia" 560445 "Pepper" 8)
   (Finish "Craig" "Beyonce" 561214 "Harper" 6)
   (Finish "Cohen" "Melissa" 561588 "Central" 5)
   (Finish "Cohen" "Martha" 561907 "Laurel" 6)
   (Finish "Sierpinski" "Arna" 563289 "Harper" 6)
   (Finish "Green" "Pauline" 563466 "Central" 8)
   (Finish "Bingham" "Sally" 564730 "Mercy" 7)
   (Finish "Jones" "Mona" 564842 "Laurel" 7)
   (Finish "Miller" "Mia" 564866 "Harper" 8)
   (Finish "Rosen" "Selena" 566634 "Harper" 7)
   (Finish "Kershaw" "Alice" 568149 "Harper" 5)
   (Finish "Anderson" "Kathy" 568258 "Southeast" 8)
   (Finish "Sellers" "Mia" 568429 "Ridgemont" 5)
   (Finish "Brown" "Samantha" 568941 "West" 7)
   (Finish "Cameron" "Margaret" 570560 "Central" 7)
   (Finish "Jackson" "Peggy" 571115 "Mercy" 6)
   (Finish "Douglas" "Marcia" 571229 "Southeast" 6)
   (Finish "Bingham" "Elizabeth" 572615 "Mercy" 7)
   (Finish "Mandelbrot" "Alexandra" 573950 "Central" 7)
   (Finish "Douglas" "Jennifer" 574276 "Southeast" 7)
   (Finish "Maclean" "Alice" 575320 "Southeast" 6)
   (Finish "Douglas" "Arna" 575839 "Harper" 7)
   (Finish "Maclean" "Claire" 576470 "West" 5)
   (Finish "Miller" "Rebecca" 577740 "Harper" 8)
   (Finish "White" "Martha" 578099 "Ridgemont" 7)
   (Finish "Ramsey" "Beyonce" 580197 "West" 6)
   (Finish "Kershaw" "Arna" 580497 "Laurel" 6)
   (Finish "Maclean" "Rebecca" 580692 "Harper" 6)
   (Finish "Williams" "Adithi" 580849 "Southeast" 6)
   (Finish "Anderson" "Rhea" 581138 "Pepper" 5)
   (Finish "Miller" "Martha" 581324 "Central" 6)
   (Finish "Niven" "Christine" 581720 "Laurel" 5)
   (Finish "Anderson" "Zetta" 582455 "Pepper" 6)
   (Finish "Mandelbrot" "Mia" 582624 "Southeast" 8)
   (Finish "Hopper" "Marci" 582916 "Pepper" 6)
   (Finish "Bingham" "Kathy" 583331 "Harper" 8)
   (Finish "Brown" "Elizabeth" 584290 "Central" 7)
   (Finish "Patel" "Anne" 584533 "Central" 6)
   (Finish "Sellers" "Esther" 586198 "Harper" 5)
   (Finish "Cameron" "Kathy" 586389 "Ridgemont" 8)
   (Finish "Connery" "Rebecca" 586636 "Ridgemont" 7)
   (Finish "Cohen" "Jennifer" 586732 "Harper" 8)
   (Finish "Johnson" "Zelda" 587009 "West" 8)
   (Finish "Bingham" "Anne" 588437 "Pepper" 6)
   (Finish "Peyton" "Rhea" 589620 "Mercy" 7)
   (Finish "Cameron" "Sara" 590284 "West" 6)
   (Finish "Jones" "Samantha" 591396 "Central" 7)
   (Finish "Jameson" "Marci" 591555 "Pepper" 7)
   (Finish "Mattison" "Esther" 592215 "Mercy" 6)
   (Finish "Bingham" "Caroline" 592892 "Ridgemont" 5)
   (Finish "Douglas" "Pavithra" 595182 "Harper" 8)
   (Finish "Ramsey" "Mia" 595465 "Ridgemont" 8)
   (Finish "Maclean" "Elizabeth" 596543 "Ridgemont" 5)
   (Finish "Maclean" "Mona" 597687 "Laurel" 5)
   (Finish "James" "Melissa" 597920 "Mercy" 5)
   (Finish "Cohen" "Selena" 598903 "Laurel" 7)
   (Finish "Craig" "Melissa" 598977 "West" 6)
   (Finish "Hopper" "Patricia" 599633 "Mercy" 7)
   (Finish "Schultz" "Samantha" 599684 "Mercy" 7)
   (Finish "Douglas" "Rachel" 599728 "Ridgemont" 6)
   (Finish "Watts" "Penelope" 600465 "Southeast" 7)
   (Finish "James" "Mona" 601092 "Southeast" 7)
   (Finish "Shapiro" "Jennifer" 601150 "Ridgemont" 5)
   (Finish "Narayan" "Pavithra" 601491 "Ridgemont" 6)
   (Finish "Williams" "Alice" 602112 "Central" 7)
   (Finish "Craig" "Arna" 602230 "Harper" 6)
   (Finish "Ramsey" "Solange" 602879 "Southeast" 5)
   (Finish "Johnson" "Sophia" 603051 "Laurel" 8)
   (Finish "Kershaw" "Sally" 603332 "Laurel" 6)
   (Finish "Black" "Sally" 603401 "West" 8)
   (Finish "Schultz" "Marcia" 603521 "Harper" 5)
   (Finish "Brown" "Rachel" 603918 "Ridgemont" 5)
   (Finish "Moore" "Elizabeth" 604221 "Laurel" 7)
   (Finish "Watts" "Cecilia" 605602 "Pepper" 7)
   (Finish "Hopkinson" "Zetta" 606148 "West" 5)
   (Finish "McBride" "Selena" 606932 "Harper" 5)
   (Finish "Kumar" "Sophia" 607778 "Ridgemont" 8)
   (Finish "Cohen" "Samantha" 608190 "Pepper" 5)
   (Finish "Miller" "Mona" 608326 "Southeast" 6)
   (Finish "Ballard" "Shreya" 609172 "West" 7)
   (Finish "Bingham" "Rachel" 609177 "Mercy" 8)
   (Finish "Petersen" "Pavithra" 611143 "Laurel" 7)
   (Finish "Mattison" "Rhea" 611204 "Laurel" 7)))

(test)