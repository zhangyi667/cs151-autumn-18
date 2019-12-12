#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")

(define TOTAL-ROW-OF-MONTH 6)

(define-struct Date
 ([m : Integer]
 [d : Integer]
 [y : Integer]))

(define-struct CalendarFormat
  ([cell-size : Integer]
   [banner-bg-color : Image-Color]
   [banner-text-color : Image-Color]
   [day-bg-color : Image-Color]
   [day-text-color : Image-Color]
   [border-color : Image-Color]
   [number-color : Image-Color]))

(: draw-banner-text (Image-Color Integer Integer -> Image))
; to draw the text part of each month title
(define (draw-banner-text tc m y)
  (beside
    (match m
       [ 1 (text "January" 18 tc)]
       [ 2 (text "February" 18 tc)]
       [ 3 (text "March" 18 tc)]
       [ 4 (text "April" 18 tc)]
       [ 5 (text "May" 18 tc)]
       [ 6 (text "June" 18 tc)]
       [ 7 (text "July" 18 tc)]
       [ 8 (text "August" 18 tc)]
       [ 9 (text "September" 18 tc)]
       [ 10 (text "October" 18 tc)]
       [ 11 (text "November" 18 tc)]
       [ 12 (text "December" 18 tc)])
    (text (string-append " " (number->string y)) 18 tc)))

(: draw-banner (Image-Color Image-Color Integer Integer Integer -> Image))
; draw the banner of each month
(define (draw-banner bgc tc m y size)
  (overlay
   (draw-banner-text tc m y)
   (rectangle (* 7 size) size "solid" bgc)))

(: draw-day (Image-Color Image-Color Integer -> Image))
; draw the row of weekday label
(define (draw-day bgc tc size)
  (beside
   (overlay
    (text "SUN" 18 tc)
   (square size "solid" bgc))
    (overlay
    (text "MON" 18 tc)
   (square size "solid" bgc))
    (overlay
    (text "TUE" 18 tc)
   (square size "solid" bgc))
    (overlay
    (text "WED" 18 tc)
   (square size "solid" bgc))
    (overlay
    (text "THU" 18 tc)
   (square size "solid" bgc))
    (overlay
    (text "FRI" 18 tc)
   (square size "solid" bgc))
    (overlay
    (text "SAT" 18 tc)
   (square size "solid" bgc))))

(: leap? (-> Integer Boolean))
; to test if a year is a leap year (from lab2)
(define (leap? y)
  (or
   (and (= (remainder y 4) 0) (not (= (remainder y 100) 0)))
   (= (remainder  y 400) 0)))

(check-expect (leap? 1999) #f)
(check-expect (leap? 2000) #t)

(: month-adjustment (-> Integer Integer Integer))
(define (month-adjustment m y)
  (cond
     [(= 1 m) (if (leap? y) 0 1)]
     [(= 2 m) (if (leap? y) 3 4)]
     [(= 3 m) 4]
     [(= 4 m) 0]
     [(= 5 m) 2]
     [(= 6 m) 5]
     [(= 7 m) 0]
     [(= 8 m) 3]
     [(= 9 m) 6]
     [(= 10 m) 1]
     [(= 11 m) 4]
     [(= 12 m) 6]
     [else 0]))

(check-expect (month-adjustment 1 2000) 0)
(check-expect (month-adjustment 1 2001) 1)
(check-expect (month-adjustment 2 2000) 3)
(check-expect (month-adjustment 2 2001) 4)
(check-expect (month-adjustment 5 2004) 2)

(: date->weekday (-> Date Integer))
; a formula that can help us determine the weekday of a date (from lab2)
(define (date->weekday date)
  (remainder (+ (- (Date-y date) 1900)
                (month-adjustment (Date-m date) (Date-y date))
                (Date-d date) (exact-floor (/ (Date-y date) 4))) 7))

(check-expect (date->weekday (Date 1 18 2019)) 5)
(check-expect (date->weekday (Date 1 25 2019)) 5)

(: day-in-month : Integer Integer -> Integer)
(define (day-in-month m y)
  (match m
    [2 (if (leap? y) 29 28)]
    [(or 1 3 5 7 8 10 12) 31]
    [else 30]))

(check-expect (day-in-month 2 2000) 29)
(check-expect (day-in-month 12 2000) 31)
    
(: yesterday (Date -> Date))
(define (yesterday date)
  (match date
    [(Date 1 1 y) (Date 12 31 (- y 1))]
    [(Date m 1 y) (Date (- m 1) (day-in-month (- m 1) y) y)]
    [(Date m d y) (Date  m (- d 1) y)]))

(check-expect (yesterday (Date 12 1 2019)) (Date 11 30 2019))
(check-expect (yesterday (Date 1 1 2019)) (Date 12 31 2018))
(check-expect (yesterday (Date 3 1 2000)) (Date 2 29 2000))

(: tomorrow (Date -> Date))
;given a date,return the date of tomorrow (from lecturer)
(define (tomorrow date)
  (match date
    [(Date 2 29 y) (Date 3 1 y)]
    [(Date 2 28 y) (if (leap? y) (Date 2 29 y) (Date 3 1 y))]
    [(Date 12 31 y) (Date 1 1 (+ 1 y))]
    [(Date 1 31 y) (Date 2 1 y)]
    [(Date 3 31 y) (Date 4 1 y)]
    [(Date 5 31 y) (Date 6 1 y)]
    [(Date 7 31 y) (Date 8 1 y)]
    [(Date 8 31 y) (Date 9 1 y)]
    [(Date 10 31 y) (Date 11 1 y)]
    [(Date 4 30 y) (Date 5 1 y)]
    [(Date 6 30 y) (Date 7 1 y)]
    [(Date 9 30 y) (Date 10 1 y)]
    [(Date 11 30 y) (Date 12 1 y)]
    [(Date m d y) (Date m (+ 1 d) y)]))

(check-expect (tomorrow (Date 12 31 2019)) (Date 1 1 2020))
(check-expect (tomorrow (Date 2 29 2000)) (Date 3 1 2000))
                              
(: add-days (-> Integer Date Date))
;given a date and an integer i, return the date after i days (from lecturer)
(define (add-days i date)
  (cond
    [(positive? i) (add-days (sub1 i) (tomorrow date))]
    [(negative? i) (add-days (add1 i) (yesterday date))]
    [else date]))

(check-expect (add-days 5 (Date 1 4 2019)) (Date 1 9 2019))
(check-expect (add-days 2 (Date 2 29 2000)) (Date 3 2 2000))
(check-expect (add-days 3 (Date 12 31 2019)) (Date 1 3 2020))

(: date-to-row : Date Image-Color Image-Color Integer Integer -> Image)
;given a date, assume it's sunday on that date, return the row on calender
;include that date, let the date dose not belong to this month be "white text"
(define (date-to-row date tc bc size m)
  (draw-row-by-nst-date date 0 tc bc size m)
)

(: draw-row-by-nst-date : Date Integer Image-Color Image-Color Integer Integer -> Image)
;recursively calling the method to draw in total 7 images and merge them into one
(define (draw-row-by-nst-date date num tc bc size origin-month)
  (match* (date num)
    [(_ 7) empty-image]
    [((Date current-month d year) num) 
    (beside
      (if (= current-month origin-month)
        (draw-cell d size tc bc)
        (draw-cell -1 size tc bc)
      )
      (draw-row-by-nst-date (add-days 1 date) (add1 num) tc bc size origin-month)
    )
    ]
  )
)

(: draw-cell : Integer Integer Image-Color Image-Color -> Image)
;draw a cell of a single day
(define (draw-cell n size tc bc)
  (match n
    [-1 (overlay (text "" 24 "darkgray") (square size "solid" bc))]
    [_ (overlay (text (number->string n) 24 tc) (square size "solid" bc))]
    )
  )    
 
;(date-to-row (Date 11 3 2019) "black" "blue" 50 11)
;(date-to-row (Date 10 27 2019) "black" "blue" 50 11)

(: draw-row : Date Integer Image-Color Image-Color Integer -> Image)
;to draw the real rows on a calender, becasue last function we assume
;the input date is Sunday, which is not always true
(define (draw-row date m tc bc size)
  (match (date->weekday date)
    [num (if (or (< num 0) (> num 6)) 
          empty-image 
          (date-to-row (add-days (- 0 num) date) tc bc size m)
        )
    ]
  )
)

; deprecated: now use TOTAL-ROW-OF-MONTH as 6 row hard coded
(: total-row-of-month : Integer Integer -> Integer)
;return how many rows in total of a month
(define (total-row-of-month m y)
  (cond
    [(and (leap? y) (= m 2) (= (date->weekday (Date m 1 y)) 0)) 4]
    [(and (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12)) (= (date->weekday (Date m 31 y)) 0)) 6]
    [(and (not (= m 2)) (= (date->weekday (Date m 30 y)) 0)) 6]
    [else 5]
  )
)

(: draw-month-cell-helper : Date Integer Integer Image-Color Image-Color Integer -> Image)
;to adjust the rows of each month, for example in some months there should be
;only five rows,so we should let the last row be "white boxes".
(define (draw-month-cell-helper date row-count m tc bc size)
  (match row-count
    [0 empty-image]
    [_ (above 
      (draw-row date m tc bc size) 
      (draw-month-cell-helper (add-days 7 date) (- row-count 1) m tc bc size))
    ]
  )
)

(: get-sunday-by-date : Date -> Date)
;returns the sunday of that week where the input date is in
(define (get-sunday-by-date date)
  (add-days (- 0 (date->weekday date)) date)
)

(check-expect (get-sunday-by-date (Date 11 4 2019)) (Date 11 3 2019))
(check-expect (get-sunday-by-date (Date 11 2 2019)) (Date 10 27 2019))
(check-expect (get-sunday-by-date (Date 10 27 2019)) (Date 10 27 2019))

(: draw-month-cell : Integer Integer Image-Color Image-Color Integer -> Image)
;to adjust the rows of each month, for example in some months there should be
;only five rows,so we should let the last row be "white boxes".
(define (draw-month-cell m y tc bc size)
  (draw-month-cell-helper (get-sunday-by-date (Date m 1 y)) TOTAL-ROW-OF-MONTH m tc bc size)  
)

(: draw-month : CalendarFormat Integer Integer -> Image)
;finally draw the calendar for a given month
(define (draw-month cf m y)
  (above
   (draw-banner (CalendarFormat-banner-bg-color cf)
                (CalendarFormat-banner-text-color cf) m y
                (CalendarFormat-cell-size cf))
   (draw-day (CalendarFormat-day-bg-color cf)
             (CalendarFormat-day-text-color cf)
             (CalendarFormat-cell-size cf))
   (draw-month-cell m y (CalendarFormat-number-color cf)
    (CalendarFormat-border-color cf) (CalendarFormat-cell-size cf))))

(: draw-banner-year : Image-Color Image-Color CalendarFormat Integer Integer
   -> Image)
; draw the banner of year calendar
(define (draw-banner-year bgc tc cf y space)
  (overlay
   (text (number->string y) 24 tc)
   (beside
    (draw-banner-rectangle (CalendarFormat-cell-size cf) space bgc)
    (draw-space space)
    (draw-banner-rectangle (CalendarFormat-cell-size cf) space bgc)
    (draw-space space)
    (draw-banner-rectangle (CalendarFormat-cell-size cf) space bgc)
  )
 )
)

(: draw-banner-rectangle : Integer Integer Image-Color -> Image)
(define (draw-banner-rectangle cell-size space bgc)
  (rectangle (* 7 cell-size) 70 "solid" bgc)
)

(: draw-space : Integer -> Image)
(define (draw-space space)
  (rectangle space 0 "solid" "darkgray")
)

(: draw-3-months : (Listof Integer) CalendarFormat Integer Integer -> Image)
(define (draw-3-months months cf y space)
  (match months
    [(cons f (cons s (cons t '()))) 
      (beside (draw-month cf f y)
       (draw-space space)
       (draw-month cf s y )
       (draw-space space)
       (draw-month cf t y )
      )
    ]
    [_ (error "Input list should be 3 length only")]
  )
)

(: draw-year (CalendarFormat Integer Integer -> Image))
;combine everthing together, finally draw the calendar of a given year
(define (draw-year cf y space )
  (above
     (draw-banner-year (CalendarFormat-banner-bg-color cf)
                       (CalendarFormat-banner-text-color cf) cf y space)
     (draw-3-months (build-list 3 (lambda ([x : Integer]) (+ 1 x))) cf y space)
     (draw-3-months (build-list 3 (lambda ([x : Integer]) (+ 4 x))) cf y space)
     (draw-3-months (build-list 3 (lambda ([x : Integer]) (+ 7 x))) cf y space)
     (draw-3-months (build-list 3 (lambda ([x : Integer]) (+ 10 x))) cf y space)
   )
)

(test)
 (draw-year
  (CalendarFormat 50 "darkgray" "white" "darkgray" "white" "darkgray" "white")
            2004 50)
 ; (define-struct CalendarFormat
 ;  ([cell-size : Integer]
 ;   [banner-bg-color : Image-Color]
 ;   [banner-text-color : Image-Color]
 ;   [day-bg-color : Image-Color]
 ;   [day-text-color : Image-Color]
 ;   [border-color : Image-Color]
 ;   [number-color : Image-Color]))