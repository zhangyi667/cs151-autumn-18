#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(define-struct Date
  ([month : Integer]
   [day   : Integer]
   [year  : Integer]))

(define-type Day
  (U 'Sun 'Mon 'Tue 'Wed 'Thu 'Fri 'Sat))

;; two-digits
(: two-digits : Integer -> String)
(define (two-digits n)
  (cond
    [(< n 10) (string-append "0" (number->string n))]
    [else (number->string n)]))

(check-expect (two-digits 3) "03")
(check-expect (two-digits 10) "10")

;; display the date in "MM/DD/YYYY" format
(: format : Date -> String)
(define (format date)
  (match date
    [(Date m d y) (string-append
                   (two-digits m) "/"
                   (two-digits d) "/"
                   (number->string y))]))

(check-expect (format (Date 1 8 2000)) "01/08/2000")
(check-expect (format (Date 12 22 1999)) "12/22/1999")

;; report the number of days given the month and the year, in that order
(: days-in-month : Integer Integer -> Integer)
(define (days-in-month m y)
  (match* (m y)
    [(1 _) 31]
    [(2 y) (if (= (remainder y 4) 0) 29 28)]
    [(3 _) 31]
    [(4 _) 30]
    [(5 _) 31]
    [(6 _) 30]
    [(7 _) 31]
    [(8 _) 31]
    [(9 _) 30]
    [(10 _) 31]
    [(11 _) 30]
    [(12 _) 31]))

(check-expect (days-in-month 2 2000) 29)
(check-expect (days-in-month 2 2001) 28)
(check-expect (days-in-month 1 2000) 31)

;; a date is valid if it is not past the end of the month (e.g., Feb 30 or Apr 33)
;; and it is between 1900 and 2099 inclusive
(: valid? : Date -> Boolean)
(define (valid? date)
  (match date
    [(Date m d y)
     (and (<= m 12)
          (<= d (days-in-month m y))
          (and (>= y 1900) (<= y 2099)))]))

(check-expect (valid? (Date 1 40 2000)) #f)
(check-expect (valid? (Date 1 30 2100)) #f)
(check-expect (valid? (Date 12 25 2005)) #t)

;; Is the first date before the second?
(: before? : Date Date -> Boolean)
(define (before? date1 date2)
  (match* (date1 date2)
    [((Date m1 d1 y1) (Date m2 d2 y2))
     (cond
       [(< y1 y2) #t]
       [(> y1 y2) #f]
       [(< m1 m2) #t]
       [(> m1 m2) #f]
       [(< d1 d2) #t]
       [else #f])]))

(check-expect (before? (Date 3 1 2000) (Date 1 1 2000)) #f)
(check-expect (before? (Date 1 30 2000) (Date 1 20 2000)) #f)
(check-expect (before? (Date 1 20 2000) (Date 1 30 2000)) #t)
(check-expect (before? (Date 3 1 2001) (Date 3 2 2000)) #f)
(check-expect (before? (Date 5 1 2000) (Date 4 1 2000)) #f)

;; Is the first date after the second?
(: after? : Date Date -> Boolean)
(define (after? date1 date2)
  (match* (date1 date2)
    [((Date m1 d1 y1) (Date m2 d2 y2))
     (if (and (= m1 m2) (= d1 d2) (= y1 y2))
         #f
         (not (before? date1 date2)))]))

(check-expect (after? (Date 1 1 2000) (Date 2 1 2000)) #f)
(check-expect (after? (Date 3 1 2000) (Date 1 1 2000)) #t)
(check-expect (after? (Date 1 30 2000) (Date 1 20 2000)) #t)
(check-expect (after? (Date 1 20 2000) (Date 1 30 2000)) #f)

;; Is the given date the last date of the month?
(: last-of-month? : Date -> Boolean)
(define (last-of-month? date)
  (match date
    [(Date m d y) (= d (days-in-month m y))]))

(check-expect (last-of-month? (Date 2 29 2000)) #t)
(check-expect (last-of-month? (Date 1 31 2018)) #t)
(check-expect (last-of-month? (Date 10 30 2018)) #f)

;; Given a date, return the day immediately after.
(: tomorrow : Date -> Date)
(define (tomorrow date)
  (match date
    [(Date m d y)
     (cond
       [(and (= m 12) (= d 31)) (Date 1 1 (add1 y))]
       [(last-of-month? (Date m d y)) (Date (add1 m) 1 y)]
       [else (Date m (add1 d) y)])]))

(check-expect (tomorrow (Date 12 31 2018)) (Date 1 1 2019))
(check-expect (tomorrow (Date 1 20 2018)) (Date 1 21 2018))
(check-expect (tomorrow (Date 10 31 2018)) (Date 11 1 2018))

;; Given a date, return the day immediately before.
(: yesterday : Date -> Date)
(define (yesterday date)
  (match date
    [(Date m d y)
     (cond
       [(and (= m 1) (= d 1)) (Date 12 31 (sub1 y))]
       [(= d 1) (Date (sub1 m) (days-in-month (sub1 m) y) y)]
       [else (Date m (sub1 d) y)])]))

(check-expect (yesterday (Date 1 1 2019)) (Date 12 31 2018))
(check-expect (yesterday (Date 1 21 2018)) (Date 1 20 2018))
(check-expect (yesterday (Date 11 1 2018)) (Date 10 31 2018))

;; Given a date, advance that many days on the calendar.
;; If the integer is positive, this means move ahead in time.
;; If the integer is negative, this means move back in time.
(: add-days : Integer Date -> Date)
(define (add-days n date)
  (cond
    [(= n 0) date]
    [(> n 0) (add-days (sub1 n) (tomorrow date))]
    [(< n 0) (add-days (add1 n) (yesterday date))]))

(check-expect (add-days -10 (Date 1 11 2018)) (Date 1 1 2018))
(check-expect (add-days 32 (Date 1 1 2018)) (Date 2 2 2018))
(check-expect (add-days 31 (Date 12 1 2018)) (Date 1 1 2019))
(check-expect (add-days -31 (Date 1 1 2019)) (Date 12 1 2018))

;; Return the whole month as a list of dates in ascending order, given month and year.
(: whole-month : Integer Integer -> (Listof Date))
(define (whole-month m y)
  (local {(: ds : Integer Integer Integer -> (Listof Date))
          (define (ds m n y)
            (cond
              [(<= n (days-in-month m y)) (cons (Date m n y) (ds m (add1 n) y))]
              [else '()]))}
    (ds m 1 y)))

(check-expect (whole-month 2 2000) practice)

;; find remainder to figure out day of week
(: day : Date -> Integer)
(define (day date)
  (local {(: leap? : Date -> Boolean)
          (define (leap? date)
            (match date
              [(Date _ _ y) (= (remainder y 4) 0)]))}
    (local {(define (month-adj date)
              (match date
                [(Date m _ _)
                 (match m
                   [1 (if (leap? date) 0 1)]
                   [2 (if (leap? date) 3 4)]
                   [3 4]
                   [4 0]
                   [5 2]
                   [6 5]
                   [7 0]
                   [8 3]
                   [9 6]
                   [10 1]
                   [11 4]
                   [12 6])]))}
      (match date
        [(Date m d y)
         (remainder
          (+ (- y 1900)
             (month-adj date)
             d
             (exact-floor
              (/ y 4)))
          7)]))))

(check-expect (day (Date 11 1 2018)) 4)

;; Compute the day of the week for the given date. The formula is below.
(: dow : Date -> Day)
(define (dow date)
  (match (day date)
    [0 'Sun]
    [1 'Mon]
    [2 'Tue]
    [3 'Wed]
    [4 'Thu]
    [5 'Fri]
    [6 'Sat]))

(check-expect (dow (Date 11 1 2018)) 'Thu)
(check-expect (dow (Date 11 1 2019)) 'Fri)
(check-expect (dow (Date 8 1 2018)) 'Wed)
(check-expect (dow (Date 2 29 2016)) 'Mon)
(check-expect (dow (Date 2 29 2020)) 'Sat)

;; true if Date is a Sat or Sun
(: weekend? : Date -> Boolean)
(define (weekend? date)
  (or (= (day date) 0) (= (day date) 6)))

(check-expect (weekend? (Date 11 3 2018)) #t)
(check-expect (weekend? (Date 11 1 2018)) #f)

;; Return the Saturdays and Sundays in the given month and year, in ascending order.
(: weekends : Integer Integer -> (Listof Date))
(define (weekends m y)
  (filter weekend? (whole-month m y)))

(check-expect (weekends 11 2018)
              (list
               (Date 11 3 2018)
               (Date 11 4 2018)
               (Date 11 10 2018)
               (Date 11 11 2018)
               (Date 11 17 2018)
               (Date 11 18 2018)
               (Date 11 24 2018)
               (Date 11 25 2018)))

;; pick item in list at given index
(: pick : Integer (Listof Date) -> Date)
(define (pick n list)
  (match list
    [(cons hd tl) (if (= n 0) hd (pick (sub1 n) tl))]))

(check-expect (pick 0 (list (Date 1 1 2018) (Date 1 2 2018) (Date 1 3 2018)))
              (Date 1 1 2018))
(check-expect (pick 1 (list (Date 1 1 2018) (Date 1 2 2018) (Date 1 3 2018)))
              (Date 1 2 2018))

;; last one in list
(: last : (Listof Date) -> Date)
(define (last list)
  (match list
    [(cons hd '()) hd]
    [(cons hd tl) (last tl)]))

(check-expect (last (list (Date 1 1 2018) (Date 1 2 2018) (Date 1 3 2018)))
              (Date 1 3 2018))

;; returns a list of dates that are all that day of the week
(: list-day : Symbol (Listof Date) -> (Listof Date))
(define (list-day sym list)
  (match list
    ['() '()]
    [(cons hd tl) (if (symbol=? (dow hd) sym)
                      (cons hd (list-day sym tl))
                      (list-day sym tl))]))

;; Memorial Day is the last Monday in May.
(: memorial-day : Integer -> Date)
(define (memorial-day year)
  (last (list-day 'Mon (whole-month 5 year))))

(check-expect (memorial-day 2019) (Date 5 27 2019))
(check-expect (memorial-day 2000) (Date 5 29 2000))

;; Labor Day is the first Monday in September.
;; The integer argument is the year.
(: labor-day : Integer -> Date)
(define (labor-day year)
  (pick 0 (list-day 'Mon (whole-month 9 year))))

(check-expect (labor-day 2018) (Date 9 3 2018))
(check-expect (labor-day 2000) (Date 9 4 2000))

;; Thanksgiving (US) is the fourth Thursday of November.
;; The integer argument is the year.
(: thanksgiving : Integer -> Date)
(define (thanksgiving year)
  (pick 3 (list-day 'Thu (whole-month 11 year))))

(check-expect (thanksgiving 2018) (Date 11 22 2018))
(check-expect (thanksgiving 2000) (Date 11 23 2000))

;; returns a list of dates constituting a week from given day to the next Sat
(: weeks : Date -> (Listof Date))
(define (weeks date)
  (match (dow date)
    ['Sat (list date)]
    [_ (cons date (weeks (add-days 1 date)))]))

(check-expect (weeks (Date 11 4 2018))
              (list
               (Date 11 4 2018)
               (Date 11 5 2018)
               (Date 11 6 2018)
               (Date 11 7 2018)
               (Date 11 8 2018)
               (Date 11 9 2018)
               (Date 11 10 2018)))
(check-expect (weeks (Date 11 3 2018)) (list (Date 11 3 2018)))

;; turn a list of Sundays into a list containing lists of weeks
;; note, leaves out the first week if it does not start with a Sun
(: listlist : (Listof Date) -> (Listof (Listof Date)))
(define (listlist sundays)
  (match sundays
    ['() '()]
    [(cons hd tl) (cons (weeks hd) (listlist tl))])) 

(check-expect (listlist (list-day 'Sun (whole-month 11 2018)))
              (list
 (list
  (Date 11 4 2018)
  (Date 11 5 2018)
  (Date 11 6 2018)
  (Date 11 7 2018)
  (Date 11 8 2018)
  (Date 11 9 2018)
  (Date 11 10 2018))
 (list
  (Date 11 11 2018)
  (Date 11 12 2018)
  (Date 11 13 2018)
  (Date 11 14 2018)
  (Date 11 15 2018)
  (Date 11 16 2018)
  (Date 11 17 2018))
 (list
  (Date 11 18 2018)
  (Date 11 19 2018)
  (Date 11 20 2018)
  (Date 11 21 2018)
  (Date 11 22 2018)
  (Date 11 23 2018)
  (Date 11 24 2018))
 (list
  (Date 11 25 2018)
  (Date 11 26 2018)
  (Date 11 27 2018)
  (Date 11 28 2018)
  (Date 11 29 2018)
  (Date 11 30 2018)
  (Date 12 1 2018))))

;; draws the date
(: draw : Date -> Image)
(define (draw date)
  (match date
    [(Date _ d _)
     (overlay (text (number->string d) 20 'black)
              (square 50 'outline 'black))]))

"eyeball check"
(draw (Date 11 5 2018))

;; draw week
(: draw-week : (Listof Date) -> Image)
(define (draw-week list)
  (match list
    ['() empty-image]
    [(cons hd tl) (beside (draw hd) (draw-week tl))]))
     
;"eyeball check"
;(draw-week (listlist (list-day 'Sun (whole-month 11 2018))))

;; draw month
(: draw-month : (Listof (Listof Date)) -> Image)
(define (draw-month listy)
  (match listy
    ['() empty-image]
    [(cons hd tl) (above (draw-week hd) (draw-month tl))]))

;eyeball check
;(draw-month (listlist (list-day 'Sun (whole-month 11 2018))))

;; calendar headers and full layout
(: headers : Image)
(define headers
  (local
    {(: s : Image) (define s (square 50 'outline 'black))
     (: row : Image) (define row (beside s s s s s s s))}
    (place-image
     (text "Sun" 20 'black) 25 25
     (place-image
      (text "Mon" 20 'black) 75 25
      (place-image
       (text "Tue" 20 'black) 125 25
       (place-image
        (text "Wed" 20 'black) 175 25
        (place-image
         (text "Thu" 20 'black) 225 25
         (place-image
          (text "Fri" 20 'black) 275 25
          (place-image
           (text "Sat" 20 'black) 325 25 row)))))))))

;; Produce an image of one month on the calendar, given the month and year.
;; You have latitude as to how this looks, but it should have cells in rows and
;; 7 columns, according to standard calendar style.
(: calendar : Integer Integer -> Image)
(define (calendar month year)
  (above headers
         (draw-week
          (weeks
           (add-days -7 (pick 0 (list-day 'Sun (whole-month month year))))))
         (draw-month (listlist (list-day 'Sun (whole-month month year))))
         (text (string-append (number->string month) "/" (number->string year))
               30 'black)))

"eyeball check"
(calendar 11 2018)

;; used for check-expect
(: practice (Listof Date))
(define practice
  (list
   (Date 2 1 2000)
   (Date 2 2 2000)
   (Date 2 3 2000)
   (Date 2 4 2000)
   (Date 2 5 2000)
   (Date 2 6 2000)
   (Date 2 7 2000)
   (Date 2 8 2000)
   (Date 2 9 2000)
   (Date 2 10 2000)
   (Date 2 11 2000)
   (Date 2 12 2000)
   (Date 2 13 2000)
   (Date 2 14 2000)
   (Date 2 15 2000)
   (Date 2 16 2000)
   (Date 2 17 2000)
   (Date 2 18 2000)
   (Date 2 19 2000)
   (Date 2 20 2000)
   (Date 2 21 2000)
   (Date 2 22 2000)
   (Date 2 23 2000)
   (Date 2 24 2000)
   (Date 2 25 2000)
   (Date 2 26 2000)
   (Date 2 27 2000)
   (Date 2 28 2000)
   (Date 2 29 2000)))
              
(test)