#lang typed/racket

;; ellyn liu

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require (only-in typed/racket/gui/base put-file get-file))

;; data definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))

;; ------ a bunch of boards for check-expects ------
(: sample-board : Board)
(define sample-board
  (vector
   (vector (Some 'black) 'None (Some 'black))
   (vector 'None (Some 'white) (Some 'white))
   (vector 'None 'None (Some 'black))))

(: d : Board)
(define d
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'black) (Some 'black) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))))

(: e : Board)
(define e
  (vector
   (vector 'None 'None 'None (Some 'white) 'None)
   (vector 'None 'None 'None (Some 'white) (Some 'white))
   (vector 'None 'None 'None 'None 'None)
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector 'None 'None 'None 'None 'None)))

;; empty board of dimension n
;; this must be called within board-copy
(: empty-board : Integer -> Board)
(define (empty-board n)
  (local
    {(: none : (Optional Stone))
     (define none 'None)}
    (make-vector n (make-vector n none))))
   
;; the integer argument is the dimension (locations per side) of the board
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical ll n bs)
  (match* (ll bs)
    [((LogicalLoc x y) (BoardSpec _ cell mar _))
     (PhysicalLoc (+ mar (* x cell)) (+ mar (* (- n y 1) cell)))]))

(check-expect (logical->physical (LogicalLoc 0 0) 3 (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 12 32))
(check-expect (logical->physical (LogicalLoc 1 0) 3 (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 22 32))

;; distance between two physical locations
(: dist : PhysicalLoc PhysicalLoc -> Real)
(define (dist orig calc)
  (match* (orig calc)
    [((PhysicalLoc x y) (PhysicalLoc xx yy))
     (sqrt (+ (sqr (- xx x)) (sqr (- yy y))))]))

;; true if the physical location is within the margins
(: in-margin? : PhysicalLoc Integer BoardSpec -> Boolean)
(define (in-margin? pl n bs)
  (match* (pl bs)
    [((PhysicalLoc x y) (BoardSpec _ cell mar _))
     (local {(define d (+ mar mar (* cell (sub1 n))))}
       (and (<= x d) (<= y d)))]))

;; in-margin checks if the point is in the grid
;; llx and lly are place-holder values that get fed into log-> phy
;; to be compared to the original physical location
;; the integer argument is the dimension (locations per side) of the board
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical pl n bs)
  (if (in-margin? pl n bs)
      (match* (pl bs)
        [((PhysicalLoc x y) (BoardSpec _ cell mar r))
         (local {(define llx (round (/ (- x mar) cell)))
                 (define lly (round (- (sub1 n) (/ (- y mar) cell))))}
           (match (logical->physical (LogicalLoc llx lly) n bs)
             [(PhysicalLoc xx yy)
              (if (< (dist (PhysicalLoc xx yy) (PhysicalLoc x y)) r)
                  (Some (LogicalLoc llx lly))
                  'None)]))])
      'None))

(check-expect
 (physical->logical (PhysicalLoc 90 45) 19 (BoardSpec 'tan 30 5 10)) 'None)
(check-expect
 (physical->logical (PhysicalLoc 27 377) 19 (BoardSpec 'tan 20 12 5)) 'None)                                                           

;; takes an integer and converts it to a char
(: num->let : Integer -> Char)
(define (num->let x)
  (cond
    [(<= x 7) (integer->char (+ 65 x))]
    [else (integer->char (+ 66 x))]))

;; uses num->let to make a string of appropriate length
(: int->string : Integer -> String)
(define (int->string n)
  (make-string (+ 1 (quotient n 25)) (num->let (remainder n 25))))

;; Convert logical locations to strings such as "A1", "B3", etc.
;; Note the letter "I" is skipped in Go labeling.
;; When you get a column past "Z", use "AA", then "BB", then "CC", etc.
;; When you get past "ZZ", use "AAA", then "BBB", etc.
(: logical->string : LogicalLoc -> String)
(define (logical->string ll)
  (match ll
    [(LogicalLoc x y)
     (string-append (int->string x) (number->string (+ y 1)))]))

(check-expect (logical->string (LogicalLoc 0 0)) "A1")
(check-expect (logical->string (LogicalLoc 1 10)) "B11")
(check-expect (logical->string (LogicalLoc 19 19)) "U20")

;; new board-ref that takes a Go (which contains a board) and a LL
;; and returns the Stone or 'None at that LL
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref g l)
  (match* (g l)
    [((Go Board _ _ _ _ _ _) (LogicalLoc c r))
     (vector-ref (vector-ref Board c) r)]))

(check-expect (board-ref (Go sample-board 'white '() 'None '() '() 0)
                         (LogicalLoc 0 0)) (Some 'black))

;; true if two Optional Stones are the same
(: optional=? : (Optional Stone) (Optional Stone) -> Boolean)
(define (optional=? s1 s2)
  (match* (s1 s2)
    [('None 'None) #t]
    [('None _) #f]
    [(_ 'None) #f]
    [((Some x) (Some y)) (symbol=? x y)]))

(check-expect (optional=? 'None (Some 'white)) #f)

;; true if two vectors of Optional Stones are the same
(: vector=? : (Vectorof (Optional Stone)) (Vectorof (Optional Stone))
   -> Boolean)
(define (vector=? v1 v2)
  (local {(define len1 (vector-length v1))
          (define len2 (vector-length v2))
          (: lp : Integer -> Boolean)
          (define (lp n)
            (cond
              [(= n len1) #t]
              [else (and (optional=? (vector-ref v1 n) (vector-ref v2 n))
                         (lp (add1 n)))]))}
    (and (= len1 len2) (lp 0))))

(check-expect (vector=? (vector (Some 'black) (Some 'white) 'None)
                        (vector (Some 'black) (Some 'white) 'None)) #t)

;; true if two Boards are the same
(: board=? : Board Board -> Boolean)
(define (board=? b1 b2)
  (local {(define len1 (vector-length b1))
          (define len2 (vector-length b2))
          (: lp : Integer -> Boolean)
          (define (lp n)
            (cond
              [(= n len1) #t]
              [else (and (vector=? (vector-ref b1 n) (vector-ref b2 n))
                         (lp (add1 n)))]))}
    (and (= len1 len2) (lp 0))))

(check-expect (board=? sample-board 
                       (vector
                        (vector (Some 'black) 'None (Some 'black))
                        (vector 'None (Some 'white) (Some 'white))
                        (vector 'None 'None (Some 'black)))) #t)
(check-expect (board=? sample-board 
                       (vector
                        (vector (Some 'black) (Some 'white) (Some 'black))
                        (vector 'None (Some 'white) 'None)
                        (vector (Some 'white) 'None 'None))) #f)

;; makes a copy of a vector
(: vector-copy : (Vectorof (Optional Stone)) -> (Vectorof (Optional Stone)))
(define (vector-copy v)
  (local {(define len (vector-length v))
          (define newv (make-vector len (vector-ref v 0)))
          (: lp : Integer -> (Vectorof (Optional Stone)))
          (define (lp i)
            (cond
              [(= i len) newv]
              [else (begin (vector-set! newv i (vector-ref v i))
                           (lp (add1 i)))]))}
    (lp 1)))

(check-expect (vector-copy (vector 'None (Some 'white) 'None))
              (vector 'None (Some 'white) 'None))

;; makes a new copy of board to add to history list
(: board-copy : Board -> Board)
(define (board-copy b)
  (local {(define len (vector-length b))
          (define newb (make-vector len (vector-copy (vector-ref b 0))))
          (: lp : Integer -> Board)
          (define (lp i)
            (cond
              [(= i len) newb]
              [else (begin (vector-set! newb i (vector-copy (vector-ref b i)))
                           (lp (add1 i)))]))}
    (lp 1)))

;; reverses color of stone
(: rev : Stone -> Stone)
(define (rev stone)
  (match stone
    ['black 'white]
    ['white 'black]))

;; true if a board repeats a board in history
(: repeat? : Board (Listof Board) -> Boolean)
(define (repeat? b list)
  (match list
    ['() #f]
    [(cons h t) (or (board=? b h) (repeat? b t))]))

(check-expect (repeat? sample-board
                       (list (vector
                              (vector (Some 'black) (Some 'white) (Some 'black))
                              (vector 'None (Some 'white) 'None)
                              (vector (Some 'black) 'None 'None))
                             (vector
                              (vector (Some 'black) (Some 'white) (Some 'black))
                              (vector (Some 'black) (Some 'white) 'None)
                              (vector (Some 'black) 'None 'None)))) #f)

;; modifies a board to store the specified stone (or no stone at all)
;; at the given location
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! g l opt)
  (match* (g l)
    [((Go b _ _ _ _ _ _) (LogicalLoc c r))
     (local
       {(define col (vector-ref b c))}
     (vector-set! col r opt))]))

;; takes a board and LL and returns an optional stone
;; exact same as board-ref, but doesn't need an entire Go structure
(: bd-ref  : Board LogicalLoc -> (Optional Stone))
(define (bd-ref b l)
  (match l
    [(LogicalLoc c r)
     (vector-ref (vector-ref b c) r)]))

;; takes a board, LL, and stone and changes the board
;; same as board-set!, but doesn't need an entire Go structure
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! b l opt)
  (match l
    [(LogicalLoc c r)
     (local
       {(define col (vector-ref b c))}
     (vector-set! col r opt))]))

;; turns an optional stone into a symbol so symbol=? can be used
(: to-symbol : (Optional Stone) -> Symbol)
(define (to-symbol s)
  (match s
    ['None 'None]
    [(Some c) c]))

(check-expect (to-symbol (Some 'white)) 'white)
	   
;; takes a stone and a stone color and returns a list of LogicalLocs of where
;; the stones of that color are located
;; is used to draw stones
(: get-lls : Board (Optional Stone) -> (Listof LogicalLoc))
(define (get-lls b s)
  (local
    {(define len (vector-length b))
     (: lp : Integer Integer (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (lp c r acc)
       (cond [(= c len) acc]
             [else (if (< r len)
                       (match (vector-ref (vector-ref b c) r)
                         [a
                          (if (stone=? a s)
                              (lp c (+ r 1) (cons (LogicalLoc c r) acc))
                              (lp c (+ r 1) acc))]
                         ['None (lp c (+ r 1) acc)])
                       (lp (+ c 1) 0 acc))]))}
    (lp 0 0 '())))

(check-expect (get-lls sample-board (Some 'black))
              (list (LogicalLoc 2 2) (LogicalLoc 0 2) (LogicalLoc 0 0)))
(check-expect (get-lls sample-board (Some 'white))
              (list (LogicalLoc 1 2) (LogicalLoc 1 1)))

;; board is valid if given positive inputs, stone radius is less than half
;; cell size, and the margin is greater than the radius
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? bs)
  (match bs
    [(BoardSpec _ cell mar r) (and (>= cell 0)
                                   (>= mar 0)
                                   (>= r 0)
                                   (> (/ cell 2) r)
                                   (> mar r))]))

(check-expect (valid-board-spec? (BoardSpec 'tan 10 12 5)) #f)
(check-expect (valid-board-spec? (BoardSpec 'tan 9 12 3)) #t)

;; draw a dot at click position
(: draw-ll : LogicalLoc Integer BoardSpec Integer Image-Color Image -> Image)
(define (draw-ll ll dim bs rad color img)
  (match (logical->physical ll dim bs)
    [(PhysicalLoc x y) (place-image (circle rad 'solid color) x y img)]))

;; ----- the next four functions are used in draw-world for formatting -----

;; draws a square with a letter in it
(: box : Integer Integer -> Image)
(define (box x cell)
  (overlay (text (int->string x) 8 'black)
           (square cell 'solid 'white)))

;; puts squares with letters together to make the footer at the bottom
(: footer : Integer Integer Integer -> Image)
(define (footer x cell dim)
  (if (< x dim) (beside (box x cell) (footer (add1 x) cell dim)) empty-image))

;; draws a box with a number in it
(: num-box : Integer Integer -> Image)
(define (num-box y cell)
  (overlay (text (number->string y) 8 'black)
           (square cell 'solid 'white)))

;; shows the numbers along the side of the grid
(: side : Integer Integer -> Image)
(define (side dim cell)
  (if (> dim 0) (above (num-box dim cell) (side (sub1 dim) cell)) empty-image))

;; two-digits: adds a 0 if necessary to front of integer to make it 2 digits
(: two-digits (Integer -> String))
(define (two-digits i)
  (if (< i 10)
      (string-append "0" (number->string i))
      (number->string i)))

(check-expect (two-digits 0) "00")
(check-expect (two-digits 5) "05")

;; number of minutes, seconds, and tenths of seconds (in the format 1:02.3)
(: format-time (Integer -> String))
(define (format-time t)
  (string-append (number->string (quotient t 600))
                 ":"
                 (two-digits (quotient (remainder t 600) 10))
                 "."
                 (number->string (remainder t 10))))

(check-expect (format-time 623) "1:02.3")

;; true if number of passes is 2
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (match go
    {(Go _ _ _ _ _ _ passes)
     (>= passes 2)}))

(check-expect (two-passes? (Go sample-board 'white '() 'None '() '() 2)) #t)

;; false, or illegal move if the new move would repeat a previous position
;; or if there is a stone already there
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go ll)
  (match* (go ll)
    [((Go b _ _ _ _ _ _) (LogicalLoc x y))
     (local {(define len (vector-length b))}
       (if (and (and (< x len) (< y len) (not (two-passes? go))))           
           (match (board-ref go ll)
             [(Some x) #f]
             [_ (match go
                  [(Go board next history l op sc p)
                   (local {(: psuedo : Go)
                           (define psuedo
                             (Go (board-copy board) next history l op sc p))}
                     (match (apply-move psuedo ll)
                       [(Go temp-b _ _ _ _ _ _) 
                        (not (repeat? temp-b history))]))])])
           #f))]))

;; this function does not detect whether or not a move is illegal
;; after player puts down a stone, if necessary, capture/self-capture,
;; and updates the Go structure
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go ll)
  (match go
    [(Go b next history l op sc p)
     (local {(define oldboard (board-copy b))
             ;; listlist takes a list of LL (by calling neighbors)
             ;; and returns an appended (Listof LogicalLoc) to be
             ;; removed from the board
             (: listlist : (Listof LogicalLoc) -> (Listof LogicalLoc))
             (define (listlist lls)
               (match lls
                 ['() '()]
                 [(cons hd tl)
                  (match (identify-chain b (list hd) (list hd))
                    ['None (listlist tl)]
                    [(Some liszt) (append liszt (listlist tl))])]))}
       (begin (bd-set! b ll (Some next))
              (local {(define op-cap
                        (listlist (neighbors b ll (rev-op (bd-ref b ll)))))}
                (remove! b (listlist (neighbors b ll (rev-op (bd-ref b ll)))))
                (local {(define sf-cap
                          (identify-chain b (list ll) (list ll)))}
                  (remove-self! b (identify-chain b (list ll) (list ll)))
                  (Go b
                      (rev next)
                      (cons oldboard history)
                      (Some ll)  
                      op-cap
                      (opt->list sf-cap)
                      0)))))]))

;; ------ following functions are used for identify-chain ------

;; true if stone has at least one liberty available
;; aka the stone cannot be captured
(: liberties? : Board LogicalLoc -> Boolean)
(define (liberties? b ll)
  (local {(: exists? : LogicalLoc -> Boolean)
          (define (exists? ll)
            (match ll
              [(LogicalLoc x y) (and (< -1 x (vector-length b))
                                     (< -1 y (vector-length b)))]))
          (: openloc : LogicalLoc -> Boolean)
          (define (openloc ll)
            (symbol=? 'None (to-symbol (bd-ref b ll))))}
    (match ll
      [(LogicalLoc x y)
       (or (if (exists? (LogicalLoc (add1 x) y))
               (openloc (LogicalLoc (add1 x) y))
               #f)
           (if (exists? (LogicalLoc (sub1 x) y))
               (openloc (LogicalLoc (sub1 x) y))
               #f)
           (if (exists? (LogicalLoc x (add1 y)))
               (openloc (LogicalLoc x (add1 y)))
               #f)
           (if (exists? (LogicalLoc x (sub1 y)))
               (openloc (LogicalLoc x (sub1 y)))
               #f))])))

(check-expect (liberties? sample-board (LogicalLoc 2 2)) #t)
(check-expect (liberties? sample-board (LogicalLoc 1 1)) #t)

;; true if two stones are the same
(: stone=? : (Optional Stone) (Optional Stone) -> Boolean)
(define (stone=? s1 s2)
  (match* (s1 s2)
    [('None 'None) #t]
    [((Some x) (Some y)) (symbol=? x y)]
    [(_ _) #f]))

(check-expect (stone=? 'None (Some 'black)) #f)

;; takes a board, a LL, and a stone color and returns a list of LLs of
;; where adjacent stones of the color are located
;; makes no sense to call this on a LL that contains 'None
(: neighbors : Board LogicalLoc (Optional Stone) -> (Listof LogicalLoc))
(define (neighbors b ll stone)
  (local {(define len (vector-length b))
          (: exists? : LogicalLoc -> Boolean)
          (define (exists? ll)
            (match ll
              [(LogicalLoc x y) (and (< -1 x (vector-length b))
                                     (< -1 y (vector-length b)))]))
          ;; on the board and the same color
          (: f : LogicalLoc -> Boolean)
          (define (f ll)
            (and (exists? ll) (stone=? stone (bd-ref b ll))))}
    (match ll
      [(LogicalLoc x y)
       (filter f (list (LogicalLoc (add1 x) y)
                       (LogicalLoc (sub1 x) y)
                       (LogicalLoc x (add1 y))
                       (LogicalLoc x (sub1 y))))])))
          
(check-expect (neighbors sample-board (LogicalLoc 0 2) (Some 'black))
              '())

;; takes a Board, LL, and a list of marked locations,
;; and returns a list of unmarked locations
(: unmarked : Board LogicalLoc (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (unmarked b ll marked)
  (local {(: adj-list : (Listof LogicalLoc))
          (define adj-list (neighbors b ll (bd-ref b ll)))}
    (remove* marked adj-list)))

(check-expect (unmarked e (LogicalLoc 3 2) '())
              (list (LogicalLoc 3 3) (LogicalLoc 3 1)))

;; used to return a list of LLs of stones that are part of a chain
;; returns the list only if the chain has no liberties left
;; otherwise if the stone has a liberty, automatically returns 'None
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc) ->
   (Optional (Listof LogicalLoc)))
(define (identify-chain b to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons e _) (if (liberties? b e) 'None 
                    (local {(: m : (Listof LogicalLoc))
                            (define m (unmarked b e marked))}
                      (identify-chain b
                                      (append (remove e to-explore) m)
                                      (append marked m))))]))

(check-expect (identify-chain d (list (LogicalLoc 2 2)) (list (LogicalLoc 2 2)))
              (Some
               (list (LogicalLoc 2 2)
                     (LogicalLoc 3 2)
                     (LogicalLoc 1 2)
                     (LogicalLoc 2 3)
                     (LogicalLoc 2 1)
                     (LogicalLoc 4 2)
                     (LogicalLoc 0 2))))

;; replace each LL in this list with 'None because they have been captured
(: remove! : Board (Listof LogicalLoc) -> Void)
(define (remove! b lls)
  (match lls
    ['() (void)]
    [(cons hd tl) (begin (bd-set! b hd 'None) (remove! b tl))]))

;; only used for self-capture to replace with 'None
(: remove-self! : Board (Optional (Listof LogicalLoc)) -> Void)
(define (remove-self! b opt)
  (match opt
    ['None (void)]
    [(Some list) (remove! b list)]))

;; similar to rev but with optional stones
(: rev-op : (Optional Stone) -> (Optional Stone))
(define (rev-op op)
  (match op
    [(Some 'white) (Some 'black)]
    [(Some 'black) (Some 'white)]
    ['None 'None]))

;; optional list into a list
(: opt->list : (Optional (Listof LogicalLoc)) -> (Listof LogicalLoc))
(define (opt->list lls)
  (match lls
    ['None '()]
    [(Some l) l]))

;; ------- calculate territory ---------------------

;; iterates over the board to return a list of LogicalLocs of where
;; the 'None stones are located
(: get-nones : Board -> (Listof LogicalLoc))
(define (get-nones b)
  (local {(define len (vector-length b))
          (: lp : Integer Integer (Listof LogicalLoc) -> (Listof LogicalLoc))
          (define (lp r c acc)
            (cond
              [(= r len) acc]
              [(= c len) (lp (add1 r) 0 acc)]
              [else (match (vector-ref (vector-ref b r) c)
                      ['None
                       (lp r
                           (add1 c)
                           (cons (LogicalLoc c (abs (- r (sub1 len)))) acc))]
                      [_ (lp r (add1 c) acc)])]))}
          (lp 0 0 '())))

(check-expect (length (get-nones e)) 17)

;; similar to identify-chain, but returns a list of LLs of empty intersections
;; that are not neutral space
;; used to calculate territory
;; returns the list only if the 'None chain does not touch any opposing color
;; instead of checking for liberties, checks for opposite color
(: identify-ter : Board (Listof LogicalLoc) (Listof LogicalLoc) Stone ->
   (Optional (Listof LogicalLoc)))
(define (identify-ter b to-explore marked stone)
  (match to-explore
    ['() (Some marked)]
    [(cons e _) (match (neighbors b e (Some (rev stone)))
                  ['() (local {(: m : (Listof LogicalLoc))
                               (define m (unmarked b e marked))}
                         (identify-ter b
                                         (append (remove e to-explore) m)
                                         (append marked m)
                                         stone))]
                  [_ 'None])]))

(check-expect (identify-ter e
                            (list (LogicalLoc 0 0))
                            (list (LogicalLoc 0 0))
                            'white)
              'None)
(check-expect (identify-ter e
                            (list (LogicalLoc 0 4))
                            (list (LogicalLoc 0 4))
                            'white)
              (Some (list (LogicalLoc 0 4))))

;; appends to make a list of stone's territory
(: territory : Board Stone -> (Listof LogicalLoc))
(define (territory b color)
  (local {(define list-nones (get-lls b 'None))
          (: lp : (Listof LogicalLoc) (Listof LogicalLoc)
             -> (Listof LogicalLoc))
          (define (lp lls acc)
            (match lls
              ['() acc]
              [(cons hd tl)
               (match (identify-ter b (list hd) (list hd) color)
                 ['None (lp tl acc)]
                 [(Some liszt) (lp tl (append liszt acc))])]))}
    (remove-duplicates (lp list-nones '()))))

(check-expect (territory e 'black)
              (list (LogicalLoc 4 0)
                    (LogicalLoc 4 1)
                    (LogicalLoc 4 2)
                    (LogicalLoc 4 3)
                    (LogicalLoc 4 4)))

;; list of black's territory
(: black-ter : Board -> (Listof LogicalLoc))
(define (black-ter b)
  (territory b 'black))

(check-expect (length (black-ter e)) 5)

;; list of white's territory
(: white-ter : Board -> (Listof LogicalLoc))
(define (white-ter b)
  (territory b 'white))

(check-expect (length (white-ter e)) 1)

;; returns an outcome of the winner or draw
(: outcome : Go -> Outcome)
(define (outcome go)
  (match go
    [(Go b _ _ _ _ _ _)
     (local {(: b-area : Integer)
             (define b-area
               (+ (length (get-lls b (Some 'black))) (length (black-ter b))))
             (: w-area : Integer)
             (define w-area
               (+ (length (get-lls b (Some 'white))) (length (white-ter b))))}
       (Outcome b-area w-area
                (cond
                  [(< b-area w-area) 'white]
                  [(> b-area w-area) 'black]
                  [else 'draw])))]))

(check-expect (outcome (Go e 'black '() 'None '() '() 2))
              (Outcome 10 4 'black))
(check-expect
 (outcome (Go (board-copy (empty-board 5)) 'black'() 'None '() '() 2))
 (Outcome 25 25 'draw))

;; draws the world with the board, grid, labels, stones, and message
(: draw-world : World -> Image)
(define (draw-world w)
  (match w
    [(World bs go mes b-time w-time hover)
     (match* (bs go)
       [((BoardSpec col cell mar rad)
         (Go board next _ recent-ll opp-cap sf-cap cp))
        (local
          {(define dim (vector-length board))
           ;; gen-block and grid are used to make gridd
           (: gen-block : (Image Image -> Image) Image Integer -> Image)
           (define (gen-block arr img n)
             (foldr arr empty-image (make-list n img)))
           (: grid : Integer Integer -> Image)
           (define (grid cell dim)
             (gen-block above
                        (gen-block beside
                                   (square cell 'outline 'black)
                                   (sub1 dim)) (sub1 dim)))
           ;; gridd is the actual grid that is displayed 
           (: gridd : Image)
           (define gridd
             (overlay
              (grid cell dim)
              (square (+ mar mar (* cell (sub1 dim))) 'solid col)))
           ;; draw-bstone draws black stones
           (: draw-bstone : LogicalLoc Image -> Image)
           (define (draw-bstone ll img)
             (draw-ll ll dim bs rad 'black img))
           ;; draw-wstone draws white stones
           (: draw-wstone : LogicalLoc Image -> Image)
           (define (draw-wstone ll img)
             (draw-ll ll dim bs rad 'white img))
           ;; shape serves as an invisible/empty image the size of the board
           (: shape : Image)
           (define shape
             (square (+ mar mar (* cell (sub1 dim))) 'outline (color 0 0 0 0)))
           ;; sm-sq is a small square that is white
           (: sm-sq : Image)
           (define sm-sq
             (square cell 'solid 'white))
           ;; twhite is a transparent white circle
           (: twhite : Image)
           (define twhite
             (circle rad 'solid (color 255 255 255 128)))
           ;; tblack is a transparent black circle
           (: tblack : Image)
           (define tblack
             (circle rad 'solid (color 0 0 0 128)))
           ;; draw a transparent circle if legal move
           (: draw-transparent : (Optional LogicalLoc) Image Image -> Image)
           (define (draw-transparent ll stone grid)
             (match ll
               ['None empty-image]
               [(Some loc)
                (match loc
                  [(LogicalLoc x y)
                   (if (and (and (< -1 x (vector-length board))
                                 (< -1 y (vector-length board)))
                            (legal-move? go loc))
                       (match (logical->physical loc dim bs)
                         [(PhysicalLoc x y) (place-image stone x y grid)])
                       empty-image)])]))
           ;; a star for most recent move
           (: recentstone : Image)
           (define recentstone
             (star (- rad 2) 'solid 'royalblue))
           ;; draws the star in the location
           (: draw-recent : (Optional LogicalLoc) Image Image -> Image)
           (define (draw-recent ll stone grid)
             (match ll
               ['None empty-image]
               [(Some loc)
                (match (logical->physical loc dim bs)
                  [(PhysicalLoc x y) (place-image stone x y grid)])]))
           ;; OPPONENT capture
           (: captured : Image)
           (define captured
             (square (- rad 3) 'solid 'red))
           ;; SELF capture
           (: sf-capt : Image)
           (define sf-capt
             (square (- rad 3) 'solid 'lime))
           (: draw-captured : LogicalLoc Image -> Image)
           (define (draw-captured ll img)
             (match (logical->physical ll dim bs)
               [(PhysicalLoc x y) (place-image captured x y img)]))
           (: draw-sfcaptured : LogicalLoc Image -> Image)
           (define (draw-sfcaptured ll img)
             (match (logical->physical ll dim bs)
               [(PhysicalLoc x y) (place-image sf-capt x y img)]))}
          (above 
           (beside
            (overlay
             (draw-recent recent-ll recentstone shape)
             (foldr draw-bstone shape (get-lls board (Some 'black)))
             (foldr draw-wstone shape (get-lls board (Some 'white)))
             (if (symbol=? next 'white)
                 (draw-transparent hover twhite shape)
                 (draw-transparent hover tblack shape))
             (foldr draw-captured shape opp-cap)
             (foldr draw-sfcaptured shape sf-cap)
             gridd)
            (side dim cell))
           (beside (footer 0 cell dim)
                   sm-sq)
           (beside (text mes 15 'black)
                   sm-sq)
           (beside (text "Black  " 15 'black)
                   (text (format-time b-time) 15 'black)
                   (square 30 'solid 'white)
                   (text (format-time w-time) 15 'black)
                   (text "  White" 15 'black)
                   sm-sq)
           (beside (text (format-time (+ b-time w-time)) 20 'black)
                   sm-sq)
           (beside (text "Total Time" 15 'black)
                   sm-sq)))])]))

"eyeball check"
(draw-world (World (BoardSpec 'moccasin 40 20 8)
                   (Go e 'black '() 'None '() '() 0)
                   "board: e"
                   0
                   0
                   'None))

;; when the player clicks, add the stone to the board and history list in Go
;; if the point is outside the margins, nothing is added
(: handle-mouse : World Integer Integer Mouse-Event -> World)
(define (handle-mouse w x y e)
  (match e
    ["move"
     (match w
       [(World bs go mes b-time w-time hover)
        (match go
          [(Go board _ _ _ _ _ _)
           (World bs go mes b-time w-time
                  (physical->logical
                   (PhysicalLoc x y) (vector-length board) bs))])])]         
    ["button-down"
     (match w
       [(World bs go mes b-time w-time hover)
        (match go
          [(Go board next history _ _ _ _)
           (local {(define oldboard (board-copy board))}
             (match (physical->logical
                     (PhysicalLoc x y) (vector-length board) bs)
               ['None w]
               [(Some ll)
                (if (legal-move? go ll)
                    (World bs
                           (apply-move go ll)
                           (string-append (symbol->string next)
                                          " moved to "
                                          (logical->string ll)
                                          ". "
                                          (symbol->string (rev next))
                                          "'s turn.")
                           b-time
                           w-time
                           'None)
                    (World bs go "illegal move!" b-time w-time hover))]))])])]
    [_ w]))

;; turns outcome into a string to be displayed
(: outcome->string : Go -> String)
(define (outcome->string go)
  (match (outcome go)
    [(Outcome bl wh win)
     (string-append "Black: " (number->string bl)
                    "  "
                    "White: " (number->string wh)
                    "  "
                    (string-upcase (symbol->string win))
                    "!")]))

(check-expect (outcome->string (Go sample-board 'black '() 'None '() '() 2))
              "Black: 3  White: 2  BLACK!")

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))

;; the player may pass by typing "p"
;; switches the color of the stone
(: handle-key : World String -> World)
(define (handle-key w key)
  (match w
    [(World bs go mes b-time w-time hover)
     (match go
       [(Go board next history place opp-cap self-cap cp)
        (match key
          [(or "S" "s")
           (begin (save-game! w) w)]
          [(or "L" "l")
           (load-game bs)]
          [(or "P" "p")
           (cond
             [(= cp 1)
              (World bs
                     (Go board next history place opp-cap self-cap (add1 cp))
                     (outcome->string go)
                     b-time
                     w-time
                     hover)]
             [(>= cp 2) w]
             [else (World bs
                          (Go board
                              (rev next)
                              (cons board history)
                              'None
                              '()
                              '()
                              (add1 cp))
                          (string-append (symbol->string next) " passed.")
                          b-time
                          w-time
                          hover)])])])]
    [_ w]))

;; increases timer
(: tick (World -> World))
(define (tick w)
  (match w
    [(World bs go mes btime wtime hover)
     (match go
       [(Go _ next _ _ _ _ _)
        (if (two-passes? go)
            w
            (if (symbol=? next 'black)
                (World bs go mes (+ btime 1) wtime hover)
                (World bs go mes btime (+ wtime 1) hover)))])]))
                 
;; initiates big bang
(: play : Integer BoardSpec -> World)
(define (play n bs)
  (if (and (valid-board-spec? bs) (>= n 2))
      (big-bang
          (World bs
                 (Go (board-copy (empty-board n)) 'black '() 'None '() '() 0)
                 "Welcome to Go!"
                 0
                 0
                 'None) : World
        [to-draw draw-world]
        [on-mouse handle-mouse]
        [on-key handle-key]
        [on-tick tick 1/10])
      (error "play: invalid board and dimension must be at least 2")))

;(play 9 (BoardSpec 'tan 30 15 8))

;; change optional stone to string
(: to-letter : (Optional Stone) -> String)
(define (to-letter s)
  (match s
    ['None "_"]
    [(Some 'white) "o"]
    [(Some 'black) "*"]))

(check-expect (to-letter 'None) "_")
(check-expect (to-letter (Some 'white)) "o")

;; turns a board into a string of characters
(: board->string : Board -> String)
(define (board->string b)
  (local {(define len (vector-length b))
          (: lp : Integer Integer String -> String)
          (define (lp c r acc)
            (cond
              [(= c len) acc]
              [else (if (< r len)
                        (match (vector-ref (vector-ref b c) r)
                          [a (lp c (+ r 1) (string-append acc (to-letter a)))])
                        (if (= c (sub1 len))
                            (lp (+ c 1) 0 acc)
                            (lp (+ c 1) 0 (string-append acc "|"))))]))}
    (lp 0 0 "")))

(check-expect (board->string sample-board) "*_*|_oo|__*")

;; history list to string
(: history->string : (Listof Board) -> String)
(define (history->string lbs)
  (local {(define len (length lbs))
          (: lp : Integer String -> String)
          (define (lp i acc)
            (cond
              [(= i len) acc]
              [(= i (sub1 len))
               (lp (add1 i)
                   (string-append acc (board->string (list-ref lbs i))))]
              [else (lp (add1 i)
                        (string-append acc
                                       (board->string
                                        (list-ref lbs i)) "!"))]))}
    (lp 0 "")))

(check-expect (history->string (list sample-board e d))
              "*_*|_oo|__*!___o_|___oo|_____|*****|_____!oo*oo|oo*oo|o***o|oo*oo|oo*oo")

;; turns a go into a string
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go b next his _ _ _ cp)
     (string-append
      (to-letter (Some next))
      "~"
      (board->string b)
      "~"
      (history->string his)
      "~"
      (number->string cp))]))

(check-expect (go->string (Go e 'black '() 'None '() '() 0))
              "*~___o_|___oo|_____|*****|_____~~0")

;; turns a world into a string
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World bs go mes b-ten w-ten hover)
     (string-append
      (number->string b-ten)
      "@"
      (number->string w-ten)
      "@"
      (go->string go))]))
      
(check-expect (world->string
               (World (BoardSpec 'tan 20 20 8)
                      (Go e 'black '() 'None '() '() 1)
                      "hello" 88 99 'None))
              "88@99@*~___o_|___oo|_____|*****|_____~~1")

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(check-expect (string->integer "1234") 1234)

;; letter to Optional Stone
(: let->st : String -> (Optional Stone))
(define (let->st st)
  (match st
    ["*" (Some 'black)]
    ["o" (Some 'white)]
    ["_" 'None]))

(check-expect (let->st "*") (Some 'black))
(check-expect (let->st "_") 'None)

;; tests that all vectors will be the same length as input
;; doesn't check for extra vectors
(: sq-board? : (Listof String) Integer -> Boolean)
(define (sq-board? xs len)
  (match xs
    ['() #t]
    [(cons h t) (and (= len (string-length h)) (sq-board? t len))]))

(check-expect (sq-board? '("_*o" "o*_" "****") 3) #f)
(check-expect (sq-board? '("*oo" "ooo" "***" "___") 4) #f)
(check-expect (sq-board? '("o__" "_*_" "*__") 3) #t)

;; string to board
(: string->board : String -> Board)
(define (string->board str)
  (local {(define boardstring (string-split str "|"))
          (define len (length boardstring))
          (define newboard : Board (board-copy (empty-board len)))
          (: lp : Integer Integer -> Void)
          (define (lp c r)
            (cond
              [(= c len) (void)]
              [else (if (< r len)
                        (begin
                          (bd-set! newboard
                                   (LogicalLoc c r)
                                   (let->st
                                    (string
                                     (string-ref (list-ref boardstring c) r))))
                          (lp c (add1 r)))
                        (lp (add1 c) 0))]))}
    (if (sq-board? boardstring len)
        (begin (lp 0 0) newboard)
        (error "input is not a square or wrong dimensions"))))

(check-expect (string->board "o__|_*_|*__")
              (vector
               (vector (Some 'white) 'None 'None)
               (vector 'None (Some 'black) 'None)
               (vector (Some 'black) 'None 'None)))
(check-error (string->board "o__|_*_|*__|ooo") "input is not a square or wrong dimensions")
(check-error (string->board "o__|_*_|*_*_") "input is not a square or wrong dimensions")

;; string to Go
(: string->go : String -> Go)
(define (string->go str)
  (local {(define split-str (string-split str "~"))
          (define len (length split-str))}
    (if (= len 4)
        (match split-str
          [(cons next (cons board (cons history (cons passes '()))))
           (Go (string->board board)
               (match next
                 ["*" 'black]
                 [_ 'white])
               (local {(define hisplit (string-split history "!"))}
                 (map string->board hisplit))
               'None
               '()
               '()
               (string->integer passes))])
        (error "improper format of Go structure"))))

(check-error (string->go "*~___o_|___oo|_____|*****|_____~0")
             "improper format of Go structure")
(check-error (string->go "*~___o_|__o|_____|*****|_____~~0")
             "input is not a square or wrong dimensions")
(check-expect (string->go "*~___o_|___oo|_____|*****|_____~~0")
              (Go e 'black '() 'None '() '() 0))

;; string to a world
(: string->world : BoardSpec String -> World)
(define (string->world bs str)
  (local {(define split-str (string-split str "@"))
          (define len (length split-str))}
    (if (= len 3)
        (match split-str
          [(cons b-time (cons w-time (cons go '())))
           (World bs
                  (string->go go)
                  "Welcome back to Go!"
                  (string->integer b-time)
                  (string->integer w-time)
                  'None)])
        (error "improper format of World structure"))))

(check-expect (string->world (BoardSpec 'tan 20 20 8)
                             "88@99@*~___o_|___oo|_____|*****|_____~~1")
              (World (BoardSpec 'tan 20 20 8)
                     (Go e 'black '() 'None '() '() 1)
                     "Welcome back to Go!" 88 99 'None))
(check-error (string->world (BoardSpec 'tan 20 20 8)
                            "8899@*~___o_|___oo|_____|*****|_____~~1")
             "improper format of World structure")
(check-error (string->world (BoardSpec 'tan 20 20 8)
                            "88@99@*___o_|___oo|_____|*****|_____1")
             "improper format of Go structure")

(test)