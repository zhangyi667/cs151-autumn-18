#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

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
   
(define-struct Go
  ([dimension : Integer]
   [black-stones : (Listof LogicalLoc)]
   [white-stones : (Listof LogicalLoc)]
   [next-to-play : Stone]))

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
   [status-message : String]))

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
(check-expect (logical->physical (LogicalLoc 0 1) 3 (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 12 22))

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
 (physical->logical (PhysicalLoc 12 32) 3 (BoardSpec 'tan 10 12 3))
 (Some (LogicalLoc 0 0)))
(check-expect
 (physical->logical (PhysicalLoc 14 31) 3 (BoardSpec 'tan 10 12 3))
 (Some (LogicalLoc 0 0)))
(check-expect
 (physical->logical (PhysicalLoc 22 32) 3 (BoardSpec 'tan 10 12 3))
 (Some (LogicalLoc 1 0)))
(check-expect
 (physical->logical (PhysicalLoc 250 250) 19 (BoardSpec 'tan 12 5 4)) 'None)
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

(check-expect (build-list 10 num->let)
              '(#\A
                #\B
                #\C
                #\D
                #\E
                #\F
                #\G
                #\H
                #\J
                #\K))

;; uses num->let to make a string of appropriate length
(: int->string : Integer -> String)
(define (int->string n)
  (make-string (+ 1 (quotient n 25)) (num->let (remainder n 25))))

(check-expect (build-list 60 int->string) strings)

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

;; takes either black or white's list of LogicalLoc and a given LogicalLoc
;; and returns the matching one, if there is one
(: find-ll : (Listof LogicalLoc) LogicalLoc -> (Optional LogicalLoc))
(define (find-ll list ll)
  (local
    ;; same-ll? is true if two Logical Loc are the same
    {(: same-ll? : LogicalLoc LogicalLoc -> Boolean)
     (define (same-ll? ll1 ll2)
       (match* (ll1 ll2)
         [((LogicalLoc x1 y1) (LogicalLoc x2 y2))
          (and (= x1 x2) (= y1 y2))]))}
    (match list
      ['() 'None]
      [(cons hd tl) (if (same-ll? hd ll)
                        (Some ll)
                        (find-ll tl ll))])))

(check-expect
 (find-ll (list (LogicalLoc 0 0) (LogicalLoc 1 1)) (LogicalLoc 0 1))
 'None)
(check-expect
 (find-ll (list (LogicalLoc 0 0) (LogicalLoc 1 1)) (LogicalLoc 1 1))
 (Some (LogicalLoc 1 1)))

;; Return the stone at the specified location on the board,
;; or indicate it is unoccupied
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref go ll)
  (match go
    [(Go _ b w _)
     (match (find-ll b ll)
       [(Some x) (Some 'black)]
       ['None (match (find-ll w ll)
                [(Some x) (Some 'white)]
                ['None 'None])])]))
              
(check-expect (board-ref test-go (LogicalLoc 2 2)) (Some 'white))
(check-expect (board-ref test-go (LogicalLoc 3 4)) (Some 'black))
(check-expect (board-ref test-go (LogicalLoc 1 1)) (Some 'black))
(check-expect (board-ref test-go (LogicalLoc 5 3)) 'None)

;; If the named location is unoccupied, put the stone there and advance the
;; player, and return Some Go struct. Return 'None if the location is already
;; occupied.
;; Raise an error if the stone to be placed does not match the color of the
;; player whose turn is indicated by the Go struct.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at ll s go)
  (match go
    [(Go d b w next)
     (if (not (symbol=? s next))
         (error "not your turn")
         (match (board-ref go ll)
               ['None
                (match next
                  ['black (Some (Go d (cons ll b) w 'white))]
                  ['white (Some (Go d b (cons ll w) 'black))])]
               [_ 'None]))]))

(check-expect (put-stone-at (LogicalLoc 1 1) 'black test-go) 'None)
(check-expect (put-stone-at (LogicalLoc 9 1) 'black test-go)
              (Some
               (Go
                19
                (list
                 (LogicalLoc 9 1)
                 (LogicalLoc 1 1)
                 (LogicalLoc 3 4))
                (list (LogicalLoc 2 2))
                'white)))
(check-error (put-stone-at (LogicalLoc 9 1) 'white test-go) "not your turn")
(check-expect (put-stone-at (LogicalLoc 2 6) 'white test1)
              (Some
               (Go
                19
                (list
                 (LogicalLoc 1 1)
                 (LogicalLoc 3 4)
                 (LogicalLoc 2 1))
                (list
                 (LogicalLoc 2 6)
                 (LogicalLoc 2 2)
                 (LogicalLoc 1 2))
                'black)))

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
(check-expect (valid-board-spec? (BoardSpec 'tan -9 12 3)) #f)
(check-expect (valid-board-spec? (BoardSpec 'tan 10 2 3)) #f)
(check-expect (valid-board-spec? (BoardSpec 'tan 9 12 3)) #t)

;; draw a dot at click position
(: draw-ll : LogicalLoc Integer BoardSpec Integer Image-Color Image -> Image)
(define (draw-ll ll dim bs rad color img)
  (match (logical->physical ll dim bs)
    [(PhysicalLoc x y) (place-image (circle rad 'solid color) x y img)]))

;; used in check-expects
(: test-go : Go)
(define test-go
  (Go 19
      (list (LogicalLoc 1 1) (LogicalLoc 3 4))
      (list (LogicalLoc 2 2))
      'black))

;; used in check-expects
(: test1 : Go)
(define test1
  (Go 19
      (list (LogicalLoc 1 1) (LogicalLoc 3 4) (LogicalLoc 2 1))
      (list (LogicalLoc 2 2) (LogicalLoc 1 2))
      'white))

;; draws the world with the board, grid, labels, stones, and message
(: draw-world : World -> Image)
(define (draw-world w)
  (match w
    [(World bs go mes)
     (match* (bs go)
       [((BoardSpec col cell mar rad) (Go dim b w next))
        (local
          ;; gen-block and grid are used to make gridd
          {(: gen-block : (Image Image -> Image) Image Integer -> Image)
           (define (gen-block arr img n)
             (foldr arr empty-image (make-list n img)))
           (: grid : Integer Integer -> Image)
           (define (grid cell dim)
             (gen-block above (gen-block beside
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
           ;; shape serves as an invisible/empty image
           (: shape : Image)
           (define shape
             (square (+ mar mar (* cell (sub1 dim))) 'outline (color 0 0 0 0)))}
          (above 
           (above/align "left"
                        (beside
                         (overlay
                          (foldr draw-bstone shape b)
                          (foldr draw-wstone shape w)
                          gridd)
                         (side dim cell))
                        (beside (footer 0 cell dim)
                                (square cell 'solid 'white)))
           (text mes 15 'black)))])]))

;;; converts an int to a byte
;(: int->byte : Integer -> Byte)
;(define (int->byte n)
;  (cond
;    [((make-predicate Byte) n) n]
;    [else 255])) ; default value for when we cannot use n for text size

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

"eyeball test"
(draw-world (World (BoardSpec 'moccasin 20 10 5) test1 "hello"))

;; when the player clicks, add the stone to the list of LogicalLoc in Go
;; if the point is outside the margins, nothing is added
(: handle-click : World Integer Integer Mouse-Event -> World)
(define (handle-click w x y e)
  (match e
    ["button-down"
     (match w
       [(World bs go mes)
        (match go
          [(Go dim _ _ next) 
           (match (physical->logical (PhysicalLoc x y) dim bs)
             ['None w]
             [(Some (LogicalLoc x1 y1)) 
              (match (put-stone-at (LogicalLoc x1 y1) next go)
                [(Some newgo) (World bs newgo mes)]
                [_ w])])])])]
    [_ w]))

;; the player may pass by typing "p"
;; switches the color of the stone
(: handle-key : World String -> World)
(define (handle-key w key)
  (match key
    ["p"
     (match w
       [(World bs go mes)
        (match go
          [(Go dim b w next)
           (match next
             ['black (World bs (Go dim b w 'white) mes)]
             ['white (World bs (Go dim b w 'black) mes)])])])]
    [_ w]))

;; initiates big bang
(: play : Integer BoardSpec -> World)
(define (play n bs)
  (if (and (> n 1) (valid-board-spec? bs)) 
      (big-bang (World bs (Go n '() '() 'black) "Welcome to Go!") : World
        [to-draw draw-world]
        [on-mouse handle-click]
        [on-key handle-key])
      (error "play: unable to produce given board")))

(play 19 (BoardSpec 'tan 15 10 4))

;; used for check-expect
(: strings : (Listof String))
(define strings
  '("A"
    "B"
    "C"
    "D"
    "E"
    "F"
    "G"
    "H"
    "J"
    "K"
    "L"
    "M"
    "N"
    "O"
    "P"
    "Q"
    "R"
    "S"
    "T"
    "U"
    "V"
    "W"
    "X"
    "Y"
    "Z"
    "AA"
    "BB"
    "CC"
    "DD"
    "EE"
    "FF"
    "GG"
    "HH"
    "JJ"
    "KK"
    "LL"
    "MM"
    "NN"
    "OO"
    "PP"
    "QQ"
    "RR"
    "SS"
    "TT"
    "UU"
    "VV"
    "WW"
    "XX"
    "YY"
    "ZZ"
    "AAA"
    "BBB"
    "CCC"
    "DDD"
    "EEE"
    "FFF"
    "GGG"
    "HHH"
    "JJJ"
    "KKK"))

(test)