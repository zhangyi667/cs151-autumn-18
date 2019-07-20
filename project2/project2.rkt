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

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]))

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

;; ------ a bunch of boards for check-expects ------
(: sample-board : Board)
(define sample-board
  (vector
   (vector (Some 'black) (Some 'white) (Some 'black))
   (vector 'None (Some 'white) 'None)
   (vector (Some 'black) 'None 'None)))

(: b : Board)
(define b
    (vector
     (vector (Some 'black) (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'black) (Some 'white) (Some 'white))
     (vector 'None 'None (Some 'white) (Some 'white))
     (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))))

(: c : Board)
(define c
  (vector
   (vector (Some 'white) (Some 'black) (Some 'white))
   (vector (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'black) (Some 'white))))

(: d : Board)
(define d
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'black) (Some 'black) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))))

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
(define (board-ref go ll)
  (match* (go ll)
    [((Go board _ _) (LogicalLoc x y))
     (vector-ref
      (vector-ref board (abs (- y (- (vector-length board) 1)))) x)]))

(check-expect (board-ref (Go sample-board 'white '())
                         (LogicalLoc 0 0)) (Some 'black))
(check-expect (board-ref (Go sample-board 'white '())
                         (LogicalLoc 1 0)) 'None)
(check-expect (board-ref (Go sample-board 'white '())
                         (LogicalLoc 0 2)) (Some 'black))
(check-expect (board-ref (Go sample-board 'white '())
                         (LogicalLoc 2 1)) 'None)
(check-expect (board-ref (Go sample-board 'white '())
                         (LogicalLoc 2 2)) (Some 'black))

;; true if two Optional Stones are the same
(: optional=? : (Optional Stone) (Optional Stone) -> Boolean)
(define (optional=? s1 s2)
  (match* (s1 s2)
    [('None 'None) #t]
    [('None _) #f]
    [(_ 'None) #f]
    [((Some x) (Some y)) (symbol=? x y)]))

(check-expect (optional=? 'None (Some 'white)) #f)
(check-expect (optional=? (Some 'black) (Some 'white)) #f)
(check-expect (optional=? (Some 'black) (Some 'black)) #t)

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
(check-expect (vector=? (vector (Some 'black) (Some 'white) 'None)
                        (vector (Some 'black) (Some 'white) (Some 'black))) #f)

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
                        (vector (Some 'black) (Some 'white) (Some 'black))
                        (vector 'None (Some 'white) 'None)
                        (vector (Some 'black) 'None 'None))) #t)
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

(check-expect (board-copy sample-board)
              (vector
               (vector (Some 'black) (Some 'white) (Some 'black))
               (vector 'None (Some 'white) 'None)
               (vector (Some 'black) 'None 'None)))

;; modifies a board to store the specified stone (or no stone at all)
;; at the given location
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! go ll some)
  (match* (go ll)
    [((Go board _ _) (LogicalLoc x y))
     (local {(define len (vector-length board))}
       (vector-set! (vector-ref board (abs (- y (- len 1)))) x some))]))

(define test-board (board-copy sample-board))
(check-expect (begin
                (board-set!
                 (Go test-board 'white '()) (LogicalLoc 0 1) (Some 'black))
                test-board)
              (vector
               (vector (Some 'black) (Some 'white) (Some 'black))
               (vector (Some 'black) (Some 'white) 'None)
               (vector (Some 'black) 'None 'None)))
(check-expect (begin
                (board-set!
                 (Go test-board 'white '()) (LogicalLoc 1 1) (Some 'black))
                test-board)
              (vector
               (vector (Some 'black) (Some 'white) (Some 'black))
               (vector (Some 'black) (Some 'black) 'None)
               (vector (Some 'black) 'None 'None)))

;; If the named location is unoccupied, put the stone there and advance the
;; player, and return Some Go struct. Return 'None if the location is already
;; occupied.
;; Raise an error if the stone to be placed does not match the color of the
;; player whose turn is indicated by the Go struct.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at ll s go)
  (match go
    [(Go board next history)
     (local {(define oldboard (board-copy board))}
     (if (not (symbol=? s next))
         (error "not your turn")
         (match (board-ref go ll)
               ['None
                (match next
                  ['black (Some (Go (begin (board-set! go ll (Some s)) board)
                                    'white (cons oldboard history)))]
                  ['white (Some (Go (begin (board-set! go ll (Some s)) board)
                                    'black (cons oldboard history)))])]
               [_ 'None])))]))

;; takes a stone and a stone color and returns a list of LogicalLocs of where
;; the stones of that color are located
;; is used to draw stones
(: get-lls : Board Stone -> (Listof LogicalLoc))
(define (get-lls b s)
  (local {(define len (vector-length b))
          (: lp : Integer Integer (Listof LogicalLoc) -> (Listof LogicalLoc))
          (define (lp r c acc)
            (cond
              [(= r len) acc]
              [(= c len) (lp (add1 r) 0 acc)]
              [else (match (vector-ref (vector-ref b r) c)
                      [(Some color)
                       (cond
                         [(symbol=? color s)
                          (lp r (add1 c)
                              (cons (LogicalLoc c (abs (- r (sub1 len)))) acc))]
                         [else (lp r (add1 c) acc)])]
                      [_ (lp r (add1 c) acc)])]))}
    (lp 0 0 '())))

(check-expect (get-lls sample-board 'black)
              (list (LogicalLoc 0 0) (LogicalLoc 2 2) (LogicalLoc 0 2)))
(check-expect (get-lls sample-board 'white)
              (list (LogicalLoc 1 1) (LogicalLoc 1 2)))

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

;; draws the world with the board, grid, labels, stones, and message
(: draw-world : World -> Image)
(define (draw-world w)
  (match w
    [(World bs go mes)
     (match* (bs go)
       [((BoardSpec col cell mar rad) (Go board _ _))
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
           ;; shape serves as an invisible/empty image
           (: shape : Image)
           (define shape
             (square (+ mar mar (* cell (sub1 dim))) 'outline (color 0 0 0 0)))}
          (above 
           (beside
            (overlay
             (foldr draw-bstone shape (get-lls board 'black))
             (foldr draw-wstone shape (get-lls board 'white))
             gridd)
            (side dim cell))
           (beside (footer 0 cell dim)
                   (square cell 'solid 'white))
           (text mes 15 'black)))])]))

"eyeball check"
(draw-world (World (BoardSpec 'moccasin 20 15 5)
                   (Go sample-board 'white '()) "board: sample-board"))
(draw-world (World (BoardSpec 'moccasin 30 30 8)
                   (Go b 'black '()) "board: b"))
(draw-world (World (BoardSpec 'moccasin 30 30 8)
                   (Go c 'black '()) "board: c"))
(draw-world (World (BoardSpec 'moccasin 30 30 8)
                   (Go d 'black '()) "board: d"))

;; reverses color of stone
(: rev : Stone -> Stone)
(define (rev stone)
  (match stone
    ['black 'white]
    ['white 'black]))

(check-expect (rev 'black) 'white)
(check-expect (rev 'white) 'black)

;; when the player clicks, add the stone to the board and history list in Go
;; if the point is outside the margins, nothing is added
(: handle-click : World Integer Integer Mouse-Event -> World)
(define (handle-click w x y e)
  (match e
    ["button-down"
     (match w
       [(World bs go mes)
        (match go
          [(Go board next history)
           (local {(define oldboard (board-copy board))}
           (match (physical->logical (PhysicalLoc x y) (vector-length board) bs)
             ['None w]
             [(Some ll)
              (if (legal-move? go ll)
                  (World bs (apply-move go ll)
                         (string-append (symbol->string next)
                                        " moved to "
                                        (logical->string ll)
                                        ". "
                                        (symbol->string (rev next))
                                        "'s turn."))
                  (World bs go "illegal move!"))]))])])]
    [_ w]))

;; the player may pass by typing "p"
;; switches the color of the stone
(: handle-key : World String -> World)
(define (handle-key w key)
  (match key
    [(or "P" "p")
     (match w
       [(World bs go mes)
        (match go
          [(Go board next history)
           (World bs (Go board (rev next) (cons board history))
                  (string-append
                   (symbol->string next)
                   " passed."))])])]
    [_ w]))

;; empty board of dimension n
;; this must be called within board-copy
(: empty-board : Integer -> Board)
(define (empty-board n)
  (local
    {(: none : (Optional Stone))
     (define none 'None)}
    (make-vector n (make-vector n none))))

;; initiates big bang
(: play : Integer BoardSpec -> World)
(define (play n bs)
  (if (and (valid-board-spec? bs) (>= n 2))
      (big-bang (World bs
                       (Go (board-copy (empty-board n)) 'black '())
                       "Welcome to Go!") : World
        [to-draw draw-world]
        [on-mouse handle-click]
        [on-key handle-key])
      (error "play: invalid board and dimension must be at least 2")))

;(play 19 (BoardSpec 'moccasin 20 15 6))

;; true if a board repeats a board in history
(: repeat? : Board (Listof Board) -> Boolean)
(define (repeat? b list)
  (match list
    ['() #f]
    [(cons h t) (or (board=? b h) (repeat? b t))]))

(check-expect (repeat? sample-board
                       (list (vector
                              (vector (Some 'black) (Some 'white) (Some 'black))
                              (vector (Some 'black) (Some 'white) 'None)
                              (vector (Some 'black) 'None 'None)))) #f)
(check-expect (repeat? sample-board
                       (list (vector
                              (vector (Some 'black) (Some 'white) (Some 'black))
                              (vector 'None (Some 'white) 'None)
                              (vector (Some 'black) 'None 'None))
                             (vector
                              (vector (Some 'black) (Some 'white) (Some 'black))
                              (vector (Some 'black) (Some 'white) 'None)
                              (vector (Some 'black) 'None 'None)))) #t)

;; takes a board and LL and returns an optional stone
;; exact same as board-ref, but doesn't need an entire Go structure
(: bd-ref  : Board LogicalLoc -> (Optional Stone))
(define (bd-ref board ll)
  (match ll
    [(LogicalLoc x y)
     (vector-ref
      (vector-ref board (abs (- y (- (vector-length board) 1)))) x)]))

;; takes a board, LL, and stone and changes the board
;; same as board-set!, but doesn't need an entire Go structure
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! board ll some)
  (match ll
    [(LogicalLoc x y)
     (local {(define len (vector-length board))}
       (vector-set! (vector-ref board (abs (- y (- len 1)))) x some))]))

(define copy
  (board-copy d))
(check-expect (begin (bd-set! copy (LogicalLoc 2 2) 'None) copy)
              (vector
               (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
               (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
               (vector (Some 'white) (Some 'black) 'None (Some 'black) (Some 'white))
               (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))
               (vector (Some 'white) (Some 'white) (Some 'black) (Some 'white) (Some 'white))))

;; turns an optional stone into a symbol so symbol=? can be used
(: to-symbol : (Optional Stone) -> Symbol)
(define (to-symbol s)
  (match s
    ['None 'None]
    [(Some c) c]))

(check-expect (to-symbol (Some 'white)) 'white)
(check-expect (to-symbol 'None) 'None)

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

(check-expect (liberties? sample-board (LogicalLoc 0 2)) #t)
(check-expect (liberties? sample-board (LogicalLoc 1 2)) #f)
(check-expect (liberties? sample-board (LogicalLoc 2 2)) #t)
(check-expect (liberties? sample-board (LogicalLoc 1 1)) #t)

;; true if two stones are the same
(: stone=? : (Optional Stone) (Optional Stone) -> Boolean)
(define (stone=? s1 s2)
  (match* (s1 s2)
    [('None 'None) #t]
    [((Some x) (Some y)) (symbol=? x y)]
    [(_ _) #f]))

(check-expect (stone=? (Some 'black) (Some 'white)) #f)
(check-expect (stone=? 'None 'None) #t)
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
          
(check-expect (neighbors b (LogicalLoc 2 2) (Some 'white))
              (list (LogicalLoc 3 2) (LogicalLoc 2 1)))
(check-expect (neighbors b (LogicalLoc 2 2) (Some 'black))
              (list (LogicalLoc 1 2) (LogicalLoc 2 3)))
(check-expect (neighbors sample-board (LogicalLoc 1 1) (Some 'white))
              (list (LogicalLoc 1 2)))
(check-expect (neighbors sample-board (LogicalLoc 0 2) (Some 'black))
              '())
(check-expect (neighbors b (LogicalLoc 3 1) (Some 'white))
              (list (LogicalLoc 2 1) (LogicalLoc 3 2) (LogicalLoc 3 0)))

;; takes a Board, LL, and a list of marked locations,
;; and returns a list of unmarked locations
(: unmarked : Board LogicalLoc (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (unmarked b ll marked)
  (local {(: adj-list : (Listof LogicalLoc))
          (define adj-list (neighbors b ll (bd-ref b ll)))}
    (remove* marked adj-list)))

(check-expect (unmarked b (LogicalLoc 2 2) (list (LogicalLoc 3 2)))
              (list (LogicalLoc 2 1)))
(check-expect (unmarked b (LogicalLoc 1 3) '())
              (list (LogicalLoc 2 3) (LogicalLoc 0 3) (LogicalLoc 1 2)))

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
                     (LogicalLoc 2 4)
                     (LogicalLoc 2 0))))
(check-expect (identify-chain d (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
              (Some
               (list
                (LogicalLoc 0 0)
                (LogicalLoc 1 0)
                (LogicalLoc 0 1)
                (LogicalLoc 1 1)
                (LogicalLoc 0 2)
                (LogicalLoc 0 3)
                (LogicalLoc 1 3)
                (LogicalLoc 0 4)
                (LogicalLoc 1 4))))
(check-expect (identify-chain c (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
              (Some (list (LogicalLoc 0 0))))
(check-expect (identify-chain b (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
              'None)

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

;; check-expects
(define newcopy
  (board-copy d))
(check-expect
 (begin
   (remove! newcopy
            (list
             (LogicalLoc 0 0)
             (LogicalLoc 1 0)
             (LogicalLoc 0 1)
             (LogicalLoc 1 1)
             (LogicalLoc 0 2)
             (LogicalLoc 0 3)
             (LogicalLoc 1 3)
             (LogicalLoc 0 4)
             (LogicalLoc 1 4))) newcopy)
 (vector
  (vector 'None 'None (Some 'black) (Some 'white) (Some 'white))
  (vector 'None 'None (Some 'black) (Some 'white) (Some 'white))
  (vector 'None (Some 'black) (Some 'black) (Some 'black) (Some 'white))
  (vector 'None 'None (Some 'black) (Some 'white) (Some 'white))
  (vector 'None 'None (Some 'black) (Some 'white) (Some 'white))))

;; similar to rev but with optional stones
(: rev-op : (Optional Stone) -> (Optional Stone))
(define (rev-op op)
  (match op
    [(Some 'white) (Some 'black)]
    [(Some 'black) (Some 'white)]
    ['None 'None]))

;; false, or illegal move if the new move would repeat a previous position
;; or if there is a stone already there
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go ll)
  (match (board-ref go ll)
    [(Some x) #f]
    [_ (match go
         [(Go board next history)
          (local {(: psuedo : Go)
                  (define psuedo (Go (board-copy board) next history))}
            (match (apply-move psuedo ll)
              [(Go temp-b _ _) 
               (not (repeat? temp-b history))]))])]))

(check-expect (legal-move?
               (Go sample-board 'white
                   (list (vector
                          (vector (Some 'black) (Some 'white) (Some 'black))
                          (vector (Some 'black) (Some 'white) 'None)
                          (vector (Some 'black) 'None 'None))))
               (LogicalLoc 1 0)) #t)

;; this function does not detect whether or not a move is illegal
;; after player puts down a stone, if necessary, capture/self-capture,
;; and updates the Go structure
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move go ll)
  (match go
    [(Go b next history)
     (local {(define oldboard (board-copy b))
             ;; listlist takes a list of LL (by calling neighbors)
             ;; and returns an appended (Listof LogicalLoc) to be removed from the board
             (: listlist : (Listof LogicalLoc) -> (Listof LogicalLoc))
             (define (listlist lls)
               (match lls
                 ['() '()]
                 [(cons hd tl)
                  (match (identify-chain b (list hd) (list hd))
                    ['None (listlist tl)]
                    [(Some liszt) (append liszt (listlist tl))])]))}
       (begin (bd-set! b ll (Some next))
              (remove! b (listlist (neighbors b ll (rev-op (bd-ref b ll)))))
              (remove-self! b (identify-chain b (list ll) (list ll)))
              (Go b (rev next) (cons oldboard history))))]))
      
(test)