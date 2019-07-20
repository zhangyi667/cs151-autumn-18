#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; CMSC 15100, University of Chicago, Autumn 2018
;; Project 1 reference implementation.
;; Prepared Nov 2018 by Adam Shaw.

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

;; ======

;; convert logical location to physical
;; - raise error if logical loc is off the board
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical lloc dim spec)
  (match lloc
    [(LogicalLoc col row)
     (if (or (< col 0) (< row 0) (>= col dim) (>= row dim))
         (error "off the board")
         (match spec
           [(BoardSpec _ cell margin _)
            (PhysicalLoc (+ margin (* cell col))
                         (+ margin (* cell (- dim row 1))))]))]))

(check-expect
 (logical->physical (LogicalLoc 0 0) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 10 50))

(check-expect
 (logical->physical (LogicalLoc 0 1) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 10 40))

(check-expect
 (logical->physical (LogicalLoc 1 1) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 20 40))

(check-expect
 (logical->physical (LogicalLoc 4 4) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 50 10))



;; Euclidean distance formula
(: distance : Integer Integer Integer Integer -> Real)
(define (distance x0 y0 x1 y1)
  (local {(define dx (- x0 x1))
          (define dy (- y0 y1))}
    (sqrt (+ (* dx dx) (* dy dy)))))

(check-expect (distance 0 0 10 0) 10)
(check-expect (distance 3 0 0 4) 5)

;; snap-to-grid moves a number on a number line to its nearest "grid line."
;; That is, it assumes something like this, with "m...m" meaning margin:
;; mmmmmG----G----G----G
;; The unknown is rounded to the nearest G.
;; The arguments to this function are as follows: 
;; - the first argument is margin size
;; - the second argument is the size of the gap between grid lines
;; - the third argument is the unknown that needs to be mapped
;; Negatives should not arise in practice, but they are mapped to the leftmost G.
;; There is no right end to the number line -- it extends forever.
(: snap-to-grid : Integer Integer Real -> Integer)
(define (snap-to-grid margin gap-size x)
  (if (>= x 0)
      (+ margin (* gap-size (exact-round (/ (- x margin) gap-size))))
      margin))

;; NOTE: I discovered in the process of testing this that
;;   (exact-round 0.5) => 0 and
;;   (round 0.5) => 0.0
;; This behavior, which I didn't expect, causes this test to fail:
;;   (check-expect (snap-to-grid 5 1 5.5) 6)
;; I'm not sure what to do about it, but it seems not to be a problem in practice.

(check-expect (snap-to-grid 5 1 4.9) 5)
(check-expect (snap-to-grid 5 1 5.1) 5)
(check-expect (snap-to-grid 5 1 5.6) 6)
(check-expect (snap-to-grid 5 1 5.9) 6)
(check-expect (snap-to-grid 5 1 9.9) 10)
(check-expect (snap-to-grid 5 5 4.9) 5)
(check-expect (snap-to-grid 5 5 5.9) 5)
(check-expect (snap-to-grid 5 5 9.9) 10)
(check-expect (snap-to-grid 5 5 -99999) 5)

;; on-the-board? checks if a logical location is on the board, given dimension
(: on-the-board? : Integer LogicalLoc -> Boolean)
(define (on-the-board? dim lloc)
  (match lloc
    [(LogicalLoc x y) (and (<= 0 x (sub1 dim)) (<= 0 y (sub1 dim)))]))

;; convert physical location to logical, if within stone radius of a stone location
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical ploc dim spec)
  (match ploc
    [(PhysicalLoc x y)
     (match spec
       [(BoardSpec _ cell margin stone)
        (local
          {(define nearx (snap-to-grid margin cell x))
           (define neary (snap-to-grid margin cell y))
           (define lloc-candidate
             (LogicalLoc (quotient (- nearx margin) cell)
                         ;; BUG1 -- comment this out, replace with below: (quotient (- (* dim cell) neary) cell)))}
                         (quotient (- (* (sub1 dim) cell) (- neary margin)) cell)))}
          (if (and (on-the-board? dim lloc-candidate)
                   (<= (distance x y nearx neary) stone))
              (Some lloc-candidate)
              'None))])]))

(check-expect
 (physical->logical (PhysicalLoc 10 50) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 0 0)))

(check-expect
 (physical->logical (PhysicalLoc 20 50) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 1 0)))

(check-expect
 (physical->logical (PhysicalLoc 20 40) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 1 1)))
        
(check-expect
 (physical->logical (PhysicalLoc 50 10) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 4 4)))

;; ======

;; The alphabet without the letter I.
(: alphabet-no-i (Listof Char))
(define alphabet-no-i
  (string->list "ABCDEFGHJKLMNOPQRSTUVWXYZ"))

;; convert the column index to a string label
;; 0 => "A", ..., 24 => "Z", 25 => "AA", ...
(: column->string : Integer -> String)
(define (column->string n)
  (make-string (add1 (quotient n 25))
               (list-ref alphabet-no-i (remainder n 25))))

(check-expect (column->string 0) "A")
(check-expect (column->string 24) "Z")
(check-expect (column->string 25) "AA")
(check-expect (column->string 26) "BB")

;; produce a string label for a logical location
;; ex: (logical->string (LogicalLoc 0 0)) => "A1"
(: logical->string : LogicalLoc -> String)
(define (logical->string lloc)
  (match lloc
    [(LogicalLoc col row)
     (string-append (column->string col) (number->string (add1 row)))]))

(check-expect (logical->string (LogicalLoc 0 0))  "A1")
(check-expect (logical->string (LogicalLoc 1 0))  "B1")
(check-expect (logical->string (LogicalLoc 0 1))  "A2")
(check-expect (logical->string (LogicalLoc 25 0)) "AA1")


;; Is the location among the given list of locations?
(: contains-loc? : LogicalLoc (Listof LogicalLoc) -> Boolean)
(define (contains-loc? lloc locs)
  (local
    {(define (== [k : LogicalLoc])
       (and (= (LogicalLoc-col lloc) (LogicalLoc-col k))
            (= (LogicalLoc-row lloc) (LogicalLoc-row k))))}
    (ormap == locs)))

(check-expect (contains-loc? (LogicalLoc 0 0)
                             (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
              #t)

(check-expect (contains-loc? (LogicalLoc 0 4)
                             (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
              #f)


;; return either Some stone or None by inspecting the board
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref g lloc)
  (match g
    [(Go _ bs ws _)
     (cond
       [(contains-loc? lloc bs) (Some 'black)]
       [(contains-loc? lloc ws) (Some 'white)]
       [else 'None])]))

;; flip to the next player
(: adv : Stone -> Stone)
(define (adv s)
  (match s
    ['white 'black]
    ['black 'white]))

;; put-stone-at:
;; Return (Some go+), where go+ includes the new stone, if possible.
;; Return 'None if location is already occupied.
;; Raise an error if it's not the turn to place that stone.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at lloc s g)
  (match g
    [(Go dim bs ws next)
     (if (not (symbol=? s next))
         (error (string-append "It's not " (symbol->string s) "'s turn."))
         (match (board-ref g lloc)
           [(Some _) 'None]
           ['None
            (match s
              ['black (Some (Go dim (cons lloc bs) ws (adv next)))]
              ['white (Some (Go dim bs (cons lloc ws) (adv next)))])]))]))

;; flip the next player, and leave all else about game the same
(: pass : Go -> Go)
(define (pass g)
  (match g
    [(Go dim bs ws next) (Go dim bs ws (adv next))]))

;; read the next player out of the go struct
(: next-player : World -> Stone)
(define (next-player w)
  (match w
    [(World _ (Go _ _ _ next) _) next]))

;; this applies pass to the go struct within the world and changes the status message
(: pass/world : World -> World)
(define (pass/world w)
  (World (World-spec w)
         (pass (World-game w))
         (string-append (capstone (next-player w)) " passed.")))

;; ======

;; check board-spec for non-ridiculosity
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? spec)
  (match spec
    [(BoardSpec bg cell margin stone)
     (and (positive? cell)
          (positive? margin)
          (positive? stone)
          (< stone (quotient cell 2))
          (> margin stone))]))

;; ======

;; build a new game of given dim, with default board spec if none is provided
(: initial-world : Integer (Optional BoardSpec) -> World)
(define (initial-world dim opt-spec)
  (local
    {(define sp
       (match opt-spec
         [(Some spec) spec]
         ['None (BoardSpec 'moccasin 24 16 4)]))}
    (World sp (Go dim '() '() 'black) "Welcome to Go!")))

;; draw the grid only, for overlay later
(: draw-grid : Integer Integer -> Image)
(define (draw-grid dim cell)
  (local {(define c (square cell 'outline 'black))
          (define row (foldr beside empty-image (make-list (sub1 dim) c)))}
    (foldr above empty-image (make-list (sub1 dim) row))))

"eyeball tests: draw-grid"
(draw-grid 2 10)
(draw-grid 3 12)
(draw-grid 19 8)

;; draw the row labels that go on the right edge of the board
;; 1 goes at the bottom, and numbers increase going up
(: row-labels : World -> Image)
(define (row-labels w)
  (match w
    [(World (BoardSpec _ cell margin _) (Go dim _ _ _) _)
     (above (square (max 0 (- margin (quotient cell 2))) 'solid 'white)
            (foldr (lambda ([row-label : String] [img : Image])
                     (above (overlay (text row-label 11 'black)
                                     (square cell 'outline 'white))
                            img))
                   empty-image
                   (build-list dim (lambda ([i : Integer]) (number->string (- dim i))))))]))

"eyeball tests: row-labels"
(row-labels (initial-world 3 'None))

;; draw column labels to go along the bottom of the board
(: column-labels : World -> Image)
(define (column-labels w)
  (match w
    [(World (BoardSpec _ cell margin _) (Go dim _ _ _) _)
     (foldr (lambda ([column-label : String] [img : Image])
              (beside (overlay (text column-label 10 'black)
                               (square cell 'outline 'white))
                      img))
            empty-image
            (build-list dim column->string))]))

"eyeball-tests: column-labels"
(column-labels (initial-world 19 'None))

;; produce the message panel that will appear at the bottom of the display
(: message-panel : World -> Image)
(define (message-panel w)
  (match w
    [(World (BoardSpec bg cell margin _) (Go dim _ _ next) stat)
     (local
       {(define msg : String
          (string-append stat
                         (if (symbol=? next 'black) " (b)" " (w)")))
        (define panel : Image
          (rectangle (+ (* cell (sub1 dim)) (* 2 margin)) 40 'solid bg))}
       (overlay (text msg 12 'black) panel))]))
     
;; draw the current state of the game, including labels and status message
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World (BoardSpec bg cell margin stone) (Go dim bs ws next) stat)
     (local
       {(define msg-panel (message-panel w))
        (define wood (square (+ (* 2 margin) (* cell (sub1 dim))) 'solid bg))
        (define empty-board (overlay (draw-grid dim cell) wood))
        (: draw-stone : Stone -> Image)
        (define (draw-stone s)
          (circle stone 'solid s))
        (: draw-on-board : Stone (Listof LogicalLoc) Image -> Image)
        (define (draw-on-board s llocs b)
          (match llocs
            ['() b]
            [(cons lloc tl)
             (match (logical->physical lloc dim (World-spec w))
               [(PhysicalLoc x y)
                (place-image (draw-stone s) x y (draw-on-board s tl b))])]))}
       (beside/align
        "top"
        (above (draw-on-board 'white ws (draw-on-board 'black bs empty-board))
               (column-labels w)
               msg-panel)
        (row-labels w)))]))

"eyeball tests: draw"
(draw (initial-world  8 'None))
(draw (initial-world  8 (Some (BoardSpec 'lightgreen 22 30 3))))
(draw (initial-world 10 'None))
(draw (initial-world 19 'None))

;; capstone -- return capitalized player color :-)
(define (capstone s)
  (match s
    ['black "Black"]
    ['white "White"]))

;; Place stone on click, if location is unoccupied.
(: react-to-mouse : World Integer Integer Mouse-Event -> World)
(define (react-to-mouse w x y e)
  (match w
    [(World spec (Go dim bs ws next) _)
     (match e
       ["button-down"
        (match (physical->logical (PhysicalLoc x y) dim spec)
          ['None w]
          [(Some lloc)
           (match (put-stone-at lloc next (World-game w))
             ['None (World spec (World-game w) "The selected location is already occupied.")]
             [(Some go+) (World spec
                                go+
                                (string-append (capstone next)
                                               " moved to "
                                               (logical->string lloc)
                                               "."))])])]
       [_ w])]))

;; Pass on "p" or "P".
(: react-to-keyboard : World String -> World)
(define (react-to-keyboard w key)
  (match key
    [(or "P" "p") (pass/world w)]
    [_ w]))

;; Play the game given dimension and board spec.
;; Raise error if dimension is less than 2 or spec is invalid.
(: play : Integer BoardSpec -> World)
(define (play dim spec)
  (cond
    [(< dim 2) (error "dimension must be at least 2")]
    [(not (valid-board-spec? spec)) (error "invalid board spec")]
    [else (big-bang (initial-world dim (Some spec)) : World
            [to-draw draw]
            [on-key react-to-keyboard]
            [on-mouse react-to-mouse])]))

;; (play 19 (BoardSpec 'moccasin 30 22 8))
      
(test)