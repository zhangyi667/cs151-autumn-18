#lang typed/racket
(require (only-in typed/racket/gui/base put-file get-file))
(require "../include/cs151-universe.rkt")
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)


(define-type Player (U 'Black 'White))

(define-struct OccupiedPoint
    ([color : Player]
     [count : Integer]))

(define-type Point (U OccupiedPoint 'EmptyPoint))

(define-struct Board
    ([points : (Listof Point)]
     [black-bar : Integer]
     [white-bar : Integer]
     [black-off : Integer]
     [white-off : Integer]))


(: draw-bar : Integer -> Image)
; to draw a black rectangle in preparation of the board
(define (draw-bar rad)
  (rectangle (* 2 rad) (* 10 rad) "solid" (color 80 20 20 255)))

(: draw-white-checker : Integer -> Image)
; built for white checker, used for testing
(define (draw-white-checker rad)
  (circle rad "solid" "white"))

(: draw-black-checker : Integer -> Image)
; built for black-checker, used for testing
(define (draw-black-checker rad)
  (circle rad "solid" "black"))

(: draw-label : String -> Image)
; built for label,used for testing
(define (draw-label str)
  (text str 12 "olive"))

(: draw-n-checkers : Integer Integer (Integer -> Image) (String -> Image)
   -> Image)
; to draw n checkers above one by one, if n>5, draw 5 only 
(define (draw-n-checkers rad n fdraw-checker label)
  (match n
    [0 empty-image]
    [(or 1 2 3 4 5) (above
        (fdraw-checker rad)
        (draw-n-checkers rad (- n 1) fdraw-checker label))]
    [else
     (above
      (overlay
      (label (number->string n))
             (fdraw-checker rad))
            (draw-n-checkers rad 4 fdraw-checker label))]))

(: draw-six-point : Integer Boolean Integer (Integer Boolean -> Image)
   (Integer Boolean -> Image) Integer
   -> Image)
; draw 6 points by direction and light/darck point color
(define (draw-six-point rad direction space fpoint1 fpoint2 current-index)
  (match current-index
    [5 (fpoint1 rad direction)]
    [_ (beside
      (fpoint1 rad direction)
      (rectangle space (* 10 rad) "outline" (color 80 20 20 255))
      (draw-six-point rad direction space fpoint2 fpoint1 (add1 current-index))
      )]))

(: draw-point : Integer Boolean Image-Color -> Image)
; a draw-point function for the convennient of draw-drak-point and
;draw-light-point function
(define (draw-point rad direction pointcolor)
  (if direction
      (isosceles-triangle (* 10.1 rad) 11.44 "solid" pointcolor)
      (isosceles-triangle (* 10.1 rad) 348.56 "solid" pointcolor)))

(: draw-dark-point : Integer Boolean -> Image)
; built for dark-point,used for testing
(define (draw-dark-point rad direction)
  (draw-point rad direction "saddlebrown"))

(: draw-light-point : Integer Boolean -> Image)
; built for light point,used for testing
(define (draw-light-point rad direction) (draw-point rad direction "moccasin"))

(: draw-above-checkers : Point Integer (Integer -> Image) (Integer -> Image)
   (String -> Image ) -> Image)
(define (draw-above-checkers p rad black-checker white-checker label)
; to draw the collection of checkers on one point
(match p
['EmptyPoint (rectangle (* 2 rad) (* 10 rad) "outline" (color 80 20 20 255))]
[(OccupiedPoint 'White count) (draw-n-checkers rad count white-checker label)]
[(OccupiedPoint 'Black count)
 (draw-n-checkers rad count black-checker label)]))

(: draw-checkers-helper : Integer  Integer (Integer -> Image)
   (Integer -> Image) (Listof Point) Y-Place Integer Integer Integer
   (String -> Image) -> Image)
;draw checkers of 6 points from deginning index to end index, which from 0 to 5
(define (draw-checkers-helper rad space
       black-checker white-checker ps y-position begin-index end-index offset
       label)
  (match end-index
    [5 (draw-above-checkers (list-ref ps begin-index) rad black-checker
                            white-checker label)]
    [_ (beside/align y-position 
      (draw-above-checkers (list-ref ps begin-index) rad
                           black-checker white-checker label)
      (rectangle space (* 10 rad) "outline" (color 80 20 20 255))
      (draw-checkers-helper rad space black-checker
        white-checker ps y-position (+ begin-index offset)
        (add1 end-index) offset label))]))

(define listofpoints
(list (OccupiedPoint 'Black 6)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint
  (OccupiedPoint 'Black 5)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint
        (OccupiedPoint 'Black 5)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint
        (OccupiedPoint 'Black 5)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint))

(: draw-checkers-in-secion-1 : Integer  Integer (Integer -> Image)
   (Integer -> Image) 
   (Listof Point) (String -> Image) -> Image)
; draw checkers on section 1(0 to 5 reversely)
(define (draw-checkers-in-secion-1 rad space black-checker white-checker ps
                                   label)
  (draw-checkers-helper rad space black-checker white-checker ps
                        "bottom" 5 0 -1 label))

(: draw-checkers-in-section-2 : Integer  Integer (Integer -> Image)
   (Integer -> Image) 
   (Listof Point) (String -> Image) -> Image)
; draw checkers on section 1(6 to 11 reversely)
(define (draw-checkers-in-section-2 rad space black-checker white-checker ps
                                    label)
  (draw-checkers-helper rad space black-checker white-checker ps "bottom" 11
                        0 -1 label))

(: draw-checkers-in-section-3 : Integer Integer (Integer -> Image)
   (Integer -> Image) 
   (Listof Point) (String -> Image) -> Image)
; draw checkers on section 3(12 to 17)
(define (draw-checkers-in-section-3 rad space black-checker white-checker ps
                                    label)
  (draw-checkers-helper rad space black-checker white-checker ps "top" 12 0 1
                        label))

(: draw-checkers-in-section-4 : Integer Integer (Integer -> Image)
   (Integer -> Image) 
   (Listof Point) (String -> Image) -> Image)
; draw checkers on section 3(18 to 23)
(define (draw-checkers-in-section-4 rad space black-checker white-checker ps
                                    label)
  (draw-checkers-helper rad space black-checker white-checker ps "top" 18 0 1
                        label))

(: draw-upper-board-background : Integer Integer (Integer Boolean -> Image)
   (Integer Boolean -> Image)
   -> Image)
; draw the background-triangle under the checker, on the upper part
(define (draw-upper-board-background rad space light-point dark-point)
  (beside
   (draw-bar rad)
   (draw-six-point rad #f space dark-point light-point 0)
   (draw-bar rad)
   (draw-six-point rad #f space dark-point light-point 0)
   (draw-bar rad)))

(: draw-upper-board : Integer  Integer (Integer -> Image)
   (Integer -> Image) (Integer Boolean -> Image) (Integer Boolean -> Image)
   (Listof Point) (String -> Image) Integer Integer -> Image)
; draw the upper part of the board. it contains section 3 and 4, and background
(define (draw-upper-board rad space black-checker white-checker dark-point
                          light-point ps label bb bo)
  (overlay
   (beside
    (draw-bar rad)
    (draw-checkers-in-section-3 rad space black-checker white-checker ps label)
     (overlay (text (number->string bb) 12 "white")
             (black-checker rad) (draw-bar rad))    
   (draw-checkers-in-section-4 rad space black-checker white-checker ps label)
    (overlay (text (number->string bo) 12 "white")
             (black-checker rad) (draw-bar rad)))
   (draw-upper-board-background rad space light-point dark-point)))

(: draw-lower-board-background : Integer Integer (Integer Boolean -> Image)
   (Integer Boolean -> Image)
   -> Image)
; draw the background-triangle under the checker, on the lower part
(define (draw-lower-board-background rad space light-point dark-point)
  (beside
   (draw-bar rad)
   (draw-six-point rad #t space light-point dark-point 0)
   (draw-bar rad)
   (draw-six-point rad #t space light-point dark-point 0)
   (draw-bar rad)))


(: draw-lower-board :  Integer Integer (Integer -> Image) (Integer -> Image)
 (Integer Boolean -> Image) (Integer Boolean -> Image) (Listof Point) 
 (String -> Image) Integer Integer -> Image)
; draw the lower part of the board, section 1 and 2, and background
(define (draw-lower-board rad space black-checker white-checker dark-point
                          light-point ps label wb wo)
  (overlay
   (beside
    (draw-bar rad)
    (draw-checkers-in-section-2 rad space black-checker white-checker ps label)
    (overlay (text (number->string wb) 12 "black")
                   (white-checker rad)
                   (draw-bar rad))
   (draw-checkers-in-secion-1 rad space black-checker white-checker ps label)
   (overlay (text (number->string wo) 12 "black")
                   (white-checker rad)
                   (draw-bar rad)))
   (draw-lower-board-background rad space light-point dark-point)))

(: draw-background : Integer Integer -> Image)
;built for background
(define (draw-background rad space)
  (rectangle (+ (* 26 rad) (* 10 space)) (* 30 rad)
             "solid" (color 80 20 20 255)))

(define-struct Style
    ([checker-radius : Integer]
     [spacing : Integer]
     [black-checker : (Integer -> Image)]
     [white-checker : (Integer -> Image)]
     [dark-point : (Integer Boolean -> Image)]
     [light-point : (Integer Boolean -> Image)]
     [background : (Integer Integer -> Image)]
     [label : (String -> Image)]
     [black-die : (Integer Integer -> Image)]
     [white-die : (Integer Integer -> Image)]))

(: listofinteger : Integer -> (Listof Integer))
; to get different list of integers for different number of pips
(define (listofinteger x)
  (cond 
    ; 0 represents an empty die
      [(= x 0) (list 0 0 0 0 0 0 0 0 0)]
      [(= x 1) (list 0 0 0 0 1 0 0 0 0)]
      [(= 2 x)(list 1 0 0 0 0 0 0 0 1)]
      [(= x 3) (list 0 0 1 0 1 0 1 0 0)]
      [(= x 4) (list 1 0 1 0 0 0 1 0 1)]
      [(= x 5) (list 1 0 1 0 1 0 1 0 1)]
      [(= x 6) (list 1 0 1 1 0 1 1 0 1)]
      [else (error"bad input")]))

(check-expect (listofinteger 5) '(1 0 1 0 1 0 1 0 1))
(check-expect (listofinteger 1) '(0 0 0 0 1 0 0 0 0))

(: dice-dot : Integer Integer (Listof Integer) Image-Color Image-Color -> Image)
(define (dice-dot n radius xs background-color die-color)
  (circle (* 0.3 radius) "solid" (if (= (list-ref xs n) 0) background-color die-color))
)

(: dice-row : Integer (Listof Integer) (Listof Integer) Image-Color Image-Color -> Image)
(define (dice-row radius indice xs background-color die-color)
  (match indice
    [(cons f (cons s (cons t '())))
      (beside
        (dice-dot f radius xs background-color die-color)
        (dice-dot s radius xs background-color die-color)
        (dice-dot t radius xs background-color die-color)
      )
    ]
    [_ (error "Indice should be length 3 only")]
  )
)

(: draw-die-with-color : Integer (Listof Integer)
   Image-Color Image-Color -> Image)
; to draw the black or white die with different number of pips
(define (draw-die-with-color radius xs background-color die-color)
  (overlay
    (above
      (dice-row radius (build-list 3 (lambda ([x : Integer]) (+ 0 x))) xs background-color die-color)
      (dice-row radius (build-list 3 (lambda ([x : Integer]) (+ 3 x))) xs background-color die-color)
      (dice-row radius (build-list 3 (lambda ([x : Integer]) (+ 6 x))) xs background-color die-color)  
    )
    (square (* 3 radius) "solid" background-color)
  )
)

(: black-die : Integer Integer -> Image)
; to define a black-die use for testing
(define (black-die radius number)
  (draw-die-with-color radius (listofinteger number) "black" "white"))

(: white-die : Integer Integer -> Image)
; to define a white-die use for testing
(define (white-die radius number)
  (draw-die-with-color radius (listofinteger number) "white" "black"))

(: draw-middle-part : Integer Integer (Integer Integer -> Image)
   (Integer Integer -> Image) (Listof Integer)
   Image-Color -> Image)
;to draw the middle part of the board, which shows the number of black bar,
;white bar, number of black-off and white-off
(define (draw-middle-part rad space black-die white-die pips
                          outlinec)
  (beside
   (draw-bar rad)
   (overlay
        (above
             (white-die rad (list-ref pips 0))
             (white-die rad (list-ref pips 1)))         
   (rectangle (+ (* 12 rad) (* 5 space))  (* 10 rad) "outline" outlinec))
   (draw-bar rad)
   (overlay
           (above
             (black-die rad (list-ref pips 2))
             (black-die rad (list-ref pips 3)))
       (rectangle (+ (* 12 rad) (* 5 space))  (* 10 rad) "outline" outlinec))
   (draw-bar rad)))

(: draw-board : Style Board (Listof Integer) -> Image)
; draw board, given the inputs style and board, and the list of integer of
;a given number of pips on the dice
(define (draw-board style board dice-value)
  (match* (style board)
    
    [((Style rad space black-checker white-checker dark-point
             light-point background label blackdie whitedie) 
      (Board ps bb wb bo wo)) 
      (overlay
        (above
          (draw-upper-board rad space black-checker
                            white-checker dark-point light-point ps label bb bo)
          ;middle part: render the black/white bar,
          ;and the checkers that has been bourned off
          ;consider using outline to display the actual background color
          (draw-middle-part rad space blackdie whitedie dice-value
                          (color 80 20 20 255))
          (draw-lower-board rad  space black-checker
                        white-checker dark-point light-point ps label wb wo))
        (background rad space))]))

(define-struct PointNum
   ([num : Integer]))

(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
    'BlackDice 'WhiteDice 'Nowhere))

(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
    'Nowhere))

(: highlight : ClickLoc Integer Integer -> Image)
; to draw the red outlined recatngle on the highlight area,given the radius of 
;checker, the spacing and the click location of the area which needs to
;highlight
(define (highlight cl rad spacing)
  (match cl
    ['BlackBar (above (beside (rectangle (+ (* 14 rad) (* 5 spacing))
                                         (* 10 rad)  "outline"
                          "white")
                       (rectangle (* 2 rad) (* 10 rad) "outline" "red")
                       (rectangle (+ (* 14 rad) (* 5 spacing)) (* 10 rad)
                                  "outline"
                          "white"))
                      (rectangle (+ (* 30 rad) (* 10 spacing)) (* 20 rad)
                                 "outline" "white"))]
    ['BlackOff (above (beside (rectangle (+ (* 28 rad) (* 10 spacing))
                                         (* 10 rad) "outline" "white")
                              (rectangle (* 2 rad) (* 10 rad) "outline" "red"))
                      (rectangle (+ (* 30 rad) (* 10 spacing))
                                 (* 20 rad) "outline" "white"))]
    ['WhiteBar (above (rectangle (+ (* 30 rad) (* 10 spacing))
                                 (* 20 rad) "outline" "white")
                      (beside (rectangle (+ (* 14 rad) (* 5 spacing))
                                         (* 10 rad)  "outline"
                          "white")
                       (rectangle (* 2 rad) (* 10 rad) "outline" "red")
                       (rectangle (+ (* 14 rad) (* 5 spacing)) (* 10 rad)
                                  "outline"
                          "white")))]
    ['WhiteOff (above (rectangle (+ (* 30 rad) (* 10 spacing))
                                 (* 20 rad) "outline" "white")
                      (beside (rectangle (+ (* 28 rad) (* 10 spacing))
                                         (* 10 rad) "outline" "white")
                              (rectangle (* 2 rad) (* 10 rad)
                                         "outline" "red")))]
    [(PointNum x) (cond 
                     [(and (>= x 13) (<= x 18))
                      (above  (beside   (rectangle (+ (* 2 rad)
                                                      (* (- x 13)
                          (+ (* 2 rad) spacing))) (* 10 rad) "outline" "black")
                                             (rectangle (* 2 rad)
                                               (* 10 rad) "outline" "red")
                                      (rectangle (+ (+ (* 16 rad)
                           (* 5 spacing)) (* (- 18 x) (+ (* 2 rad) spacing)))
                                                 (* 10 rad) "outline" "black"))
                                                (rectangle (+ (* 30 rad)
                               (* 10 spacing)) (* 20 rad) "outline" "white"))]
                     [(and (>= x 7) (<= x 12)) (above (rectangle
                                        (+ (* 30 rad) (* 10 spacing))
                                        (* 20 rad) "outline" "white")
                                                (beside   (rectangle
                            (+ (* 2 rad) (* (- 12 x) (+ (* 2 rad) spacing)))
                            (* 10 rad) "outline" "white")
                                                     (rectangle
                                    (* 2 rad) (* 10 rad) "outline" "red")
                                                     (rectangle
                     (+ (+ (* 16 rad) (* 5 spacing)) (* (- x 7) (+ (* 2 rad)
                                 spacing))) (* 10 rad) "outline" "white")))]
                     [(and (>= x 1) (<= x 6)) (above (rectangle
                   (+ (* 30 rad) (* 10 spacing)) (* 20 rad) "outline" "white")
                                                     (beside
                (rectangle (+ (* 16 rad) (* 5 spacing) (* (- 6 x)
                         (+ (* 2 rad) spacing))) (* 10 rad) "outline" "white")
                                                        (rectangle
                                      (* 2 rad) (* 10 rad) "outline" "red")
                    (rectangle (+ (* 2 rad) (* (- x 1) (+ (* 2 rad) spacing)))
                               (* 10 rad) "outline" "white")))]
                     [(and (>= x 19) (<= x 24)) (above 
                                                     (beside
                              (rectangle (+ (* 16 rad) (* 5 spacing)
                                      (* (- x 19) (+ (* 2 rad) spacing)))
                                         (* 10 rad) "outline" "white")
                                                        (rectangle
                                         (* 2 rad) (* 10 rad) "outline" "red")
                                                        (rectangle
      (+ (* 2 rad) (* (- 24 x) (+ (* 2 rad) spacing)))
      (* 10 rad) "outline" "white"))
      (rectangle (+ (* 30 rad) (* 10 spacing)) (* 20 rad) "outline" "white"))]
                     [else empty-image])]
  [_ empty-image]))

(: draw-board-with-highlight : Style Board (Listof Integer) ClickLoc -> Image)
; to overlay the red outlined rectangle(highlight) on the board
(define (draw-board-with-highlight style board pips cl)
  (overlay (highlight cl (Style-checker-radius style) (Style-spacing style))
           (draw-board style board pips)))

(define test-style : Style (Style 10 10 draw-black-checker
      draw-white-checker draw-dark-point draw-light-point
      draw-background draw-label black-die white-die))

(define test-board : Board (Board 
  (list (OccupiedPoint 'Black 2)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint
  (OccupiedPoint 'Black 5)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint
        (OccupiedPoint 'Black 5)
        (OccupiedPoint 'White 4)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint
        (OccupiedPoint 'Black 5)
        (OccupiedPoint 'White 6)
        (OccupiedPoint 'Black 3)
        (OccupiedPoint 'White 2)
        (OccupiedPoint 'Black 1)
        'EmptyPoint)
  1 0 3 0)
)

(define game-almost-finished-board : Board (Board 
  (append
    (list (OccupiedPoint 'White 1))
    (make-list 22 'EmptyPoint)
    (list (OccupiedPoint 'Black 1))
  )
  0 14 14 0)
)

(define initial-board : Board (Board 
  (list (OccupiedPoint 'Black 2)
        'EmptyPoint
        'EmptyPoint
        'EmptyPoint
        'EmptyPoint
        (OccupiedPoint 'White 5)
        
        'EmptyPoint
        (OccupiedPoint 'White 3)
        'EmptyPoint
        'EmptyPoint
        'EmptyPoint
        (OccupiedPoint 'Black 5)
        
        (OccupiedPoint 'White 5)
        'EmptyPoint
        'EmptyPoint
        'EmptyPoint
        (OccupiedPoint 'Black 3)
        'EmptyPoint
        
        (OccupiedPoint 'Black 5)
        'EmptyPoint        
        'EmptyPoint
        'EmptyPoint
        'EmptyPoint
        (OccupiedPoint 'White 2))
  0 0 0 0)
)

(: euclidean-distance : Real Real Real Real -> Real)
; to caculate the euclidean-distance of two points, given the x y respectively
(define (euclidean-distance x y x1 y1)
  (sqrt (+ (* (- x x1) (- x x1)) (* (- y y1) (- y y1)))))

(check-expect (euclidean-distance 2 1 2 3) 2)
(check-within (euclidean-distance 3 5 7 6) 4.12 0.1) 

(: in-left-section? : Integer Integer Integer -> Boolean)
; to determine whether a click is in the left section
(define (in-left-section? x rad space)
  (and (>= x (* 2 rad)) (< x (+ (* 14 rad) (* 5 space))))
)

(check-expect (in-left-section? 60 10 10) #t)
(check-expect (in-left-section? 300 10 10) #f)

(: in-right-section? : Integer Integer Integer -> Boolean)
; to determine whether a click is in the right section
(define (in-right-section? x rad space)
  (and (>= x (+ (* 16 rad) (* 5 space))) (< x (+ (* 28 rad) (* 10 space))))
)

(check-expect (in-right-section? 60 10 10) #f)
(check-expect (in-right-section? 300 10 10) #t)


(: calculate-point-from-x-axis-upper : Integer Integer Integer
   Integer Integer Integer -> ClickLoc)
;; this method returns a ClickLoc according to the x-axis given,
;;either a PointNum or 'Nowhere
;; in my world the euclidean-distance between Point1 and Point2 is (2*rad + space),
;;use this recursive method
;; to guess if the mouse has clicked in a point or not
;; target: the x-axis value when click a point
;; current: the x-axis of the left side of a Point.
;;if (target - current) < 2*rad, means it's in the Point
;; this method is used in the upper part of board
(define (calculate-point-from-x-axis-upper current result
                                           init rad space target)
  (if (and (< current target) (>= (+ current rad rad space) target))
    (if (< target (+ current rad rad))
      (PointNum (+ init result -1))
      'Nowhere
    )
    (calculate-point-from-x-axis-upper (+ current rad rad space) (add1 result)
                                       init rad space target)
  )
)

(: calculate-point-from-x-axis-lower : Integer Integer Integer Integer
   Integer Integer -> ClickLoc)
;; this method is the same with calculate-point-from-x-axis-upper except
;;it's only uesd in lower board
(define (calculate-point-from-x-axis-lower current result init rad space
                                           target)
  (if (and (< current target) (>= (+ current rad rad space) target))
    (if (< target (+ current rad rad))
      (PointNum (- init (- result 1)))
      'Nowhere
    )
    (calculate-point-from-x-axis-lower (+ current rad rad space)
                                       (add1 result) init rad space target)))

(: click-where-helper : Integer Integer Integer Integer -> ClickLoc)
; the helper function of click where
(define (click-where-helper rad space x y)
  (cond
    ; off/bar
    [(<= (euclidean-distance x y (+ (* rad 15) (* 5 space)) (* rad 5))
      rad)
    'BlackBar]

    [(<= (euclidean-distance x y (+ (* rad 29)(* 10 space)) (* rad 5))
      rad)
    'BlackOff]

    [(<= (euclidean-distance x y (+ (* rad 15)(* 5 space)) (* rad 25))
      rad)
    'WhiteBar]

    [(<= (euclidean-distance x y (+ (* rad 29)(* 10 space)) (* rad 25))
      rad)
    'WhiteOff]  

    ; upper part 13~24
    [(<= y (* rad 10)) 
      (cond
        ; if in section 3
        [(in-left-section? x rad space)
         (calculate-point-from-x-axis-upper (* 2 rad) 1 13 rad space x)]
        ; if in section 4
        [(in-right-section? x rad space)
         (calculate-point-from-x-axis-upper (+ (* 16 rad)
                                               (* 5 space)) 1 19 rad space x)]
        [else 'Nowhere]
      )
    ]
    ; mid part dice
    [(and (> y (* 10 rad)) (< y (* 20 rad)))
      (cond
        [(and (<= x (+ (* 9 rad) (* 3 space)))
              (>= x (+ (* 7 rad) (* 2 space)))
              (<= y (* 18 rad))
              (>= y (* 12 rad))
          ) 
        'WhiteDice]

        [(and (<= x (+ (* 23 rad) (* 8 space)))
              (>= x (+ (* 21 rad) (* 7 space)))
              (<= y (* 18 rad))
              (>= y (* 12 rad))
          ) 
        'BlackDice]

        [else 'Nowhere]  
      )
    ]
    ; lower part 1~12
    [(>= y (* rad 20)) 
      (cond
        ; if in section 2
        [(in-left-section? x rad space)
         (calculate-point-from-x-axis-lower (* 2 rad) 1 12 rad space x)]
        ; if in section 1
        [(in-right-section? x rad space)
         (calculate-point-from-x-axis-lower
          (+ (* 16 rad) (* 5 space)) 1 6 rad space x)]
        [else 'Nowhere]
      )
    ]
    [else 'Nowhere]
  ) 
)

(: click-where : Style Integer Integer -> ClickLoc)
; to determine a click location of given style and x y of a click
(define (click-where style x y)
  (match style
    [(Style rad space _ _ _ _ _ _ _ _) (click-where-helper rad space x y)]))

(check-expect (click-where test-style 30 10) (PointNum 13))
(check-expect (click-where test-style 5 5) 'Nowhere)
(check-expect (click-where test-style 25 5) (PointNum 13))
(check-expect (click-where test-style 45 5) 'Nowhere)
(check-expect (click-where test-style 25 250) (PointNum 12))
(check-expect (click-where test-style 45 250) 'Nowhere)
(check-expect (click-where test-style 220 5) (PointNum 19))
(check-expect (click-where test-style 250 5) (PointNum 20))
(check-expect (click-where test-style 265 5) 'Nowhere)
(check-expect (click-where test-style 280 5) (PointNum 21))
(check-expect (click-where test-style 220 250) (PointNum 6))
(check-expect (click-where test-style 250 250) (PointNum 5))
(check-expect (click-where test-style 370 20) (PointNum 24))
(check-expect (click-where test-style 370 250) (PointNum 1))
(check-expect (click-where test-style 370 170) 'Nowhere)


(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace the item at the given position
;; position counting starts at 0
;; ex: (replace-at 0 'Z '(a b c)) -> '(Z b c)
;; ex: (replace-at 1 'Z '(a b c)) -> '(a Z c)
;; get from lab4
(define (replace-at i x xs)
  (match* (i xs)
    [(0  (cons hd tl)) (cons x tl)]
    [(1  (cons hd tl)) (cons hd (replace-at 0 x tl))]
    [(_  (cons hd tl)) (cons hd (replace-at (- i 1) x tl))]
    [(_ _) (error "badinput")]))

(check-expect (replace-at 0 'Z '(a b c)) '(Z b c))
(check-expect (replace-at 1 'Z '(a b c)) '(a Z c))

(: replace-point : Player Point Integer -> Point)
; to add i to point 
(define (replace-point c p i)
  (match p
    ['EmptyPoint (if (> i 0) (OccupiedPoint c 1)
                     (error "cannot remove a checker from empty point"))]
    [(OccupiedPoint c count) (if (= 0 (+ count i))
                                 'EmptyPoint (OccupiedPoint c (+ count i)))]))

(check-expect (replace-point 'White (OccupiedPoint 'White 5) 1) (OccupiedPoint
                                                                 'White 6))

(: replace-point-at : Player Integer (Listof Point) Integer -> (Listof Point))
;to add v to a list of points whose index is "index"
(define (replace-point-at player index points v)
  (replace-at 
    (- index 1) 
      (replace-point 
         player
        (list-ref points (- index 1)) 
        v) points))

(check-expect (replace-point-at 'White 1 (append (list (OccupiedPoint 'Black 2))
  (make-list 4 'EmptyPoint)
  (list (OccupiedPoint 'White 5))) -1) (list (OccupiedPoint 'Black 1)
                                             'EmptyPoint 'EmptyPoint
                                             'EmptyPoint 'EmptyPoint
                                             (OccupiedPoint 'White 5)))

(define-struct Game
 ([board : Board]
 [turn : Player]
 [moves : (Listof Integer)]))

(define-struct World
  ( [game : Game]
    [style : Style]
    ; clicked-point records the last click, so the code could handle the highlight
    [clicked-point : ClickLoc]
    [dice-value : (Listof Integer)]
    ; status is either "start" or "progress", to help the program to identify the status of game
    [status : String]
    [history : (Listof Game)]
  )
)

(: opposite-bar : Player -> (U 'BlackBar 'WhiteBar))
; to determine the opposite bar of a given player type
(define (opposite-bar player)
  (if (symbol=? player 'Black) 'WhiteBar 'BlackBar))

(check-expect (opposite-bar 'White) 'BlackBar)
(check-expect (opposite-bar 'Black) 'WhiteBar)

(: click (World Integer Integer Mouse-Event -> World))
(define (click w x y e)
  (match* (w e)
    [((World game style prev-loc dice-value status _) "button-down")
      (click-helper w (click-where style x y))
    ]
    [(_ _) w]
  )  
)

(: is-prev-loc-nowhere? : ClickLoc -> Boolean)
; returns true when prev-loc is 'Nowhere
(define (is-prev-loc-nowhere? clickloc)
  (match clickloc
    ['Nowhere #t]
    [_ #f]
  )
)

(check-expect (is-prev-loc-nowhere? 'Nowhere) #t)
(check-expect (is-prev-loc-nowhere? 'BlackBar) #f)

(: is-same-location? : ClickLoc ClickLoc -> Boolean)
; return true if l1 and l2 are same location
(define (is-same-location? l1 l2)
  (match* (l1 l2)
    [((PointNum n1) (PointNum n2)) (= n1 n2)]
    [('BlackBar 'BlackBar) #t]
    [('WhiteBar 'WhiteBar) #t]
    [('BlackOff 'BlackOff) #t]
    [('WhiteOff 'WhiteOff) #t]
    [('BlackDice 'BlackDice) #t]
    [('WhiteDice 'WhiteDice) #t]
    [('Nowhere 'Nowhere) #t]
    [(_ _) #f]
  )
)
(check-expect (is-same-location? (PointNum 1) (PointNum 2)) #f)
(check-expect (is-same-location? (PointNum 1) (PointNum 1)) #t)
(check-expect (is-same-location? 'BlackBar 'BlackBar) #t)
(check-expect (is-same-location? 'BlackBar 'BlackOff) #f)
(check-expect (is-same-location? 'BlackBar 'WhiteBar) #f)
(check-expect (is-same-location? 'BlackBar 'WhiteOff) #f)
(check-expect (is-same-location? 'WhiteBar 'WhiteBar) #t)
(check-expect (is-same-location? 'BlackOff 'BlackOff) #t)
(check-expect (is-same-location? 'WhiteOff 'WhiteOff) #t)
(check-expect (is-same-location? 'BlackDice 'BlackDice) #t)
(check-expect (is-same-location? 'WhiteDice 'WhiteDice) #t)
(check-expect (is-same-location? 'Nowhere 'Nowhere) #t)

(: click-loc->board-loc : ClickLoc -> BoardLoc)
; transfer a ClickLoc to BoardLoc
(define (click-loc->board-loc loc)
  (match loc
    ['BlackDice (error "cannot convert BlackDice to BoardLoc")]
    ['WhiteDice (error "cannot convert WhiteDice to BoardLoc")]
    [(PointNum num) (PointNum num)]
    ['BlackBar 'BlackBar]
    ['WhiteBar  'WhiteBar]
    ['BlackOff 'BlackOff]
    ['WhiteOff 'WhiteOff]
    ['Nowhere 'Nowhere]
  )
)

(check-expect (click-loc->board-loc (PointNum 1)) (PointNum 1))
(check-expect (click-loc->board-loc 'BlackBar) 'BlackBar)
(check-expect (click-loc->board-loc 'WhiteBar) 'WhiteBar)

(: click-helper : World ClickLoc -> World)
(define (click-helper w loc)
  (match w
    [(World (Game board turn moves) style prev-loc dice-value status histories)
    ; if this is first click or previous click was on dice, record this click and highlight it
      (if (is-prev-loc-nowhere? prev-loc) 
          (match loc
            ['Nowhere w]
            ; if there are available moves, no actions will be taken
            ; else, change turn
            ; update-world-moves-by-rolling-dice
            ['BlackDice (if (and (not (available-moves? (World-game w))) (symbol=? turn 'White))
                          (update-world-moves-by-rolling-dice w (+ 1 (random 6)) (+ 1 (random 6)) 'Black)
                          w
                           )
            ]
            ['WhiteDice (if (and (not (available-moves? (World-game w))) (symbol=? turn 'Black)) 
                          (update-world-moves-by-rolling-dice w (+ 1 (random 6)) (+ 1 (random 6)) 'White)
                          w
                           )]
            [_ (World (Game board turn moves) style loc dice-value status histories)]
          )
          ; if both clicks are same place, cancel the highlight
          (if (is-same-location? prev-loc loc)
            (World (Game board turn moves) style 'Nowhere dice-value status histories)
            ; else, make move from previous click and this click
            ; and if this click is on Nowhere or Dice, then no action
            (match loc
              ['Nowhere w]
              ['BlackDice w]
              ['WhiteDice w]
              [_ (validate-and-apply-moves w (click-loc->board-loc loc))]
            )
          )
      )
    ]
  )
)

(: validate-and-apply-moves : World BoardLoc -> World)
; this method validate and apply the moves from prev-loc in world, and current-loc
; if the move prev-loc to current-loc is illegal, no action will be taken.
; else, make the move.
(define (validate-and-apply-moves w current-loc)
  (match w
    [(World game style prev-loc dice-value status histories)
    ; do validation
      (if (legal-move? game (click-loc->board-loc prev-loc) current-loc)
        (World 
          (apply-move game (click-loc->board-loc prev-loc) current-loc)
          style
          'Nowhere
          dice-value
          status
          (cons game histories)
        )
        w
      )
    ]
  )
)

(: distance : BoardLoc BoardLoc -> Integer)
; returns 0 if it's an illegal move
(define (distance l1 l2)
  (match* (l1 l2)
    [((PointNum n1) (PointNum n2)) (- n2 n1)]
    [('BlackBar (PointNum num)) num]
    [('WhiteBar (PointNum num)) (- num 25)]
    [((PointNum num) 'BlackOff) (- 25 num)]
    [((PointNum num) 'WhiteOff) (- 0 num)]
    ; if a checker would be eaten
    [((PointNum num) 'BlackBar) 0]
    [((PointNum num) 'WhiteBar) 0]
    [(_ _) 0]
  )
)
(check-expect (distance (PointNum 1) (PointNum 12)) 11)
(check-expect (distance (PointNum 12) (PointNum 7)) -5)
(check-expect (distance 'BlackBar (PointNum 12)) 12)
(check-expect (distance 'WhiteBar (PointNum 22)) -3)
(check-expect (distance (PointNum 20) 'BlackOff) 5)
(check-expect (distance (PointNum 20) 'WhiteOff) -20)
(check-expect (distance (PointNum 20) 'Nowhere) 0)


(: is-move-from-bar-legal? : (U 'BlackBar 'WhiteBar) Point Player -> Boolean)
; check if move from bar to point is legal. 
; the validation includes turn compare, moves check, etc.
(define (is-move-from-bar-legal? bar point turn)
  (match* (bar turn point)
    [('WhiteBar 'Black _) #f]
    [('BlackBar 'White _) #f]
    [('BlackBar 'Black 'EmptyPoint) #t]
    [('WhiteBar 'White 'EmptyPoint) #t]
    [('BlackBar 'Black (OccupiedPoint color count)) (or (symbol=? color 'Black) (= count 1))]
    [('WhiteBar 'White (OccupiedPoint color count)) (or (symbol=? color 'White) (= count 1))]
    [(_ _ _) (error "Invalid input")]
  )
)

(check-expect (is-move-from-bar-legal? 'WhiteBar 'EmptyPoint 'Black) #f)
(check-expect (is-move-from-bar-legal? 'WhiteBar (OccupiedPoint 'Black 1) 'Black) #f)
(check-expect (is-move-from-bar-legal? 'BlackBar 'EmptyPoint 'White) #f)
(check-expect (is-move-from-bar-legal? 'BlackBar (OccupiedPoint 'Black 1) 'White) #f)
(check-expect (is-move-from-bar-legal? 'BlackBar 'EmptyPoint 'Black) #t)
(check-expect (is-move-from-bar-legal? 'WhiteBar 'EmptyPoint 'White) #t)
(check-expect (is-move-from-bar-legal? 'BlackBar (OccupiedPoint 'Black 2) 'Black) #t)
(check-expect (is-move-from-bar-legal? 'BlackBar (OccupiedPoint 'White 1) 'Black) #t)
(check-expect (is-move-from-bar-legal? 'BlackBar (OccupiedPoint 'White 2) 'Black) #f)
(check-expect (is-move-from-bar-legal? 'WhiteBar (OccupiedPoint 'White 2) 'White) #t)
(check-expect (is-move-from-bar-legal? 'WhiteBar (OccupiedPoint 'Black 1) 'White) #t)
(check-expect (is-move-from-bar-legal? 'WhiteBar (OccupiedPoint 'Black 2) 'White) #f)


(: is-move-to-off-legal? : Point (U 'BlackOff 'WhiteOff) Player -> Boolean)
; check if move from a point to off is legal. 
; the validation includes turn compare, moves check, etc.
(define (is-move-to-off-legal? point off turn)
  (match* (off turn point)
    [('BlackOff 'Black (OccupiedPoint color count)) (symbol=? color 'Black)]
    [('WhiteOff 'White (OccupiedPoint color count)) (symbol=? color 'White)]
    [(_ _ _) #f]
  )
)

(check-expect (is-move-to-off-legal? 'EmptyPoint 'BlackOff 'Black) #f)
(check-expect (is-move-to-off-legal? 'EmptyPoint 'BlackOff 'White) #f)
(check-expect (is-move-to-off-legal? 'EmptyPoint 'WhiteOff 'Black) #f)
(check-expect (is-move-to-off-legal? 'EmptyPoint 'WhiteOff 'White) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'Black 2) 'BlackOff 'White) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'Black 2) 'WhiteOff 'White) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'Black 2) 'WhiteOff 'Black) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'White 2) 'BlackOff 'White) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'White 2) 'BlackOff 'Black) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'White 2) 'WhiteOff 'Black) #f)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'Black 2) 'BlackOff 'Black) #t)
(check-expect (is-move-to-off-legal? (OccupiedPoint 'White 2) 'WhiteOff 'White) #t)

(: is-point-to-point-legal-move? : Point Point Player -> Boolean)
; this method returns true if a move from point p1 to point p2 is legal
(define (is-point-to-point-legal-move? p1 p2 turn)
  (match* (p1 p2)
    [('EmptyPoint _) #f]
    [((OccupiedPoint color count) 'EmptyPoint) (symbol=? turn color)]
    [((OccupiedPoint color1 count1) (OccupiedPoint color2 count2)) 
      (and
        (symbol=? turn color1)
        (or (symbol=? turn color2) (= count2 1))
        )
    ]
  )
)

(check-expect (is-point-to-point-legal-move? 'EmptyPoint 'EmptyPoint 'Black) #f)
(check-expect (is-point-to-point-legal-move? 'EmptyPoint (OccupiedPoint 'Black 1) 'Black) #f)
(check-expect (is-point-to-point-legal-move? (OccupiedPoint 'Black 1) 'EmptyPoint 'Black) #t)
(check-expect (is-point-to-point-legal-move? (OccupiedPoint 'Black 1) 'EmptyPoint 'White) #f)
(check-expect (is-point-to-point-legal-move? (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 3) 'White) #f)
(check-expect (is-point-to-point-legal-move? (OccupiedPoint 'Black 2) (OccupiedPoint 'White 3) 'White) #f)
(check-expect (is-point-to-point-legal-move? (OccupiedPoint 'Black 2) (OccupiedPoint 'Black 3) 'Black) #t)
(check-expect (is-point-to-point-legal-move? (OccupiedPoint 'Black 2) (OccupiedPoint 'White 1) 'Black) #t)

(: move-list-contains-the-move? : Integer (Listof Integer) -> Boolean)
; return if the given list ls contains the value num
(define (move-list-contains-the-move? num ls)
  (match ls
    ['() false]
    [(cons head tail) (if (= head num) #t (move-list-contains-the-move? num tail))]
  )
)

(check-expect (move-list-contains-the-move? 1 (list 1 2 3)) #t)
(check-expect (move-list-contains-the-move? 4 (list 1 2 3)) #f)
(check-expect (move-list-contains-the-move? 4 '()) #f)

(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
; this method return true if the move list contains such steps
; and the move l1->l2 is legal
; e.g. if distance of l1 to l2 is 5, and moves is '(1 5) then it's true
(define (legal-move? game l1 l2)
  (match game
    [(Game board turn moves) 
      (match* (l1 l2)
        ; when a check is eaten, this move shall always be true
        [((PointNum n) 'BlackBar) #t]
        [((PointNum n) 'WhiteBar) #t]
        [(_ _) (and 
          (move-list-contains-the-move? (abs (distance l1 l2)) moves) 
          (legal-action? game l1 l2)
          )
        ]
      )
    ]
  )
)

(check-expect (legal-move? (Game initial-board 'White '(2 5)) (click-loc->board-loc (PointNum 24)) (PointNum 22)) #t)

(: legal-action-from-black-bar : Game BoardLoc BoardLoc -> Boolean)
; if there's checkers in black bar, then you can't do any other moves 
; except moving from black bar to point
(define (legal-action-from-black-bar game l1 l2)
  (match game
    [(Game (Board points _ _ _ _) turn moves)
      (match* (l1 l2)
        [('BlackBar (PointNum n2))
          (is-move-from-bar-legal? 'BlackBar (list-ref points (- n2 1)) turn)
        ]
        [(_ _) #f]
      )
    ]
  )
)


(: legal-action-from-white-bar : Game BoardLoc BoardLoc -> Boolean)
; if there's checkers in white bar, then you can't do any other moves 
; except moving from white bar to point
(define (legal-action-from-white-bar game l1 l2)
  (match game
    [(Game (Board points _ _ _ _) turn moves)
      (match* (l1 l2)
        [('WhiteBar (PointNum n2))
          (is-move-from-bar-legal? 'WhiteBar (list-ref points (- n2 1)) turn)
        ]
        [(_ _) #f]
      )
    ]
  )
)

(: legal-action-of-normal-case? : Game BoardLoc BoardLoc -> Boolean)
; a general case where current turn has no checker in bar
; check if turn is right
; if there's such move in moves
; if from and to is valid
(define (legal-action-of-normal-case? game l1 l2)
  (match game
    [(Game (Board points _ _ _ _) turn moves)
      (match* (l1 l2)
        ; check if move from point n1 to point n2 is legal
        [((PointNum n1) (PointNum n2)) 
          (is-point-to-point-legal-move? (list-ref points (- n1 1)) (list-ref points (- n2 1)) turn)
        ]
        ; check if move from point n1 to blackoff is legal
        [((PointNum n1) 'BlackOff) 
          (is-move-to-off-legal? (list-ref points (- n1 1)) 'BlackOff turn)
        ]
        ; check if move from point n1 to whiteoff is legal
        [((PointNum n1) 'WhiteOff)
          (is-move-to-off-legal? (list-ref points (- n1 1)) 'WhiteOff turn)
        ]
        ; check if move from blackbar to point n2 is legal
        [('BlackBar (PointNum n2))
          (is-move-from-bar-legal? 'BlackBar (list-ref points (- n2 1)) turn)
        ]
        ; check if move from whitebar to point n2 is legal
        [('WhiteBar (PointNum n2))
          (is-move-from-bar-legal? 'WhiteBar (list-ref points (- n2 1)) turn)
        ]
        [(_ _) #f]
      )
    ]
  )
)

(: legal-action? : Game BoardLoc BoardLoc -> Boolean)
; this method checks if the move from l1 to l2 is valid
; valid means it has to be from a bar/point to a point/off
; and the color has to be matched
(define (legal-action? game l1 l2)
  (match game
    [(Game (Board points bb wb bo wo) turn moves) 
      ; if current turn has checkers in bar
      ; then no move is legal except the move from such bar
      (if (and (> bb 0) (symbol=? turn 'Black))
        (legal-action-from-black-bar game l1 l2)
        (if (and (> wb 0) (symbol=? turn 'White))
          (legal-action-from-white-bar game l1 l2)
          (legal-action-of-normal-case? game l1 l2)
        )
      )
    ]
  )
)

(: apply-move-in-board : Board BoardLoc BoardLoc -> Board)
; this method moves a checker from l1 to l2
; this method doesn't do validation as validation has been made before calling this method
; this method doesn't include a checker "eats" another checker as it's been implemented in a different way
(define (apply-move-in-board board l1 l2)
  (match board
    [(Board points bb wb bo wo) 
      (match* (l1 l2)
     [('BlackBar (PointNum index)) (Board (replace-point-at 'Black index
                                            (Board-points board) 1) (- bb 1)
                                          wb bo wo)]
     [('WhiteBar (PointNum index)) (Board (replace-point-at 'White index
                                                  (Board-points board) 1) bb
                                          (- wb 1) bo wo)]
     [((PointNum index) 'BlackOff) (Board (replace-point-at
                              (get-color-from-list points (- index 1)) index
                                            (Board-points board) -1) bb
                                          wb (+ 1 bo) wo)]
     [((PointNum index) 'WhiteOff) (Board (replace-point-at
                              (get-color-from-list points (- index 1)) index
                                                  (Board-points board) -1) bb
                                          wb bo (+ 1 wo))]
     [((PointNum index1) (PointNum index2)) (Board
           (replace-point-at (get-color-from-list points (- index1 1)) index2
     (replace-point-at (get-color-from-list points (- index1 1)) index1
                       points -1) 1) bb wb bo wo)]
     [((PointNum index) 'BlackBar) (Board (replace-point-at
                    'Black index (Board-points board) -1) (+ bb 1) wb bo wo)]
     [((PointNum index) 'WhiteBar) (Board
    (replace-point-at 'White index (Board-points board) -1) bb (+ wb 1) bo wo)]
     [(_ _) (error "Invalid move.")]
   )]))


(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'Black 2) 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 'EmptyPoint 
(OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 0) 
(PointNum 1) (PointNum 2)) (Board (list (OccupiedPoint 'Black 1) (OccupiedPoint 
'Black 1) 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 
0))
(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'Black 1) (OccupiedPoint 
'Black 1) 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 
0) (PointNum 1) (PointNum 3)) (Board (list 'EmptyPoint (OccupiedPoint 'Black 1) 
(OccupiedPoint 'Black 1) 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 
0))
(check-expect (apply-move-in-board (Board (list 'EmptyPoint (OccupiedPoint 'Black 1) 
(OccupiedPoint 'Black 1) 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 
0) (PointNum 2) (PointNum 3)) (Board (list 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 
0))
(check-expect (apply-move-in-board (Board (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 2) 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5) 'EmptyPoint 
(OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 0) 
(PointNum 6) (PointNum 5)) (Board (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 2) 'EmptyPoint (OccupiedPoint 'White 1) (OccupiedPoint 'White 4) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 
0))
(check-expect (apply-move-in-board (Board (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 1) 'EmptyPoint (OccupiedPoint 'Black 1) (OccupiedPoint 'White 4) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 0 1 0 
0) 'WhiteBar (PointNum 21)) (Board (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 1) 'EmptyPoint (OccupiedPoint 'Black 1) (OccupiedPoint 'White 4) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint (OccupiedPoint 'White 1) 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'White 2)) 0 0 0 0))
(check-expect (apply-move-in-board (Board (list 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'Black 1) 'EmptyPoint (OccupiedPoint 'Black 1) (OccupiedPoint 'White 3) 
'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint (OccupiedPoint 'Black 5) 
'EmptyPoint (OccupiedPoint 'White 1) 'EmptyPoint 'EmptyPoint (OccupiedPoint 
'White 2)) 0 1 0 0) 'WhiteBar (PointNum 21)) (Board (list 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'Black 1) 'EmptyPoint (OccupiedPoint 'Black 1) 
(OccupiedPoint 'White 3) 'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint 
'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5) (OccupiedPoint 'White 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint 
(OccupiedPoint 'Black 5) 'EmptyPoint (OccupiedPoint 'White 2) 'EmptyPoint 
'EmptyPoint (OccupiedPoint 'White 2)) 0 0 0 0))

(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'White 4) (OccupiedPoint 
'White 2) (OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 
'White 1) (OccupiedPoint 'White 6) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5) (OccupiedPoint 
'Black 1) (OccupiedPoint 'Black 4) (OccupiedPoint 'Black 3) (OccupiedPoint 
'Black 2) 'EmptyPoint) 0 0 0 0) (PointNum 1) 'WhiteOff) (Board (list 
(OccupiedPoint 'White 3) (OccupiedPoint 'White 2) (OccupiedPoint 'White 1) 
(OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 'White 6) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 5) (OccupiedPoint 'Black 1) (OccupiedPoint 'Black 4) 
(OccupiedPoint 'Black 3) (OccupiedPoint 'Black 2) 'EmptyPoint) 0 0 0 1))
(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'White 3) (OccupiedPoint 
'White 2) (OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 
'White 1) (OccupiedPoint 'White 6) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5) (OccupiedPoint 
'Black 1) (OccupiedPoint 'Black 4) (OccupiedPoint 'Black 3) (OccupiedPoint 
'Black 2) 'EmptyPoint) 0 0 0 1) (PointNum 19) 'BlackOff) (Board (list 
(OccupiedPoint 'White 3) (OccupiedPoint 'White 2) (OccupiedPoint 'White 1) 
(OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 'White 6) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 4) (OccupiedPoint 'Black 1) (OccupiedPoint 'Black 4) 
(OccupiedPoint 'Black 3) (OccupiedPoint 'Black 2) 'EmptyPoint) 0 0 1 1))
(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'White 3) (OccupiedPoint 
'White 2) (OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 
'White 1) (OccupiedPoint 'White 6) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 4) (OccupiedPoint 
'Black 1) (OccupiedPoint 'Black 4) (OccupiedPoint 'Black 3) (OccupiedPoint 
'Black 2) 'EmptyPoint) 0 0 1 1) (PointNum 21) 'BlackOff) (Board (list 
(OccupiedPoint 'White 3) (OccupiedPoint 'White 2) (OccupiedPoint 'White 1) 
(OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 'White 6) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 4) (OccupiedPoint 'Black 1) (OccupiedPoint 'Black 3) 
(OccupiedPoint 'Black 3) (OccupiedPoint 'Black 2) 'EmptyPoint) 0 0 2 1))
(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'White 3) (OccupiedPoint 
'White 2) (OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 
'White 1) (OccupiedPoint 'White 6) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 4) (OccupiedPoint 
'Black 1) (OccupiedPoint 'Black 3) (OccupiedPoint 'Black 3) (OccupiedPoint 
'Black 2) 'EmptyPoint) 0 0 2 1) (PointNum 6) 'WhiteOff) (Board (list 
(OccupiedPoint 'White 3) (OccupiedPoint 'White 2) (OccupiedPoint 'White 1) 
(OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 'White 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 4) (OccupiedPoint 'Black 1) (OccupiedPoint 'Black 3) 
(OccupiedPoint 'Black 3) (OccupiedPoint 'Black 2) 'EmptyPoint) 0 0 2 2))
(check-expect (apply-move-in-board (Board (list (OccupiedPoint 'White 3) (OccupiedPoint 
'White 2) (OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 
'White 1) (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 4) (OccupiedPoint 
'Black 1) (OccupiedPoint 'Black 3) (OccupiedPoint 'Black 3) (OccupiedPoint 
'Black 2) 'EmptyPoint) 0 0 2 2) (PointNum 1) 'WhiteOff) (Board (list 
(OccupiedPoint 'White 2) (OccupiedPoint 'White 2) (OccupiedPoint 'White 1) 
(OccupiedPoint 'White 1) (OccupiedPoint 'White 1) (OccupiedPoint 'White 5) 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 
(OccupiedPoint 'Black 4) (OccupiedPoint 'Black 1) (OccupiedPoint 'Black 3) 
(OccupiedPoint 'Black 3) (OccupiedPoint 'Black 2) 'EmptyPoint) 0 0 2 3))


(: update-moves-after-moving : Integer (Listof Integer) -> (Listof Integer))
; this method remove value num from the list ls
; e.g. (update-moves-after-moving 1 (list 1 2 3)) -> (list 2 3)
(define (update-moves-after-moving num ls)
  (if (move-list-contains-the-move? num ls)
    (remq num ls)
    (error "cannot update: the move is not in moves")
  )
)

(check-expect (update-moves-after-moving 1 (list 1 2 3)) (list 2 3))
(check-error (update-moves-after-moving 4 (list 1 2 3)) "cannot update: the move is not in moves")

(: get-color-from-list : (Listof Point) Integer -> Player)
(define (get-color-from-list ps i)
; to get the color of a list of points whose index is i
  (match (list-ref ps i)
    ['EmptyPoint (error "cannot get color from empty point")]
    [(OccupiedPoint color count) color]))

(check-expect (get-color-from-list listofpoints 7) 'White)
(check-expect (get-color-from-list listofpoints 13) 'White)

(: is-to-eat? : BoardLoc BoardLoc (Listof Point) -> Boolean)
;; return if a move is to "eat" a checker of opponent player
;; if the move is from point A "from-index" to point B "to-index", it returns true
;; when the two points have different colors and point B has only 1 checker
;; if the move is from Bar A to point B "to-index", it returns true
;; when the bar and the point B have different colors and point B has only 1 checker
(define (is-to-eat? l1 l2 ps)
  (match* (l1 l2)
    [((PointNum from-index) (PointNum to-index))
        (match* ((list-ref ps (- from-index 1)) (list-ref ps (- to-index 1)))
          [((OccupiedPoint color1 _) (OccupiedPoint color2 count2)) 
            (and (not (symbol=? color1 color2)) (= 1 count2))
          ]
          [(_ _) #f]
        )
    ]
    [('BlackBar (PointNum to-index))
      (match (list-ref ps (- to-index 1))
        [(OccupiedPoint color count) (and (symbol=? color 'White) (= 1 count))]
        [_ #f]
      )
    ]
    [('WhiteBar (PointNum to-index))
      (match (list-ref ps (- to-index 1))
        [(OccupiedPoint color count) (and (symbol=? color 'Black) (= 1 count))]
        [_ #f]
      )
    ]
    [(_ _) #f]
  )
)

(check-expect (is-to-eat? (PointNum 1) (PointNum 2) listofpoints) #f)
(check-expect (is-to-eat? (PointNum 1) (PointNum 3) listofpoints) #f)
(check-expect (is-to-eat? (PointNum 2) (PointNum 5) listofpoints) #t)
(check-expect (is-to-eat? (PointNum 2) (PointNum 6) listofpoints) #f)
(check-expect (is-to-eat? (PointNum 6) (PointNum 2) listofpoints) #f)
(check-expect (is-to-eat? 'BlackBar (PointNum 6) listofpoints) #f)
(check-expect (is-to-eat? 'WhiteBar (PointNum 6) listofpoints) #f)
(check-expect (is-to-eat? 'WhiteBar (PointNum 5) listofpoints) #t)
(check-expect (is-to-eat? 'BlackOff (PointNum 6) listofpoints) #f)
(check-expect (is-to-eat? 'WhiteOff (PointNum 6) listofpoints) #f)
(check-expect (is-to-eat? (PointNum 6) 'BlackBar listofpoints) #f)
(check-expect (is-to-eat? (PointNum 6) 'WhiteBar listofpoints) #f)
(check-expect (is-to-eat? (PointNum 6) 'BlackOff listofpoints) #f)
(check-expect (is-to-eat? (PointNum 6) 'WhiteOff listofpoints) #f)


(: apply-move : Game BoardLoc BoardLoc -> Game)
(define (apply-move game l1 l2)
  (match game
    [(Game board turn moves) 
      (if (is-to-eat? l1 l2 (Board-points board))
        (Game (apply-move-in-board (apply-move-in-board board l2 (opposite-bar turn)) l1 l2) turn (update-moves-after-moving (abs (distance l1 l2)) moves))
        (Game (apply-move-in-board board l1 l2) turn (update-moves-after-moving (abs (distance l1 l2)) moves))
      )
    ]
  )
)

(: available-moves-from-bar? : Game (Listof Integer) (U 'BlackBar 'WhiteBar) -> Boolean)
; check if there's any moves available from bar to a point
; moves: move list in game
; bar: either BlackBar or WhiteBar
(define (available-moves-from-bar? game moves bar)
  (match moves
    ['() #f]
    [(cons head tail) 
      (or  
        (legal-move? game bar (if (symbol=? bar 'BlackBar) (PointNum head) (PointNum (- 25 head)))) 
        (available-moves-from-bar? game tail bar)
      )
    ]
  )
)

(check-expect (available-moves-from-bar? (Game (Board listofpoints 1 0 0 0) 'Black '(6)) '(6) 'BlackBar) #t)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 1 0 0 0) 'Black '(5)) '(5) 'BlackBar) #t)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 1 0 0 0) 'Black '(4)) '(4) 'BlackBar) #f)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 1 0 0 0) 'Black '(4 5)) '(4 5) 'BlackBar) #t)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 1 0 0 0) 'Black '()) '() 'BlackBar) #f)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 0 1 0 0) 'White '(1)) '(1) 'WhiteBar) #t)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 0 1 0 0) 'White '(2)) '(2) 'WhiteBar) #t)
(check-expect (available-moves-from-bar? (Game (Board listofpoints 0 1 0 0) 'White '(4)) '(4) 'WhiteBar) #f)


(: available-single-move-between-points? : Game Integer Integer Integer Integer Player -> Boolean)
; check if there's any legal move from point n to point n+num
; index is accumulator, as begin value
; index-offset and target help do recursive and code-reuse
(define (available-single-move-between-points? game num index index-offset target player)
  (if (= index target) #f
    (if (or (> (+ 1 index num) 24) (<= (+ 1 index num) 0) )
      #f
      (match game
        [(Game (Board points _ _ _ _) turn moves) 
          (match (list-ref points index)
            ['EmptyPoint (available-single-move-between-points? game num (+ index-offset index) index-offset target player)]
            [(OccupiedPoint color _) 
              (if (symbol=? color player)
                (or
                  (legal-move? game (PointNum (+ 1 index)) (PointNum (+ 1 index num)))
                  (available-single-move-between-points? game num (+ index-offset index) index-offset target player)
                )
                (available-single-move-between-points? game num (+ index-offset index) index-offset target player)
              )
            ]
          )
        ]
      )
    )
  )
)

(: available-moves-between-points-for-black? : Game (Listof Integer) -> Boolean)
; returns true if there existing any available move in the moves list
; this method only checks black player's moves
(define (available-moves-between-points-for-black? game moves)
  (match* (game moves)
    [(_ '()) #f]
    [((Game (Board points _ _ _ _) turn moves) (cons head tail))
      (or
        (available-single-move-between-points? game head 0 1 24 'Black)
        (available-moves-between-points-for-black? game tail)
      )
    ]
  )
)


(: available-moves-between-points-for-white? : Game (Listof Integer) -> Boolean)
; returns true if there existing any available move in the moves list
; this method only checks white player's moves
(define (available-moves-between-points-for-white? game moves)
  (match* (game moves)
    [(_ '()) #f]
    [((Game (Board points _ _ _ _) turn moves) (cons head tail))
      (or
        (available-single-move-between-points? game (- 0 head) 23 -1 -1 'White)
        (available-moves-between-points-for-white? game tail)
      )
    ]
  )
)



(define test-available-moves-board-for-black : Board (Board 
  (append
    (list (OccupiedPoint 'Black 1))
    (list (OccupiedPoint 'White 1))
    (make-list 22 (OccupiedPoint 'White 2)
    )
  )
  0 0 0 0)
)

(define test-available-moves-board-for-white : Board (Board 
  (append
    (make-list 22 (OccupiedPoint 'Black 2))
    (list (OccupiedPoint 'Black 1))
    (list (OccupiedPoint 'White 1))
  )
  0 0 0 0)
)

; test board where black checker can only move 1 step
(check-expect (available-moves-between-points-for-black? (Game test-available-moves-board-for-black 'Black '(4)) '(4)) #f)
(check-expect (available-moves-between-points-for-black? (Game test-available-moves-board-for-black 'Black '()) '()) #f)
(check-expect (available-moves-between-points-for-black? (Game test-available-moves-board-for-black 'Black '(1)) '(1)) #t)
(check-expect (available-moves-between-points-for-black? (Game test-available-moves-board-for-black 'Black '(1 4)) '(1 4)) #t)
; test board where white checker can only move 1 step
(check-expect (available-moves-between-points-for-white? (Game test-available-moves-board-for-white 'White '(4)) '(4)) #f)
(check-expect (available-moves-between-points-for-white? (Game test-available-moves-board-for-white 'White '()) '()) #f)
(check-expect (available-moves-between-points-for-white? (Game test-available-moves-board-for-white 'White '(1)) '(1)) #t)
(check-expect (available-moves-between-points-for-white? (Game test-available-moves-board-for-white 'White '(1 4)) '(1 4)) #t)


(: available-moves? : Game -> Boolean)
; 1. if list moves is empty, it's false
; 2. if there's checker in bar, move bar first, which means
; if cannot move from bar to points, it's false
; 3. if no legal moves from any point to any other point, it's false
(define (available-moves? game) 
  (match game
    [(Game board turn '()) #f]
    [(Game (Board points bb wb bo wo) turn moves)
     (if (and (> bb 0) (symbol=? turn 'Black))
        (available-moves-from-bar? game moves 'BlackBar)
        (if (and (> wb 0) (symbol=? turn 'White))
          (available-moves-from-bar? game moves 'WhiteBar)
          (if (symbol=? turn 'Black) 
            (available-moves-between-points-for-black? game moves)
            (available-moves-between-points-for-white? game moves)
            )
          )
      )
    ]
    [_ #f]
  )
)

(: game-over? : Game -> Boolean)
; if either black off or white off is 15 then true, else false
(define (game-over? game) 
  (match game
    [(Game (Board _ _ _ bo wo) _ _) (or (= 15 bo) (= 15 wo))]
    [_ #f]
  )
)
(check-expect (game-over? (Game (Board '() 1 0 3 0) 'Black '()))#f)
(check-expect (game-over? (Game (Board '() 1 0 15 0) 'Black '())) #t)
(check-expect (game-over? (Game (Board '() 1 0 14 15) 'Black '())) #t)

(: winner : Game -> Player)
(define (winner game) 
  (if (game-over? game)
    (match game
      [(Game (Board _ _ _ bo wo) _ _) (if (= bo 15) 'Black 'White)]
    )
    (error "can't call this method when game is not over")
  )
)

(check-expect (winner (Game (Board '() 1 0 15 0) 'Black '())) 'Black)
(check-expect (winner (Game (Board '() 1 0 12 15) 'Black '())) 'White)
(check-error (winner (Game (Board '() 1 0 1 1) 'Black '())) "can't call this method when game is not over")

(: draw-dice-roll-image : World -> Image)
; image where the initial dice are drawn
; there're only two dice, 1 black and 1 white
(define (draw-dice-roll-image w) 
  (match w
    [(World game style _ dice-value _ _)
      (overlay
        (beside
          (white-die (Style-checker-radius style) (list-ref dice-value 0))
          (black-die (Style-checker-radius style) (list-ref dice-value 2))
          )    
        (beside
          (draw-background (Style-checker-radius style) (Style-spacing style))
          (draw-bar (Style-checker-radius style))
          (draw-bar (Style-checker-radius style))
        )
      )
    ]
  )
)

(: draw-end-image : Player Style -> Image)
; if black wins, it would render
; Black
; Wins
(define (draw-end-image p style) 
  (overlay
    (text (string-append (symbol->string p) "\nWins") 24 "orange")
    (beside
      (draw-background (Style-checker-radius style) (Style-spacing style))
      (above
        (draw-bar (Style-checker-radius style))
        (draw-bar (Style-checker-radius style))
        (draw-bar (Style-checker-radius style))
      )
      (above
        (draw-bar (Style-checker-radius style))
        (draw-bar (Style-checker-radius style))
        (draw-bar (Style-checker-radius style))
      )
    )
  )
)


(: draw-world : World -> Image)
; to draw the board but the input is world
(define (draw-world w)
  (match w
    [(World (Game board turn moves) style prev-loc dice-value "start" _) 
    ; if it's in the beginning of game, only draw two dice
      (draw-dice-roll-image w)
    ]
    [(World (Game board turn moves) style prev-loc dice-value status _)
     (if (game-over? (Game board turn moves)) 
      ; if game is over, draw a text of winner
      (draw-end-image (winner (Game board turn moves)) style) 
      ; else, draw the board
      (draw-board-with-highlight style board dice-value prev-loc)
      )
     ]
  )
)

(: update-world-moves-by-rolling-dice : World Integer Integer Player -> World)
; updates World-moves by given random numbers n1 n2 and Player
; e.g. if player is 'Black and n1 n2, then World-moves is (list n1 n2) and dice-value is (list _ _ n1 n2)
; if n1 equals to n2, then World-moves is (list n1 n1 n1 n1) and dice-value is (list _ _ n1 n1)
(define (update-world-moves-by-rolling-dice w n1 n2 turn)
  (match* (w turn)
    [((World (Game board turn _) style prev-loc (list a b c d) status histories) 'Black)
      (if (= n1 n2) 
        (World (Game board 'Black (list n1 n1 n1 n1)) style prev-loc (list a b n1 n1) "progress" histories)
        (World (Game board 'Black (list n1 n2)) style prev-loc (list a b n1 n2) "progress" histories)
      )
    ]
    [((World (Game board turn _) style prev-loc (list a b c d) status histories) 'White)
      (if (= n1 n2) 
        (World (Game board 'White (list n1 n1 n1 n1)) style prev-loc (list n1 n1 c d) "progress" histories)
        (World (Game board 'White (list n1 n2)) style prev-loc (list n1 n2 c d) "progress" histories)
      )
    ]
  )
)

(check-expect (update-world-moves-by-rolling-dice (World (Game test-board 'White '()) test-style 'Nowhere (list 1 0 2 0) "start" '()) 3 4 'Black)
  (World (Game test-board 'Black (list 3 4)) test-style 'Nowhere (list 1 0 3 4) "progress" '()))

(check-expect (update-world-moves-by-rolling-dice (World (Game test-board 'White '()) test-style 'Nowhere (list 1 0 2 0) "start" '()) 3 3 'White)
  (World (Game test-board 'White (list 3 3 3 3)) test-style 'Nowhere (list 3 3 2 0) "progress" '()))


(: tick : World -> World)
; this method is called from big-bang
; in the beginning of game, it randonmly generates values of two dice, if the worls is in "start" status
; if world's status is "progress", do nothing
(define (tick w)
  (match w
    [(World game style prev-loc (list n 0 m 0) "start" histories)
      (if (= n m) 
        (World game style prev-loc (list (+ 1 (random 6)) 0 (+ 1 (random 6)) 0) "start" histories)
        (local
          {
            (define n1 (+ 1 (random 6)))
            (define n2 (+ 1 (random 6)))
          }
          (update-world-moves-by-rolling-dice w n1 n2 (if (> n m) 'White 'Black))
        )
      )
    ]
    [(World _ _ _ _ "progress" _) w]
  )
)

(: world->string : World -> String)
(define (world->string w)
  "todo: this hasn't been implemented yet."
)

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
 (local
   {(define path : (U Path False) (put-file))}
   (if (path? path)
     (begin
       (write-string (world->string w)
       (open-output-file path))
       (void)
     )
     (void)
   )
 )
)


(: string->world : Style String -> World)
(define (string->world style str)
  ; todo: not implemented
  (World (Game initial-board 'Black '()) test-style 'Nowhere (list (+ 1 (random 6)) 0 (+ 1 (random 6)) 0) "start" '())
)

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : Style -> World)
(define (load-game s)
 (local
   {(define path : (U Path False) (get-file))}
   (if (path? path)
     (string->world s (port->string (open-input-file path)))
     (error "load-game: user cancelled")
   )
 )
)

(: key-handle : World String -> World)
(define (key-handle w key)
  (match* (w key)
    ; if "u" is pressed, undo step
    [((World game style _ _ "progress" '()) "u") w]
    ; the feature is not implemented as when undo, the dice should also undo
    [((World game style prev-loc dice-value "progress" histories) "u") 
      (World (first histories) style prev-loc dice-value "progress" (rest histories))
    ]
    ; [(_ "s") ((save-game! w) w)]
    ; if "l" is pressed, load game from a chosen file
    [(_ "l") (load-game test-style)]
    [(_ _) w]
  )
)


(: run : Style -> World)
; run function using bigbang
; it uses initial-board to represent the game
; if you want to quickly view the end-game-image, use game-almost-finished-board instead
(define (run style)
  (big-bang (World (Game initial-board 'Black '()) style
         'Nowhere (list (+ 1 (random 6)) 0 (+ 1 (random 6)) 0) "start" '()) : World
    [to-draw draw-world]
    [on-mouse click]
    [on-key key-handle]
    [on-tick tick 1/2]
  )
)

(test)
; to view the whole game, use (run test-style)
 ;(run test-style)