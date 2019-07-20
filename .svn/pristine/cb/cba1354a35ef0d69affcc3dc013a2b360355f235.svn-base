#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(: empty Image)
(define empty (rectangle 40 40 "solid" "white"))

(: windows Image)
(define windows (beside (rectangle 20 20 "solid" "cornflowerblue")
                        (rectangle 20 20 "solid" "white")
                        (rectangle 20 20 "solid" "cornflowerblue")
                        (rectangle 20 20 "solid" "white")
                        (rectangle 20 20 "solid" "cornflowerblue")))

(define blankWindows (rectangle 100 20 "solid" "darkmagenta"))

(: manyWindows Image)
(define manyWindows (above windows blankWindows windows blankWindows windows blankWindows windows blankWindows windows))

(: building Image)
(define building (overlay manyWindows (rectangle 120 250 "solid" "darkmagenta")))

(: tallBuilding Image)
(define tallBuilding (above (triangle 5 "solid" "gray")
                            (rectangle 5 60 "solid" "gray")                             
                            (rectangle 60 240 "solid" "gray")
                            (rectangle 100 300 "solid" "gray")))

(: shortBuilding Image)
(define shortBuilding (above (triangle 50 "solid" "burlywood")
                             (rectangle 50 60 "solid" "burlywood")
                             (rectangle 80 90 "solid" "burlywood")))

(beside/align "bottom" building empty tallBuilding empty shortBuilding)
