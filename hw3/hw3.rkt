#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

;; PROBLEM 1

(define-struct Salary
  ([per-period : Exact-Rational]
   [periods-per-year : Integer]
   [min-hours-week : Integer]))

(define-struct Wage
  ([hourly : Exact-Rational]
   [max-hours-week : Integer]))

(define-type Job (U Salary Wage))

;; error?: returns true if given any nonsensical negative input or if
;; number of hours per week exceeds 80
;; used for error messages
(: error? (Job -> Boolean))
(define (error? j)
  (match j
    [(Salary pay per min) (or (< pay 0) (< per 0) (< min 0))]
    [(Wage pay max) (or (< pay 0) (< max 0) (> max 80))]))

(check-expect (error? (Salary 1000 24 -10)) #t)
(check-expect (error? (Wage 20 82)) #t)

;; annual-income: given a Job and number of hours worked per week, calculate
;; the annual income. If salaried employee does not work min number of hours,
;; income is 0. If paid wages, there is no benefit to working over max hours
(: annual-income : Job Integer -> Exact-Rational)
(define (annual-income j i)
  [if (error? j) (error "annual-income: given negative or exeeding 80 hrs/wk")
      (match j    
        [(Salary pay per min) (if (< i min) 0 (* pay per))]
        [(Wage pay max) (if (> i max) (* pay max 50) (* pay i 50))])])

(check-expect (annual-income (Salary 4000 12 40) 40) 48000)
(check-expect (annual-income (Salary 4000 12 40) 30) 0)
(check-expect (annual-income (Wage 20 60) 40) 40000)
(check-error (annual-income (Salary -9 1 1) 1)
             "annual-income: given negative or exeeding 80 hrs/wk")

;; hourly-rate: calculates effictive hourly rate being paid at given job
(: hourly-rate : Job Integer -> Exact-Rational)
(define (hourly-rate j i)
  [if (error? j) (error "hourly-rate: given negative or exeeding 80 hrs/wk")
      (match j    
        [(Salary _ _ min) (/ (annual-income j i) min 50)]
        [(Wage pay _) pay])])

(check-expect (hourly-rate (Salary 4000 12 40) 30) 0)
(check-expect (hourly-rate (Salary 100000 1 40) 42) 50)
(check-expect (hourly-rate (Wage 25 50) 40) 25)

;; job=?: determines if two jobs are exactly the same in terms of hours and
;; money earned
(: job=? : Job Job -> Boolean)
(define (job=? j1 j2)
  (match* (j1 j2)
    [((Salary a1 b1 c1) (Salary a2 b2 c2)) (and (= a1 a2) (= b1 b2) (= c1 c2))]
    [((Wage a1 b1) (Wage a2 b2)) (and (= a1 a2) (= b1 b2))]
    [(_ _) #f]))

(check-expect (job=? (Salary 1000 12 40) (Salary 1000 12 30)) #f)
(check-expect (job=? (Salary 1000 12 40) (Salary 1000 12 40)) #t)
(check-expect (job=? (Salary 1000 12 40) (Wage 40 40)) #f)
(check-expect (job=? (Wage 12 40) (Wage 12 40)) #t)

;; earnings>?: determines if the first job has more earnings potential
;; than the second job
(: earnings>? : Job Job -> Boolean)
(define (earnings>? j1 j2)
  [if (or (error? j1) (error? j2)) (error "earnings>?: given negative or exeeding 80 hrs/wk")
      (match* (j1 j2)
        [((Salary pay per _) (Wage hour max)) (> (* pay per) (* hour max 52))]
        [((Wage hour max) (Salary pay per _)) (> (* hour max 50) (* pay per))]
        [((Salary pay per _) (Salary pay2 per2 _)) (> (* pay per) (* pay2 per2))]
        [((Wage hour max) (Wage hour2 max2)) (> (* hour max) (* hour2 max2))])])

(check-expect (earnings>? (Salary 10000 12 40) (Wage 20 60)) #t)
(check-expect (earnings>? (Wage 500 20) (Salary 1000 24 40)) #t)
(check-expect (earnings>? (Salary 10000 12 40) (Salary 30000 4 40)) #f)
(check-error (earnings>? (Wage 30 100) (Salary -1 -1 -1))
             "earnings>?: given negative or exeeding 80 hrs/wk")

;; PROBLEM 2

(define-type Player (U 'Black 'White))
(define-type PieceType (U 'Pawn 'Bishop 'Knight 'Rook 'Queen 'King))
(define-struct ChessPiece
  ([color : Player]
   [type : PieceType]))

;; points: assigns points to a piece, with pawn = 1, bishop = 3, knight = 3
;; rook = 5, queen = 9, king = 0
(: points : ChessPiece -> Integer)
(define (points p)
  (match p
    [(ChessPiece _ 'Pawn) 1]
    [(ChessPiece _ (or 'Bishop 'Knight)) 3]
    [(ChessPiece _ 'Rook) 5]
    [(ChessPiece _ 'Queen) 9]
    [(ChessPiece _ _) 0]))
    
(check-expect (points (ChessPiece 'Black 'King)) 0)
(check-expect (points (ChessPiece 'White 'Bishop)) 3)

;; to-letter: helper function for piece->char that converts the piece
;; type to a lowercase letter
(: to-letter (PieceType -> Char))
(define (to-letter s)
  (match s
    ['Pawn #\p]
    ['Bishop #\b]
    ['Knight #\n]
    ['Rook #\r]
    ['Queen #\q]
    ['King #\k]))

(check-expect (to-letter 'Knight) #\n)

;; piece->char: takes in a ChessPiece and returns an abbreviation:
;; uppercase letters for black, lowercase letters for white
;; P = pawn, B = bishop, N = knight, R = rook, Q = queen, K = king
(: piece->char : ChessPiece -> Char)
(define (piece->char p)
  (match p
    [(ChessPiece 'Black f) (char-upcase (to-letter f))]
    [(ChessPiece 'White f) (to-letter f)]))

(check-expect (piece->char (ChessPiece 'Black 'King)) #\K)
(check-expect (piece->char (ChessPiece 'White 'Rook)) #\r)

;; to-piece: used in char->piece to convert a character into a PieceType
;; also contains an error message in case an invalid character is entered
(: to-piece (Char -> PieceType))
(define (to-piece c)
  (match c
    [#\p 'Pawn]
    [#\b 'Bishop]
    [#\n 'Knight]
    [#\r 'Rook]
    [#\q 'Queen]
    [#\k 'King]
    [_ (error "char->piece: piece does not exist")]))

(check-expect (to-piece #\p) 'Pawn)

;; char->piece: given a character, return the matching ChessPiece
;; raises error if character does not match a piece
(: char->piece : Char -> ChessPiece)
(define (char->piece c)
  (cond
    [(char-upper-case? c) (ChessPiece 'Black (to-piece (char-downcase c)))]
    [else (ChessPiece 'White (to-piece c))])) 

(check-expect (char->piece #\K) (ChessPiece 'Black 'King))
(check-error (char->piece #\A) "char->piece: piece does not exist")
(check-expect (char->piece #\b) (ChessPiece 'White 'Bishop))
(check-expect (char->piece #\Q) (ChessPiece 'Black 'Queen))

;; PROBLEM 3

(define-type IntTree (U IntNode 'IEmpty))

(define-struct IntNode
  ([val   : Integer]
   [left  : IntTree]
   [right : IntTree]))

(define-type StringTree (U StringNode 'SEmpty))

(define-struct StringNode
  ([val   : String]
   [left  : StringTree]
   [right : StringTree]))

;; practice-tree: used in check-expects in later functions
(define practice-tree
  (IntNode 1
           (IntNode 5
                    (IntNode 9 'IEmpty 'IEmpty)
                    (IntNode 3 'IEmpty 'IEmpty))
           (IntNode 2 'IEmpty 'IEmpty)))

;; mirror: takes an IntTree and returns a mirror image of the IntTree 
(: mirror (IntTree -> IntTree))
(define (mirror t)
  (match t
    [(IntNode val 'IEmpty 'IEmpty) (IntNode val 'IEmpty 'IEmpty)]
    [(IntNode val l r) (IntNode val (mirror r) (mirror l))]))

(check-expect (mirror practice-tree)
              (IntNode 1
                       (IntNode 2 'IEmpty 'IEmpty)
                       (IntNode 5
                                (IntNode 3 'IEmpty 'IEmpty)
                                (IntNode 9 'IEmpty 'IEmpty))))

;; int-tree->string-tree: takes an IntTree and generates a StringTree with
;; the integers being represented as strings
(: int-tree->string-tree (IntTree -> StringTree))
(define (int-tree->string-tree i)
  (match i
    [(IntNode val 'IEmpty 'IEmpty)
     (StringNode (number->string val) 'SEmpty 'SEmpty)]
    [(IntNode val l r)
     (StringNode (number->string val)
                 (int-tree->string-tree l)
                 (int-tree->string-tree r))]))

(check-expect (int-tree->string-tree practice-tree)
              (StringNode "1"
                          (StringNode "5"
                                      (StringNode "9" 'SEmpty 'SEmpty)
                                      (StringNode "3" 'SEmpty 'SEmpty))
                          (StringNode "2" 'SEmpty 'SEmpty)))

;; right-edge: takes a StringTree and produces a string from all the
;; strings along the right edge of the tree
(: right-edge (StringTree -> String))
(define (right-edge s)
  (match s
    [(StringNode val 'SEmpty 'SEmpty) val]
    [(StringNode val _ r) (string-append val (right-edge r))])) 

(check-expect (right-edge
               (StringNode "x"
                           (StringNode "b"
                                       (StringNode "a" 'SEmpty 'SEmpty) 'SEmpty)
                           (StringNode "yy"
                                       (StringNode "w" 'SEmpty 'SEmpty)
                                       (StringNode "z" 'SEmpty 'SEmpty))))
              "xyyz")

;; PROBLEM 4

(define-type 3Tree (U 3Node '3Empty))

(define-struct 3Node
  ([root : Integer]
   [lsub : 3Tree]
   [msub : 3Tree]
   [rsub : 3Tree]))

;; test-tree: used in check-expects
(define test-tree 
  (3Node 1
         (3Node 2
                (3Node 3 '3Empty '3Empty '3Empty)
                (3Node 4 '3Empty '3Empty '3Empty)
                '3Empty)
         (3Node 8
                '3Empty
                (3Node 7
                       (3Node 5 '3Empty '3Empty '3Empty)
                       '3Empty
                       (3Node 6 '3Empty '3Empty '3Empty))
                '3Empty)
         (3Node 9
                '3Empty
                '3Empty
                (3Node 0 '3Empty '3Empty '3Empty))))

;; num-nodes: counts number of nodes
(: num-nodes : 3Tree -> Integer)
(define (num-nodes t)
  (match t
    ['3Empty 0]
    [(3Node root '3Empty '3Empty '3Empty) 1]    
    [(3Node _ l m r) (+ 1 (num-nodes l) (num-nodes m) (num-nodes r))]))

(check-expect (num-nodes test-tree) 10)

;; sum-nodes: sums all the nodes in the tree
(: sum-nodes : 3Tree -> Integer)
(define (sum-nodes t)
  (match t
    ['3Empty 0]
    [(3Node root '3Empty '3Empty '3Empty) root]
    [(3Node val l m r) (+ val (sum-nodes l) (sum-nodes m) (sum-nodes r))]))

(check-expect (sum-nodes test-tree) 45)

;; height: returns height of tree
(: height : 3Tree -> Integer)
(define (height t)
  (match t
    ['3Empty 0]
    [(3Node root '3Empty '3Empty '3Empty) 1]
    [(3Node _ l m r) (+ 1 (max (height l) (height m) (height r)))]))

(check-expect (height test-tree) 4)

;; contains?: returns true if given tree contains given integer
(: contains? : 3Tree Integer -> Boolean)
(define (contains? t i)
  (match t
    ['3Empty #f]
    [(3Node root '3Empty '3Empty '3Empty) (= root i)]
    [(3Node _ l m r) (or (contains? l i) (contains? m i) (contains? r i))]))

(check-expect (contains? test-tree 5) #t)
(check-expect (contains? test-tree 11) #f)

;; leftmost: returns the item farthest to the left in the tree
(: leftmost : 3Tree -> (U Integer 'None))
(define (leftmost t)
  (match t
    ['3Empty 'None]
    [(3Node root '3Empty '3Empty '3Empty) root]
    [(3Node _ l _ _) (leftmost l)]))

(check-expect (leftmost test-tree) 3)
(check-expect (leftmost '3Empty) 'None)

; farthest-item: returns the item farthest away from the root of the tree
; if tied, return item on left
(: farthest-item : 3Tree -> (U Integer 'None))
(define (farthest-item t)
  (match t
    ['3Empty 'None]
    [(3Node root '3Empty '3Empty '3Empty) root]
    [(3Node _ l m r)
     (cond
       [(and (> (height r) (height m)) (> (height r) (height l)))
        (farthest-item r)]
       [(> (height m) (height l)) (farthest-item m)]
       [else (farthest-item l)])]))

(check-expect (farthest-item test-tree) 5)
(check-expect (farthest-item (3Node 1
                                    '3Empty
                                    (3Node 3 '3Empty '3Empty '3Empty)
                                    (3Node 4 '3Empty '3Empty '3Empty))) 3)
(check-expect (farthest-item '3Empty) 'None)

;; double: doubles every integer in the tree
(: double : 3Tree -> 3Tree)
(define (double t)
  (match t
    ['3Empty '3Empty]
    [(3Node root '3Empty '3Empty '3Empty)
     (3Node (* 2 root) '3Empty '3Empty '3Empty)]
    [(3Node root l m r)
     (3Node (* 2 root) (double l) (double m) (double r))]))

(check-expect (double test-tree)
              (3Node 2
                     (3Node 4
                            (3Node 6 '3Empty '3Empty '3Empty)
                            (3Node 8 '3Empty '3Empty '3Empty) '3Empty)
                     (3Node 16 '3Empty
                            (3Node 14
                                   (3Node 10 '3Empty '3Empty '3Empty) '3Empty
                                   (3Node 12 '3Empty '3Empty '3Empty)) '3Empty)
                     (3Node 18 '3Empty '3Empty
                            (3Node 0 '3Empty '3Empty '3Empty))))

(test)