#lang typed/racket
(require typed/test-engine/racket-tests)

(define-type IntTree (U IntNode 'Empty))
(define-struct IntNode
 ([val : Integer]
 [left : IntTree]
 [right : IntTree])
)
; (define-struct (A ...) name (fields ...) #:transparent)

(define-type StringTree (U StringNode 'Empty))
(define-struct StringNode
 ([val : String]
 [left : StringTree]
 [right : StringTree]))


(: in-order (-> IntTree String))
(define (in-order tree)
	(match tree
		['Empty ""]
		[(IntNode val left right) 
			(string-append 
				(in-order left) 
				(number->string val) 
				(in-order right)
			)]
	)
)

(: example-tree IntTree)
(define example-tree
	(
		IntNode 1
		(IntNode 2 
			(IntNode 4 'Empty 'Empty) 
			(IntNode 5 'Empty 'Empty)
			)
		(IntNode 3 'Empty 'Empty)
	)
)

(: example-tree-stringtree StringTree)
(define example-tree-stringtree
	(
		StringNode "1"
		(StringNode "2" 
			(StringNode "4" 'Empty 'Empty) 
			(StringNode "5" 'Empty 'Empty)
			)
		(StringNode "3" 'Empty 'Empty)
	)
)

(: mirror (-> IntTree IntTree))
(define (mirror tree)
	(match tree
		['Empty 'Empty]
		[(IntNode val left right) (IntNode val (mirror right) (mirror left))]
	)	
)

(: int-tree->string-tree (-> IntTree StringTree))
(define (int-tree->string-tree intTree)
	(match intTree
		['Empty 'Empty]
		[(IntNode val left right) 
		(StringNode 
			(number->string val) 
			(int-tree->string-tree left) 
			(int-tree->string-tree right) 
			)]
	)
)

(: right-edge (-> StringTree String))
(define (right-edge sTree)
	(match sTree
		['Empty ""]
		[(StringNode val left 'Empty) val]
		[(StringNode val left right) (string-append val (right-edge right))]
	)
)

; (check-expect (in-order (IntTree 1 (IntTree 2 'Empty 'Empty))) "42513")
; (displayln (in-order example-tree))
; (displayln (in-order (mirror example-tree)))
(check-expect (string=? (in-order 'Empty) "") #t)
(check-expect (string=? (in-order example-tree) "42513") #t)
(check-expect (string=? (in-order (mirror example-tree)) "31524") #t)
(check-expect (string=? (right-edge example-tree-stringtree) "13") #t)
(test)