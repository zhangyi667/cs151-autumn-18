#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

; this file contains 3 sorting method: bubble sort, quick sort and merge sort

(: generate-random-list : Integer (Listof Integer) -> (Listof Integer))
(define (generate-random-list len ls)
	(match len
		[0 ls]
		[_ (generate-random-list (- len 1) (cons (random 1000) ls))]
	)
)

(check-expect (length (generate-random-list 5 '())) 5)
(check-expect (length (generate-random-list 1 '())) 1)

(: is-sorted? : (Listof Integer) -> Boolean)
(define (is-sorted? ls)
	(match ls
		['() #t]
		[(cons head '()) #t]
		[(cons head tail) (and (<= head (list-ref tail 0)) (is-sorted? tail))]
	)
)

(check-expect (is-sorted? '()) #t)
(check-expect (is-sorted? '(1)) #t)
(check-expect (is-sorted? '(1 1)) #t)
(check-expect (is-sorted? '(1 1 2)) #t)
(check-expect (is-sorted? '(1 2 1)) #f)
(check-expect (is-sorted? '(1 1 1 2 2 2 3 4 5 5 5)) #t)


(: insert-first : Integer (Listof Integer) -> (Listof Integer))
; insert an Integer n to a list, make sure all elements in front of it is smaller than it
(define (insert-first n ls)
	(match ls
		['() (list n)]
		[(cons head tail) (if (< n head) (cons n ls) (cons head (insert-first n tail)))]
	)
)

(check-expect (insert-first 1 '()) (list 1))
(check-expect (insert-first 4 '(1 5 2 3)) (list 1 4 5 2 3))

(: bubble-sort : (Listof Integer) -> (Listof Integer))
; bubble-sort on a list
(define (bubble-sort ls)
	(match ls
		['() '()]
		[(cons head tail) (insert-first head (bubble-sort tail))]
	)
)

(check-expect (is-sorted? (bubble-sort (list 1 4 5 2 3))) #t)
(check-expect (is-sorted? (bubble-sort (generate-random-list 10 '()))) #t)

(: quick-sort : (Listof Integer) -> (Listof Integer))
(define (quick-sort ls)
  (match ls
  	['() '()]
  	[(cons head '()) (list head)]
  	[_ (local
  		{(define pivot (list-ref ls (exact-ceiling (/ (length ls) 2))))}
  		(append
                 (quick-sort (filter (lambda ([num : Integer]) (> pivot num)) ls))
                 (filter (lambda ([num : Integer]) (= pivot num)) ls)
                 (quick-sort (filter (lambda ([num : Integer]) (< pivot num)) ls)) )
		)]
	)
)


(check-expect (is-sorted? (quick-sort (generate-random-list 10 '()))) #t)
(check-expect (is-sorted? (quick-sort (generate-random-list 20 '()))) #t)
(check-expect (is-sorted? (quick-sort (generate-random-list 30 '()))) #t)
(check-expect (is-sorted? (quick-sort (generate-random-list 40 '()))) #t)


; preffix n elements of a list
(: preffix (All (T) (Listof T) Integer -> (Listof T)))
(define (preffix ls num)
	(if (or (<= num 0) (empty? ls)) 
		'() 
		(match ls
			[(cons head tail) (cons head (preffix tail (sub1 num)))]
		)
	)
)
(check-expect (preffix '(a b c d e) 3) '(a b c))
(check-expect (preffix '(a b c) 0) '())
(check-expect (preffix '(x y z) 99) '(x y z))

(: suffix (All (T) (Listof T) Integer -> (Listof T)))
; suffix elements of a list from the nth element
(define (suffix ls num)
	(cond
	 [(<= num 0) ls]
	 [(empty? ls) '()]
	 [else (match ls
	 		[(cons head tail) (suffix tail (sub1 num))]
 		)]
 	)
)
(check-expect (suffix '(a b c d e f) 3) '(d e f))
(check-expect (suffix '(a b c d e f) 4) '(e f))
(check-expect (suffix '(a b c) 5) '())
(check-expect (suffix '(a b c) 0) '(a b c))


(: merge : (Listof Integer) (Listof Integer) -> (Listof Integer))
(define (merge left right)
	(match* (left right)
		[('() '()) '()]
		[('() _) right]
		[(_ '()) left]
		[((cons hl tl) (cons hr tr)) (if (<= hl hr)
			(append (list hl) (merge tl right))
			(append (list hr) (merge left tr))
			)]
	)
)


(: merge-sort : (Listof Integer) -> (Listof Integer))
(define (merge-sort ls)
	(local
		{(define n (exact-floor (/ (length ls) 2)))}
		(if (= n 0) ls
			(merge (merge-sort (preffix ls n)) (merge-sort (suffix ls n)))
		)
	)
)

(check-expect (is-sorted? (merge-sort (generate-random-list 10 '()))) #t)
(check-expect (is-sorted? (merge-sort (generate-random-list 20 '()))) #t)
(check-expect (is-sorted? (merge-sort (generate-random-list 30 '()))) #t)
(check-expect (is-sorted? (merge-sort (generate-random-list 40 '()))) #t)
(check-expect (merge-sort '(1 4 -1 2 5 5 3 16 3)) '(-1 1 2 3 3 4 5 5 16))

(test)
