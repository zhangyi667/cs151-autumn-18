#lang typed/racket
(require typed/test-engine/racket-tests)

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
; suffix n elements of a list
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
(check-expect (suffix '(a b c) 5) '())
(check-expect (suffix '(a b c) 0) '(a b c))

(: len (All (T) (Listof T) -> Integer))
;; count the number of items in the list
(define (len m)
	(match m
		['() 0]
		[(cons head tail) (add1 (len tail))]
	) 
)
(check-expect (len '()) 0)
(check-expect (len (list 1 2 3 4)) 4)


(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace the item at the given position
;; position counting starts at 0
(define (replace-at i x xs)
  (match* (i xs)
    [(0  (cons hd tl)) (cons x tl)]
    [(_  (cons hd tl)) (cons hd (replace-at (- i 1) x tl))]
	)
)
(check-expect (replace-at 0 'Z '(a b c)) '(Z b c))
(check-expect (replace-at 1 3 '(1 2 3)) '(1 3 3))

(: split-to-n (All (A) (Integer (Listof A) -> (Listof (Listof
A)))))
;; cut list into size-n sublists
(define (split-to-n n xs)
 (cond
 [(empty? xs) '()]
 [else (cons (preffix xs n)
 (split-to-n n (suffix xs n)))]))

(check-expect (split-to-n 2 '(a b c d e f)) '((a b) (c d) (e f)))
(check-expect (split-to-n 4 '(a b c d e f)) '((a b c d) (e f)))
(check-expect (split-to-n 2 '(a b c d e)) '((a b) (c d) (e)))

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

(check-expect (bubble-sort (list 1 4 5 2 3)) (list 1 2 3 4 5))
(check-expect (bubble-sort (list 1)) (list 1))
(test)