#lang typed/racket
(require typed/test-engine/racket-tests)
;(require "../include/cs151-core.rkt")
;; data definitions: lists from scratch
(define-syntax cs151-core-define-struct
  (syntax-rules ()
    [(cs151-core-define-struct (name A ...) (fields ...))
     (define-struct (A ...) name (fields ...) #:transparent)]
    [(cs151-core-define-struct (name A ...) (fields ...) #:mutable)
     (define-struct (A ...) name (fields ...) #:transparent #:mutable)]
    [(cs151-core-define-struct name (fields ...) #:mutable)
     (define-struct name (fields ...) #:transparent #:mutable)]
    [(cs151-core-define-struct name (fields ...))
     (define-struct name (fields ...) #:transparent)]))
(provide (rename-out [cs151-core-define-struct define-struct]))

(: cs151-core-error : String -> Nothing)
(define (cs151-core-error msg) (error msg))
(provide (rename-out [cs151-core-error error]))

(: cs151-core-cons : (All (A) (-> A (Listof A) (Listof A))))
(define (cs151-core-cons hd tl) (cons hd tl))
(provide (rename-out [cs151-core-cons cons]))

(: cs151-core-first : (All (A) (Listof A) -> A))
(define (cs151-core-first xs) (first xs))
(provide (rename-out [cs151-core-first first]))

(: cs151-core-map : (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (cs151-core-map f xs) (map f xs))
(provide (rename-out [cs151-core-map map]))

(define-type (LinkedList A)
 (U 'Nil (Cons A)))
(define-struct (Cons A)
 ([root : A]
 [sub : (LinkedList A)]))
(: list1 (LinkedList Integer))
(define list1
 (Cons 1 (Cons 2 (Cons 3 'Nil))))
(: list2 (LinkedList String))
(define list2
 (Cons "a" (Cons "bb" (Cons "ccc" (Cons "d" 'Nil)))))
;; operations: len and rev
(: len (All (T) (LinkedList T) -> Integer))
;; count the number of items in the list
(define (len m)
 (match m
 ['Nil 0]
 [(Cons _ r) (+ 1 (len r))]))
(check-expect (len list1) 3)
(check-expect (len list2) 4)
(: putlast (All (T) (T (LinkedList T) -> (LinkedList T))))
;; put given item last in the list
(define (putlast x m)
 (match m
 ['Nil (Cons x 'Nil)]
 [(Cons f r) (Cons f (putlast x r))]))
(check-expect (putlast 99 list1) (Cons 1 (Cons 2 (Cons 3
(Cons 99 'Nil)))))
(check-expect (putlast "Q" list2) (Cons "a" (Cons "bb" (Cons
"ccc" (Cons "d" (Cons "Q" 'Nil))))))
(: rev (All (T) (LinkedList T) -> (LinkedList T)))
;; reverse the order of items in the list
(define (rev m)
 (match m
 ['Nil 'Nil]
 [(Cons f r) (putlast f (rev r))]))
(check-expect (rev list1) (Cons 3 (Cons 2 (Cons 1 'Nil))))
(check-expect (rev list2) (Cons "d" (Cons "ccc" (Cons
"bb" (Cons "a" 'Nil)))))
(test)