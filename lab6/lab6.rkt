#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-type (Order a)
  (a a -> Boolean))

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-struct (Pair a b)
  ([item1 : a]
   [item2 : b]))

(define-struct (Nd a b)
  ([key   : a]
   [value : b]
   [lsub : (BST a b)]
   [rsub : (BST a b)]))

(define-type (BST a b)
  (U 'E (Nd a b)))

;; The type of a BST map that maps alphas to betas.
(define-struct (BSTMap a b)
  ([ord  : (Order a)]
   [data : (BST a b)]))

;; used for check expects
(: m : (BSTMap Integer String))
(define m
  (BSTMap < (Nd 1 "a" 'E
                (Nd 3 "c"
                    (Nd 2 "b" 'E 'E)
                    (Nd 4 "d" 'E 'E)))))

(: m1 : (BSTMap Symbol String))
(define m1
  (BSTMap symbol<? (Nd 'C "Cat"
                       (Nd 'B "Bagel"
                           (Nd 'A "Apple" 'E 'E) 'E)
                       (Nd 'D "Dog" 'E 'E))))

(: m2 : (BSTMap Integer Symbol))
(define m2
  (BSTMap < (Nd 10 'ten
                (Nd 6 'six 'E
                    (Nd 7 'seven 'E
                        (Nd 8 'eight 'E 'E)))
                (Nd 11 'eleven 'E 'E))))

;; If the given key is in the map and associated with value v, return Some v.
;; If the key is absent from the map, return 'None.
;; Be sure to navigate the BST and not to search it exhaustively.
(: lookup : All (a b) a (BSTMap a b) -> (Optional b))
(define (lookup x map)
  (match map
    [(BSTMap order data)
     (local {(: lp : (BST a b) -> (Optional b))
             (define (lp t)
               (match t
                 ['E 'None]
                 [(Nd key value lsub rsub)
                  (cond
                    [(order x key) (lp lsub)]
                    [(order key x) (lp rsub)]
                    [else (Some value)])]))}
       (lp data))]))

(check-expect (lookup 4 m) (Some "d"))
(check-expect (lookup 1 m) (Some "a"))
(check-expect (lookup 5 m) 'None)
(check-expect (lookup 'D m1) (Some "Dog"))

;; change BSTMap to BST
(: map->bst : All (a b) (BSTMap a b) -> (BST a b))
(define (map->bst map)
  (match map
    [(BSTMap ord data) data]))

;; Insert the given key/value pair into the map.
;; If the given key is already present in the tree,
;; replace its value with the new one.
(: insert : All (a b) a b (BSTMap a b) -> (BSTMap a b))
(define (insert newk newv map)
  (match map
    [(BSTMap order tree)
     (match tree
       ['E (BSTMap order (Nd newk newv 'E 'E))]
       [(Nd k v l r)
        (cond
          [(order newk k)
           (BSTMap order
                   (Nd k v (map->bst (insert newk newv (BSTMap order l))) r))]
          [(order k newk)
           (BSTMap order
                   (Nd k v l (map->bst (insert newk newv (BSTMap order r)))))]
          [else (BSTMap order (Nd newk newv l r))])])]))
     
(check-expect (map->bst (insert 'E "Elephant" m1))
              (Nd 'C "Cat"
                  (Nd 'B "Bagel"
                      (Nd 'A "Apple" 'E 'E) 'E)
                  (Nd 'D "Dog" 'E
                      (Nd 'E "Elephant" 'E 'E))))
(check-expect (map->bst (insert 'AB "Abe" m1))
              (Nd 'C "Cat"
                  (Nd 'B "Bagel"
                      (Nd 'A "Apple" 'E
                          (Nd 'AB "Abe" 'E 'E)) 'E)
                  (Nd 'D "Dog" 'E 'E)))
(check-expect (BSTMap-data (insert 'C "Calling" m1))
              (Nd 'C "Calling"
                  (Nd 'B "Bagel"
                      (Nd 'A "Apple" 'E 'E) 'E)
                  (Nd 'D "Dog" 'E 'E)))
(check-expect (BSTMap-data (insert 'A "Anything" m1))
              (Nd 'C "Cat"
                  (Nd 'B "Bagel"
                      (Nd 'A "Anything" 'E 'E) 'E)
                  (Nd 'D "Dog" 'E 'E)))

;; Return all the keys from the map in left-to-right order,
;; i.e., an inorder traversal.
(: keys-inorder : All (a b) (BSTMap a b) -> (Listof a))
(define (keys-inorder map)
  (match map
    [(BSTMap _ tree)
     (local {(: lp : (BST a b) -> (Listof a))
             (define (lp t)
               (match t
                 ['E '()]
                 [(Nd node _ l r) (append
                                   (lp l)
                                   (list node)
                                   (lp r))]))}
       (lp tree))]))

(check-expect (keys-inorder m) '(1 2 3 4))
(check-expect (keys-inorder m1) '(A B C D))
(check-expect (keys-inorder m2) '(6 7 8 10 11))

;; Return all the key/value pairs from the map in left-to-right order,
;; i.e., an inorder traversal.
(: pairs-inorder : All (a b) (BSTMap a b) -> (Listof (Pair a b)))
(define (pairs-inorder map)
  (match map
    [(BSTMap _ tree)
     (local {(: lp : (BST a b) -> (Listof (Pair a b)))
             (define (lp t)
               (match t
                 ['E '()]
                 [(Nd node value l r) (append
                                       (lp l)
                                       (list (Pair node value))
                                       (lp r))]))}
       (lp tree))]))

(check-expect (pairs-inorder m)
              (list (Pair 1 "a")
                    (Pair 2 "b")
                    (Pair 3 "c")
                    (Pair 4 "d")))
(check-expect (pairs-inorder m1)
              (list
               (Pair 'A "Apple")
               (Pair 'B "Bagel")
               (Pair 'C "Cat")
               (Pair 'D "Dog")))

;; Return the leftmost key/value pair in the tree.
;; Note this is purely positional and we don't need to
;; know the tree ordering to do this.
;; It is an error to ask for the leftmost pair from the empty tree.
(: leftmost : All (a b) (BST a b) -> (Pair a b))
(define (leftmost tree)
  (match tree
    ['E (error "cannot find leftmost of an empty tree")]
    [(Nd node value 'E 'E) (Pair node value)]
    [(Nd node value 'E r) (Pair node value)]
    [(Nd node value l r) (leftmost l)]))

(check-error (leftmost 'E) "cannot find leftmost of an empty tree")
(check-expect (leftmost (BSTMap-data m)) (Pair 1 "a"))
(check-expect (leftmost (BSTMap-data m1)) (Pair 'A "Apple"))
(check-expect (leftmost (BSTMap-data m2)) (Pair 6 'six))

;; Remove the leftmost key/value pair in the tree.
;; Note this is purely positional and we don't need to know the tree
;; ordering to do this.
;; It is not an error to remove-leftmost from the empty tree; in that case,
;; just return the empty tree.
(: remove-leftmost : All (a b) (BST a b) -> (BST a b))
(define (remove-leftmost tree)
  (match tree
    ['E 'E]
    [(Nd node value 'E 'E) 'E]
    [(Nd k v (Nd _ _ 'E r2) r1) (Nd k v r2 r1)]
    [(Nd node value 'E r) r]
    [(Nd node value l r) (Nd node value (remove-leftmost l) r)]))

(check-expect (remove-leftmost (BSTMap-data m1))
              (Nd 'C "Cat"
                  (Nd 'B "Bagel" 'E 'E)
                  (Nd 'D "Dog" 'E 'E)))
(check-expect (remove-leftmost (BSTMap-data m))
              (Nd 3 "c"
                  (Nd 2 "b" 'E 'E)
                  (Nd 4 "d" 'E 'E)))
(check-expect (remove-leftmost (BSTMap-data m2))
              (Nd 10 'ten
                  (Nd 7 'seven 'E
                      (Nd 8 'eight 'E 'E))
                  (Nd 11 'eleven 'E 'E)))

;; Remove the key/value pair at the root of the tree.
;; Note this is purely positional and we don't need to
;; know the tree ordering to do this.
(: remove-root : All (a b) (BST a b) -> (BST a b))
(define (remove-root tree)
  (match tree
    ['E 'E]
    [(Nd key value l 'E) l]
    [(Nd key value l r)
     (match (leftmost r)
       [(Pair k v) (Nd k v l (remove-leftmost r))])])) 

(check-expect (remove-root (BSTMap-data m))
              (Nd 2 "b" 'E
                  (Nd 3 "c" 'E
                      (Nd 4 "d" 'E 'E))))
(check-expect (remove-root (BSTMap-data m1))
              (Nd 'D "Dog"
                  (Nd 'B "Bagel"
                      (Nd 'A "Apple" 'E 'E)
                      'E)
                  'E))
(check-expect (remove-root (BSTMap-data m2))
              (Nd 11 'eleven
                  (Nd 6 'six 'E
                      (Nd 7 'seven 'E
                          (Nd 8 'eight 'E 'E)))
                  'E))

;; Remove the given key, and its value, from the map.
;; If that key is absent from the map, return the map as is.
;; If it is present, navigate to the subtree where the given key is the root,
;; and use remove-root on that subtree.
;; We do need to know the ordering to do this
;; so we can navigate to the desired key.
(: remove : All (a b) a (BSTMap a b) -> (BSTMap a b))
(define (remove key map)
  (match (lookup key map)
    ['None map]
    [_ (match map
         [(BSTMap ord tree)
          (match tree
            [(Nd ky vl lsub rsub)
             (cond
               [(ord key ky)
                (BSTMap ord
                        (Nd ky vl
                            (map->bst (remove key (BSTMap ord lsub))) rsub))]
               [(ord ky key)
                (BSTMap ord
                        (Nd ky vl lsub
                            (map->bst (remove key (BSTMap ord rsub)))))]
               [else (BSTMap ord (remove-root tree))])])])]))
             
(check-expect (BSTMap-data (remove 11 m2))
              (Nd 10 'ten
                  (Nd 6 'six 'E
                      (Nd 7 'seven 'E
                          (Nd 8 'eight 'E 'E)))
                  'E))
(check-expect (BSTMap-data (remove 6 m2))
              (Nd 10 'ten
                  (Nd 7 'seven 'E
                      (Nd 8 'eight 'E 'E))
                  (Nd 11 'eleven 'E 'E)))
(check-expect (BSTMap-data (remove 7 m2))
              (Nd 10 'ten
                  (Nd 6 'six 'E
                      (Nd 8 'eight 'E 'E))
                  (Nd 11 'eleven 'E 'E)))
(check-expect (BSTMap-data (remove 10 m2))
              (Nd 11 'eleven
                  (Nd 6 'six 'E
                      (Nd 7 'seven 'E
                          (Nd 8 'eight 'E 'E)))
                  'E))

(test)