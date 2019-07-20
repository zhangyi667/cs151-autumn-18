#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Candidate
  Symbol)
  
(define-type Ballot
  (Listof Candidate))

(define-type Ballots
  (Listof Ballot))

(define-struct CandCount
  ([cand  : Candidate]
   [count : Integer]))

(define-type Tally
  (Listof CandCount))

;; used in checks
(define sample-ballots
  (list (list 'A 'C 'B) (list 'A 'B) (list 'B 'C) (list 'C 'B 'A) (list 'C 'B)))

;; true if candidate doesn't have a CandCount and a new CandCount has to be made
(: need-CandCount? : Candidate Tally -> Boolean)
(define (need-CandCount? candidate list)
  (match list
    ['() #t]
    [(cons hd tl)
     (match hd
       [(CandCount cand _)
        (and (not (symbol=? candidate cand))
             (need-CandCount? candidate tl))])]))

(check-expect (need-CandCount? 'B (list (CandCount 'B 10) (CandCount 'C 10))) #f)
(check-expect (need-CandCount? 'A (list (CandCount 'B 10) (CandCount 'C 10))) #t)
(check-expect (need-CandCount? 'C (list (CandCount 'B 10) (CandCount 'C 10))) #f)

;; add one to candidate count inside Tally
(: add-count : Candidate Tally -> Tally)
(define (add-count newcand tally)
  (match tally
    ['() '()]
    [(cons h t)
     (match h
       [(CandCount cand count)
        (if (symbol=? cand newcand)
            (cons (CandCount cand (add1 count)) t)
            (cons h (add-count newcand t)))])]))

(check-expect (add-count 'A (list (CandCount 'A 10) (CandCount 'B 0)))
              (list (CandCount 'A 11) (CandCount 'B 0)))

;; make a Tally with all candidates with count 0
(: mktally : Ballots -> Tally)
(define (mktally ballots)
  (local {(: mk : Candidate -> CandCount)
          (define (mk cand)
            (CandCount cand 0))}
    (map mk (all-cands ballots))))

;; Count all the first-choice votes in the current round.
(: tally : Ballots -> Tally)
(define (tally bl)
  (local
    {(: all-cand : Tally)
     (define all-cand (mktally bl))
     (define ballot (choice1 bl))
     (: lp : Tally Ballots Tally -> Tally)
     (define (lp tally bls acc)
       (match tally
         ['() acc]
         [(cons (CandCount c v) t)
          (lp t bls (cons (CandCount c (occurences c ballot)) acc))]))}
    (lp all-cand bl '())))

;; append all the ballots and get a list of all candidates without duplicates
(: all-cands : Ballots -> Ballot)
(define (all-cands list)
  (local {(: long : Ballot)
          (define long
            (match list
              ['() '()]
              [(cons hd tl) (append hd (all-cands tl))]))}
    (remove-duplicates long)))

;; count how many times a cand occures in a list of Candidates
(: occurences : Candidate Ballot -> Integer)
(define (occurences cand ballot)
  (local
    {(: lp : Integer Candidate Ballot -> Integer)
     (define (lp acc cand ballot)
       (match ballot
         ['() acc]
         [(cons h t) (if (symbol=? h cand)
                         (lp (add1 acc) cand t)
                         (lp acc cand t))]))}
     (lp 0 cand ballot)))
                   
(check-expect (occurences 'A '(A B C A A A A D F)) 5)

;; returns a list of the first choices of a list of ballots
(: choice1 : Ballots -> (Listof Candidate))
(define (choice1 list)
  (match list
    ['() '()]
    [(cons h t)
     (match h
       ['() '()]
       [(cons hd _) (cons hd (choice1 t))])]))

(check-expect (choice1 sample-ballots) '(A A B C C))
(check-expect (choice1 '((A B C) (B C A))) '(A B))

;; returns the most votes of one candidate
(: most : Tally -> Integer)
(define (most list)
  (foldr (lambda ([a : CandCount] [b : Integer])
           (max (CandCount-count a) b)) 0 list))

(check-expect (most (tally sample-ballots)) 2)

;; sum of votes
(: sum-votes : Tally -> Integer)
(define (sum-votes list)
  (foldr (lambda ([a : CandCount] [b : Integer])
           (+ (CandCount-count a) b)) 0 list))

(check-expect (sum-votes (tally sample-ballots)) 5)

;; true if one candidate has the majority of the votes
(: majority? : Tally -> Boolean)
(define (majority? tally)
  (if (> (sum-votes tally) 0)
      (> (/ (most tally) (sum-votes tally)) 1/2)
      #f))

(check-expect (majority? (tally sample-ballots)) #f)

;; find the candidate given how many votes they have earned
;; this assumes there are no ties
;; only used for winner?, because there cannot be a tie for winner
(: find-cand : Integer Tally -> Candidate)
(define (find-cand votes tally)
  (match tally
    ['() 'None]
    [(cons hd tl)
     (match hd
       [(CandCount cand count)
        (if (= count votes)
            cand
            (find-cand votes tl))])]))

(check-expect (find-cand 10 (list (CandCount 'A 40)
                                  (CandCount 'B 30)
                                  (CandCount 'C 10))) 'C)

;; Is there a winner in *this* round?
;; That is, tally the votes in the current round, and return Some winner if
;; there exists a majority for a candidate. Otherwise, return 'None.
(: winner? : Ballots -> (Optional Candidate))
(define (winner? ballots)
  (local {(: local-tally : Tally)
          (define local-tally (tally ballots))}
    (if (majority? local-tally)
        (Some (find-cand (most local-tally) local-tally))
        'None)))

(check-expect (winner? sample-ballots) 'None)
(check-expect (winner? (list '(A) '(A) '(A))) (Some 'A))

;; min number of votes a candidate got
(: least : Tally -> Integer)
(define (least list)
  (foldr (lambda ([a : CandCount] [b : Integer])
           (min (CandCount-count a) b)) (most list) list))

(check-expect (least (tally sample-ballots)) 1)

;; finds a list of candidates in a tally instead of just one
;; is only used in losing
(: find-losing : Integer Tally -> (Listof Candidate))
(define (find-losing votes tally)
  (match tally
    ['() '()]
    [(cons hd tl)
     (match hd
       [(CandCount cand count)
        (if (= count votes)
            (cons cand (find-losing votes tl))
            (find-losing votes tl))])]))

(check-expect (find-losing 1 (tally sample-ballots)) '(B))

;; finds the losing candidate(s)
(: losing : Ballots -> (Listof Candidate))
(define (losing ballots)
  (local {(: local-tally : Tally)
          (define local-tally (tally ballots))}
    (find-losing (least local-tally) local-tally)))

(check-expect (losing sample-ballots) '(B))
(check-expect (losing
               (list '(A C B) '(A B) '(B C) '(B) '(C B A) '(C B))) '(B C A))

;; true if candidate is found in list of losing candidates
(: is-found? : Candidate (Listof Candidate) -> Boolean)
(define (is-found? cand list)
  (match list
    ['() #f]
    [(cons hd tl) (or (symbol=? cand hd)                     
                      (is-found? cand tl))]))

(check-expect (is-found? 'A '(C B E T EE A)) #t)
(check-expect (is-found? 'A '(C B E)) #f)

;; if losing candidate is found on ballot, remove the occurences of the cand
(: remove-ballot : Ballot (Listof Candidate) -> Ballot)
(define (remove-ballot ballot losinglist)
  (match ballot
    ['() '()]
    [(cons hd tl) (if (is-found? hd losinglist)
                      (remove-ballot tl losinglist)
                      (cons hd (remove-ballot tl losinglist)))]))

;; remove all occurences of candidate(s) on Ballots
(: remove : Ballots (Listof Candidate) -> Ballots)
(define (remove ballots losinglist)
  (match ballots
    ['() '()]
    [(cons first rest)
     (cons (remove-ballot first losinglist) (remove rest losinglist))]))
         
(check-expect (remove sample-ballots '(B)) '((A C) (A) (C) (C A) (C)))

;; removes the votes for the losing candidate(s)
(: choice2 : Ballots -> Ballots)
(define (choice2 ballots)
  (remove ballots (losing ballots)))

(check-expect (choice2 sample-ballots) '((A C) (A) (C) (C A) (C)))

;; remove empty Ballot in Ballots
(: emptylist : Ballots -> Ballots)
(define (emptylist ballots)
  (match ballots
    ['() '()]
    [(cons hd tl) (if (empty? hd)
                      (emptylist tl)
                      (cons hd (emptylist tl)))]))

;; Run rounds until Some winner is found. 
;; Eliminate the trailing candidate (or candidates if there is a tie)
;; after each round.
;; Return 'None in case of no majority after any round, and no more rounds left
(: election : Ballots -> (Optional Candidate))
(define (election ballots)
  (match (emptylist ballots)
    ['() 'None]
    [_ (match (winner? ballots)
         ['None (election (emptylist (choice2 ballots)))]
         [(Some cand) (Some cand)])]))

(check-expect (election (list '(A B C) '(B C A) '(A B C) '(B A A))) 'None)
(check-expect (election (list (list 'A 'B)
                              (list 'A 'C)
                              (list 'D 'B)
                              (list 'D 'C)
                              (list 'B 'A)
                              (list 'C 'A))) (Some 'A))
(check-expect (election (list '(A) '(B) '(C) '(D))) 'None)
(check-expect (election '((a b c d)
                          (b b b b)
                          (a a a a)
                          (c d c c)
                          (c c c c))) 'None)
(check-expect (election '((A B C)
                          (A C C)
                          (B A A)
                          (B C B)
                          (C B B)
                          (C C))) 'None)

;; returns a list of the losers of each round
(: listlist : Ballots -> (Listof (Listof Candidate)))
(define (listlist ballots)
  (match (losing ballots)
    ['() (list '())]
    [list (cons list (listlist (choice2 ballots)))]))

(check-expect (listlist sample-ballots) '((B) (A) (C) ()))

;; prints the results of a round
(: printround : Integer Ballots -> String)
(define (printround n ballots)
  (string-append "In round " (number->string n)
                 ", the following candidate(s) were eliminated: "
                 (foldr string-append "" (map symbol->string (losing ballots)))))

(check-expect (printround 1 sample-ballots)
              "In round 1, the following candidate(s) were eliminated: B")

;; prints the result of a round
(: rounds : Integer Ballots -> String)
(define (rounds n ballots)
  (if (< n (sub1 (length (listlist ballots))))
      (string-append (printround n ballots) ". " (rounds (add1 n) (choice2 ballots)))
      (printround n ballots)))

;(check-expect (rounds 1 sample-ballots)
;              "In round 1, the following candidate(s) were eliminated: B.
;In round 2, the following candidate(s) were eliminated: A")

;; Build a string in plain English that describes the election. The exact wording
;; is up to you, but the following requirements must be met. The number of rounds
;; must be included in the report. The candidate or candidates eliminated in each
;; round must be identified. The report must be in grammatically correct language
;; suitable for reading by a general reader.
(: election-report : Ballots -> String)
(define (election-report ballots)
  (match (winner? ballots)
    [(Some x)
     (string-append (symbol->string x) " won in round 1")]
    ['None
     (string-append
      (rounds 1 ballots) ". "
      (match (election ballots)
        ['None "No winner"]
        [(Some x) (symbol->string x)])
      " won in round "
      (number->string (sub1 (length (listlist ballots))))
      ", the final round.")]))

;(check-expect (election-report sample-ballots)
;              "In round 1, the following candidate(s) were eliminated: B.
;In round 2, the following candidate(s) were eliminated: A.
;C won in round 3, the final round.")

"eyeball check"
(election-report sample-ballots)
(election-report '((A) (B) (C) (D) (A)))
(election-report (list (list 'A) (list 'A) (list 'D) (list 'D) (list 'A) (list 'A)))
(election-report '((A) (B) (A) (B) (A) (B)))
(election-report '((A B C) (A C C) (B A A) (B C B) (C B B) (C C)))
(election-report '((a b c d)(b b b b)(a a a a)(c d c c)(c c c c)))
(election-report '((A) (A) (A)))

;; sorry, I didn't do check-expects

(test)