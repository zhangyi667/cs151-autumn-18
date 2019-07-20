#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

;; marginal-tax: compute the amount of tax owed on the given income.
;; Use exact-ceiling to round up.
(: marginal-tax (Integer -> Integer))
(define (marginal-tax income)
  (cond
    [(<= income 30000) 0]
    [(<= income 50000) (exact-ceiling (* 0.1 (- income 30000)))]
    [(<= income 80000) (exact-ceiling (+ 2000 (* 3/10 (- income 50000))))]
    [else (exact-ceiling (+ 11000 (* 1/2 (- income 80000))))]))

(check-expect (marginal-tax 40000) 1000)
(check-expect (marginal-tax 60000) 5000)
(check-expect (marginal-tax 100000) 21000)

;; pet-deduction: given a number of pets, compute the deduction
(: pet-deduction (Integer -> Integer))
(define (pet-deduction pets)
  (cond
    [(< pets 3) 0]
    [(<= pets 9) (* pets 1000)]
    [else 15000]))

(check-expect (pet-deduction 3) 3000)
(check-expect (pet-deduction 5) 5000)
(check-expect (pet-deduction 100) 15000)

;; hybrid-deduction: given a Boolean indicating whether the family unit
;; has a hybrid car or not, the family's income, and the number of
;; people in the family unit, compute the hybrid car deduction.
;; Use exact-ceiling to round up.
(: hybrid-deduction (Boolean Integer Integer -> Integer))
(define (hybrid-deduction hybrid income people)
  (cond
    [hybrid (exact-ceiling (* 0.02 income people))]
    [else 0]))

(check-expect (hybrid-deduction #t 120000 3) 7200)
(check-expect (hybrid-deduction #f 120000 3) 0)

;; itemized: compute the tax on a family unit choosing to itemize.
;; The parameters are the family's income, the number of people in
;; the family unit, the number of pets, and a Boolean indicating 
;; ownership of a hybrid car.
(: itemized (Integer Integer Integer Boolean -> Integer))
(define (itemized income people pets hybrid)
  (marginal-tax (- income (pet-deduction pets)
                   (hybrid-deduction hybrid income people))))

(check-expect (itemized 120000 3 4 #t) 25400)

;; standard: compute the tax on a family unit choosing to take the 
;; standard deduction. The parameters are income and number of people.
(: standard (Integer Integer -> Integer))
(define (standard income people)
  (marginal-tax (- income (* 4000 people))))

(check-expect (standard 120000 3) 25000)

;; should-itemize?: Determine whether a family unit would pay lower taxes
;; if they were to itemize. If so, return true; otherwise false.
;; The parameters are the unit's income, the number of people, the number of
;; pets, and a Boolean indicating ownership of a hybrid car.
(: should-itemize? (Integer Integer Integer Boolean -> Boolean))
(define (should-itemize? income people pets hybrid)
  (< (itemized income people pets hybrid) (standard income people)))

(check-expect (should-itemize? 120000 3 4 #t) #f)
(check-expect (should-itemize? 100000 2 15 #t) #t)

;; tax-return: Determine the negative (refund) or positive (payment) due
;; a family unit, given their income as well as the amount withheld.
;; The parameters are the income, the number of people, the number of pets,
;; ownership of a hybrid car, and the amount withheld and already paid to
;; the government. Assume the family chooses either to itemize their
;; deductions or to take the standard deduction according to the choice
;; that results in the lower tax payment.
(: tax-return (Integer Integer Integer Boolean Integer -> Integer))
(define (tax-return income people pets hybrid paid)
  (- (if (should-itemize? income people pets hybrid)
         (itemized income people pets hybrid)
         (standard income people)) paid))

(check-expect (tax-return 120000 3 4 #t 24000) 1000)
(check-expect (tax-return 100000 2 15 #f 20000) -6500)

(test)
    