#lang racket

(require "constants.rkt")
(require "predicates.rkt")
(require "structs.rkt")

;; Simplificadores de predicados.
(define (seq-lazy rhs1-thunk rhs2-thunk)
  (define rhs1 (rhs1-thunk))
  (cond
    [(rhs-invalid? rhs1) ∅]
    [(rhs-empty? rhs1) (rhs2-thunk)]
    [else (define rhs2 (rhs2-thunk))
          (cond
            [(rhs-invalid? rhs2) ∅]
            [(rhs-empty? rhs2) rhs1]
            [else (Seq rhs1 rhs2)])]
  ))

(define (seq rhs1 rhs2)
  (cond
    ((rhs-invalid? rhs1) ∅)
    ((rhs-invalid? rhs2) ∅)
    ((rhs-empty? rhs1) rhs2)
    ((rhs-empty? rhs2) rhs1)
    (else (Seq rhs1 rhs2))))

(define (alt rhs1 rhs2)
  (cond
    ((rhs-invalid? rhs1) rhs2)
    ((rhs-invalid? rhs2) rhs1)
    (else (Alt rhs1 rhs2))))

(define (terminal rhs) (T rhs))

(define (non-terminal rhs) (NT rhs))

(define (production-list-to-hash productions)
  (apply hash (append* productions)))

(define (reduce-production productions)
  (production-list-to-hash (map reduce-production-ruleset productions)))

(define (reduce-production-ruleset production)
  (match production
    [(Production (NT l) r) (list l (reduce-production-ruleset r))]
    [(Seq l r) (seq (reduce-production-ruleset l) (reduce-production-ruleset r))]
    [(Alt l r) (alt (reduce-production-ruleset l) (reduce-production-ruleset r))]
    [x x]
    ))

(define (alt-to-list rhs)
  (match rhs
    [(Alt l r) (flatten (list (alt-to-list l) (alt-to-list r)))]
    [x x]
    ))

(provide seq seq-lazy alt terminal non-terminal reduce-production alt-to-list)
