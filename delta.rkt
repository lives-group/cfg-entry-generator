#lang racket

(require rackunit)

(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")

(define (rhs-delta grammar-hash rhs [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] #:ignore-NT[ignore-NT #f])
  (define new-depth (+ depth 1))
  (cond
    ((> depth max-depth) ∅)
    ((rhs-empty? rhs) ε)
    ((rhs-invalid? rhs) ∅)
    ((rhs-terminal? rhs) ∅)
    ((match-non-terminal rhs (lambda (non-terminal) (if (eq? ignore-NT #t) ∅ (rhs-delta grammar-hash (hash-ref grammar-hash non-terminal ∅) new-depth max-depth  #:ignore-NT ignore-NT)))))
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (seq-lazy (thunk (rhs-delta grammar-hash rhs1 new-depth max-depth  #:ignore-NT ignore-NT)) (thunk (rhs-delta grammar-hash rhs2 new-depth max-depth  #:ignore-NT ignore-NT))))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (evaluate-alt grammar-hash rhs1 rhs2 new-depth max-depth ignore-NT))))
    (else ∅)))

(define (evaluate-alt grammar-hash rhs1 rhs2 depth max-depth ignore-NT)
  (cond
    ((or (rhs-empty? rhs1) (rhs-empty? rhs2)) ε)
    (else (alt (rhs-delta grammar-hash rhs1 depth max-depth #:ignore-NT ignore-NT) (rhs-delta grammar-hash rhs2 depth max-depth #:ignore-NT ignore-NT)))
    )
  )

(test-case
 "Exceeding depth should fail"
 (check-equal? (rhs-delta NOOP-GRAMMAR NOOP-RHS EXCEEDING-DEPTH MAX-DEPTH) ∅)
 )

(test-case
 "Empty rhs should pass"
 (check-equal? (rhs-delta NOOP-GRAMMAR ε) ε)
 )

(test-case
 "Invalid rhs should fail"
 (check-equal? (rhs-delta NOOP-GRAMMAR ∅) ∅)
 )

(test-case
 "Terminal rhs should fail"
 (check-equal? (rhs-delta NOOP-GRAMMAR (T 'a)) ∅)
 )

(test-case
 "First empty sequence should fail"
 (check-equal? (rhs-delta NOOP-GRAMMAR (Seq ε (T 'a))) ∅)
 )

(test-case
 "Second empty sequence should fail"
 (check-equal? (rhs-delta NOOP-GRAMMAR (Seq (T 'a) ε)) ∅)
 )

(test-case
 "Empty sequences should pass"
 (check-equal? (rhs-delta NOOP-GRAMMAR (Seq ε ε)) ε)
 )

(test-case
 "First empty alternative should pass"
 (check-equal? (rhs-delta NOOP-GRAMMAR (Alt ε (T 'a))) ε)
 )

(test-case
 "Second empty alternative should pass"
 (check-equal? (rhs-delta NOOP-GRAMMAR (Alt (T 'a) ε)) ε)
 )

(test-case
 "Alt should fail if not empty choices are found"
 (check-equal? (rhs-delta NOOP-GRAMMAR (Alt (T 'a) (T 'b))) ∅)
 )

(test-case
 "NT should pass if expression in hash is ε"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) ε))) (NT 'A)) ε)
 )

(test-case
 "NT should pass if expression in hash is empty sequences"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) (Seq ε ε)))) (NT 'A)) ε)
 )

(test-case
 "NT should pass if expression in hash is valid alt"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) (Alt (T 'a) ε)))) (NT 'A)) ε)
 )

(test-case
 "NT should lazily pass even if first choice induces a loop"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) (Alt (NT 'A) ε)))) (NT 'A)) ε)
 )

(test-case
 "NT should fail if no imediate options are empty"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) (Alt (Seq (T 'a) (T 'b)) (NT 'A))))) (NT 'A)) ∅)
 )

(test-case
 "NT should fail if first term forces a loop"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) (Seq (NT 'A) ε)))) (NT 'A)) ∅)
 )

(test-case
 "NT should fail if direct term forces a loop"
 (check-equal? (rhs-delta (reduce-production (list (Production (NT 'A) (NT 'A)))) (NT 'A)) ∅)
 )

(test-case
 "Unrecognizable expression should fail"
 (check-equal? (rhs-delta NOOP-GRAMMAR UNKNOWN) ∅)
 )

(provide rhs-delta)
