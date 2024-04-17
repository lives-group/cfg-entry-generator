#lang racket

(require rackunit)

(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")
(require "./delta.rkt")

(define (rhs-derivative grammar-hash rhs symbol [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (define new-depth (+ depth 1))
  (cond
    ((> depth max-depth) ∅)
    ((rhs-empty? rhs) ∅)
    ((rhs-invalid? rhs)  ∅)
    ((match-terminal rhs (lambda (terminal)
                           (cond
                             ((eq? terminal symbol) ε)
                             ((empty? symbol) rhs)
                             (else ∅)))))
    ((match-non-terminal rhs (lambda (non-terminal)
                               (rhs-derivative grammar-hash (hash-ref grammar-hash non-terminal ∅) symbol new-depth max-depth))))
    ((match-seq rhs     (lambda (rhs1 rhs2)
                          (alt (seq (rhs-derivative grammar-hash rhs1 symbol new-depth max-depth) rhs2)
                               (seq (rhs-delta grammar-hash rhs1 new-depth max-depth) (rhs-derivative grammar-hash rhs2 symbol new-depth max-depth))))))
    ((match-alt rhs     (lambda (rhs1 rhs2)
                          (alt (rhs-derivative grammar-hash rhs1 symbol new-depth max-depth) (rhs-derivative grammar-hash rhs2 symbol new-depth max-depth)))))
    (else ∅)
    ))

(test-case
 "Exceeding depth should fail"
 (check-equal? (rhs-derivative NOOP-GRAMMAR NOOP-RHS UNKNOWN EXCEEDING-DEPTH MAX-DEPTH) ∅)
 )

(test-case
 "d(a , a) = ε"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (T 'a) 'a) ε)
 )

(test-case
 "d(a , ε) = ∅"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (T 'a) ε) ∅)
 )

(test-case
 "d(a , b) = ∅"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (T 'a) 'b) ∅)
 )

(test-case
 "d(ab , a) = b"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Seq (T 'a) (T 'b)) 'a) (T 'b))
 )

(test-case
 "d(εb , b) = ε"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Seq ε (T 'b)) 'b) ε)
 )

(test-case
 "d(εa , b) = ∅"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Seq ε (T 'a)) 'b) ∅)
 )

(test-case
 "d(ab , b) = ∅"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Seq (T 'a) (T 'b)) 'b) ∅)
 )

(test-case
 "d(a|b , a) = ε"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Alt (T 'a) (T 'b)) 'a) ε)
 )

(test-case
 "d(a|b , b) = ε"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Alt (T 'a) (T 'b)) 'b) ε)
 )

(test-case
 "d(a|ε , a) = ε"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Alt (T 'a) ε) 'a) ε)
 )

(test-case
 "d(a|ε , b) = ∅"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Alt (T 'a) ε) 'b) ∅)
 )

(test-case
 "d(ab|cd , c) = d"
 (check-equal? (rhs-derivative NOOP-GRAMMAR (Alt (Seq (T 'a) (T 'b)) (Seq (T 'c) (T 'd))) 'c) (T 'd))
 )

(test-case
 "A -> a AND d(A , a) = ε"
 (check-equal? (rhs-derivative (reduce-production (list (Production (NT 'A) (T 'a)))) (NT 'A) 'a) ε)
 )

(test-case
 "A -> a AND d(A , b) = ∅"
 (check-equal? (rhs-derivative (reduce-production (list (Production (NT 'A) (T 'a)))) (NT 'A) 'b) ∅)
 )

(test-case
 "A -> ab AND d(A , a) = b"
 (check-equal? (rhs-derivative (reduce-production (list (Production (NT 'A) (Seq (T 'a) (T 'b))))) (NT 'A) 'a) (T 'b))
 )

(test-case
 "A -> Bc, B -> ab AND d(AB , a) = bcB"
 (check-equal? (rhs-derivative 
                 (reduce-production (list 
                                      (Production (NT 'A) (Seq (NT 'B) (T 'c)))
                                      (Production (NT 'B) (Seq (T 'a) (T 'b)))
                                      )) 
                 (Seq (NT 'A) (NT 'B)) 'a) (Seq (Seq (T 'b) (T 'c)) (NT 'B)))
 )

(test-case
 "A -> ab AND d(B , a) = ∅"
 (check-equal? (rhs-derivative (reduce-production (list (Production (NT 'A) (Seq (T 'a) (T 'b))))) (NT 'B) 'a) ∅)
 )

(test-case
 "A -> Aa AND d(A , a) = ∅"
 (check-equal? (rhs-derivative (reduce-production (list (Production (NT 'A) (Seq (NT 'A) (T 'a))))) (NT 'A) 'a) ∅)
 )


(provide rhs-derivative)
