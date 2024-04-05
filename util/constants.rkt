#lang racket

(define ∅ #f)
(define ε #t)

(define INITIAL-DEPTH 1)
(define MAX-DEPTH 20)
(define UNKNOWN 'unknown)
(define NOOP-GRAMMAR '())
(define NOOP-RHS '())
(define EXCEEDING-DEPTH (+ MAX-DEPTH 1))

(provide ∅ ε INITIAL-DEPTH MAX-DEPTH UNKNOWN NOOP-GRAMMAR NOOP-RHS EXCEEDING-DEPTH)

