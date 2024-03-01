#lang racket

(require "constants.rkt")
(require "structs.rkt")

(define (rhs-empty? re)
  (eq? re ε))

(define (rhs-invalid? re)
  (eq? re ∅))

(define (rhs-terminal? rhs)
  (match rhs [(T _) #true] [_ #false]))

(define (rhs-non-terminal? rhs)
  (match rhs [(NT _) #true] [_ #false]))

(define (match-seq rhs lambda)
  (match rhs
    [(Seq l r) (lambda l r)]
    [_ ∅]
    ))

(define (match-alt rhs lambda)
  (match rhs
    [(Alt l r) (lambda l r)]
    [_ ∅]
    ))

(define (match-terminal rhs lambda)
  (match rhs
    [(T x) (lambda x)]
    [_ ∅]
    ))

(define (match-non-terminal rhs lambda)
  (match rhs
    [(NT x) (lambda x)]
    [_ ∅]
    ))

(provide
 match-alt
 match-non-terminal
 match-seq
 match-terminal
 rhs-empty?
 rhs-invalid?
 rhs-non-terminal?
 rhs-terminal?
 )
