#lang racket

(require rackcheck)

(require "../util/constants.rkt")

(define gen:nothing (gen:const null))
(define gen:invalid (gen:const ∅))
(define gen:valid (gen:const ε))

(provide gen:nothing gen:invalid gen:valid)
