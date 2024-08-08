#lang racket

(require rackcheck)
(provide (all-defined-out))

; Generates a list of naturals 
(define (gen:listNat n k)
  (if (eq? n 0) (gen:bind (gen:integer-in 0 k) (lambda (e) (gen:const (list e))) )
      (gen:bind (gen:listNat (- n 1) k)
                (lambda (xs) (gen:bind ( gen:integer-in 0 k)
                                      (lambda (x) (gen:const (list* x xs)) ) ))))
  )

(define (gen:var n)
  (gen:map (gen:listNat n 23) (lambda (s) (list->string (map (lambda (z) (integer->char (+ z 65))) s ))  ) )
  )

(define (gen:symbolVar n)
  (gen:map (gen:listNat n 23) (lambda (s) (string->symbol (list->string (map (lambda (z) (integer->char (+ z 65))) s )))  ) )
  )
