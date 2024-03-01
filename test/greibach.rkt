#lang racket

(require rackunit)

(require "../util/structs.rkt")

(define (is-greibach-normal-form? productions)
  (andmap
   (lambda (production) (check-normal-form production)) productions))

(define (check-normal-form exp)
  (match exp
    [(Production (NT _) rhs) (check-normal-form rhs)]
    [(Alt l r) (and (check-normal-form l) (check-normal-form r))]
    [(Seq l _) (check-normal-form l)]
    [(T _) #true]
    [_ #false]))

(check-equal? (is-greibach-normal-form?
               (list
                (Production (NT 'A) (Seq (T 'd) (NT 'C)))
                (Production (NT 'B) (Alt (Seq (Seq (T 'b) (T 'e)) (T 'd)) (Alt (Seq (Seq (T 'c) (NT 'B)) (T 'c)) (Seq (Seq (T 'd) (NT 'C)) (NT 'B)))))
                (Production (NT 'C) (Seq (Seq (T 'b) (NT 'A)) (T 'e)))
                )
               ) #true)

(test-case
 "A -> Aa should fail"
 (check-equal? (is-greibach-normal-form? (list (Production (NT 'A) (Seq (NT 'A) (T 'a))))) #false)
 )

(test-case
 "Invalid Production format should return false"
 (check-equal? (is-greibach-normal-form? (list 'wrong-data)) #false)
 )

(provide is-greibach-normal-form?)
