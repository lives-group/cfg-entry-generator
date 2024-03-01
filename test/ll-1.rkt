#lang racket

(require rackunit)

(require "../util/constants.rkt")
(require "../util/structs.rkt")

(define (is-ll-1? productions)
  (let ([list-of-firsts
         (map (lambda (production)
                (get-first-plus production productions)) productions)])
    (if (ormap has-duplicates? list-of-firsts) #false #true)))

(define (get-first-plus production productions)
  (match production
    [(Production (NT l) r) (get-first-plus-from-rhs l r productions)]
    [_ (error "get-first-plus failed!")]
    ))

(define (get-first-plus-from-rhs lhs rhs productions)
  (match rhs
    [(Seq l r) (let ([result (get-first-plus-from-rhs lhs l productions)])
                 (if (equal? result ε) (get-first-plus-from-rhs lhs r productions) result))]
    [(Alt l r) (flatten (list
                         (get-first-plus-from-rhs lhs l productions)
                         (get-first-plus-from-rhs lhs r productions)))] ; TODO1: Remover repetições; Tratar vazio (Follow)
    [(NT x) (if
             (equal? x lhs)
             (error "Same non-terminal")
             (get-first-plus-from-rhs lhs (find-production x productions) productions))]
    [(T x) (list x)]
    [_ (error "get-first-plus-from-rhs failed!")]
    ))

(define (find-production non-terminal productions)
  (findf (lambda (production)
           (match production
             [(Production (NT l) _) (equal? l non-terminal)]
             [_ #\f])) productions))

(define (has-duplicates? lst)
  (not (= (length lst) (length (remove-duplicates lst)))))

(check-equal? (is-ll-1?
               (list
                (Production (NT 'A) (Seq (T 'd) (NT 'C)))
                (Production (NT 'B) (Alt (Seq (Seq (T 'b) (T 'e)) (T 'd)) (Alt (Seq (Seq (T 'c) (NT 'B)) (T 'c)) (Seq (Seq (T 'd) (NT 'C)) (NT 'B)))))
                (Production (NT 'C) (Seq (Seq (T 'b) (NT 'A)) (T 'e)))
                )
               ) #true)

(test-case
 "A -> a | a should fail"
 (check-equal? (is-ll-1? (list (Production (NT 'A) (Alt (T 'a) (T 'a))))) #false)
 )

(test-case
 "A -> a | ab should fail"
 (check-equal? (is-ll-1? (list (Production (NT 'A) (Alt (T 'a) (Seq (T 'a) (T' b)))))) #false)
 )

(test-case
 "A -> A should throw"
 (check-exn
  exn:fail?
  (lambda ()
    (is-ll-1? (list (Production (NT 'A) (NT 'A)))))))

(test-case
 "Invalid Production format should throw"
 (check-exn
  exn:fail?
  (lambda ()
    (is-ll-1? (list (Production (NT 'A)))))))

(provide is-ll-1?)
