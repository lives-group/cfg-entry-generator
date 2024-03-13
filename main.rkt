#lang racket

(require rackcheck)

(require "./generator/util.rkt")
(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")

(provide gen:word-from-grammar in-grammar? Production Alt Seq NT T)

;; return - gerador
(define (gen:word-from-grammar grammar-list [depth 1] [max-depth 100])
  (let ([grammar-hash (reduce-production grammar-list)]
        [rhs (match (car grammar-list) [(Production _ rhs) rhs] [_ ∅])])
    (gen:_grammar-derivate-data grammar-hash rhs (list '()) depth max-depth)))

;; grammar-list : (list (Productions ...)) - Primeira produção de partida
;; word - Lista de símbolos
(define (in-grammar? grammar-list word)
  (let ([grammar-hash (reduce-production grammar-list)]
        [rhs (match (car grammar-list) [(Production (NT _) r) r] [_ ∅])])
    (check-in-grammar? grammar-hash rhs word)))


;; IMPLEMENTAÇÕES PRIVADAS

(define (gen:_grammar-derivate-data grammar-hash rhs entries depth max-depth)
  (let ([derivative-rhs (rhs-derivative grammar-hash rhs (last entries))])
    derivative-rhs
    (cond
      ((> depth max-depth) gen:invalid)
      ((rhs-empty? derivative-rhs) (gen:const entries))
      ((rhs-invalid? derivative-rhs) gen:invalid)
      (else (gen:let ([new-symbol (gen:symbol grammar-hash derivative-rhs)])
                     (if (rhs-empty? new-symbol)
                         (gen:const entries)
                         (gen:_grammar-derivate-data grammar-hash derivative-rhs (flatten (append entries (list new-symbol))) (+ depth 1) max-depth)))))))

(define (gen:symbol grammar-hash rhs)
  (cond
    ((rhs-empty? rhs) gen:valid)
    ((rhs-invalid? rhs) gen:invalid)
    ((match-terminal rhs (lambda (terminal) (gen:const terminal))))
    ((rhs-non-terminal? rhs) gen:nothing)
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (gen:symbol grammar-hash (if (rhs-empty? rhs1) rhs2 rhs1)))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (gen:let ([symbol1 (gen:symbol grammar-hash rhs1)]
                                [symbol2 (gen:symbol grammar-hash rhs2)])
                               (gen:one-of (list symbol1 symbol2))))))
    (else ∅)))


(define (rhs-derivative grammar-hash rhs symbol)
  (cond
    ((rhs-empty? rhs) ∅)
    ((rhs-invalid? rhs)  ∅)
    ((match-terminal rhs (lambda (terminal)
                           (cond
                             ((eq? terminal symbol) ε)
                             ((empty? symbol) rhs)
                             (else ∅)))))
    ((match-non-terminal rhs (lambda (non-terminal)
                               (hash-ref grammar-hash non-terminal))))
    ((match-seq rhs     (lambda (rhs1 rhs2)
                          (alt (seq (rhs-derivative grammar-hash rhs1 symbol) rhs2)
                               (seq (rhs-empty grammar-hash rhs1) (rhs-derivative grammar-hash rhs2 symbol))))))
    ((match-alt rhs     (lambda (rhs1 rhs2)
                          (alt (rhs-derivative grammar-hash rhs1 symbol) (rhs-derivative grammar-hash rhs2 symbol)))))
    (else ∅)
    ))

(define (rhs-empty grammar-hash rhs)
  (cond
    ((rhs-empty? rhs) ε)
    ((rhs-invalid? rhs) ∅)
    ((rhs-terminal? rhs) ∅)
    ((rhs-non-terminal? rhs) ∅)
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (seq (rhs-empty grammar-hash rhs1) (rhs-empty grammar-hash rhs2)))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (alt (rhs-empty grammar-hash rhs1) (rhs-empty grammar-hash rhs2)))))
    (else ∅)))

(define (check-in-grammar? grammar-hash rhs word)
  (let ([derivative-rhs (rhs-derivative grammar-hash rhs (car word))])
    (cond
      ((equal? derivative-rhs ∅) #false)
      ((equal? derivative-rhs ε) #true)
      (else (check-in-grammar? grammar-hash derivative-rhs (cdr word)))
  )))

