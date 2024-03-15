#lang racket

(require rackcheck)

(require "./generator/util.rkt")
(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")

(provide gen:word-from-grammar in-grammar? Production Alt Seq NT T ∅ ε)

(define INITIAL-DEPTH 1)
(define MAX-DEPTH 100)

;; return - gerador
(define (gen:word-from-grammar grammar-list [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (let ([grammar-hash (reduce-production grammar-list)]
        [rhs (match (car grammar-list) [(Production _ rhs) rhs] [_ ∅])])
    (gen:_grammar-derivate-data grammar-hash rhs (list '()) depth max-depth)))

;; grammar-list : (list (Productions ...)) - Primeira produção de partida
;; word - Lista de símbolos
(define (in-grammar? grammar-list word [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (let ([grammar-hash (reduce-production grammar-list)]
        [rhs (match (car grammar-list) [(Production (NT _) r) r] [_ ∅])])
    (check-in-grammar? grammar-hash rhs word depth max-depth)))


;; IMPLEMENTAÇÕES PRIVADAS
(define (gen:_grammar-derivate-data grammar-hash rhs entries depth max-depth)
  (let ([derivative-rhs (if (empty? entries) ∅ (rhs-derivative grammar-hash rhs (last entries) depth max-depth))])
    derivative-rhs
    (cond
      ((> depth max-depth) gen:invalid)
      ((rhs-empty? derivative-rhs) (gen:const entries))
      ((rhs-invalid? derivative-rhs) gen:invalid)
      (else (gen:let ([new-symbol (gen:symbol grammar-hash derivative-rhs INITIAL-DEPTH max-depth)])
                     (cond
                       ((rhs-invalid? new-symbol) gen:invalid)
                       ((rhs-empty? new-symbol) (gen:const entries))
                       (else (gen:_grammar-derivate-data grammar-hash derivative-rhs (flatten (append entries (list new-symbol))) (+ depth 1) max-depth))))))))

(define (gen:symbol grammar-hash rhs depth max-depth)
  (cond
    ((> depth max-depth) gen:invalid)
    ((rhs-empty? rhs) gen:valid)
    ((rhs-invalid? rhs) gen:invalid)
    ((match-terminal rhs (lambda (terminal) (gen:const terminal))))
    ((rhs-non-terminal? rhs) gen:nothing)
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (gen:symbol grammar-hash (if (rhs-empty? rhs1) rhs2 rhs1) (+ depth 1) max-depth))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (let ([get-symbol1 (thunk (gen:symbol grammar-hash rhs1 (+ depth 1) max-depth))]
                            [get-symbol2 (thunk (gen:symbol grammar-hash rhs2 (+ depth 1) max-depth))])
                               (gen:let ([get-chosen-symbol (gen:one-of (list get-symbol1 get-symbol2))])
                                        (get-chosen-symbol)
                                 )))))
    (else ∅)))


(define (rhs-derivative grammar-hash rhs symbol [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
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
                               (hash-ref grammar-hash non-terminal))))
    ((match-seq rhs     (lambda (rhs1 rhs2)
                          (alt (seq (rhs-derivative grammar-hash rhs1 symbol depth max-depth) rhs2)
                               (seq (rhs-empty grammar-hash rhs1 depth max-depth) (rhs-derivative grammar-hash rhs2 symbol depth max-depth))))))
    ((match-alt rhs     (lambda (rhs1 rhs2)
                          (alt (rhs-derivative grammar-hash rhs1 symbol depth max-depth) (rhs-derivative grammar-hash rhs2 symbol depth max-depth)))))
    (else ∅)
    ))

(define (rhs-empty grammar-hash rhs depth max-depth)
  (cond
    ((> depth max-depth) ∅)
    ((rhs-empty? rhs) ε)
    ((rhs-invalid? rhs) ∅)
    ((rhs-terminal? rhs) ∅)
    ((rhs-non-terminal? rhs) ∅)
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (seq (rhs-empty grammar-hash rhs1 depth max-depth) (rhs-empty grammar-hash rhs2 depth max-depth)))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (alt (rhs-empty grammar-hash rhs1 depth max-depth) (rhs-empty grammar-hash rhs2 depth max-depth)))))
    (else ∅)))

(define (check-in-grammar? grammar-hash rhs word depth max-depth)
  (let ([derivative-rhs (rhs-derivative grammar-hash rhs (car word) depth max-depth)])
    (cond
      ((equal? derivative-rhs ∅) #false)
      ((equal? derivative-rhs ε) #true)
      (else (check-in-grammar? grammar-hash derivative-rhs (cdr word)))
  )))
