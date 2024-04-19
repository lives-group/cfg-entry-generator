#lang racket

(require rackcheck)
(require racket/set)

(require "./generator/util.rkt")
(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")
(require "./derivative.rkt")
(require "./delta.rkt")

(provide gen:word-from-grammar gen:distinct-word-from-grammar in-grammar? Production Alt Seq NT T ∅ ε)

;; return - gerador
(define (gen:word-from-grammar grammar-list [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (let ([grammar-hash (reduce-production grammar-list)]
        [rhs (match (car grammar-list) [(Production _ rhs) rhs] [_ ∅])])
    (gen:_grammar-derivate-data grammar-hash rhs (list '()) (make-hash) depth max-depth)))

;; return - gerador (palavras unicas
(define (gen:distinct-word-from-grammar grammar-list [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] [word-set (mutable-set)])
  (gen:let ([word (gen:word-from-grammar grammar-list depth max-depth)])
           (cond
             [(rhs-invalid? word) '∅]
             [(not (set-member? word-set word))
              (set-add! word-set word)
              word]
             [else (gen:distinct-word-from-grammar grammar-list depth max-depth word-set)]
             )))

;; grammar-list : (list (Productions ...)) - Primeira produção de partida
;; word - Lista de símbolos
(define (in-grammar? grammar-list word [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (let ([grammar-hash (reduce-production grammar-list)]
        [rhs (match (car grammar-list) [(Production (NT _) r) r] [_ ∅])])
    (check-in-grammar? grammar-hash rhs word depth max-depth)))

;; IMPLEMENTAÇÕES PRIVADAS
(define (gen:_grammar-derivate-data grammar-hash rhs entries old-results depth max-depth)
  (define old-result (hash-ref old-results (list rhs entries) UNKNOWN))
  (cond
    ; Se o resultado for UNKNOWN, precisamos computar o novo generator.
    [(eq? old-result UNKNOWN)
     (define new-result
       (let ([derivative-rhs (if (empty? entries) ∅ (rhs-derivative grammar-hash rhs (last entries) depth max-depth))])
         (cond
           [(> depth max-depth) gen:invalid]
           [(rhs-empty? derivative-rhs) (gen:const entries)]
           [(rhs-invalid? derivative-rhs) gen:invalid]
           [else (gen:let ([new-symbol (gen:symbol grammar-hash derivative-rhs INITIAL-DEPTH max-depth)])
                          (cond
                            [(rhs-invalid? new-symbol) gen:invalid]
                            [(rhs-empty? new-symbol) (gen:const entries)]
                            [else (gen:_grammar-derivate-data
                                   grammar-hash
                                   derivative-rhs
                                   (flatten (append entries (list new-symbol)))
                                   old-results
                                   (+ depth 1)
                                   max-depth)]))]
           )))
     (hash-set! old-results (list rhs entries) new-result)
     new-result]

    ; Caso contrário, retornamos um resultado previamente computado.
    [else old-result]))

(define (gen:symbol grammar-hash rhs depth max-depth)
  (define new-depth (+ depth 1))
  (cond
    ((> depth max-depth) gen:invalid)
    ((rhs-empty? rhs) gen:valid)
    ((rhs-invalid? rhs) gen:invalid)
    ((match-terminal rhs (lambda (terminal) (gen:const terminal))))
    ((match-non-terminal rhs (lambda (non-terminal)
                               (gen:symbol grammar-hash (hash-ref grammar-hash non-terminal ∅) new-depth max-depth)
                               )))
    ((rhs-non-terminal? rhs) gen:nothing)
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (gen:let ([symbol1 (gen:symbol grammar-hash rhs1 new-depth max-depth)])
                               (cond
                                 [(rhs-empty? symbol1) (gen:symbol grammar-hash rhs2 new-depth max-depth)]
                                 [else symbol1]
                                 )))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      ; Jeito Novo
                      (let ([alt-list (alt-to-list rhs)])
                        (gen:let ([get-chosen-symbol (gen:one-of (map (lambda (alternative) (thunk (gen:symbol grammar-hash alternative new-depth max-depth))) alt-list))])
                                 (get-chosen-symbol)))
                      ; Jeito Antigo
                      #;(let ([get-symbol1 (thunk (gen:symbol grammar-hash rhs1 new-depth max-depth))]
                            [get-symbol2 (thunk (gen:symbol grammar-hash rhs2 new-depth max-depth))])
                        (gen:let ([get-chosen-symbol (gen:one-of (list get-symbol1 get-symbol2))])
                                 (get-chosen-symbol))))))
    (else ∅)))

(define (check-in-grammar? grammar-hash rhs word depth max-depth) ; TODO - depth faz sentido aqui?
  (define symbol (if (empty? word) ε (car word)))
  (define remaining-word (if (empty? word) '() (cdr word)))
  (let ([derivative-rhs (rhs-derivative grammar-hash rhs symbol depth max-depth)])
    (cond
      ((equal? derivative-rhs ∅) #false)
      ((equal? derivative-rhs ε) #true)
      (else (check-in-grammar? grammar-hash derivative-rhs remaining-word depth max-depth))
      )))
