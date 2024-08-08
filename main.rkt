#lang racket

(require rackcheck)
(require racket/generator)
(require racket/set)

(require "./generator/util.rkt")
(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")
(require "./derivative.rkt")
(require "./delta.rkt")

(provide generate-words gen:word-from-grammar gen:distinct-word-from-grammar in-grammar? Production Alt Seq NT T ∅ ε)

(define (gen:word grammar-list number-of-iterations [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] #:starting-NT[starting-NT 'NONE])
  (let ([grammar-hash (reduce-production grammar-list)])
    (let ([rhs (if (eq? starting-NT 'NONE) (match (car grammar-list) [(Production (NT _) r) r] [_ ∅]) (hash-ref grammar-hash starting-NT))])
      (let ([words (_expand-words grammar-hash (list (cons '() rhs)) 1 number-of-iterations depth max-depth)])
        (if (empty? words) (gen:const ∅) (gen:one-of words))))))

(define (generate-words grammar-list number-of-iterations [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] #:starting-NT[starting-NT 'NONE])
  (let ([grammar-hash (reduce-production grammar-list)])
    (let ([rhs (if (eq? starting-NT 'NONE) (match (car grammar-list) [(Production (NT _) r) r] [_ ∅]) (hash-ref grammar-hash starting-NT))])
      (_expand-words grammar-hash (list (cons '() rhs)) 1 number-of-iterations depth max-depth))))

;; return - gerador
(define (gen:word-from-grammar grammar-list [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] #:starting-NT[starting-NT 'NONE])
  (let ([grammar-hash (reduce-production grammar-list)])
    (let ([rhs (if (eq? starting-NT 'NONE) (match (car grammar-list) [(Production (NT _) r) r] [_ ∅]) (hash-ref grammar-hash starting-NT))])
    (gen:_grammar-derivate-data grammar-hash #;rhs (_rewrite-with-terminals grammar-hash rhs) (list '()) (make-hash) depth max-depth))))

;; return - gerador palavras unicas
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
(define (in-grammar? grammar-list word [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] #:starting-NT[starting-NT 'NONE])
  (let ([grammar-hash (reduce-production grammar-list)])
    (let ([rhs (if (eq? starting-NT 'NONE) (match (car grammar-list) [(Production (NT _) r) r] [_ ∅]) (hash-ref grammar-hash starting-NT))])
    (check-in-grammar? grammar-hash rhs word depth max-depth))))

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
                            [(rhs-invalid? new-symbol) #;gen:invalid (gen:_grammar-derivate-data grammar-hash (_rewrite-with-terminals grammar-hash rhs) (list '()) (make-hash) depth max-depth)] ;;Tentando um fallback
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
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (gen:let ([symbol1 (gen:symbol grammar-hash rhs1 new-depth max-depth)])
                               (cond
                                 [(rhs-empty? symbol1) (gen:symbol grammar-hash rhs2 new-depth max-depth)]
                                 [else symbol1]
                                 )))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (let ([alt-list (alt-to-list rhs)])
                        (gen:let ([get-chosen-symbol (gen:one-of (map (lambda (alternative) (thunk (gen:symbol grammar-hash alternative new-depth max-depth))) alt-list))])
                                 (get-chosen-symbol))))))
    (else ∅)))

;; TODO - n funciona com recurção à esquerda
(define (check-in-grammar? grammar-hash rhs word depth max-depth) ; TODO - depth faz sentido aqui?
  (define symbol (if (empty? word) ε (car word)))
  (define remaining-word (if (empty? word) '() (cdr word)))
  (if (and (empty? word) (rhs-delta grammar-hash rhs))
      #t
      (let ([derivative-rhs (rhs-derivative grammar-hash rhs symbol depth max-depth)])
        (cond
          ((equal? derivative-rhs ∅) #false)
          ((equal? derivative-rhs ε) #true)
          (else (check-in-grammar? grammar-hash derivative-rhs remaining-word depth max-depth))
          ))))

(define (has-only-terminals? rhs)
  (match rhs
    [(NT _) #f]
    [∅ #f]
    [(Seq l r) (and (has-only-terminals? l) (has-only-terminals? r))]
    [(Alt l r) (and (has-only-terminals? l) (has-only-terminals? r))]
    [_ #t]))

;; start para derivar a gramatica por forca bruta
(define (expand-words grammar-list number-of-iterations [depth INITIAL-DEPTH] [max-depth MAX-DEPTH] #:starting-NT[starting-NT 'NONE])
  (define grammar-hash (reduce-production grammar-list))
  (let ([rhs (if (eq? starting-NT 'NONE) (match (car grammar-list) [(Production (NT _) r) r] [_ ∅]) (hash-ref grammar-hash starting-NT))])
    (_expand-words grammar-hash (list (cons '() rhs)) 1 number-of-iterations depth max-depth)
    ))

(define (_expand-words grammar-hash pairs it max-it depth max-depth)
  (if (> it max-it)
      '()
      (let ([new-pairs (derivate-all-pairs grammar-hash pairs depth max-depth)])
        (let ([words (map (lambda (pair) (car pair))
                          (filter (lambda (pair) (rhs-delta grammar-hash (cdr pair))) new-pairs))])
          (append words (_expand-words grammar-hash new-pairs (+ it 1) max-it depth max-depth))))))

;; dado uma lista de pares (cons word rhs), deriva todos os pares
(define (derivate-all-pairs grammar-hash pairs [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (foldr
   (lambda (x xs) (append x xs))
   (list)
   (map (lambda (pair)
         (let ([next-symbols (first grammar-hash (cdr pair) depth max-depth)])
           (_derivate-all-symbols grammar-hash (car pair) (cdr pair) next-symbols depth max-depth))
         ) pairs)))

;; rhs pode possuir mais de um simbolo possivel, deriva o mesmo rhs para cada simbolo
(define (_derivate-all-symbols grammar-hash word rhs symbols depth max-depth)
  (map (lambda (symbol)
         (cons (append word (list symbol)) (rhs-derivative grammar-hash rhs symbol depth max-depth))
         ) symbols))

(define (process-derivate-symbol grammar-hash symbol word rhs depth max-depth)
  (if (rhs-empty? rhs)
      (cons word rhs)
      (cons (append word (list symbol)) (rhs-derivative grammar-hash rhs symbol depth max-depth))
  ))

(define (first grammar-hash rhs depth max-depth)
  (let ([symbol (_first grammar-hash rhs depth max-depth)])
    (if (list? symbol) (remove-duplicates symbol) (list symbol))
  ))

(define (_first grammar-hash rhs depth max-depth)
  (define new-depth (+ depth 1))
  (cond
    ((> depth max-depth) ∅)
    ((rhs-empty? rhs) (list))
    ((rhs-invalid? rhs) ∅)
    ((match-terminal rhs (lambda (terminal) (list terminal))))
    ((match-non-terminal rhs (lambda (non-terminal)
                               (_first grammar-hash (hash-ref grammar-hash non-terminal ∅) new-depth max-depth)
                               )))
    ((match-seq rhs (lambda (rhs1 rhs2)
                       (_first grammar-hash (if (rhs-delta grammar-hash rhs1) rhs2 rhs1) new-depth max-depth)
                       )))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (flatten (list
                       (_first grammar-hash rhs1 new-depth max-depth)
                       (_first grammar-hash rhs2 new-depth max-depth)
                       )))))))
  

;; Rewrite logic
(define (rewrite-with-terminals grammar rhs [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (_rewrite-with-terminals (reduce-production grammar) rhs depth max-depth))

(define (_rewrite-with-terminals grammar-hash rhs [depth INITIAL-DEPTH] [max-depth MAX-DEPTH])
  (define new-depth (+ depth 1))
  (if (> depth max-depth) ∅
      (match rhs
        [(NT x) (_rewrite-with-terminals grammar-hash (hash-ref grammar-hash x ∅) new-depth max-depth)]
        [(Seq l r) (let ([left (_rewrite-with-terminals grammar-hash l new-depth max-depth)] [right (_rewrite-with-terminals grammar-hash r new-depth max-depth)])
                     (seq left right))]
        [(Alt l r) (let ([left (_rewrite-with-terminals grammar-hash l new-depth max-depth)] [right (_rewrite-with-terminals grammar-hash r new-depth max-depth)])
                     (alt left right))]
        [_ rhs])))

; seed 1233142528
; A -> cA | bB
; B -> aaBbC | bcC
; C -> aBcB | cCAc | b
(define problematic-grammar
  (list (Production (NT 'A) (Alt (Seq (T 'c) (NT 'A)) (Seq (T 'b) (NT 'B))))
        (Production (NT 'B) (Alt (Seq (Seq (Seq (Seq (T 'a) (T 'a)) (NT 'B)) (T 'b)) (NT 'C)) (Seq (Seq (T 'b) (T 'c)) (NT 'C))))
        (Production (NT 'C) (Alt
                             (Seq (Seq (Seq (T 'a) (NT 'B)) (T 'c)) (NT 'B))
                             (Alt
                              (Seq (Seq (Seq (T 'c) (NT 'C)) (NT 'A)) (T 'c))
                              (T 'b))))))

(define g2
  (list
   (Production
    (NT 'S)
    (Alt
     (Seq (NT 'S) (Seq (NT 'B) (Seq (NT 'S) (Seq (NT 'A) (T 3)))))
     (Alt (Seq (NT 'S) (Seq (NT 'B) (Seq (NT 'A) (Seq (NT 'B) (T 3)))))
          (Seq (T 2) (T 3)))))
   (Production (NT 'A) (Alt (Seq (NT 'A) (Seq (T 3) (Seq (T 2) (Seq (NT 'B) (T 3)))))
                            (Seq (T 3) (Seq (T 3) (T 2)))))
   (Production (NT 'B) (Alt (Seq (NT 'B) (Seq (T 1) (T 1)))
                            (Seq (T 2) (Seq (T 3) (T 1)))))))

(define g-dummy
  (list
   (Production
    (NT 'S)
    (Alt (Seq (T 'a) (T 'b)) (Seq (T 'c) (T 'd))))))

(define g-dummy2
  (list
   (Production
    (NT 'S)
    (Alt (Alt (T 'a) (T 'b)) (Alt (T 'c) (T 'd))))))

(define g-dummy3
  (list
   (Production
    (NT 'S)
    (Alt (Seq (T 'a) (Alt (T 'b) (T' e))) (Seq (T 'c) (Alt (T 'd) (T' f)))))))

(define g-dummy4
  (list
   (Production
    (NT 'S)
    (Alt (Seq (T 'a) (NT 'S)) (T 'a)))))

(define g-dummy5
  (list
   (Production (NT 'S) (Alt (Seq (T 'a) (NT 'B)) (T 'a)))
   (Production (NT 'B) (Seq (T 'b) (NT 'S)))
   ))

; S -> A | B
; A -> a
; B -> a
(define g-dummy6
  (list
   (Production (NT 'S) (Alt (NT 'A) (NT 'B)))
   (Production (NT 'A) (Alt (Seq (T 'a) (NT 'A)) (T 'c)))
   (Production (NT 'B) (Seq (T 'a) (T 'c)))
   ))

; E -> T + E | T - E | T
; T -> F * T | F / T | F
; F -> 0 | ( E )
(define expr
  (list
   (Production (NT 'E) (Alt (Alt
                             (Seq (Seq (NT 'T) (T '+)) (NT 'E))
                             (Seq (Seq (NT 'T) (T '-)) (NT 'E)))
                            (NT 'T)))
   (Production (NT 'T) (Alt (Alt
                             (Seq (Seq (NT 'F) (T '*)) (NT 'T))
                             (Seq (Seq (NT 'F) (T '/)) (NT 'T)))
                            (NT 'F)))
   (Production (NT 'F) (Alt (T 0)
                            (Seq (Seq (T '\() (NT 'E)) (T '\)))))
   ))


; d(aS | a, a) = S | ε
#;(list (cons '(a a) (Alt (NT 'S) #t))
      (cons '(a a) (Alt (NT 'S) #t))
      
      (cons '(a a) (Alt (NT 'S) #t))
      (cons '(a a) (Alt (NT 'S) #t))
      
      '((a #t) . #f)
      '((a #t) . #f)
      )


;;;;;; PRINTING

(define (print-rhs rhs) ;; TODO: esse print aqui pode ser usado pra depurar o restante do projeto
  (cond ((rhs-empty? rhs) "ε")
        ((rhs-invalid? rhs) "∅")
        (else (match rhs
                [(Production l r) (string-append (print-rhs l) " -> " (print-rhs r))]
                [(Seq x1 (Alt x2 x3)) (string-append (print-rhs x1) "(" (print-rhs x2) " | " (print-rhs x3) ")")]
                [(Seq (Alt x1 x2) x3) (string-append "(" (print-rhs x1) " | " (print-rhs x2) ")" (print-rhs x3) )]
                [(Seq l r) (string-append (print-rhs l) (print-rhs r))]
                [(Alt l r) (string-append (print-rhs l) " | " (print-rhs r))]
                [(T x) (if (number? x) (number->string x) (symbol->string x))]
                [(NT x) (symbol->string x)]))))

(define (print-grammar list)
  (if (empty? list)
      "\n"
      (string-append (print-rhs (car list)) "\n" (print-grammar (cdr list)))))
  