#lang racket

(require racket/trace racket/vector)

(require "./util/constants.rkt")
(require "./util/predicates.rkt")
(require "./util/reducers.rkt")
(require "./util/structs.rkt")
(require "./delta.rkt")

(define : vector-ref)
(define := vector-set!)

(define (can-parse? word grammar starts)
  (car (cyk word grammar starts)))

; CYK Parser
(define (cyk input grammar starts)
  (cond
    ((list? input)(cyk-sent input grammar starts))
    (else '(ERROR: Unsupported input format. Use lists or strings))))

(define (cyk-sent s g starts)
  (call/cc
   (λ(K)
     (define terminals '())
     (define non-terminals '())
     (define sent (list->vector s))
     (define N (vector-length sent))
     (define M (length g))
     (define rules (make-hash))
     (let ((j 1))
       (dict-for-each g(λ(k v)(hash-set! rules k j)(set! j (+ j 1)))))
     (define P (make-vector (+ N 1)))
     (for ([i (in-range 1 (+ N 1))])
       (:= P i (make-vector (+ N 1)))
       (for ([j (in-range 1 (+ N 1))])
         (:= (: P i) j (make-vector (+ M 1) #f))))
     (for ([i (in-range 1 (+ N 1))])
       (let ((a (: sent (- i 1))))
         (dict-for-each g(λ(k v)(when(eq? v a)
                                  (begin(:=(:(: P i)1)(hash-ref rules k) #t)
                                        (set! terminals (cons (list k '-> v) terminals))))))))
     (set! terminals (reverse terminals))
     (when(< (length terminals)N)(K '(ERROR: Input contains word not given in grammar)))
     (for ([length (in-range 2 (+ N 1))])
       (for ([start (in-range 1 (+(- N length)2))])
         (for ([len1 (in-range 1 length)])
           (let ((len2 (- length len1)))
             (dict-for-each g(λ(k v)(when(pair? v)
                                      (when(and (:(:(: P start)len1)(hash-ref rules(car v)))
                                                (:(:(: P (+ start len1))len2)(hash-ref rules(cadr v))))
                                        (begin (:=(:(: P start)length)(hash-ref rules k) #t)
                                               (set! non-terminals (cons (list k '-> v) non-terminals)))))))
          ))))
     (when(< (length non-terminals)(- N 1))(K (cons #f (cons non-terminals terminals))))
     (define result #f)
     (for ([i (vector-length starts)])
       (for ([j (in-range 1 (+ N 1))])
         (when(eq?(:(:(: P 1)j)(hash-ref rules(: starts i)))#t)(set! result #t))))
     (cons result (cons non-terminals terminals)))))


; S -> B D
; B -> C b | ε
; C -> c
; D -> d | ε


(define g1 (list
 (Production (NT 'S) (Seq (Seq (NT 'B) (NT 'D)) (T 'e)))
 (Production (NT 'B) (Alt (Seq (NT 'C) (T 'b)) ε))
 (Production (NT 'C) (Alt (T 'c) (NT 'D)))
 (Production (NT 'D) (Alt (T 'd) ε))
 ))

;; 0) Remover regras ε
(define (has-ε? rhs nullable-set)
  (match rhs
    [(Alt l r) (or (has-ε? l nullable-set) (has-ε? r nullable-set))]
    [(Seq l r) (and (has-ε? l nullable-set) (has-ε? r nullable-set))]
    [(T x) #f]
    [(NT x) (not (eq? (member x nullable-set) #f))]
    [ε #t]
    [_ #f] ;; TODO: aqui vai ser sempre #t, refatorar as constantes.
  ))

(define (nullable-prods grammar [initial-nullable-set '()]) ; TODO: repensar o nome
  (match grammar
    [(cons (Production (NT l) r) xs) (if (has-ε? r initial-nullable-set)
                                         (cons l (nullable-prods xs initial-nullable-set))
                                         (nullable-prods xs initial-nullable-set))]
    ['() '()]
  ))

(define (nullable-NTs grammar [initial-nullable-set '()])
  (define nullable-set (nullable-prods grammar initial-nullable-set))
  (if (> (length nullable-set) (length initial-nullable-set))
      (nullable-NTs grammar nullable-set)
      nullable-set))

(define (symplify-ε rhs)
  (match rhs
    [(Seq #t (Alt x1 x2)) (alt x1 x2)]
    [(Seq x1 (Alt #t x2)) (alt x1 (seq x1 x2))]
    [(Seq x1 (Alt x2 #t)) (alt (seq x1 x2) x1)]

    [(Seq (Alt #t x1) x2) (alt x2 (seq x1 x2))]
    [(Seq (Alt x1 #t) x2) (alt (seq x1 x2) x2)]
    [(Seq (Alt x1 x2) #t) (alt x1 x2)]

    [(Seq (Alt x1 x2) x3) (cond
                            ((rhs-delta 'NOOP x2 #:ignore-NT #t) (alt (seq x1 x3) (symplify-ε (seq x2 x3))))
                            ((rhs-delta 'NOOP x1 #:ignore-NT #t) (alt (symplify-ε (seq x1 x3)) (seq x2 x3)))
                            (else (seq (alt x1 x2) x3))
                            )]
    [(Seq x1 (Alt x2 x3)) (cond
                            ((rhs-delta 'NOOP x3 #:ignore-NT #t) (alt (seq x1 x2) (symplify-ε (seq x1 x3))))
                            ((rhs-delta 'NOOP x2 #:ignore-NT #t) (alt (symplify-ε (seq x1 x2)) (seq x1 x3)))
                            (else (seq x1 (alt x2 x3)))
                            )]
    [x x]
  ))
  

(define (remove-ε grammar)
  (define nullable-set (reverse (nullable-NTs grammar)))
  (_remove-ε grammar nullable-set)
  )

(define (_remove-ε grammar nullable-set)
  (let ([evaluated-NT (car nullable-set)])
    (let ([rhs (find-named-rhs evaluated-NT grammar)])
      (if (empty? (cdr nullable-set))
          (replace-productions-to-NT grammar evaluated-NT rhs)
          (_remove-ε (replace-productions-to-NT grammar evaluated-NT rhs) (cdr nullable-set))
    ))))

(define (replace-productions-to-NT productions prod-name rhs) ; TODO: repensar o nome
  (match productions
    [(cons (Production (NT l) r) xs) (cons (Production (NT l) (replace-rhs-to-NT r prod-name rhs)) (replace-productions-to-NT xs prod-name rhs))]
    ['() '()]
  ))

(define (replace-rhs-to-NT production prod-name rhs)
  (match production
    [(Seq l r) (symplify-ε (seq (replace-rhs-to-NT l prod-name rhs) (replace-rhs-to-NT r prod-name rhs)))]
    [(Alt l r) (alt (replace-rhs-to-NT l prod-name rhs) (replace-rhs-to-NT r prod-name rhs))]
    [(NT x) (if (eq? x prod-name) rhs (NT x))]
    [x x]
  ))

(define (find-named-rhs prod-name grammar)
  (define production (car grammar))
  (if (is-named-prod? prod-name production)
      (get-rhs production)
      (find-named-rhs prod-name (cdr grammar))))

(define (is-named-prod? prod-name production)
  (match production
    [(Production (NT x) _) (eq? prod-name x)]
    [_ #f]))

(define (get-rhs production)
  (match production
    [(Production (NT _) rhs) rhs]
    [_ #f]))



;; 1) Converter pra CNF.
;; 2) Converter pra interface do cyk
;; 3) Rodar o teste.

;; Test
#;(define g2 '((S . (S A))
             (S . b)
             (A . a)))

#;(define starts1 #(S))
#;(can-parse? '(b a a a) g2 starts1)

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