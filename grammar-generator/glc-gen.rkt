#lang racket

(require rackcheck)
(require "../util/structs.rkt")
(require "glc-gen-utils.rkt")
(provide (all-defined-out))

(define (map-T Set)
  (map (lambda (item) (match item [(T x) (T x)] [x (T x)])) Set))
(define (map-NT Set)
  (map (lambda (item) (match item [(NT x) (NT x)] [x (NT x)])) Set))

(define (pprint-gex ident p e)
     (match e
         [(Seq l r) (string-append (parens (> p 1) (pprint-gex ident 1 l))
                                   (parens (> p 1) (pprint-gex ident 1 r)))]
         [(Alt l r) (string-append (parens (> p 2)  (pprint-gex ident 1 l))
                                   "\n"
                                   ident
                                   "| "
                                   (parens (> p 2) (pprint-gex ident 1 r)))]
        [(NT s) (symbol->string s)]
        [(T s) (symbol->string s)]
        [(? char? e)   (string e)]
        [(? symbol? e) (symbol->string e)]
   )
)

(define (pprint-grm xs)
    (match xs
      ['() "\n"]  
      [(cons (Production (NT s) e) zs) (string-append (symbol->string s) " --> "
                                                  (pprint-gex (mk-ident (+ (string-length (symbol->string s)) 3)) 0 e)
                                                  "\n"
                                                  (pprint-grm zs))])
  )

(define (display-grm g)
     (display (pprint-grm g))
  )

(define (mk-ident n)
    (build-string n (lambda (n) #\ )) 
  )

(define (parens b s)
    (cond
      [b    (string-append "(" s ")")]
      [else s]))

#;(define (pprin-grm xs)
    (match xs
      [(cond (Production s e) zs) (string-append s "-->" (pprint-gex (mk-ident (+ (length s) 3)) p e))])

  )

(define (shalow-first seq)
    (match seq
      [(Seq e d) (shalow-first e)]
      [(NT s)    (error "Shalow first: the input RHS must be in Greibach Normal Form")]
      [(Alt e d) (error "Shalow first: the input must not contain alternatives")]
      [x         x]
      )
  )

(define (gen:naive-ll1-seq Σ V [max-len 5])
        (gen:let ([s (gen:one-of Σ)]
                  [l (gen:list (gen:one-of (append Σ V)) #:max-length max-len )])
                (gen:const (foldr (lambda (x r) (Seq r x)) s l)))
  )

(define (gen:naive-ll1-alt Σ V [max-alts 5] [max-len 5])
  (cond
    [(<= (length Σ) 0) (error "gen:naive-ll1-alt can not produce any viable terms with empty alphabet")]
    [(eq? (length Σ) 1) (gen:const (car Σ)) #;(gen:naive-ll1-seq Σ V max-len)]
    [else (gen:let ([t (gen:naive-ll1-seq Σ V max-len)]
                    [p (gen:integer-in 0 100)]
                    [c (gen:one-of (remove (shalow-first t) Σ))]
                    )
                   (cond
                     [(or (> p 80) (<= max-alts 0)) (gen:const (Alt t c) #;t)]
                     [else (gen:bind (gen:naive-ll1-alt (remove (shalow-first t) Σ)
                                                        V
                                                        (- max-alts 1)
                                                        max-len)
                                     (lambda (ts) (gen:const (Alt t ts)))) ] ))]
    )
  )




(define (gen:naive-ll1-ruleset raw-V1 raw-Σ raw-V accRules [max-alts 5] [max-len 5])
  (let ([V1 (map-NT raw-V1)]
        [Σ (map-T raw-Σ)]
        [V (map-NT raw-V)])
    (cond
          [(null? V1) (gen:const accRules)]
          [else       (gen:let ([rhs (gen:naive-ll1-alt Σ V max-alts max-len)])
                               (gen:naive-ll1-ruleset (cdr V1)
                                                      Σ
                                                      V
                                                      (cons (Production (car V1) rhs) accRules)
                                                      max-alts
                                                      max-len))]
       )
  ))