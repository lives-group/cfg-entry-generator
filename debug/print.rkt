#lang racket

(require "../util/constants.rkt")
(require "../util/predicates.rkt")
(require "../util/reducers.rkt")
(require "../util/structs.rkt")
(require "../derivative.rkt")

(provide debug-derivative display-grm pprint-grm pprint-production pprint-lhs pprint-rhs pprint-gex)

(define (debug-derivative g symbol [override-rhs UNKNOWN])
  (display-grm g)
  (match
      (car g)
    [(Production (NT l) rhs)
     (define derivative-rhs (rhs-derivative (reduce-production g) (if (equal? override-rhs UNKNOWN) rhs override-rhs) symbol))
     (define derivative-text (pprint-rhs l derivative-rhs))
     (display (string-append "S' --> " derivative-text "\n"))
     derivative-rhs]
    [_ (display "Wrong grammar/production, please review.") ∅]))

(define (display-grm g)
  (display (pprint-grm g))
  )

(define (pprint-grm xs)
  (match xs
    ['() "\n"]
    [(cons (Production (NT s) e) zs) (pprint-production s e zs)])
  )

(define (pprint-production s e zs)
  (string-append (pprint-lhs s) " --> "
                 (pprint-gex (mk-ident (+ (string-length (symbol->string s)) 3)) 2 e)
                 "\n"
                 (pprint-grm zs)))

(define (pprint-lhs s)
  (symbol->string s))

(define (pprint-rhs s e)
  (pprint-gex (mk-ident (+ (string-length (symbol->string s)) 3)) 2 e))

(define (mk-ident n)
  (build-string n (lambda (n) #\ ))
  )

(define (get-parens-number e)
  (match e [(Seq _ _) 2] [(Alt _ _) 1] [_ 1]))

(define (parens b s)
  (cond
    [b    (string-append "(" s ")")]
    [else s]))

(define (pprint-gex ident p e)
  (match e
    [(Seq l r) (string-append (parens (> p (get-parens-number l)) (pprint-gex ident 1 l))
                              (parens (> p (get-parens-number r)) (pprint-gex ident 1 r)))]
    [(Alt l r) (string-append (parens (> p (get-parens-number l))  (pprint-gex ident 1 l))
                              "|"
                              (parens (> p (get-parens-number r)) (pprint-gex ident 1 r)))]
    [(NT s) (symbol->string s)]
    [(T s) (symbol->string s)]
    [(? boolean? e)   (if (rhs-empty? e) "ε" "∅")]
    [(? char? e)   (string e)]
    [(? symbol? e) (symbol->string e)]
    )
  )
