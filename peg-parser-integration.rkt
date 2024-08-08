#lang racket

(require rackcheck)

(require "./util/structs.rkt")
(require "./test/ll-1.rkt")
(require "./test/greibach.rkt")
(require "main.rkt")

(require (prefix-in LL-1-Gen: "../ll-1-grammar-generator/glc-gen.rkt"))
 
(require "../peg-parser/peg-simple-recognizer.rkt")
(require (prefix-in Peg: "../peg-parser/peg-ast.rkt"))

(define NOOP_LOG (Peg:SrcLoc 1 1))
(define PEG_GRAMMAR_NAME "grammar")

(define GRAMMAR_TESTS 10)

(define (convert-to-peg ll-1-grammar)
  (Peg:PEG PEG_GRAMMAR_NAME (convert-productions-to-peg ll-1-grammar) (set-peg-starter ll-1-grammar)))

(define (convert-productions-to-peg productions)
  (apply hash (append* (map reduce-production-ruleset productions))))

(define (reduce-production-ruleset production)
  (match production
    [(Production (NT l) r) (list (symbol->string l) (reduce-production-ruleset r))]
    [(Seq l r) (Peg:Cat NOOP_LOG (reduce-production-ruleset l) (reduce-production-ruleset r))]
    [(Alt l r) (Peg:Alt NOOP_LOG (reduce-production-ruleset l) (reduce-production-ruleset r))]
    [(NT x) (Peg:Var NOOP_LOG #f (symbol->string x))]
    [(T x) (Peg:Sym NOOP_LOG (string-ref (symbol->string x) 0))]
    [#t (Peg:Eps NOOP_LOG)]
    ))

(define (set-peg-starter ll-1-grammar)
  (match (car ll-1-grammar)
    [(Production (NT x) _) (Peg:Annot NOOP_LOG 'Flat (Peg:Var NOOP_LOG #f (symbol->string x)))]
    ))

(define (parse ll-1-grammar s)
  (peg-parse (convert-to-peg ll-1-grammar) (open-input-string s)))

(define (accept-word? ll-1-grammar s)
  (define result (parse ll-1-grammar s))
  (match result
    ; TODO: Trocar pra PTFail?
    [(PTStr _) #t]
    [_ #f]))

(define (symbol-list->string list)
  (list->string
   (map
    (lambda (symbol)
      (string-ref (symbol->string symbol) 0))
    list)))

(define (print-rhs rhs) ;; TODO: esse print aqui pode ser usado pra depurar o restante do projeto
  (cond ((eq? rhs #t) "ε")
        ((eq? rhs #f) "∅")
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

(define (test-generated-grammar-as-peg number-of-iterations)
  (property #:name "test-gen-grammar" ([grammar (LL-1-Gen:gen:naive-ll1-ruleset '(C B A) '(c b a) '(C B A) '())])
            (and (and (is-ll-1? grammar) ; Se eh LL-1
                      (is-greibach-normal-form? grammar)) ; Se passa em Greibach
                 (let ([words (map symbol-list->string (generate-words grammar number-of-iterations))])
                   (andmap (lambda (word) (accept-word? grammar word)) words)))))

(define (test-generated-grammar-as-peg2 number-of-iterations)
  (property #:name "test-grammar-accept-word" ([grammar (LL-1-Gen:gen:naive-ll1-ruleset '(C B A) '(c b a) '(C B A) '())])
            (let ([words (generate-words grammar number-of-iterations)])
                   (displayln words)
                   (displayln (print-grammar grammar))
                   (andmap (lambda (word) (in-grammar? grammar word)) words))))


#;(check-property (make-config #:tests GRAMMAR_TESTS) (test-generated-grammar-as-peg 10))
(check-property (make-config #:tests GRAMMAR_TESTS) (test-generated-grammar-as-peg2 6))

(provide test-generated-grammar-as-peg)