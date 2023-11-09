#lang racket

(require rackcheck)

; regex-derivative : regex regex-atom -> regex
(define (regex-derivative re c)
  (cond 
    ((regex-empty? re) regex-NULL)
    ((regex-null? re)  regex-NULL)
    ((eq? c re)        regex-BLANK)
    ((regex-atom? re)  regex-NULL)
    ((match-seq re     (lambda (re1 re2) 
                         (alt (seq (regex-derivative re1 c) re2)
                              (seq (regex-empty re1) (regex-derivative re2 c))))))
    ((match-alt re     (lambda (re1 re2)
                         (alt (regex-derivative re1 c) (regex-derivative re2 c)))))
    ((match-rep re     (lambda (pat)
                         (seq (regex-derivative pat c) (rep pat)))))
    (else regex-NULL)
    ))


; gen:regex-data : regex -> ? // Qual é a interface de saída?
(define (gen:regex-data re)
  (gen:_regex-data re))

; regex-match : regex list -> boolean 
(define (regex-match pattern data)
  (if (null? data)
      (regex-empty? (regex-empty pattern))
      (regex-match (regex-derivative pattern (car data)) (cdr data))))

; Exportação
(provide regex-match)
(provide regex-derivative)

;; Constantes.
(define regex-NULL #f) ; ∅
(define regex-BLANK #t) ; ε

;; Predicados.
(define (regex-alt? re)
  (and (pair? re) (eq? (car re) 'alt)))

(define (regex-seq? re)
  (and (pair? re) (eq? (car re) 'seq)))

(define (regex-rep? re)
  (and (pair? re) (eq? (car re) 'rep)))

(define (regex-null? re)
  (eq? re regex-NULL))

(define (regex-empty? re)
  (eq? re regex-BLANK))

(define (regex-atom? re)
  (or (char? re) (symbol? re)))

;; Funções lambda.
(define (match-seq re lambda)
  (and (regex-seq? re)
       (lambda (cadr re) (caddr re))))

(define (match-alt re lambda)
  (and (regex-alt? re)
       (lambda (cadr re) (caddr re))))

(define (match-rep re lambda)
  (and (regex-rep? re)
       (lambda (cadr re))))


;; Simplificadores de predicados.
(define (seq re1 re2)
  (cond
    ((regex-null? re1) regex-NULL)
    ((regex-null? re2) regex-NULL)
    ((regex-empty? re1) re2)
    ((regex-empty? re2) re1)
    (else (list 'seq re1 re2))))
     
(define (alt re1 re2)
  (cond
    ((regex-null? re1) re2)
    ((regex-null? re2) re1)
    (else (list 'alt re1 re2))))

(define (rep pat)
  (cond
    ((regex-null? pat) regex-BLANK)
    ((regex-empty? pat) regex-BLANK)
    (else (list 'rep pat))))

(define (regex-empty re)
  (cond
    ((regex-empty? re) regex-BLANK)
    ((regex-null? re) regex-NULL)
    ((regex-atom? re) regex-NULL)
    ((match-seq re (lambda (re1 re2)
                     (seq (regex-empty re1) (regex-empty re2)))))
    ((match-alt re (lambda (re1 re2)
                     (alt (regex-empty re1) (regex-empty re2)))))
    ((regex-rep? re) regex-BLANK)
    (else regex-NULL)))

;; Gerador de entradas
(define (gen:_regex-data re)
  (cond
    ((regex-empty? re) gen:nothing) 
    ((regex-atom? re) (gen:const re))
    ((match-seq re (lambda (re1 re2)
                     (gen:let ([result (gen:tuple (gen:_regex-data re1) (gen:_regex-data re2))])
                              (flatten result)))))
    ((match-alt re (lambda (re1 re2)
                     (gen:choice (gen:_regex-data re1) (gen:_regex-data re2)))))
    ((match-rep re (lambda (pat)
                     (gen:let ([head (gen:choice gen:nothing (gen:_regex-data pat))]
                               [tail (gen:choice gen:nothing (gen:_regex-data (rep pat)))])
                              (flatten (list head tail))))))
    (else (error "Wrong regular expression format."))
  ))

;; Utils
(define gen:nothing (gen:const null))

