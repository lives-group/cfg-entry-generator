#lang racket

(struct Production (nt rhs) #:transparent)
(struct Alt (l r) #:transparent)
(struct Seq (l r) #:transparent)
(struct NT  (String) #:transparent)
(struct T  (String) #:transparent)

(provide Alt Seq NT T Production)
