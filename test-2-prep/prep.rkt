#lang racket/base

(define (map-mine op lst)
  (if (null? lst)
      '()
      (cons (op (car lst)) (map-mine op (cdr lst)))
  )
)

(define (filter-mine p? lst)
  (cond ((null? lst) '())
        ((p? (car lst)) (cons (car lst) (filter-mine p? (cdr lst))))
        (else (filter-mine p? (cdr lst)))
  )
)


(map-mine (lambda (x) (* x 2)) (list 1 2 3 4))

(map + '(1 2 3) '(10 10 10))

(filter-mine even? '(1 2 3 4))