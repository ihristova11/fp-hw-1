#lang racket/base

(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst)) (map op (cdr lst)))
  )
)


(map (lambda (x) (* x 2)) (list 1 2 3 4))