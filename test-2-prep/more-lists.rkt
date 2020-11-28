#lang racket

(define (takeWhile p? xs)
  (cond ((p? (car xs)) (cons (car xs) (takeWhile p? (cdr xs))))
        ((or (null? xs) (not (p? (car xs)))) '())
  )
)

(takeWhile even? '(2 4 6 8 9 10 11 12))