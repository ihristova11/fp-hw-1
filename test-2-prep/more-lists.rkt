#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (takeWhile p? xs)
  (cond ((null? xs) '())
        ((p? (car xs)) (cons (car xs) (takeWhile p? (cdr xs))))
        (else '())
  )
)

; приема f функция и xs списък, връща списък с f върху елементите
(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))
  )
)


(define tests
  (test-suite "take while"
    (check-equal? (takeWhile even? '(2 4 6 8 9 10 11 12)) '(2 4 6 8))
    (check-equal? (takeWhile even? '(1 2 4 6 8 9 10 11 12)) '())
    (check-equal? (takeWhile even? '()) '())
  )
)

(run-tests tests 'verbose)