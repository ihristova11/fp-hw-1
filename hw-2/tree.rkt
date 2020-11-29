#lang racket/base
 ; move the tests in the other file
(require rackunit)
(require rackunit/text-ui)

(define (natural? n) (and (integer? (string->number n)) (<= 0 (string->number n)))) ; considering 0 a natural number

(define (valid-symbol? ch)  
  (or (string=? ch "*") (natural? ch))
)

(define (different-tokens? t1 t2)
  (or (and (string=? t1 "*") (natural? t2)) (and (string=? t2 "*") (natural? t1))) ; should be different for >>
)


(define (head str) (if (= 0 (string-length str)) str (substring str 0 1)))
(define (tail str ind) (if (= 0 (string-length str)) str (substring str ind (string-length str))))



(define (>> str)
  (define (termination? ch) (or (string=? ch "") (string=? ch " ") (string=? "{" ch) (string=? "}" ch))) ; strings to terminate the read operation

  (define (loop str res)
    (let ((first (head str)))
    (cond ((not (or (valid-symbol? first) (termination? first) (string=? "" str))) #f) ; invalid symbol 'a' for example -> return false
          ((or (termination? first) (not (and (natural? first) (natural? res))) (string=? "" str)) res) ; return result if string is termination or the head and the res are different types
          (else (loop (tail str 1) (string-append res first)))
    ))
  )
  (loop str "")
)

(define (tree? str)
 (cond ((not (>> str)) #f) ; invalid input
     ;  (()) ; 
 )
)

(define (string->tree str)
  #f
)

; tests for >>
(define tests>>
  (test-suite ">>"
    (check-equal? (>> "") "")
    (check-equal? (>> " ") "")
    (check-equal? (>> "}") "")
    (check-equal? (>> "{") "")
    (check-false (>> "a"))
    (check-equal? (>> "1 ") "1")
  )
)

(run-tests tests>> 'verbose)