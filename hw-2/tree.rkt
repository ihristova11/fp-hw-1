#lang racket/base

 ; move the tests in the other file
(require rackunit)
(require rackunit/text-ui)


(define (natural? n) (and (integer? (string->number n)) (<= 0 (string->number n)))) ; considering 0 a natural number

(define (valid-symbol? ch)  
  (or (string=? ch "*") (natural? ch)))

(define (type ch)
  (cond ((natural? ch) 0)
        ((string=? ch "*") 1)
        ((string=? ch "{") 2)
        ((string=? ch "}") 3)
        (else 4) ; invalid
  )
)

(define (type-equal? t1 t2)
  (= (type t1) (type t2))
)


(define (head str) (if (= 0 (string-length str)) str (substring str 0 1)))
(define (tail str ind) (if (= 0 (string-length str)) str (substring str ind (string-length str))))


(define (>> str)
  (define (termination? ch) (or (string=? ch "") (string=? ch " ") (string=? "{" ch) (string=? "}" ch))) ; strings to terminate the read operation

  (define (loop str res)
    (let ((first (head str)))
    (cond ((string=? "" str) res)
          ((not (or (valid-symbol? first) (termination? first))) #f) ; invalid symbol 'a' for example -> return false
          ((and (not (string=? "" res)) (not (type-equal? first res))) res) ; diff types
          ((and (type-equal? first res) (natural? first)) (loop (tail str 1) (string-append res first))) ; return result if string is termination or the head and the res are different types
          ((and (not (= 4 (type first))) (string=? res "")) (loop (tail str 1) (string-append res first)))
          (else (loop (tail str 1) res))
    ))
  )
  (loop str "")
)

(define (>>len str)
  (define (termination? ch) (or (string=? ch "") (string=? ch " ") (string=? "{" ch) (string=? "}" ch))) ; strings to terminate the read operation

  (define (loop str res)
    (let ((first (head str)))
    (cond ((string=? "" str) res)
          ((not (or (valid-symbol? first) (termination? first))) #f) ; invalid symbol 'a' for example -> return false
          ((and (not (string=? "" res)) (not (type-equal? first res))) res) ; diff types
          ((and (type-equal? first res) (natural? first)) (loop (tail str 1) (+ 1 res))) ; return result if string is termination or the head and the res are different types
          ((and (not (= 4 (type first))) (string=? res "")) (loop (tail str 1) (+ 1 res)))
          (else (loop (tail str 1) (+ 1 res)))
    ))
  )
  (loop str 0)
)

; "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"
; *

(define (tree? str)
  (define (update-stack s)
    (car "*" (cdr (cdr (cdr (cdr s)))))
  )
 (define (loop str stack)
   (let ((el (>> str)))
   (cond (not (>> str) #f)
         ((string=? "" str) stack)
         ((not (or (= 4 (type el) (= 3 (type el)))) (loop (tail str (>>len str)) (cons el stack)))) ; push to stack
         ((= 3 (type el)) (loop (tail str (>>len str)) (update-stack stack))) ; pop the last 4, push *
   )
     )
 )
  (loop str '())
)

(tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")

(define (string->tree str)
  #f
)

; tests for >>
(define tests>>
  (test-suite ">>"
    (check-equal? (>> "") "")
    (check-equal? (>> "     ") "")
    (check-equal? (>> "}") "}")
    (check-equal? (>> "{") "{")
    (check-false (>> "a"))
    (check-equal? (>> "1 ") "1")
    (check-equal? (>> " 1 ") "1")
    (check-equal? (>> "* 1 ") "*")
    (check-equal? (>> "     * 1 ") "*")
    (check-equal? (>> "22*1 ") "22")
    (check-equal? (>> "{12*} ") "{")
    (check-equal? (>> "{{12*} ") "{")
    (check-equal? (>> " { {12*} ") "{")
  )
)

(run-tests tests>> 'verbose)

(>> "22*1")