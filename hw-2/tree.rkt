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

  (define (loop str res len)
    (let ((first (head str)))
    (cond ((string=? "" str) len)
          ((not (or (valid-symbol? first) (termination? first))) #f) ; invalid symbol 'a' for example -> return false
          ((and (not (string=? "" res)) (not (type-equal? first res))) len) ; diff types
          ((and (type-equal? first res) (not (string=? "" res)) (not (natural? res))) len)
          ((and (type-equal? first res) (natural? first)) (loop (tail str 1) (string-append res first) (+ 1 len))) ; return result if string is termination or the head and the res are different types
          ((and (not (= 4 (type first))) (string=? res "")) (loop (tail str 1) (string-append res first) (+ 1 len)))
          (else (loop (tail str 1) res (+ 1 len)))
    ))
  )
  (loop str "" 0)
)

; "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"
; *


; refactor to use recursive check for each node between {} 
(define (tree? str)
  (define (update-stack s) 
    (cons "*" (cdr (cdr (cdr (cdr s)))))
  )
 (define (loop str stack)
   ;(display "s:") (display stack) (display "\n")
   ;(display "el:") (display (>> str)) (display "\n")
  ; (display ">>len:") (display (>>len str)) (display "\n")
   ;(display "str:") (display str) (display "\n") (display "\n") 
   (let ((el (>> str)))
   (cond ((not (>> str)) #f)
         ((string=? "" str) stack)
         ((and (> (length stack) 0) (= 1 (type el)) (string=? "{" (car stack))) #f)
         ((not (or (= 4 (type el)) (= 3 (type el)))) (loop (tail str (>>len str)) (cons el stack))) ; push to stack
         ((and (>= (length stack) 4) (= 3 (type el))) (loop (tail str (>>len str)) (update-stack stack))) ; pop the last 4, push *
         ((and (< (length stack) 4) (= 3 (type el))) #f) 
   )
     )
 )
  (let ((lst (loop str '())))
  (and lst (= 1 (length lst)) (string=? "*" (car lst))))
)

;(tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")

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
    (check-false (>> "   _{"))
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

; tests for >>
(define tests-tree?
  (test-suite "tree?"
    (check-true (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
    (check-true (tree? "{5**}"))
    (check-false (tree? "}"))
    (check-false (tree? "{5 5 5 5"))
    (check-false (tree? "{{5 5 *} {* 5 5}*}")) 
    (check-true (tree? "{2{4**}*}"))
    (check-true (tree? "{2 {4 * *} *}"))
    (check-true (tree? "{2     {4     * *}               *}"))
    (check-false (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}"))
  )
)


(run-tests tests>> 'verbose)
(run-tests tests-tree? 'verbose)