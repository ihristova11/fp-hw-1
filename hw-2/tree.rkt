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

; refactor to use recursive check for each node between {} 
(define (tree? str)
  
 (define (loop str stack)
   (define (update-stack s) 
    (cons "*" (cdr (cdr (cdr (cdr s)))))
  )
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

(define (string->tree str) ; check for the outer ()
   (define (loop str stack)
   (define (update-stack s)
     (define (construct-list count s lst)
       (if (= count 0) lst
           (construct-list (- count 1) (cdr s)
                           ((lambda ()
                              (cond ((and (not (list? (car s))) (string=? "*" (car s))) (cons '() lst))
                                  ((not (list? (car s))) (cons (string->number (car s)) lst))
                                  (else (cons (car s) lst))))))))
    (cons (construct-list 3 s '()) (cdr (cdr (cdr (cdr s)))))
  )
   ;(display "s:") (display stack) (display "\n")
   (let ((el (>> str))) ; todo: refactor, replication of code
   (cond ((not (>> str)) #f)
         ((string=? "" str) stack)
         ((and (> (length stack) 0) (= 1 (type el)) (and (not (list? (car stack))) (string=? "{" (car stack)))) #f)
         ((not (or (= 4 (type el)) (= 3 (type el)))) (loop (tail str (>>len str)) (cons el stack))) ; push to stack
         ((and (>= (length stack) 4) (= 3 (type el))) (loop (tail str (>>len str)) (update-stack stack))) ; pop the last 4, push *
         ((and (< (length stack) 4) (= 3 (type el))) #f)
   )
     )
 )
  (if (tree? str)
      (car (loop str '()))
      #f
  )
)

;(define (abs x) (if (< x 0) (* -1 x) x)) ; should be inner | delete

(define left-tree cadr)
(define right-tree caddr)
(define root car)
(define (height tree)
  (if (null? tree) 0 (+ 1 (max (height (right-tree tree)) (height (left-tree tree)))))) ; move in balanced? if needed

(define (balanced? tree)
    (cond ((null? tree) #t) 
          ((and (<= (abs (- (height (right-tree tree)) (height (left-tree tree)))) 1)
                (balanced? (right-tree tree))
                (balanced? (left-tree tree))) #t) ; -1? exists
          (else #f)))

(define (ordered? tree)
  (define (helper tree minValue maxValue)
    (cond ((and (not (null? tree)) (or (< (root tree) minValue) (>= (root tree) maxValue))) #f)
          ((and (not (null? tree)) (not (ordered? (left-tree tree)))) #f)
          ((and (not (null? tree)) (not (ordered? (right-tree tree)))) #f)
          (else #t)
    )
  )
  (helper tree -inf.0 +inf.0)
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

; tests for tree?
(define tests-tree?
  (test-suite "tree?"
    (check-true (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
    (check-true (tree? "{5**}"))
    (check-false (tree? "}"))
    (check-false (tree? "              { * * *}"))
    (check-false (tree? "{5 5 5 5"))
    (check-false (tree? "{{5 5 *} {* 5 5}*}")) 
    (check-true (tree? "{2{4**}*}"))
    (check-true (tree? "{2 {4 * *} *}"))
    (check-true (tree? "{2     {4     * *}               *}"))
    (check-false (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}"))
    (check-true (tree? "{5 * *}"))
  )
)


; tests for balanced?
(define tests-balanced? ; todo: write more, should we check if the tree is valid or not
  (test-suite "balanced?"
    (check-false (balanced? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
    (check-true (balanced? '()))
    (check-false (balanced? (string->tree "{111 {2 {2 {2 {2 * *} *} *} {6 * *}} *}")))
  )
)

(define tests-ordered?
  (test-suite "ordered?"
     (check-true (ordered? (string->tree "{8 {3 {1 * *} {6 {4 * *} {7 * *}}} {10 * {14 {13 * *} *}}}")))
     (check-true (ordered? (string->tree "{3 * *}")))
     (check-false (balanced? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
  )
)

(run-tests tests>> 'verbose)
(run-tests tests-tree? 'verbose)
(run-tests tests-balanced? 'verbose)
(run-tests tests-ordered? 'verbose)
