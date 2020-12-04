#lang racket/base
(require rackunit rackunit/gui racket/include)
(include "tree.rkt")

; tests for >>
(test/gui
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


; tests for tree?
;(define tests-tree?
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


; tests for balanced?
;(define tests-balanced? ; todo: write more, should we check if the tree is valid or not
  (test-suite "balanced?"
    (check-false (balanced? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
    (check-true (balanced? '()))
    (check-false (balanced? (string->tree "{111 {2 {2 {2 {2 * *} *} *} {6 * *}} *}")))
  )


;(define tests-ordered?
  (test-suite "ordered?"
     (check-true (ordered? (string->tree "{8 {3 {1 * *} {6 {4 * *} {7 * *}}} {10 * {14 {13 * *} *}}}")))
     (check-true (ordered? (string->tree "{3 * *}")))
     (check-false (balanced? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
  )
;)

;(define tests-tree->string ; todo: add more, would be better not to depend on other functions
  (test-suite "tree->string"
     (check-equal? (tree->string (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
                   "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
  )
)

