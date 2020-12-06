#lang racket/base
(require rackunit rackunit/gui racket/include)

(include "tree.rkt")

(test/gui
  (test-suite ">>"
    (check-equal? (>> "") "")
    (check-equal? (>> "     ") "")
    (check-equal? (>> "}") "}")
    (check-equal? (>> "{") "{")
    (test-false "a is invalid" (>> "a"))
    (test-false "   _{ is invalid" (>> "   _{"))
    (check-equal? (>> "1 ") "1")
    (check-equal? (>> " 1 ") "1")
    (check-equal? (>> "* 1 ") "*")
    (check-equal? (>> "     * 1 ") "*") 
    (check-equal? (>> "22*1 ") "22")
    (check-equal? (>> "{12*} ") "{")
    (check-equal? (>> "{{12*} ") "{")
    (check-equal? (>> " { {12*} ") "{")
  )


  (test-suite "tree?"
    (test-true "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}} is a tree"
               (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
    (test-true "{5**} is a tree"
               (tree? "{5**}"))
    (test-false "} is not a tree"
                (tree? "}"))
    (test-false "              { * * *} is not a tree"
                (tree? "              { * * *}"))
    (test-false "{5 5 5 5 is not a tree"
                (tree? "{5 5 5 5"))
    (test-false "{{5 5 *} {* 5 5}*} is not a tree"
                (tree? "{{5 5 *} {* 5 5}*}")) 
    (test-true "{2{4**}*} is not a tree"
               (tree? "{2{4**}*}"))
    (test-true "{2 {4 * *} *} is not a tree"
               (tree? "{2 {4 * *} *}"))
    (test-true "{2     {4     * *}               *} is not a tree"
               (tree? "{2     {4     * *}               *}"))
    (test-false "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}} is not a tree"
                (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}"))
    (test-true "{5 * *} is not a tree"
               (tree? "{5 * *}"))
  )


 ; todo: write more, should we check if the tree is valid or not
  (test-suite "balanced?"
    (test-false "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}} is not balanced"
                (balanced? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
    (test-true "'() is balanced"
               (balanced? '()))
    (test-false "{111 {2 {2 {2 {2 * *} *} *} {6 * *}} *} is balanced"
                (balanced? (string->tree "{111 {2 {2 {2 {2 * *} *} *} {6 * *}} *}")))
  )



  (test-suite "ordered?"
     (test-true "{8 {3 {1 * *} {6 {4 * *} {7 * *}}} {10 * {14 {13 * *} *}}} is ordered"
                (ordered? (string->tree "{8 {3 {1 * *} {6 {4 * *} {7 * *}}} {10 * {14 {13 * *} *}}}")))
     (test-true "{3 * *} is ordered"
                (ordered? (string->tree "{3 * *}")))
     (test-true "'() is ordered"
                (ordered? '()))
     (test-false "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}} is not ordered"
                 (ordered? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
  )

 ; todo: add more, would be better not to depend on other functions
  (test-suite "tree->string"
     (check-equal? (tree->string (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
                   "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
  )
)

