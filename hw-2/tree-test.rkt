#lang racket/base
(require rackunit rackunit/gui racket/include)

(include "tree.rkt")

(test/gui
  (test-suite "read-token"
    (test-equal? "(read-token empty string)->empty string" (read-token "") "")
    (test-equal? "(read-token spaces)->empty string" (read-token "     ") "")
    (test-equal? "(read-token {)->{" (read-token "}") "}")
    (test-equal? "(read-token })->}" (read-token "{") "{")
    (test-false "a is invalid" (read-token "a"))
    (test-false "   _{ is invalid" (read-token "   _{"))
    (test-equal? "(read-token 1)->1" (read-token "1 ") "1")
    (test-equal? "(read-token  1 )->1" (read-token " 1 ") "1")
    (test-equal? "(read-token * 1 )->*" (read-token "* 1 ") "*")
    (test-equal? "(read-token     * 1 )->*" (read-token "     * 1 ") "*") 
    (test-equal? "(read-token 22*1)->22" (read-token "22*1 ") "22")
    (test-equal? "(read-token {12*})->{" (read-token "{12*} ") "{")
    (test-equal? "(read-token {{12*})->{" (read-token "{{12*} ") "{") 
    (test-equal? "(read-token { {12*} )->{" (read-token " { {12*} ") "{")
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

  (test-suite "balanced?"
    (test-false "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}} is not balanced"
                (balanced? (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
    
    (test-true "'() is balanced"
               (balanced? '()))
    
    (test-false "{111 {2 {2 {2 {2 * *} *} *} {6 * *}} *} is not balanced"
                (balanced? (string->tree "{111 {2 {2 {2 {2 * *} *} *} {6 * *}} *}")))
    
    (test-false "{8 {4 {3 **} {2 * {1 * *}}} {5 **}} is not balanced"
                (balanced? (string->tree "{8 {4 {3 **} {2 * {1 * *}}} {5 **}}")))
    
    (test-true "{3**} is balanced"
                (balanced? (string->tree "{3**}")))

    (test-true "{3 {1 **} *} is balanced"
               (balanced? (string->tree "{3 {1 **} *}")))
    
    (test-false "{111 {2 {2 {2 {2 * *} *} *} {6 {3 {4 * *} *} *}} *} is not balanced"
                (balanced? (string->tree "{111 {2 {2 {2 {2 * *} *} *} {6 {3 {4 * *} *} *}} *}")))

    (test-false "{111 {2 {2 {2 {2 * *} *} *} {6 {3 {4 {1 {1 * *} *} *} *}} *} is invalid"
                (balanced? (string->tree "{111 {2 {2 {2 {2 * *} *} *} {6 {3 {4 {1 {1 * *} *} *} *}} *}")))
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

  (test-suite "tree->string"
     (test-equal? "->{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"
                  (tree->string (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
                   "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
     (test-equal? "empty tree->*"
                  (tree->string '()) "*")
     (test-equal? "{3 * *}" (tree->string (string->tree "{3**}")) "{3 * *}")
  )

  (test-suite "tree->stream"
     (test-equal? "" #t #t)
  )
)

