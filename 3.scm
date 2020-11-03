(define (positive-number? n) (and (integer? (string->number n)) (< 0 (string->number n))))

(define (operator? c) (or (string=? "+" c) (string=? "-" c) (string=? "*" c) (string=? "/" c) (string=? "^" c)))

(define (different-types? str1 str2) (or (and (positive-number? str1) (operator? str2)) (and (positive-number? str2) (operator? str1))))

(define (tail str ind) (if (= 0 (string-length str)) str (substring str ind (string-length str))))
(define (head str) (if (= 0 (string-length str)) str (substring str 0 1)))

(define (reverse-string str)
  (define (loop str res)
    (if (string=? "" str) res (loop (tail str 1) (string-append (head str) res)))
  )
  (loop str "")
)
  
(define (precedence operator)
  (cond ((or (string=? "+" operator) (string=? "-" operator)) 0)
        ((or (string=? "*" operator) (string=? "/" operator)) 1)
        (else 2)))

(define (>> str)
  (define (loop str res)
    (cond ((string=? "" str) res)
          ((different-types? (head str) res) res)
          ((and (string=? " " (head str)) (or (positive-number? res) (operator? res))) res)
          ((string=? " " (head str)) (loop (tail str 1) res))
          (else (loop (tail str 1) (string-append res (head str))))
    )
  )
  (loop str "")
)

(define (>>len str)
  (define (loop str res len)
    (cond ((string=? "" str) len)
          ((different-types? (head str) res) len)
          ((and (string=? " " (head str)) (or (positive-number? res) (operator? res))) len)
          ((string=? " " (head str)) (loop (tail str 1) res (+ 1 len)))
          (else (loop (tail str 1) (string-append res (head str)) (+ 1 len)))
    )
  )
  (loop str "" 0)
)


(define (expr-valid? expr)
 (define (loop str numcount opcount)
   ;(display (string-append "str:" str "| >>str:" (>> str) "|\n"))
   (cond ((< numcount opcount) #f)
         ((< (+ 1 opcount) numcount) #f)
         ((string=? "" (>> str)) #t)
         ((operator? (>> str)) (loop (tail str (>>len str)) numcount (+ 1 opcount)))
         ((positive-number? (>> str)) (loop (tail str (>>len str)) (+ 1 numcount) opcount))
         (else #f)
   )
 )
  (loop expr 0 0)
)

(define (pop str) (tail (reverse-string str) (>>len (reverse-string str))))
(define (top str) (reverse-string (>> (reverse-string str))))
(define (push str el delim) (string-append str delim el))

(define (expr-rp expr)
  (define (form-expr out op)
    ;(display (string-append "out:" out "| op:" op "| " ">>op:" (>> op) "|\n"))
    (if (string=? (>> op) "")
        (tail out 1) ; remove empty space at the beggining
        (form-expr (push out (top op) "") (pop op))
    )
  )
  
  (define (loop str out op)
    ;(display (string-append "str:" str "|out:" out "|op:" op "|\n" "|>>str:" (>> str) "|\n"))
    (cond ((string=? "" (>> str)) (form-expr out op))
          ((positive-number? (>> str)) (loop  (tail str (>>len str)) (push out (>> str) ",") op))
          ((and (operator? (>> str)) (string=? "" op)) (loop (tail str (>>len str)) out (push op (>> str) " "))) ; empty op stack, just add operator
          ((and (operator? (>> str)) (< (precedence (top op)) (precedence (>> str)))) (loop (tail str (>>len str)) out (push op (>> str) " "))) ; operator in stack has < priority than the currently read, push the new operator as well (TODO: CHECK EQUAL PRIORITY | LEFT)
          ((and (operator? (>> str)) (>= (precedence (top op)) (precedence (>> str))))
           ((lambda ()
              (if (>= (precedence (top (pop op))) (precedence (>> str)))
                  (loop (tail str (>>len str)) (push (push out (top op) "") (top (pop op)) "") (push (pop (pop op)) (>> str) " "))
                  (loop (tail str (>>len str)) (push out (top op)) (push (pop op) (>> str) " ") "")
              )))) ; high priority-> pop and then push the new op
    )
  )
  (if (expr-valid? expr)
      (loop expr "" "")
      #f
  )
)



;(expr-rp "")
;(expr-rp "  5;  ")
;(positive-number? (>>  "   10    "))

;(expr-valid? "   10    ")

;(expr-valid? "10   * 20")
;(expr-valid? "10 20 + 5+")
;(expr-valid? "++++ 5")
;(expr-valid? "+++")
;(expr-valid? "")
;(expr-valid? "-5")
;(expr-valid? "-5 + 2 * 2 2")
;(expr-valid? "3")
;(expr-valid? "  10 + 5       * 2")
;(expr-valid? "10+5*2")
;(expr-valid? "    10   ")
;(expr-valid? "   ")

