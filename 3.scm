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

(define (pop str) (reverse-string (tail (reverse-string str) (>>len (reverse-string str)))))
(define (top str) (reverse-string (>> (reverse-string str))))
(define (push str el delim) (string-append str delim el))

;bug to fix: 

(define (loop str out op delim_out delim_op delim_form)
    
  (define (form-expr out op)
    (display (string-append "form-expr||out:" out "| op:" op "| " ">>op:" (>> op) "|\n"))
    (if (string=? (>> op) "")
        (tail out 1) ; remove empty space at the beggining
        (form-expr (push out (top op) delim_form) (pop op))
    )
  )

  (define (loop1 str out op delim_out delim_op delim_form)
    (display (string-append "loop1||| str:" str "|out:" out "|op:" op "|\n"))
    (if (or (< (precedence (top op)) (precedence (>> str))) (string=? "" (top op)))
    (loop (tail str (>>len str)) out (push op (>> str) " ") delim_out delim_op delim_form)
    (loop1 str (push out (top op) "") (pop op) delim_out delim_op delim_form))
  )

  
    (display (string-append "str:" str "|out:" out "|op:" op "|\n" "|>>str:" (>> str) "|\n"))
    (cond ((string=? "" (>> str)) (form-expr out op))
          ((positive-number? (>> str)) (loop  (tail str (>>len str)) (push out (>> str) delim_out) op delim_out delim_op delim_form))
          ((and (operator? (>> str)) (string=? "" op)) (loop (tail str (>>len str)) out (push op (>> str) delim_op) delim_out delim_op delim_form)) ; empty op stack, just add operator
          ((and (operator? (>> str)) (< (precedence (top op)) (precedence (>> str)))) (loop (tail str (>>len str)) out (push op (>> str) delim_op) delim_out delim_op delim_form)) ; operator in stack has < priority than the currently read, push the new operator as well (TODO: CHECK EQUAL PRIORITY | LEFT)
          ((and (operator? (>> str)) (>= (precedence (top op)) (precedence (>> str))))
           (loop1 str out op delim_out delim_op delim_form)) ; high priority-> pop and then push the new op
    )
  )


  
(define (expr-rp expr)
  (if (expr-valid? expr)
      (loop expr "" "" "," " " "")
      #f
  )
)

;(loop "10   * 20+5" "" "" " " " ")

(define (eval-op operation operand1 operand2)
  (cond ((string=? "*" operation) (* operand1 operand2))
        ((string=? "+" operation) (+ operand1 operand2))
        ((string=? "-" operation) (- operand1 operand2))
        ((string=? "/" operation) (/ operand1 operand2)) ; quotation
        ((string=? "^" operation) (expt operand1 operand2))
  )
)

(define (expr-eval expr)
  (define (func str out sum)
    ;(display (string-append "str:" str "|out:" out "|sum:")) (display sum) (display "|\n")
    (cond ((and (string=? "" out) (string=? "" (>> str))) 0)
          ((string=? "" (>> str)) (string->number (>> out)))
          ((positive-number? (>> str)) (func (tail str (>>len str)) (push out (>> str) " ") sum))
          ((operator? (>> str)) (func (tail str (>>len str)) (push (pop (pop out)) (number->string (+ sum (eval-op (>> str) (string->number (top (pop out))) (string->number (top out))))) " ") sum ))
    )
  )
  (if (expr-valid? expr)
      (func (loop expr "" "" " " " " " ") "" 0)
      #f
  )
)


;(expr-rp "")
;(expr-rp "  5;  ")
;(positive-number? (>>  "   10    "))

;(expr-valid? "   10    ")

;(expr-eval "10   * 20")
;(expr-eval "10* 20 + 5")
;(expr-valid? "++++ 5")
;(expr-valid? "+++")
;(expr-valid? "")
;(expr-valid? "-5")
;(expr-valid? "-5 + 2 * 2 2")
;(expr-eval "3")
;(expr-eval "10 + 5* 2")
;(expr-rp " 10*10+   10 * 5^ 2")
;(expr-eval "10/5*2")
;(expr-eval "    10   ")
;(expr-eval "   ")

(expr-eval "10+5*8/5^2+4/5-8")
;(expr-rp "10+20*30/2^2+2^10")
