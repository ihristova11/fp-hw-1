; considering that 0 is a natural number
(define (natural-number? n) (and (integer? (string->number n)) (<= 0 (string->number n))))

(define (operator? c) (or (string=? "+" c) (string=? "-" c) (string=? "*" c) (string=? "/" c) (string=? "^" c)))

(define (different-types? str1 str2) (or (and (natural-number? str1) (operator? str2)) (and (natural-number? str2) (operator? str1))))

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

(define (read-token str)
  (define (loop str res)
    (cond ((or (string=? "" str)
               (different-types? (head str) res)
               (and (string=? " " (head str)) (or (natural-number? res) (operator? res)))
               (and (not (different-types? (head str) res)) (operator? (head str)) (not (string=? "" res)))) res)
          ((string=? " " (head str)) (loop (tail str 1) res))
          (else (loop (tail str 1) (string-append res (head str))))
    )
  )
  (loop str "")
)

(define (read-token-len str)
  (define (loop str res len)
    (cond ((or (string=? "" str)
               (different-types? (head str) res)
               (and (string=? " " (head str)) (or (natural-number? res) (operator? res)))
               (and (not (different-types? (head str) res)) (operator? (head str)) (not (string=? "" res)))) len)
          ((string=? " " (head str)) (loop (tail str 1) res (+ 1 len)))
          (else (loop (tail str 1) (string-append res (head str)) (+ 1 len)))
    )
  )
  (loop str "" 0)
)


(define (expr-valid? expr)
 (define (loop str numcount opcount)
   ;(display (string-append "str:" str "| >>str:" (read-token str) "|\n"))
   (cond ((< numcount opcount) #f)
         ((< (+ 1 opcount) numcount) #f)
         ((string=? "" (read-token str)) #t)
         ((operator? (read-token str)) (loop (tail str (read-token-len str)) numcount (+ 1 opcount)))
         ((natural-number? (read-token str)) (loop (tail str (read-token-len str)) (+ 1 numcount) opcount))
         (else #f)
   )
 )
  (loop expr 0 0)
)

(define (pop str) (reverse-string (tail (reverse-string str) (read-token-len (reverse-string str)))))
(define (top str) (reverse-string (read-token (reverse-string str))))
(define (push str el delim) (string-append str delim el))

;bug to fix: 

(define (loop str out op delim_out delim_op delim_form)
    
  (define (form-expr out op)
    ;(display (string-append "form-expr||out:" out "| op:" op "| " ">>op:" (read-token op) "|\n"))
    (if (string=? (read-token op) "")
        (tail out 1) ; remove empty space at the beggining
        (form-expr (push out (top op) delim_form) (pop op))
    )
  )

  (define (loop1 str out op delim_out delim_op delim_form)
    ;(display (string-append "loop1||| str:" str "|out:" out "|op:" op "|\n"))
    (if (or (< (precedence (top op)) (precedence (read-token str))) (string=? "" (top op)))
    (loop (tail str (read-token-len str)) out (push op (read-token str) " ") delim_out delim_op delim_form)
    (loop1 str (push out (top op) " ") (pop op) delim_out delim_op delim_form))
  )

  
    ;(display (string-append "str:" str "|out:" out "|op:" op "|\n" "|>>str:" (read-token str) "|\n"))
    (cond ((string=? "" (read-token str)) (form-expr out op))
          ((natural-number? (read-token str))
           (loop  (tail str (read-token-len str)) (push out (read-token str) delim_out) op delim_out delim_op delim_form))
          ((and (operator? (read-token str)) (string=? "" op))
           (loop (tail str (read-token-len str)) out (push op (read-token str) delim_op) delim_out delim_op delim_form)) ; empty op stack, just add operator
          ((and (operator? (read-token str)) (< (precedence (top op)) (precedence (read-token str))))
           (loop (tail str (read-token-len str)) out (push op (read-token str) delim_op) delim_out delim_op delim_form)) ; operator in stack has < priority than the currently read, push the new operator as well (TODO: CHECK EQUAL PRIORITY | LEFT)
          ((and (operator? (read-token str)) (>= (precedence (top op)) (precedence (read-token str))))
           (loop1 str out op delim_out delim_op delim_form)) ; high priority-> pop and then push the new op
    )
  )


  
(define (expr-rp expr) (if (expr-valid? expr) (loop expr "" "" "," " " " ") #f))

(define (eval-op operation operand1 operand2)
  (cond ((string=? "*" operation) (* operand1 operand2))
        ((string=? "+" operation) (+ operand1 operand2))
        ((string=? "-" operation) (- operand1 operand2))
        ((string=? "/" operation) (quotient operand1 operand2))
        ((string=? "^" operation) (expt operand1 operand2))))

(define (expr-eval expr)
  (define (func str out)
    ;(display (string-append "str:" str "|out:" out "|sum:")) (display sum) (display "|\n")
    (cond ((and (string=? "" out) (string=? "" (read-token str))) 0)
          ((string=? "" (read-token str)) (string->number (read-token out)))
          ((natural-number? (read-token str)) (func (tail str (read-token-len str)) (push out (read-token str) " ")))
          ((operator? (read-token str))
           (func (tail str (read-token-len str)) (push (pop (pop out)) (number->string (eval-op (read-token str) (string->number (top (pop out))) (string->number (top out)))) " ")))
    )
  )
  (if (expr-valid? expr) (func (loop expr "" "" " " " " " ") "") #f)
)

(expr-eval "")
(expr-rp "  5;  ")

;(expr-valid? "   10    ")

(expr-eval "10   * 20")
(expr-eval "10* 20 + 5")
(expr-eval "++++ 5")
;(expr-valid? "+++")
;(expr-valid? "")
;(expr-valid? "-5")
;(expr-valid? "-5 + 2 * 2 2")
;(expr-eval "3")
(expr-eval "10 + 5* 2")
;(expr-rp " 10*10+   10 * 5^ 2")
(expr-eval "10/5*2")
;(expr-eval "    10   ")
;(expr-eval "   ")


;(quotient 11 0)
;(expr-eval "0+4/5*5/0+2+4/5-8")
(expr-eval "10+20*30+5*12^4/2^2+2^10")
