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
   (cond ((< numcount opcount) #f)
         ((< (+ 1 opcount) numcount) #f)
         ((string=? "" (read-token str)) #t)
         ((operator? (read-token str)) (loop (tail str (read-token-len str)) numcount (+ 1 opcount)))
         ((natural-number? (read-token str)) (loop (tail str (read-token-len str)) (+ 1 numcount) opcount))
         (else #f))
 )
  (loop expr 0 0)
)

(define (pop str) (reverse-string (tail (reverse-string str) (read-token-len (reverse-string str)))))
(define (top str) (reverse-string (read-token (reverse-string str))))
(define (push str el delim) (string-append str delim el))

(define (loop str out op delim_out delim_op delim_form)
  (define (form-expr out op) (if (string=? (read-token op) "") (tail out 1) (form-expr (push out (top op) delim_form) (pop op))))

  (define (loop1 str out op delim_out delim_op delim_form)
    (if (or (< (precedence (top op)) (precedence (read-token str))) (string=? "" (top op)))
    (loop (tail str (read-token-len str)) out (push op (read-token str) " ") delim_out delim_op delim_form)
    (loop1 str (push out (top op) " ") (pop op) delim_out delim_op delim_form))
  )

  (cond ((string=? "" (read-token str)) (form-expr out op))
        ((natural-number? (read-token str))
         (loop  (tail str (read-token-len str)) (push out (read-token str) delim_out) op delim_out delim_op delim_form))
        ((and (operator? (read-token str)) (string=? "" op)) ; empty op stack
         (loop (tail str (read-token-len str)) out (push op (read-token str) delim_op) delim_out delim_op delim_form)) 
        ((and (operator? (read-token str)) (< (precedence (top op)) (precedence (read-token str)))) ; operator in stack has < priority 
         (loop (tail str (read-token-len str)) out (push op (read-token str) delim_op) delim_out delim_op delim_form)) 
        ((and (operator? (read-token str)) (>= (precedence (top op)) (precedence (read-token str)))) ; higher priority
         (loop1 str out op delim_out delim_op delim_form)) 
   )
)


  
(define (expr-rp expr) (if (expr-valid? expr) (loop expr "" "" "," " " " ") #f))

(define (expr-eval expr)
  (define (eval-op operation operand1 operand2)
  (cond ((string=? "*" operation) (* operand1 operand2))
        ((string=? "+" operation) (+ operand1 operand2))
        ((string=? "-" operation) (- operand1 operand2))
        ((string=? "/" operation) (quotient operand1 operand2))
        ((string=? "^" operation) (expt operand1 operand2))))
  
  (define (func str out)
    (cond ((and (string=? "" out) (string=? "" (read-token str))) 0)
          ((string=? "" (read-token str)) (string->number (read-token out)))
          ((natural-number? (read-token str)) (func (tail str (read-token-len str)) (push out (read-token str) " ")))
          ((operator? (read-token str))
           (func (tail str (read-token-len str)) (push (pop (pop out)) (number->string (eval-op (read-token str) (string->number (top (pop out))) (string->number (top out)))) " ")))
    )
  )
  (if (expr-valid? expr) (func (loop expr "" "" " " " " " ") "") #f)
)

(expr-valid? "   10    ")

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
(expr-eval "10+5*8/5^2+4/5-8")
(expr-eval "10+20*30+5*12^4/2^2+2^10")
