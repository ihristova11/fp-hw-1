(define (positive-number? n) (and (integer? (string->number n)) (< 0 (string->number n))))

(define (operator? c) (or (string=? "+" c) (string=? "-" c) (string=? "*" c) (string=? "/" c) (string=? "^" c)))

(define (different-types? str1 str2) (or (and (positive-number? str1) (operator? str2)) (and (positive-number? str2) (operator? str1))))

(define (tail str) (if (= 0 (string-length str)) str (substring str 1 (string-length str))))
(define (head str) (if (= 0 (string-length str)) str (substring str 0 1)))

(define (expr-valid? expr)
  (define (loop str res)
    (display (string-append "str:" str "\n"))
    (display (string-append ">> str:" (>> str) "\n"))
    (cond ((string=? "" str) #t)
          ((and (not (string=? "" (>> str))) (not (different-types? res (>> str)))) #f)
          ((string=? "" (>> str)) (loop (tail str) res))
          (else (loop (substring str (string-length (>> str)) (string-length str)) (>> str)))     
    )
   )
  (loop expr "")
)
  

;(expr-valid? "10   * 20")
;(expr-valid? "10 20 + 5")
;(expr-valid? "++++ 5")
;(expr-valid? "+++")
;(expr-valid? "")
;(expr-valid? "-5")
;(expr-valid? "-5 + 2 * 2 2")
;(expr-valid? "3")
;(expr-valid? "  10 + 5       * 2")
;(expr-valid? "10+5*2")
;(expr-valid? "    10   ")

(define (precedence operator)
  (cond ((or (string=? "+" operator) (string=? "-" operator)) 0)
        ((or (string=? "*" operator) (string=? "/" operator)) 1)
        (else 2)))



(define (>> str)
  (define (loop str res)
    ;(display (string-append "str:" str ";res:" res ";\n"))
    (cond ((string=? "" str) res)
          ((different-types? (head str) res) res)
          ((and (string=? " " (head str)) (or (positive-number? res) (operator? res))) res)    
          ((string=? " " (head str)) (loop (tail str) res))         
          (else (loop (tail str) (string-append res (head str))))
    )
  )
  (loop str "")
)



  (define (loop str res)
    (display (string-append "str:" str "\n"))
    (display (string-append (>> str) "\n"))
    ;(display  (string-length (>> str)))
    (cond ((string=? "" str) res)
          ((string=? (head str) " ") (loop (tail str) res))
          (else (loop (substring str (string-length (>> str)) (string-length str))
              (string-append res ";" (>> str))
              ))
        )
  )
    
  (loop " 10 90  * +20" "")
  ;(>> "80 90 - ")

  ;(substring? "10" " 10    +20")

;(read-token " 20")
;(different-types? (head "+ 20") "")
;(tail "+ 20")