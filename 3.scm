(define (positive-number? n) (and (integer? (string->number n)) (< 0 (string->number n))))

(define (operator? c) (or (string=? "+" c) (string=? "-" c) (string=? "*" c) (string=? "/" c) (string=? "^" c)))

(define (different-types? str1 str2) (or (and (or (string=? "" str1) (string=? "" str2))
                                              (or (positive-number? str1) (positive-number? str2) (operator? str1) (operator? str2)))
                                         (and (positive-number? str1) (operator? str2)) (and (positive-number? str2) (operator? str1))))

(define (tail str) (if (= 0 (string-length str)) str (substring str 1 (string-length str))))
(define (head str) (if (= 0 (string-length str)) str (substring str 0 1)))

(define (>> str)
  (define (loop str result)
    ;(display (string-append "head:" (head str) "\n"))
    ;(display (string-append "tail:" (tail str) "\n"))
    ;(display (string-append "str:" str "\n"))
    ;(display (string-append "result: " result "\n")) 
    (if (or (= 0 (string-length str)) (string=? " " (head str)))
        result
        (loop (tail str) (string-append result (head str)))
    )
  )
  (loop str "")
)

(define (expr-valid? expr)
  (define (loop str res)
    ;(display (string-append "str:" str "\n"))
    ;(display (string-append ">> str:" (>> str) "\n"))
    (cond ((string=? "" str) #t)
          ((and (not (string=? "" (>> str))) (not (different-types? res (>> str)))) #f)
          ((string=? "" (>> str)) (loop (tail str) res))
          (else (loop (substring str (string-length (>> str)) (string-length str)) (>> str)))     
    )
   )
  (loop expr "")
)
  

;(expr-valid? "10   + 20")
;(expr-valid? "10 20 + 5")
;(expr-valid? "++++ 5")
;(expr-valid? "+++")
;(expr-valid? "")
;(expr-valid? "-5")
;(expr-valid? "-5 + 2 * 2 2")
;(expr-valid? "3")

(define (expr-rp expr)
 #t
)

(define (expr-eval expr)
   (if (not (expr-valid? expr)) #f
      #t ; work
  )
)





(define (loop str res)
  (display (string-append res "\n"))
  ;(display (string-length (>> str)))
  (cond ((string=? "" str) res)
        ((string=? "" (>> str)) (loop (tail str) res))
        (else (loop (substring str (string-length (>> str)) (string-length str)) (>> str)))      
  )
)



;(loop "10   + 20" "")