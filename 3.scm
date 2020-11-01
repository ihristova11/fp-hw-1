(define (positive-number? n) (and (integer? n) (< 0 n)))

;(define (operator? c) (or (string=? c '+') (string=? c '-') (string=? c '*') (string=? c '/') (string=? c '^')))

(define (expr-valid? expr)
  #t
 ; (cond ((= "" expr) #t)
 ;       ((positive-number? expr) #t)
 ;       (else #t)) ; work
        
)

(define (expr-rp expr)
  (if (not (expr-valid? expr)) #f
      #t ; work
  )
)

(define (expr-eval expr)
   (if (not (expr-valid? expr)) #f
      #t ; work
  )
)

(define (tail str) (substring str 1 (string-length str)))
(define (head str) (substring str 0 1))

(define (>> str)
  (define (loop str result)
    (if (string=? " " (head str))
        result
        (loop (tail str) (string-append result (head str)))
    )
  )
  (if (not (expr-valid? str)) #f (loop str ""))
)

(define (loop str res)
  (display str)
  (cond ((= 0 (string-length str)) res)
        ((string=? "" (>> str)) (loop (tail str) res))
        (else (loop (substring str (string-length (>> str)) (string-length str)) (string-append res (>> str) ";")))      
  )
)

;(define (string-trim expr)
  
 ; (define (loop expr result lastnumber)
  ;  (cond ((= expr "") result)
   ;       ((and lastnumber (positive-number? (head expr))) (loop (tail expr) result #t))
    ;      ((and lastspace (not (= " " (head expr)))) )
     ;     (else (loop (tail expr) (string-append result (head expr)) #t))
       
 ; )
;)

(loop "in the wood" "")

;(string-trim "  in the end  ")(string-trim "  in the end  ")          =>  "in the end"