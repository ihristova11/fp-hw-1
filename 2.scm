(define (set-empty? set) (= set 0))

(define (set-contains? set elem)
 (modulo (quotient set (expt 2 elem)) 2))

(define (set-add set elem)
  (if (not (set-contains? elem)) (+ set (expt 2 elem))))

(define (set-remove set elem)
  (if (set-contains? elem) (- set (expt 2 elem))))

; set-size
; set-intersect
; set-union
; set-difference

(define (w i) (+ 1 i))
(define (p i) (* 2 i))

(define (++ i) (+ 1 i))

(define (knapsack c n w p)
  (define (sum_elements set i result) ; refactor
    (if (< i n)
        (sum_elements (++ i) (+ result (lambda () ((if (set-contains? set (expt 2 (++ i))) (p i) 0)))))
        result
    )
  )

  (define (sum_weight set i result) ; refactor
    (if (< i n)
        (sum_elements (++ i) (+ result (lambda () ((if (set-contains? set (expt 2 (++ i))) (w i) 0)))))
        result
    )
  )
  
  (define (loop i max_price max_price_set)
    (if (< current_set (+ 1 (expt 2 n))) ; i < 2^n + 1 всички възможности
        (loop (++ current_set)
              (lambda (max_price) ; update max_price
                ((if (and (<= (sum_weight current_set 0 0) c) (> (sum_elements current_set 0 0) max_price))
                     (sum_elements current_set 0 0)
                     max_price)))
               (lambda (max_price current_price) ; update max_price
                ((if (and (<= (sum_weight current_set 0 0) c) (> (sum_elements current_set 0 0) max_price)) current_set)))
        )
        max_price_set
    )
  )

  (loop 0 0 0)
)

; (knapsack 10 3 w p)
