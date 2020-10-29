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

(define (knapsack c n w p)
  (define (sum_elements set index result) ; refactor
    (define (get_value index) (if (set-contains? set (expt 2 (+ 1 index))) (p index) 0))
    (if (< index n)
        (sum_elements set (+ 1 index) (+ result  (get_value index)))
        result
    )
  )

  (define (sum_weight set index result) ; refactor
    (define (get_value index) (if (set-contains? set (expt 2 (+ 1 index))) (w index) 0))
    (if (< index n)
        (sum_weight set (+ 1 index) (+ result (get_value index)))
        result
    )
  )

  (define (find_max_price max_price current_set)
     (if (and (<= (sum_weight current_set 0 0) c) (> (sum_elements current_set 0 0) max_price))
                     (sum_elements current_set 0 0)
                     max_price))

  (define (find_max_price_set max_price current_set)
    (if (and (<= (sum_weight current_set 0 0) c) (> (sum_elements current_set 0 0) max_price)) current_set))
  
  (define (loop current_set max_price max_price_set)
    (if (< current_set (- 1 (expt 2 n))) ; i < 2^n + 1 всички възможности
        (loop (+ 1 current_set) (find_max_price max_price current_set) (find_max_price_set max_price current_set))
        max_price_set
    )
  )

  (loop 0 0 0)
)


(knapsack 24 5 w p)
