(define (set-empty? set) (= set 0))

(define (set-contains? set elem)
 (modulo (quotient set (expt 2 elem)) 2))

(define (set-add set elem)
  (if (not (set-contains? elem)) (+ set (expt 2 elem))))

(define (set-remove set elem)
  (if (set-contains? elem) (- set (expt 2 elem))))

(define (set-intersect s1 s2) ; may refactor
  (define (loop set1 set2 result index)
    (if (or (>= 0 set1) (>= 0 set2))
        result
        (if (and (= 1 (modulo set2 2)) (= 1 (modulo set1 2)))
            (loop (quotient set1 2) (quotient set2 2) (+ result (expt 2 index)) (+ 1 index))
            (loop (quotient set1 2) (quotient set2 2) result (+ 1 index))
        )
    )
  )

  (loop s1 s2 0 0)
)

(define (set-union s1 s2) ; may refactor
  (define (loop set1 set2 result index)
    (if (and (>= 0 set1) (>= 0 set2))
        result
        (if (or (= 1 (modulo set2 2)) (= 1 (modulo set1 2)))
            (loop (quotient set1 2) (quotient set2 2) (+ result (expt 2 index)) (+ 1 index))
            (loop (quotient set1 2) (quotient set2 2) result (+ 1 index))
        )
    )
  )

  (loop s1 s2 0 0)
)

(define (set-size set)
  (define (loop s counter)
    (if (>= 0 s)
        counter
        (loop (quotient s 2) (+ counter (modulo s 2)))))
  (loop set 0)
)

(define (set-difference set1 set2) ; refactor
  (define (loop s1 s2 result index)
    (if (and (>= 0 s1) (>= 0 s2))
        result
        (if (and (= 1 (modulo s1 2)) (= 0 (modulo s2 2)))
            (loop (quotient s1 2) (quotient s2 2) (+ result (expt 2 index)) (+ 1 index))
            (loop (quotient s1 2) (quotient s2 2) result (+ 1 index))
        )
    )
  )
  (loop set1 set2 0 0)
)

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


;(knapsack 24 5 w p)
