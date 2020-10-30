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
  (define (price set) ; refactor
    (define (loop s result ind)
      (if (>= 0 s)
          result
          (loop (quotient s 2) (+ result (* (modulo s 2) (p ind))) (+ 1 ind))
      )
    )
    (loop set 0 0)
  )

  (define (weight set) ; refactor
    (define (loop s result ind)
       (if (>= 0 s)
          result
          (loop (quotient s 2) (+ result (* (modulo s 2) (w ind))) (+ 1 ind))
      )
    )
    (loop set 0 0)
  )

  (define (calc_max_price max_price curr_set capacity)
    (if (and (> (price curr_set) max_price) (< (weight curr_set) capacity))
        (price curr_set)
        max_price
    )
  )

  (define (max_price_set curr_set max_set capacity)
    (if (and (> (price curr_set) (price max_set)) (< (weight curr_set) capacity))
        curr_set
        max_set
    )
  )
 
  (define (loop current_set max_price max_set)
    (display current_set)
    (display ": ")
    (display max_price)
    (display ": ")
    (display max_set)
    (display "\n")
    (if (< current_set (- (expt 2 n) 1)) ; i < 2^n - 1 всички възможности
        (loop (+ 1 current_set) (calc_max_price max_price current_set c) (max_price_set current_set max_set c))
        max_set
    )
  )

  (loop 0 0 0)
)

(knapsack 8 3 w p)
