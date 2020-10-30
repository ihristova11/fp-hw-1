(define (set-empty? set) (= set 0))

(define (set-contains? set elem)
 (modulo (quotient set (expt 2 elem)) 2))

(define (set-add set elem)
  (if (not (set-contains? elem)) (+ set (expt 2 elem))))

(define (set-remove set elem)
  (if (set-contains? elem) (- set (expt 2 elem))))

(define (set-loop s1 s2 result ind cond1 cond2 next)
  (cond ((cond1 s1 s2) result)
        ((and (not (cond1 s1 s2)) (cond2 s1 s2)) (set-loop (quotient s1 2) (quotient s2 2) (+ result (expt 2 ind)) (next ind) cond1 cond2 next))
        (else (set-loop (quotient s1 2) (quotient s2 2) result (next ind) cond1 cond2 next))))

(define (set-intersect s1 s2)
   (set-loop s1 s2 0 0 (lambda (s1 s2) (or (>= 0 s1) (>= 0 s2))) (lambda (s1 s2) (and (= 1 (modulo s2 2)) (= 1 (modulo s1 2)))) (lambda (i) (+ 1 i)))
)

(define (set-union s1 s2)
 (set-loop s1 s2 0 0 (lambda (s1 s2) (and (>= 0 s1) (>= 0 s2))) (lambda (s1 s2) (or (= 1 (modulo s2 2)) (= 1 (modulo s1 2)))) (lambda (i) (+ 1 i))))

(define (set-difference s1 s2)
  (set-loop s1 s2 0 0 (lambda (s1 s2) (and (>= 0 s1) (>= 0 s2))) (lambda (s1 s2) (and (= 1 (modulo s1 2)) (= 0 (modulo s2 2)))) (lambda (i) (+ 1 i))))

(define (set-size set)
  (define (loop s counter)
    (if (>= 0 s) counter (loop (quotient s 2) (+ counter (modulo s 2)))))
  (loop set 0))

(define (knapsack c n w p)
  (define (calc op s result ind)
    (if (>= 0 s) result (calc op (quotient s 2) (+ result (* (modulo s 2) (op ind))) (+ 1 ind))))
  
  (define (price set) (calc p set 0 0))

  (define (weight set) (calc w set 0 0))

  (define (calc_max_price max_price curr_set capacity)
    (if (and (> (price curr_set) max_price) (<= (weight curr_set) capacity)) (price curr_set) max_price))

  (define (max_price_set curr_set max_set capacity)
    (if (and (> (price curr_set) (price max_set)) (<= (weight curr_set) capacity)) curr_set max_set))
 
  (define (loop current_set max_price max_set)
    (if (< current_set (expt 2 n))
        (loop (+ 1 current_set) (calc_max_price max_price current_set c) (max_price_set current_set max_set c))
        (max_price_set current_set max_set c)))

  (loop 0 (w 0) (p 0))
)

; an example
; (knapsack 9 4 (lambda (i) (+ 1 i)) (lambda (i) (* 2 i))) ;-> 14