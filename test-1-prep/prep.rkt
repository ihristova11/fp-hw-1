;Да се напише функция (!! n), която по дадено естествено число n изчислява n!! - произведението на всички числа,
;по-малки или равни на n, със същата четност:

(define (id x) x)
(define (1+ x) (+ 1 x))

(define (accumulate from to op term next acc)
  (if (> from to)
      acc
      (accumulate (next from) to op term next (op acc (term from)))
  )
)

(define (!! n)
  (accumulate ((lambda () (if (odd? n) 1 2))) n (lambda (x y) (* x y)) id (lambda (x) (+ x 2)) 1)
)

; factorial
(define (fact n)
  (accumulate 1 n (lambda (x y) (* x y)) id 1+ 1)
)

; reverse number
(define (rev-num n)
  (accumulate-i (lambda (x y) (+ (* x 10) y)) (lambda (x) (remainder x 10)) 0 n (lambda (x) (quotient x 10)) 0))

; binomial
(define (nchk n k)
  (/ (fact n) (* (fact k) (fact (- n k))))
)

; binomial 
(define (nchk* n k)
  (accumulate 0 (- k 1) * (lambda (i) (/ (- n i) (- k i))) 1+ 1)
)

(define (2^ n)
  (accumulate 1 n * (lambda (i) 2) 1+ 1)
)

; recursive filter-accumulate
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))



;(fact 10)
;(!! 4)
;(nchk 3 2)
;(2^ 3)

; iterative filter-accumulate
(define (filter-acc p? from to op term next acc)
  (cond ((> from to) acc)
        ((p? from) (filter-acc p? (next from) to op term next (op acc (term from))))
        (else (filter-acc p? (next from) to op term next acc))
  )
)

(define (!!* n)
  (filter-acc (if (even? n) even? odd?) 1 n * id 1+ 1)
)

;(!!* 5)
;(rev-num 12345)

; Да се напише функция (divisors-sum n), която намира сумата на всички делители на естественото число n.
(define (divisors-sum n)
  (filter-acc (lambda (i) (= 0 (remainder n i))) 1 n + id 1+ 0)
)

;(divisors-sum 12)

; Да се напише функция (count p? a b), която проверява за колко измежду числата в целочисления интервал [a;b] е верен предиката p?
(define (count p? a b)
  (filter-acc p? a b + (lambda (i) 1) 1+ 0)
)

;(count even? 1 10)

; Да се напише функция (prime? n), която проверява дали дадено число е просто:

(define (prime? n)
  (= (divisors-sum n) (1+ n))
)

;(prime? 6)

(define (num-len n)
  (ceiling (log (1+ n) 10))
)

(define (narcisstic? n)
  (define (loop i res)
    (if (< 0 i)
        (loop (quotient i 10) (+ res (expt (remainder i 10) (num-len n))))
        res
    )
  )
  (= n (loop n 0))
)

;(num-len 990)

(narcisstic? 153)