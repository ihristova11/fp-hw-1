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

(define (accumulate-i op term init a next b)
  (define (loop i result)
    (if (< b i )
        (loop (next i) (op result (term i)))
        result))
  (loop a init))

(define (!! n)
  (accumulate ((lambda () (if (odd? n) 1 2))) n (lambda (x y) (* x y)) id (lambda (x) (+ x 2)) 1)
)

(define (fact n)
  (accumulate 1 n (lambda (x y) (* x y)) id 1+ 1)
)

(define (rev-num n)
  (accumulate-i (lambda (x y) (+ (* x 10) y)) (lambda (x) (remainder x 10)) 0 n (lambda (x) (quotient x 10)) 0))

(define (nchk n k)
  (/ (fact n) (* (fact k) (fact (- n k))))
)

(define (nchk* n k)
  (accumulate 0 (- k 1) * (lambda (i) (/ (- n i) (- k i))) 1+ 1)
)

(define (2^ n)
  (accumulate 1 n * (lambda (i) 2) 1+ 1)
)

; iterative
(define (filter-acc p? from to op term next acc)
  (cond ((> from to) acc)
        ((p? from) (filter-acc p? (next from) to op term next (op acc (term from))))
        (else (filter-acc p? (next from) to op term next acc))
  )
)

; recursive
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))



(fact 10)
(!! 4)
(nchk 3 2)
(2^ 3)

(define (!!* n)
  (filter-acc (if (even? n) even? odd?) 1 n * id 1+ 1)
)

(!!* 5)
;(rev-num 12345)