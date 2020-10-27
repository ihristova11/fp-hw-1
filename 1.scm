(define dr "\u250C")
(define dl "\u2510")
(define ur "\u2514")
(define ul "\u2518")
(define h "\u2500")
(define v "\u2502")

(define (++ i) (+ 1 i))

(define (accumulate op term init a next b)
  (define (loop i result)
    (if (< i b)
        (loop (next i) (op result (term i)))
        result))
  (loop a init))

(define (position i n ch1 ch2)
  (cond ((= i 0) ch1)
      ((= i (- n 1)) ch2)
      (else h)))

(define (append-symbols ch1 ch2 n)
  (accumulate string-append (lambda (i)(string-append ch1 ch2)) "" 0 ++ n))

(define (append-position-line n func)
  (accumulate string-append func "" 0 ++ n))

(define (append-half position y z ch1 ch2)
  (string-append (append-symbols v " " y) (append-position-line z (lambda (x)(position x z ch1 ch2))) (append-symbols " " v y) "\n"))

(define (draw-position-half n append-half position term1 term2 ch1 ch2)
   (define (loop i)
    (if (< i n)
       (begin (display (append-half position (term1 i) (term2 i) ch1 ch2)) 
              (loop (++ i)))))
  (loop 0))

(define (squares n)
  (draw-position-half n append-half position (lambda (i) i) (lambda (i)(+ 3 (* 4 (- (- n i) 1)))) dr dl)
  (draw-position-half n append-half position (lambda (i)(- (- n i) 1)) (lambda (i)(+ 3 (* 4 i))) ur ul))

(squares 3)