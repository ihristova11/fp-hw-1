(define dr "\u250C")
(define dl "\u2510")
(define ur "\u2514")
(define ul "\u2518")
(define h "\u2500")
(define v "\u2502")

(define (accumulate op term init a next b)
  (define (loop i result)
    (if (< i b)
        (loop (next i) (op result (term i)))
        result))
  (loop a init)
)

(define (top i b)
  (cond ((= i 0) dr)
      ((= i (- b 1)) dl)
      (else h)
   )
)

(define (bottom i b)
  (cond ((= i 0) ur)
      ((= i (- b 1)) ul)
      (else h)
   )
)


(define (append-symbols ch1 ch2 n)
  (accumulate string-append (lambda (i)(string-append ch1 ch2)) "" 0 (lambda (x)(+ 1 x)) n)
)

(define (draw-top-line n)
  (accumulate string-append (lambda (x)(top x (+ 1 (* 2 n)))) "" 0 (lambda (x) (+ 1 x)) n)
)

(define (draw-bottom-line n)
  (accumulate string-append bottom "" 0 (lambda (x) (+ 1 x)) n)
)

(define (draw-square n)
 (display (draw-top-line n))
 (display (draw-bottom-line n))
)

(define (draw-top n)
  (define (loop i)
    (if (< i n)
       (begin (display (string-append (append-symbols v " " i)
                                      (draw-top-line (+ (* 2 (- n i)) 1))
                                      (append-symbols " " v i) "\n"))
              (loop (+ 1 i))
       )
    )
  )

  (loop 0)
)

(define (draw-bottom n)
  (define (loop i)
    (if (< i n)
       (begin (display (string-append (append-symbols v " " (- n i)) (draw-bottom-line (+ (* 4 i) 1)) (append-symbols " " v (- n i)) "\n"))
              (loop (+ 1 i))
       )
    )
  )

  (loop 0)
)




(define (squares n)
  (draw-top n)
  ;(draw-bottom n)
)

(squares 3)

(display "*")

;(draw-square 5)

; todo: space to be a defined symbol
;(draw-line "")
; 2*n+1 // символи от ъгъл до ъгъл, n - номер на квадрата от вътре навън

;┌─────────┐
;│ ┌─────┐ │
;│ │ ┌─┐ │ │
;│ │ └─┘ │ │
;│ └─────┘ │
;└─────────┘
















