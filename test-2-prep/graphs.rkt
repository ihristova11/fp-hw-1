#lang racket/base

; Граф за нас ще означава списък от двойки (ребра)
; например '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (5 . 4))

(define g '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (5 . 4)))

; търсим входна степен на даден връх - колко ребра влизат в него

(define (in-degree g v)
  (length (filter (lambda (x) (= (cdr x) v)) g))
)

(filter (lambda (x) (= (cdr x) 4)) g)
(in-degree g 4)

; търсим изходна степен на даден връх - колко ребра излизат от него
(define (out-degree g v)
  (length (filter (lambda (x) (= (car x) v)) g))
)

(out-degree g 2)

; искаме списък с всички върхове на g
