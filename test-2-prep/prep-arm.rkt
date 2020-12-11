#lang racket/base
;Задачи за дървета за подготовка на контролно 2

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root empty-tree empty-tree))

;Напишете функция, която получава две дървета и проверява дали са еднакви.
(define (equal-trees? t1 t2)
  (cond ((and (null? t1) (null? t2)) #t)
        ((or (null? t1) (null? t2)) #f)
        ((equal? (root-tree t1) (root-tree t2)) (and (equal-trees? (left-tree t1) (left-tree t2))
                                                     (equal-trees? (right-tree t1) (right-tree t2))))
        (else #f)
  )
)

(equal-trees? (make-tree 3 empty-tree empty-tree)
              (make-tree 3 empty-tree empty-tree))
;Напишете функция, която получава дърво и го променя, като заменя всеки от елементите му със
;сумата от стойностите на всички елементи в под-дървото с този корен.

(define bst (make-tree 8
                               (make-tree 3 (make-leaf 1) (make-tree 6 (make-leaf 4) (make-leaf 7)))
                               (make-tree 10 empty-tree (make-tree 14 (make-leaf 13) empty-tree))))

(define (sum-tree t)
  (if (null? t)
      0
      (+ (root-tree t) (sum-tree (left-tree t)) (sum-tree (right-tree t))))
)

(define (update-tree t)
  (if (null? t) empty-tree
       (make-tree (list (- (sum-tree t) (root-tree t)))
                  (update-tree (left-tree t))                      
                  (update-tree (right-tree t))))
  )

(update-tree bst)

(define (tree-level n tree)
  (cond ((null? tree) '())
        ((< n 1) '())
        ((= n 1) (list (root-tree tree)))
        (else (append (tree-level (- n 1) (left-tree tree))
                      (tree-level (- n 1) (right-tree tree)))))
  )

(tree-level 2 bst)

;; bst selectors
(define (bst-min t)
  (cond ((null? t) #f)
        ((null? (left-tree t)) (root-tree t))
        (else (bst-min (left-tree t)))
  )
)

(bst-min bst)

;(define g '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (3 . 4)))

(define g '((a b c d e m)
            (b a)
            (c a d f)
            (d a c e f)
            (e a d)
            (f c d)
            (h m)
            (m h a)))

(define (succ g node)
  (cdar (filter (lambda (el) (equal? (car el) node)) g))
)
(succ g 'h)
;Напишете функция, която проверява дали в дадено дърво има път от корена до листо с определена сума на елементите.



;Напишете функция, която проверява дали в дадено дърво има слой (ниво) с определена сума на елементите.
;Напишете функция, която по подадено дърво и две стойности на елементи намира най-ниско разположения елемент, сред чиито наследници се срещат тези две стойности.
;Напишете функция, която по подаден списък от елементи построява идеално балансирано двоично дърво за търсене.

;Напишете функция (bst? tree), която проверява дали tree е двоично дърво за търсене.
;Напишете функции, които проверяват дали дадено двоично дърво е балансирано по височина и съответно по тегло.
;5. Имплементирайте функция, която генерира хистограма за списък. Тя представлява списък от двойки, които указват за кой елемент на началния списък колко са срещанията му.
;Пример:
;(hist '(1 2 3 1 2 3 4 5 1 2)) -> ((1 . 3) (2 . 3) (3 . 2) (4 . 1) (5 . 1))