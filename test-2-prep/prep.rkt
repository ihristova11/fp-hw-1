#lang racket/base

(define (map-mine op lst)
  (if (null? lst)
      '()
      (cons (op (car lst)) (map-mine op (cdr lst)))
  )
)

(define (filter-mine p? lst)
  (cond ((null? lst) '())
        ((p? (car lst)) (cons (car lst) (filter-mine p? (cdr lst))))
        (else (filter-mine p? (cdr lst)))
  )
)


(map-mine (lambda (x) (* x 2)) (list 1 2 3 4))

(map + '(1 2 3) '(10 10 10))

(filter-mine even? '(1 2 3 4))

; define flatten

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; (list root empty-tree empty-tree)

(define root-tree car) ; check if car exists
(define left-tree cadr)
(define right-tree caddr) ; check?

(define (tree? t)
  (cond ((null? t) #t)
        ((and
          (list? t)
          (= (length t) 3)
          (tree? (right-tree t))
          (tree? (left-tree t))) #t)
        (else #f)
  )
)

; example
(define a (make-tree 3 (make-tree 4 empty-tree empty-tree) (make-tree 5 empty-tree empty-tree)))
(tree? a)
(tree? '(1 2 3))

(define example (make-tree 1
                           (make-tree 3 (make-leaf 8) empty-tree)
                           (make-tree 7 empty-tree (make-tree 9
                                                              (make-leaf 10)
                                                              (make-leaf 11)))))

(define example-bst (make-tree 8
                               (make-tree 3 (make-leaf 1) (make-tree 6 (make-leaf 4) (make-leaf 7)))
                               (make-tree 10 empty-tree (make-tree 14 (make-leaf 13) empty-tree))))

example
example-bst

(define empty-tree? null?)

; искаме да проверим дали нещо се среща в дърво
; във всяка от задачите започваме с проверка за празно дърво
; елемент се среща в дърво, тогава когато:
; - или е корена на дървото
; - или се среща в лявото, или дясното поддърво (среща се = member-tree?)
(define (member-tree? m t) ; assuming the tree is valid
  (cond ((empty-tree? t) #f)
        ((equal? (root-tree t) m) #t)
         (else
          (or
           (member-tree? m (left-tree t))
           (member-tree? m (right-tree t))))
  )
)

(member-tree? 4 example-bst)

(define (sum-tree t) ; assuming the tree is valid
  (if (empty-tree? t)
      0
      (+ (root-tree t) (sum-tree (left-tree t)) (sum-tree (right-tree t)))
  )
)

(sum-tree (list 1 '() '()))

; искаме да намерим всички елементи на дадено ниво в дървото
; тук имахме две дъна:
; - отново гледаме за празно дърво
; - проверяваме дали сме поискали ниво 0 - тогава е окей да върнем корена на дървото ни (след като знаем, че не е празно)
; - иначе комбинираме (с append) резултатите за лявото и дясното поддърво, но с по-малко ниво
(define (tree-level n t)
  (cond ((empty-tree? t) '())
        ((= n 0) (list (root-tree t)))
        (else (append (tree-level (- n 1) (left-tree t)) (tree-level (- n 1) (right-tree t))))
  )
)

(tree-level 1 example-bst)

; искаме да приложим функцията f върху всички елементи на дървото (като истинската map, ама за дървета)
(define (map-tree f t)
  (if (empty-tree? t)
      empty-tree
      (make-tree (f (root-tree t))
                 (map-tree f (left-tree t))
                 (map-tree f (right-tree t)))
  )
)

(map-tree (lambda (x) (+ 1 x)) example-bst)

; искаме да върнем списък от елементите на дървото - ляво, корен, дясно
(define (inorder-tree t)
  (cond ((empty-tree? t) empty-tree)
        ((not (tree? t)) (list t))
        (else (append
               (inorder-tree (left-tree t))
               (inorder-tree (root-tree t))
               (inorder-tree (right-tree t))))
  )
)

(define (tree->list tree)
  (cond ((null? tree) '())
        (else (append (tree->list (left-tree tree))
                      (list (root-tree tree))
                      (tree->list (right-tree tree)))))
)

(tree->list example-bst)
(inorder-tree example-bst)

; искаме да проверим дали х се среща в двоичното наредено дърво tree
; тук правим итеративен процес (опашкова рекурсия)
(define (member-bst? x t)
    (cond ((empty-tree? t) #f)
        ((equal? x (root-tree t)) #t)
        ((< x (root-tree t)) (member-bst? x (left-tree t)))
        ((> x (root-tree t)) (member-bst? x (right-tree t)))
  )
)

(member-bst? 64 example-bst)

(define (leaf? t)
  (and (= (length t) 3)
       (list? t)
       (empty-tree? (left-tree t))
       (empty-tree? (right-tree t)))
)

; искаме да вкараме елемент в двоично наредено дърво (binary search tree - BST)
(define (insert-bst x t)
  (cond ((empty-tree? t) (make-leaf x))
        ((< x (root-tree t)) (insert-bst x (left-tree t)))
        (else (insert-bst x (right-tree t)))
  )
)

(insert-bst 21 example-bst)

(define (bst-insert x tree)
  (cond ((empty-tree? tree) (make-leaf x))
	((> x (root-tree tree)) (bst-insert x (right-tree tree)))
	(else (bst-insert x (left-tree tree))))
)

(bst-insert 21 (list 15 '() '()))

(define example-list '(1 5 4 6 2 8 7))

; искаме да сортираме даден списък, използвайки tree->list и bst-insert
(define (sort xs)
  (tree->list (foldr bst-insert empty-tree xs))
)