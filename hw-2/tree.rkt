(require racket/stream)

(define (natural? n) (and (integer? (string->number n)) (<= 0 (string->number n)))) ; considering 0 a natural number

(define (valid-symbol? ch)  
  (or (string=? ch "*") (natural? ch)))

(define (type ch)
  (cond ((natural? ch) 0)
        ((string=? ch "*") 1)
        ((string=? ch "{") 2)
        ((string=? ch "}") 3)
        (else 4) ; invalid
  )
)

(define (type-equal? t1 t2)
  (= (type t1) (type t2))
)


(define (head str) (if (= 0 (string-length str)) str (substring str 0 1)))
(define (tail str ind) (if (= 0 (string-length str)) str (substring str ind (string-length str))))


(define (>> str)
  (define (termination? ch) (or (string=? ch "") (string=? ch " ") (string=? "{" ch) (string=? "}" ch))) ; strings to terminate the read operation

  (define (loop str res)
    (let ((first (head str)))
    (cond ((string=? "" str) res)
          ((not (or (valid-symbol? first) (termination? first))) #f) ; invalid symbol 'a' for example -> return false
          ((and (not (string=? "" res)) (not (type-equal? first res))) res) ; diff types
          ((and (type-equal? first res) (natural? first)) (loop (tail str 1) (string-append res first))) ; return result if string is termination or the head and the res are different types
          ((and (not (= 4 (type first))) (string=? res "")) (loop (tail str 1) (string-append res first)))
          (else (loop (tail str 1) res))
    ))
  )
  (loop str "")
)

(define (>>len str)
  (define (termination? ch) (or (string=? ch "") (string=? ch " ") (string=? "{" ch) (string=? "}" ch))) ; strings to terminate the read operation

  (define (loop str res len)
    (let ((first (head str)))
    (cond ((string=? "" str) len)
          ((not (or (valid-symbol? first) (termination? first))) #f) ; invalid symbol 'a' for example -> return false
          ((and (not (string=? "" res)) (not (type-equal? first res))) len) ; diff types
          ((and (type-equal? first res) (not (string=? "" res)) (not (natural? res))) len)
          ((and (type-equal? first res) (natural? first)) (loop (tail str 1) (string-append res first) (+ 1 len))) ; return result if string is termination or the head and the res are different types
          ((and (not (= 4 (type first))) (string=? res "")) (loop (tail str 1) (string-append res first) (+ 1 len)))
          (else (loop (tail str 1) res (+ 1 len)))
    ))
  )
  (loop str "" 0)
)


(define (tree? str)
  
 (define (loop str stack)
   (define (update-stack s) 
    (cons "*" (cdr (cdr (cdr (cdr s)))))
  )
   (let ((el (>> str)))
   (cond ((not (>> str)) #f)
         ((string=? "" str) stack)
         ((and (> (length stack) 0) (= 1 (type el)) (string=? "{" (car stack))) #f)
         ((not (or (= 4 (type el)) (= 3 (type el)))) (loop (tail str (>>len str)) (cons el stack))) ; push to stack
         ((and (>= (length stack) 4) (= 3 (type el))) (loop (tail str (>>len str)) (update-stack stack))) ; pop the last 4, push *
         ((and (< (length stack) 4) (= 3 (type el))) #f)
   )
     )
 )
  (let ((lst (loop str '())))
  (and lst (= 1 (length lst)) (string=? "*" (car lst))))
)

(define (string->tree str) 
   (define (loop str stack)
   (define (update-stack s)
     (define (construct-list count s lst)
       (if (= count 0) lst
           (construct-list (- count 1) (cdr s)
                           ((lambda ()
                              (cond ((and (not (list? (car s))) (string=? "*" (car s))) (cons '() lst))
                                  ((not (list? (car s))) (cons (string->number (car s)) lst))
                                  (else (cons (car s) lst))))))))
    (cons (construct-list 3 s '()) (cdr (cdr (cdr (cdr s)))))
  )
  
   (let ((el (>> str))) ; todo: refactor, replication of code
   (cond ((not (>> str)) #f)
         ((string=? "" str) stack)
         ((and (> (length stack) 0) (= 1 (type el)) (and (not (list? (car stack))) (string=? "{" (car stack)))) #f)
         ((not (or (= 4 (type el)) (= 3 (type el)))) (loop (tail str (>>len str)) (cons el stack))) ; push to stack
         ((and (>= (length stack) 4) (= 3 (type el))) (loop (tail str (>>len str)) (update-stack stack))) ; pop the last 4, push *
         ((and (< (length stack) 4) (= 3 (type el))) #f)
   )
     )
 )
  (if (tree? str)
      (car (loop str '()))
      #f
  )
)

(define empty-tree? null?)
(define empty-tree '())
(define (number-length n)
  (define (loop n count)
    (if (> n 0)
        (loop (quotient n 10) (+ 1 count))
        count)
  )
  (loop n 0)
)

(define left-tree cadr)
(define right-tree caddr)
(define root car)
(define (height tree)
  (if (null? tree) 0 (+ 1 (max (height (right-tree tree)) (height (left-tree tree))))))

(define (create-sizes-tree tree)
  (define (get-visual-height tree)
    (if (empty-tree? tree) 0
        (caar tree))
  )

  (define (get-visual-width tree)
    (if (empty-tree? tree) 0
        (cdar tree))
  )
  
  (cond
    ((empty-tree? tree) empty-tree)
    (else
     (let*
         ((left (create-sizes-tree (left-tree tree)))
          (right (create-sizes-tree (right-tree tree)))
          (root-length (number-length (root tree)))
          (height-l (get-visual-height left))
          (height-r (get-visual-height right))
          (width-l (get-visual-width left))
          (width-r (get-visual-width right))
          (new-root (cons (+ height-r height-l 1) (+ 1 width-l width-r root-length))))
       (list new-root left right)))))

;(create-sizes-tree (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
(create-sizes-tree (string->tree "{10 {15 {2 * *} {7 * *}} {5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}}"))

;(width (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))

(define (balanced? tree)
    (cond ((null? tree) #t)
          ((and (<= (abs (- (height (right-tree tree)) (height (left-tree tree)))) 1)
                (balanced? (right-tree tree))
                (balanced? (left-tree tree))) #t) ; -1? exists
          (else #f)))

(define (ordered? tree)
  (define (helper tree minValue maxValue)
    (cond ((and (not (null? tree)) (or (< (root tree) minValue) (>= (root tree) maxValue))) #f)
          ((and (not (null? tree)) (not (ordered? (left-tree tree)))) #f)
          ((and (not (null? tree)) (not (ordered? (right-tree tree)))) #f)
          (else #t)
    )
  )
  (helper tree -inf.0 +inf.0)
)

(define (tree->string tree)
  (define (helper tree res)
    (if (null? tree)
        "*"
        (string-append res "{" (number->string (root tree)) " " (helper (left-tree tree) res) " " (helper (right-tree tree) res) "}")
    )
  )
  (helper tree "")
)

;(tree->string (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))


(define (tree->stream tree order)
(define (inorder tree)
  (cond ((null? tree) empty-stream)
        (else (stream-append (inorder (left-tree tree))
                             (stream (root tree))
                             (inorder (right-tree tree)))))
)

  (define (preorder tree)
    (cond ((null? tree) empty-stream)
        (else (stream-append (stream (root tree))
                             (preorder (left-tree tree))                     
                             (preorder (right-tree tree)))))
  )

  (define (postorder tree)
    (cond ((null? tree) empty-stream)
        (else (stream-append (postorder (left-tree tree))                     
                             (postorder (right-tree tree))
                             (stream (root tree)))))
  )
  
  (cond ((eq? order 'inorder) (inorder tree))
        ((eq? order 'postorder) (postorder tree))
        ((eq? order 'preorder) (preorder tree))
  )
)
