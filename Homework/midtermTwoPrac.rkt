;(reverse '(1 2 3 4 5))

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

;(accumulate + 0 '(1 2 3))


(define (filter pred seq)
  (cond ((null? seq) seq)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

;(filter even? '(1 2 3 4 5 6 7 8))






(define (atom? x)
    (and (not (null? x)) (not (pair? x))))


(define (accumulat op init seq)
  (cond ((null? seq) init)
   (else (op (car seq) (accumulat op init (cdr seq))))))

;(accumulat + 1 '(1 2 3))

(define (filterr booexp seq)
  (cond ((null? seq) '())
        ((booexp (car seq)) (cons (car seq) (filterr booexp (cdr seq))))
        (else (filterr booexp (cdr seq)))))

;(filterr even? '(1 2 3 4 5 6 7))


(define (fringe tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))


(fringe '(1 2 3 (4 5 6 7 (8 9 10)) 11 12 13 (14 15 (16))))





(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 1 5)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))


(enumerate-tree '(1 2 3 4 (5 6 7 (9) ())))


(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((atom? tree) 1)
        (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))


(count-leaves '(1 2 3 4 (5 6 7 (9) ())))



(define (count-nodes tree)
  (if (atom? tree)
      1
      (+ 1 (accumulate + 0 (map (lambda (subtree) (count-nodes subtree)) tree)))))

(define (count-nodes tree)
  (if ((atom? tree) 1)
        (+ 1 (accumulate + 0 (map (lambda (subtree) (count-nodes subtree)) tree)))))


(count-nodes '(1 2 3 4 (5 6 7 (9) ())))

(count-nodes '(1 2 3 4 ()))


































