;make the function below:
; atom, filter, enumeration-interval, accumulation, fringe, count-nodes, count-leaves

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))
;test
;(atom? 'a)
;(atom? '(a b))
;(atom? '())


(define (filter prec lst)
  (cond ((null? lst) lst)
        ((prec (car lst)) (cons (car lst) (filter prec (cdr lst))))
        (else (filter prec (cdr lst)))))

;(filter even? '(1 2 3 4 5 6))


(define (enumeration-interval low high)
  (if (> low high)
      '()
      (cons low (enumeration-interval (+ 1 low) high))))

;(enumeration-interval 1 5)

(define (accumulation op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulation op init (cdr seq))))))
;(accumulation + 0 '(1 2 3))

(define (fringe tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        (else (append (fringe (car tree)) (fringe(cdr tree))))))
;(fringe '(1 2 3 4 5 (6 7 8 (9))))

;need to work on count nodes and leaves

(define (count-nodes tree)
  (if (atom? tree)
      1
      (+ 1 (accumulation + 0 (map (lambda (subtree) (count-nodes subtree)) tree)))))
;(count-nodes '(1 2 3))


(define (count-leaves tree)
  (cond ((null? tree) 0)
      ((atom? tree) 1)
      (else (+ (count-leaves(car tree)) (count-leaves (cdr tree))))))

(define (deepRecursion tree)
  (cond
    ((null? tree) '())
    ((atom? tree) tree)
  (else (reverse (map deepRecursion tree)))))

;(deepRecursion '(1 2 (3 4) ()))


(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile,
;and branch-length and branch-structure, which return the components of a branch.

(define (left-branch mobile) (car make-mobile))

(define (right-branch mobile) (car (cdr make-mobile)))

(define (branch-length branch) (car make-branch))

(define (branch-structure branch) (car (cdr make-branch)))

; Using your selectors, define a procedure total-weight that returns the total weight of a mobile.


(define (mobile? structure) (pair? structure))
 
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))
 
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


;part c

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))
 
(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (if (mobile? (branch-structure left)) (balanced? left) #t)
         (if (mobile? (branch-structure right)) (balanced? right) #t))))



;Union of two sets

(define (union a b)
  (cond ((null? b) a)
        ((member (car b) a)
         (union a (cdr b)))
        (else (union (cons (car b) a) (cdr b)))))

(union '(1 2 3) '(2 4 2))
 

;intersection

(define (intersection a b)
  (if (null? a)
      '()
      (if (contains (car a) b)
          (cons (car a) (intersection (cdr a) b))
          (intersection (cdr a) b))))

(define contains member) ; contains wasn't defined by I guess member will do

(intersection '(1 2 3 4 7) '(3 4 5 6)) ; ==> (3 4)


;transpose

(define (transpose lol)
  (apply map list lol))

(transpose '((a b) (d e) (f g))) ; ==> ((a d f) (b e g))









