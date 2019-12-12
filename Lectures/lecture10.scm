
; CSc 335
; October 31 2019

; lecture10.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; first: discussion of the proof of the value program from lecture 9.
; Any proof of correctness requires a specification!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; next we develop some classic list and tree recursions

; see sections 2.2.1 - 2.2.3 in Abelson and Sussman


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; length

(define (mylength l)
  (cond ((null? l) 0)
	(else (+ 1 (mylength (cdr l))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list-ref

(define (mylistref n l)
  (define (aux count l)
    (cond ((= count n) (car l))
	  (else (aux (+ count 1) (cdr l)))))
  (aux 0 l))


; can we implement list-ref without having a separate count parameter?

; pre: l is a non-empty list
; pre: n is a legitimate index for l

(define (mylistref n l)
  (cond ((= n 0) (car l))
	(else (mylistref (- n 1) (cdr l)))))


; invariant-based proofs developed and discussed, in detail

; for the first, we consider
;   the (element in the) Nth position in L is (element in the) the count-th position in l
; and note that this cannot be correct.  But
;   the Nth position in L is the (N-count)th position in l
; is more plausible.

; work out the details!


; for the second, we consider
;  the Nth position in L is the nth position in l

; again, it is a good idea to work out the complete argument


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; last, all-but-last

; assumes l is a non-empty list
(define (last l)
  (cond ((null? (cdr l)) (car l))
	(else (last (cdr l)))))


; assumes l is a non-empty list
(define (all-but-last l)
  (cond ((null? (cdr l)) '())
	(else (cons (car l) (all-but-last (cdr l))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; append

(define (myappend l m)
  (cond ((null? l) m)
	(else (cons (car l) (myappend (cdr l) m)))))


; what about checking (null? m) as well as (null? l)

(define (myappend l m)
  (cond ((null? l) m)
	((null? m) l)
	(else ...)))


; discussion of feasibility of optimizing on the relative lengths
; of l and m

; mention of snoc (which is cons, spelled backwards)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; reverse

(define (myreverse l)
  (cond ((null? l) l)
	(else (append (myreverse (cdr l)) (list (car l))))))

; quadratic time, and we wonder whether we can do better

(define (myreverse l)
  (define (iter l result)
    (cond ((null? l) result)
	  (else (iter (cdr l) (cons (car l) result)))))
  (iter l '()))

; linear time, so better


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; map

(define (mymap f l)
  (cond ((null? l) l)
	(else (cons (f (car l)) (mymap f (cdr l))))))

(define (square x) (* x x))

; examples ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; accumulate

(define (accumulate op init seq)
  (cond ((null? seq) init)
	(else (op (car seq) (accumulate op init (cdr seq))))))


; examples ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; filter

(define (filter pred seq)
  (cond ((null? seq) seq)
	((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
	(else (filter pred (cdr seq)))))


; examples ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; recall atom?
(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

; some data for the next few functions


(define tree-1
  '(a ((b c) ((d e) f))))

; observe

(define x1 (cons '(d e) (cons 'f '())))
(define x2 (cons '(b c) (cons x1 '())))
(define x3 (cons 'a (cons x2 '())))
(equal? tree-1 x3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; depth

(define (depth tree)
  (cond ((null? tree) 0)
	((atom? tree) 0)
	(else (max (+ 1 (depth (car tree)))
		   (depth (cdr tree))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; count leaves

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((atom? tree) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fringe

(define (fringe tree)
  (cond ((null? tree) '())
	((atom? tree) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))




; why is the combiner function append, instead of, say, list?

(define (bad-fringe tree)
  (cond ((null? tree) '())
	((atom? tree) (list tree))
	(else (list (bad-fringe (car tree))
		      (bad-fringe (cdr tree))))))

; spend some time figuring out what specification this version satisfies!




; and another bad fringe

(define (another-bad-fringe tree)
  (cond ((null? tree) '())
	((atom? tree) (list tree))
	(else (cons (another-bad-fringe (car tree))
		      (another-bad-fringe (cdr tree))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;wrong first example 


; count nodes 

(define (count-nodes tree)
  (cond ((null? tree) 0)
	((atom? tree) 1)
	(else (+ 1 (count-nodes (car tree)) (count-nodes (cdr tree))))))

; disaster! - overcounts: can you see why?  try this instead:

(define (count-nodes tree)
  (cond ((null? tree) 0)
	((atom? tree) 1)
	(else (+ 1 (count-nodes (car tree)) (count-nodes (car (cdr tree)))))))


; does this fix the problem?  What if the input tree has outdegree > 2 at its root?  Now
; we undercount!

; what about

(define (count-nodes t)
  (cond ((null? t) 0)
        ((atom? t) 1)
        ((atom? (car t)) (+ 1 (count-nodes (cdr t))))
        (else (+ (count-nodes (car t)) (count-nodes (cdr t))))))


; Or, instead of analyzing the input tree
; in terms of its car and cdr, how about thinking of the input tree as a list of subtrees:

(define (count-nodes tree)
  (cond ((null? tree) 0)
	((atom? tree) 1)
	(else (+ 1
		 (count-nodes (car tree))
		 (accumulate + 0 (map count-nodes (cdr tree)))))))

; or, assuming tree is not an atom, just

(define (count-nodes tree)
  (+ 1 (accumulate + 0 (map (lambda (subtree)
			      (if (pair? subtree)
				  (count-nodes subtree)
				  1))
			    tree))))

; refining further -- what about this?

(define (count-nodes tree)
  (if (atom? tree)
      1
      (+ 1 (accumulate + 0 (map (lambda (subtree) (count-nodes subtree)) tree)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; deep-reverse - assigned for homework

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; enumeration

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; signal processing approach to some problems

(define tree-2 (list 1 (list (list 2 3) (list 4 5)) (list (list 6))))

(define (sum-odd-square tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (fib n)
  (define (aux curr prev count)
    (if (= count 0)
	prev
	(aux (+ curr prev) curr (- count 1))))
  (aux 1 0 n))


(define (even-fibs n)
  (accumulate cons
	      '()
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; nested mappings

; given a positive integer n, find all ordered pairs of distinct positive integers i and j,
; where 1 <= i < j <= n

(define (ordered-pairs-of-distinct-integers n)
  (accumulate append
	      '()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval (+ i 1) n)))
		   (enumerate-interval 1 (- n 1)))))


; what is the effect of accumulate-append?  Consider, for contrast,

(define (another-ordered-pairs n)
  (map (lambda (i)
	 (map (lambda (j) (list i j))
	      (enumerate-interval (+ i 1) n)))
       (enumerate-interval 1 (- n 1))))


; flatmap allows a simplification

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (ordered-pairs-of-distinct-integers n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval (+ i 1) n)))
	   (enumerate-interval 1 (- n 1))))



; flatmap turns out to be quite useful - next we use it to compute
; all permutations of a set S

; for example, the permutations of {1,2,3} are given --
; first, list all permutations with 1 in the first position
; next, list all permutations with 2 in the first position
; finally, list all permutations with 3 in the first position


(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap
       (lambda (x)
         (map (lambda (p) (cons x p))
              (permutations (remove x s))))
       s)))


; where

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))



; if we define flatten


(define (flatten x)
  (accumulate append '() x))


; then we can write

(define (flatmap proc seq)
  (flatten (map proc seq)))


; which does a better job motivating the name 'flatmap'




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Reading -- these functions are presented in Sections 2.2.1 - 2.2.3 of Abelson and Sussman

; Practice Problems -- in Abelson and Sussman 2.27, 2.29, 2.32, 2.36, 2.37, 2.38, 2.41, 2.42

; (These are included in Homework 6.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These problems - which have been included in the Homework 6 posting - will be discussed in class.
; My plan is to work through them gradually, in the
; order they are listed in the assignment.  You should solve them in the
; same order -- be ready to discuss the first problem (replacing
; the nth occurrence of old by new in a given sexp) as well as A&S 2.27
; (deep reverse) soon!
