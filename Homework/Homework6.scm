; Sixth Homework Set
; CSc 335
; Fall 2019

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First Problem

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)


; Additional Problems

; Abelson and Sussman, Exercise 2.27
;Exercise 2.27. Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a
;list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

;(define x (list (list 1 2) (list 3 4)))
;x((1 2) (3 4)) (reverse x)
;>((3 4) (1 2))
;(deep-reverse x)
;>((4 3) (2 1))




; Abelson and Sussman, Exercise 2.29
; Abelson and Sussman, Exercise 2.32
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41


; Abelson and Sussman, Exercise 2.42 n-queens problem
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))






