
; Fourth Homework Set
; CSc 335
; Fall 2019


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework4.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with their arguments) for each

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.


;Recursive

;pre conditiong list argument must be type of list
;post condition, result is an integer representing length of list
(define (my-length list)
  (cond ((null? list) 0)
        (else (+ 1 (my-length(cdr list))))))

;(my-length '(a b c d));

;(my-length '())

;Tail recursive and Iterative

(define (i-length l acc)
  (cond ((null? l) acc)
  (else (i-length (cdr l) (+ 1 acc)))))


;(i-length '(a s d f f f f f) 0)

; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?


;recursive

(define (my-list-ref l i)
  (cond ((zero? i) (car l))
        (else (my-list-ref (cdr l) (- i 1)))))

;(my-list-ref '(a b c d) 0)
;(my-list-ref '(a) 0)


;tail recrusive



; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.



(define (start lst num)
  (cond ((zero? num) '())
        (else (append (list(car lst)) (start (cdr lst) (- num 1))))))

(start '(1 2 3 4 5 6 7 8) 3)

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

(define (but-last lst num)
  )

(but-last '(1 2 3 4 5 6 7 8) 4)
; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

