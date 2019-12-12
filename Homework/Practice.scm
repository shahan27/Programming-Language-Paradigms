;should return true because a is a symbol by the quote
;(symbol? 'a)


;should return false cause a is not a symbol
;(eq? a 'b)
;this did not run becuase a is not a symbol and the precondition of eq? must have two symbols
;therefore the line above did not run

;this edited version should run and return false because both symbols are not the same.

;(eq? 'a 'b)

;should return false cause both symbols are different
;(eq? 'a 'b)

;should return true because a are the same symbol
;(eq? 'a 'a)


;;;All Correct

;(cons '(a) '())

;(append '(a) '())

;(cons '(a) 'b)

;(define x (list 3 4 5 6 7))

;(define x '(a b ((3) c) d))

;(car (cdr x))

;(cdaddr x) ;i dont understand why this returns (c)

;(char? (car '(#\a #\b))) ;I dont understand why this is a char

;(cons 'x x)

;(cons (list 1 2) (cons 3 '(4)))

;(cons (list) (list 1 (cons 2 '())))
;(list) is an empty list


(map (lambda (x) (+ x 2)) '(1 2 3 4 5 6 7 8 9))

(define atom?
  (lambda (x)
  (and (not (null? x))
       (not (pair? x)))))

(append '(a) '(a))


(define thereverse
  (lambda(lst)
         (cond ((null? lst) lst)
               (else (append (thereverse (cdr lst)) (list (car lst)))))))
  

(define (myreverse l)
  (cond ((null? l) l)
	(else (append (myreverse (cdr l)) (list (car l))))))







(map (lambda (x) (+ x 2)) '(9))






(define (depth tree)
  (cond ((null? tree) 0)
	((atom? tree) 0)
	(else (max (+ 1 (depth (car tree)))
		   (depth (cdr tree))))))


(depth '(0 3 4 5 6 (3 2 (3 4)) (3 4 5 6 (2 4 (5 (6)) 7 8))))



(define (getlargest a_list)
  (if (null? a_list) ; edge case: empty list
      #f             ; return a special value signaling error   
      (let loop ((a_list (cdr a_list))   ; rest of the list
                 (maxval (car a_list)))  ; assumed maximum
        (cond ((null? a_list) maxval)    ; if the list is empty, return max
              ((> (car a_list) maxval)   ; current element > max
               (loop (cdr a_list) (car a_list))) ; found new max
              (else                      ; otherwise
               (loop (cdr a_list) maxval))))))   ; keep the same max

(getlargest '(22 34 56 4 3 2 1 2 4))


(define l '(22 34 56 4 3 2 1 2 4))

(append (list(car l)) (cdr(cdr l)))


