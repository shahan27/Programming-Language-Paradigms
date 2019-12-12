
; CSc 335
; lecture12.scm
; December 3 and 5 2019


; We continue our initial examination of TLS



; Recall that the top level of the interpreter is given as

(define value
  (lambda (e)
    (meaning e (quote () ))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))



(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))


; where

(define (atom? e)
  (and (not (null? e)) (not (pair? e))))


; We looked previously at atom-to-action

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


; with

(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

; and

(define build
  (lambda (s1 s2)
    (list s1 s2)))

; and

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))




; as we are assuming there are no identifiers, for the moment, we say nothing further at this time regarding *identifier.


; As we noted last time, we can already evaluate some simple expressions with the code we have:


(value '1)


; returns 1 


(value '#t)

; returns #t



(value 'sub1)

; returns ('primitive sub1)

; here the tag 'primitive will be used later to further steer the evalution of expressions involving sub1.


; We can evaluate some additional expressions, still without variables.  Consider, for example,  '(sub1 4). Let's go back to the
; expression-to-action function, and fill in the missing definition


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; with

(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)



; and

(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))


; with

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))


; and

(define first car)

(define second cadr)

(define third caddr)


; and

(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))


; and finally

(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))



; now we can evaluate (sub1 4)

; (on board, in class)

(value '(sub1 4))

; you will see that we have opted to define each primitive as standing in for the mzscheme function of the same name -
; there is no requirement that we do so: TLS primitive can be defined however we like.


; we can even evaluate more complicated expressions


(value '(mul (add1 3) (sub1 4)))


; and we should spend a moment making sure everyone understands how the evaluation (said to be syntax-directed, or by
; recursive descent) works: these calculations can be done by hand, and I encourage you to do this.


; if we now add the action for quote

(define *quote
  (lambda (e table)
    (text-of e)))

; where

(define text-of second)


; we can evaluate expressions involving lists, such as


(value '(cons (quote x) (quote ())))


; of course, at this point,

(value '(cons x (quote ())) )

; results in an error: we have not yet installed means for looking up the value of the identifier x, which is
; necessary here because the symbol x is not quoted

(value '(car (cons (quote x) (quote y))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; what else can we do without variables?

; what about cond? 



; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)


; now we can evaluate expressions with conditionals

(value '(cond (#f 1) (#t 2)))

(value '(cond ((number? (quote x)) 1) (else 2)))

(value '(mul (add1 (cond (#f 1) (#f 2) (else 3))) 4))


; suggested exercise: add primitives and, or and not to the interpreter and experiment with various conditionals created
; using them












































