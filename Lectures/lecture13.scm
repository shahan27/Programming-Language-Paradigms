
; CSc 335
; lecture13.scm
; December 5 and 10 2019


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We continue building the TLS interpreter -- you will want to evaluate the definitions of this file in a scheme session
; into which those of lecture12.scm have already been loaded


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; to go beyond the simple calculations demonstrated so far, to examples involving variables and user-defined functions, we
; will require means for associating values with variables.

; to this end, we first install the environment subsystem which was discussed in lecture11.scm - I presented it separately
; to allow you to familiarize yourself with its properties apart from its role in the overall system.

; for convenience, I copy the entire subsystem here.  


(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))


(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))


(define initial-table '())

(define table-f
  (lambda (name)
    (car (quote ()))))

(define extend-table cons)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; finally, we add the lambda subsystem -- in TLS, the only way we associate values with variables is via lambda


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))


(define myapply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

; with this last definition, we pause to make sure everyone understands the concept of closure


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; we now actually have a turing-powerful programming system - we will start
; by looking at lambda without recursion, and then see how one can use a device called the Y-combinator
; to implement arbitrary recursive functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; examples for class discussion, and which I would ask you to work through - carefully, and in detail - by hand

; construct the environments! work out the closures! draw diagrams!


(value '((lambda (x) (add1 x)) 3))


(value '((lambda (x) (add1 x))
	 ((lambda (x) (add1 x)) 4)))


(value '(((lambda (y)
            (lambda (x) (cons x y)))
          3)
         4))


(value '((lambda (x z)
           (cons x
                 ((lambda (x y) (cons z x))
                  3 4)
                 ))
         1 2))


(value '((lambda (f y)
          (f y))
        (lambda (x) (add1 x))
        4))


(value '((lambda (f y)
	   (f y))
	 ((lambda (x) (cond ((number? x) add1)
			    (else (lambda (y) (cons x y)))))
	  (quote z))

	 3))


; next, let's translate the following simple illustration of a closure to tls-scheme

(let ((x 3))
  (let ((x 4)
	(f (lambda (y) (+ y x))))
    (f 2)))
	      

; to get

(value '((lambda (x) 
	   ((lambda (x f) (f 2))
	    4
	    (lambda (y) (+ y x))))
	 3))




; you see that tls-scheme features lexical scope and first-class functions --
; after you have had some time to think about how one might go about certifying that TLS correctly implements
; lexical scope and first class functions, I will describe proof outlines in class, and then ask you to write
; up the arguments for homework.  Big surprise: induction is involved.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


