
; CSc 335
; lecture15.scm
; December 10 and 12 2019






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; assignment, objects, another look at letrec, and a first look at continuations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we next introduce state - and assignment.

; you are asked to read sections 3.1 and 3.2 in Abelson & Sussman for an introduction to
; assignment in scheme, which is accomplished using the primitive set! (we pronounce this
; as 'set-bang')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; let's begin by trying to develop a counter -- we want somehow to have a function
; which remembers how many times it has been called

; what about this?


(define (counter)
  (let ((count 0))
    (begin
      (set! count (+ count 1))
      count)))


; as you can see from experimentation, successive calls return the same value, 1.

; so we have more thinking to do.  
; can we somehow exploit closures (and, of course, assignment)
; to do the job we have in mind?

; consider

(define (make-counter)
  (define count 0)
  (define (increment)
    (begin
      (set! count (+ count 1))
      count)) 
  increment)


; now (make-counter) returns a closure -- <closure count increment> -- and calling
; this closure should do the job

; we name the closure

(define f (make-counter))

; and ask you now to compute (f), and then again (f), and then again (f) ... returning
; in sequence 1, 2, 3 ... 

; it seems we have a counter!  


; to get an intuitive understanding, some further experiments are called for

; try 

(define g (make-counter))

; and convince yourself that f and g name different closures, each with its own private copy
; of count

; observe that (define count 96) at the top level has no impact on the values returned by either
; (f) or (g)


; this is a good place to display the use of letrec, as make-counter can be written

(define make-counter
  (lambda ()
    (letrec (
             (count 0)
             (increment (lambda () 
                          (begin
                            (set! count (+ count 1))
                            count)))
             )
      
      increment)))


; yet another way

(define (make-counter)
  (let (( count 0))
    (define (increment)
      (begin
        (set! count (+ count 1))
        count)) 
    increment))




; this more involved example is from Chapter 3 of Abelson and Sussman:


(define (make-account)
  
  (define balance 0)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        (error "insufficient funds")))
  
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  
  (define (dispatch m)
    (cond ((eq? m 'balance) balance)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unimplemented method"))))
  
  dispatch)


; here you will observe that the value returned by (make-bank-account) is 
; the closure, dispatch.  If  

(define myaccount (make-account))

(define youraccount (make-account))

; you should experiment to confirm the independence of the balances.  Observe
; that typical usage here would be

((myaccount 'deposit) 20)

((myaccount 'withdraw) 12)


; what is the relation between make-account and your understanding of classes?  
; between myaccount and your understanding of objects? 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; explanation of make-counter in terms of the environment model, introduced at the start of the course

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; explanation of make-account in terms of the environment model --

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; letrec explained

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; another view of letrec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; letrec from let and set!, explained via the environment model

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; explain this version of length


(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (h (cdr l)))))))
    h))



; referring to the environment model, the situation which results upon defining 
; length is

;    --------------------------
;    |                         |
;    | length --               |
;    |         |               |
;    |         |               |
;    ----------|--------^-------
;              |        |
;              |        |
;              |     ------------
;              |     |          |
;              |     | h        |
;              |     | |        |
;              |     --|----^-----
;              |       v    |
;              ------> 00---|
;                      |
;                     (lambda (l)
;                       (cond ((null? l) 0)
;                             (else (add1 (h (cdr l))))))

; that is, (set! h (lambda (l) ... ) sets h to a function whose 
; environment is that in which the right hand side of the set!
; is evaluated.  

; if you like, set! has been used to create a circle



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; continuations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; we are content to give only the briefest indication of continuations, via the scheme
; form call/cc

; continuations have many uses - we illustrate just enough for our next interpreter


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here is a simple example from Grillmeyer (Exploring Computer Science with Scheme)

; print messages during recursive descent and unwind

(define (vanilla arg)
  (cond ((> arg 3) 'done)
        (else
         (display "before recursion")
         (display " ")
         (display arg)
         (newline)
         (vanilla (+ arg 1))
         (display "after recursion")
         (display " ")
         (display arg)
         (newline))))



; print messages during recursive descent only

(define (strawberry arg)
  (call-with-current-continuation
   (lambda (stop)
     (define (inner-berry arg)
       (cond ((> arg 3) (stop 'done))
             (else 
              (display "before recursion")
	      (display " ")
	      (display arg)
	      (newline)
              (inner-berry (+ arg 1))
              (display "after recursion")
              (display " ")
	      (display arg)
	      (newline))))
     (inner-berry arg))))


; evaluate (vanilla 0), (strawberry 0), and then (list (strawberry 0) 'already)


; we ask about 'stop': from where does it take its value?  


; we introduce the idea of 'context':  the context of (proc x), 
; in 

;    (+ 3 (* 4 (proc x)))

; is the function 

;    (lambda (box) (+ 3 (* 4 box)))

; roughly, it is what the program does with the value (proc x)



; similarly, the context of (call/cc ... ) in 

;   (+ 3 (* 4 (call/cc (lambda (stop) (stop 10)))))   
   
; is 

;   (lambda (box) (+ 3 (* 4 box)))

; now we ask: what is the result of evaluating this expression?  The system
; substitutes the context of call/cc for stop in (stop 10) --
; and the result is 43.

; note that the result is _not_ (+ 3 (* 4 43))

; the context (lambda (box) (+ 3 (* 4 box))) is wrapped by call/cc with
; an 'escape function' -- when stop is applied, as above, to 10, 
; the system first computes ((lambda (box) (+ 3 (* 4 box))) 10), and then
; 'escapes': anything waiting for the result is abandoned.  

; now consider 

(+ 3 (* 4 (call/cc (lambda (stop) (* 10 (stop 10))))))

; also 43.  Why not (* 10 43)?  Again, something more
; than substituting the context for stop is going on --
; after applying the context, an 'escape' is invoked.

; precisely the same thing happens in the strawberry example:
; after (stop 'done), the procedure exits (rather than complete
; the recursion)

; consider 

(list 5 (+ 3 (* 4 (call/cc (lambda (stop) (* 10 (stop 10)))))))

; now we obtain (5 43), as the context function here is

; (lambda (box) (list 5 (+ 3 (* 4 box))))


; practice

(list 5 (+ 3 (* 4 (call/cc (lambda (stop) (+ 10 (* 10 (stop 10))))))))

; you can work this out!





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Now let's look at an example from Chapter 13 of TSS


; consider the set function intersect


(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond 
                ((null? set) (quote ()))
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))


; intersect can be extended to allow input of a list of sets

(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
                ((null? (cdr lset))
                 (car lset))
                (else (intersect (car lset)
                                 (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))


; now suppose one of the sets in lset is empty: what does intersectall do?

; Consider, for example, (intersectall '(() (a b c d) (a b c) (b c d)))

; wouldn't it be better if intersectall didn't have to intersect each set 
; with the empty set?  If it could just stop when it encountered the empty set?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we can arrange for this if we exploit continuations, specifically, 
; call-with-current-continuation.

; call-with-current-continuation is sometimes abbreviated call/cc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; we now show how call/cc can be used to redefine intersectall so that it 
; has the property we wanted

(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))
                    (hop (quote ())))
                   ((null? (cdr lset))
                    (car lset))
                   (else 
		    (intersect (car lset) (A (cdr lset))))))))

         (cond
           ((null? lset) (quote ()))
           (else (A lset))))))))


; what is the context of call/cc?  Just (lambda (box) box).  So the system substitutes this context
; for hop in (hop (quote ())), and returns (quote ()).  



; compare, for illustration,

(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))
                    (hop (quote empty-list-encountered)))
                   ((null? (cdr lset))
                    (car lset))
                   (else 
		    (intersect (car lset) (A (cdr lset))))))))

         (cond
           ((null? lset) (quote ()))
           (else (A lset))))))))



; here is a slightly more complicated example -- the function rember-upto-last takes an
; atom a and a lat and removes all the atoms from the lat up to and
; including the last occurrence of a.  If there are no occurrences
; of a, rember-upto-last returns the lat.

(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
           ((R (lambda (lat)
                 (cond
                   ((null? lat) (quote ()))
                   ((eq? (car lat) a) (skip (R (cdr lat))))
                   (else
                    (cons (car lat)
                          (R (cdr lat))))))))
         (R lat))))))



; again, the context of call/cc is (lambda (box) box) - the identity function.  The difference
; this time is that the parameter box is bound to (R (cdr lat)) -- in a sense, the rest of the
; ongoing recursion.  The escape wrap causes the system to simply forget about all the deferred
; conses; if another occurrence of a is found, all the conses deferred since the last one
; will also be forgotten.  In the end, all that remains are those list elements which occur after
; the last a.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In sum, we will use call/cc when we want to return values abruptly and promptly
;
; (Slightly modified Fourteenth Commandment from TSS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









