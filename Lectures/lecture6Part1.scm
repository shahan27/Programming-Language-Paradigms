
; CSc 335
; Lecture 6 - Part 1
; September 25, 2018

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The focus is on some homework problems

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Some Homework Solutions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; develop and certify a recursive program which inputs a nonnegative integer and which
; outputs the sum of its digits.


; how to break the problem down so that it is amenable to recursion?

; either extract the last digit and work on the rest, or extract the first
; digit and work on the rest.

; extracting the last digit: (remainder number 10)

; extracting the first digit: recursively divide by 10 until the quotient
; is less than 10 ...

; we opt for the first option

; simple recursive design


(define (sum-digits number)
  (let ((last-digit (remainder number 10))
	(rest-of-number (truncate (/ number 10))))  ; or (quotient number 10)
    (if (zero? rest-of-number)
	number
	(+ last-digit
	   (sum-digits rest-of-number)))))


; proof discussed in class, but you should work out the details on your own.  If you have questions,
; please come to see me in office hours. 


; now for an iterative version


(define (sum-digits number)

  (define (sum-iter unprocessed sum-so-far)
    (cond ((zero? unprocessed) sum-so-far)
	  (else (sum-iter (truncate (/ unprocessed 10)) (+ (remainder unprocessed 10) sum-so-far)))))

  (sum-iter number 0))


; guess-invariant: the sum of the digits in number equals the sum of the digits in unprocessed plus
; sum-so-far  

; termination: unprocessed is initially number, and is reduced by one digit from the right with each
; iteration until no digits remain


; discussion in class, but -- same deal as above: you should make an effort to write out the details on your
; own.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; develop and certify a recursive scheme program to test whether the digits in a
; non-negative integer are in increasing order.

; recursive

(define (digits-in-increasing-order? number)
  (let ((last-digit (remainder number 10))
	(rest-of-number (truncate (/ number 10))))
    (if (zero? rest-of-number)
	#t
	(let ((next-to-last-digit (remainder rest-of-number 10)))
	  (and (< next-to-last-digit last-digit)
	       (digits-in-increasing-order? rest-of-number))))))

; A proof by induction on the number of digits in number is reasonably
; straightforward, though one needs to pay attention to the various cases
; which arise.

; The basis step needs to show that the program works correctly for single-digit
; inputs: one confirms that if number has just one digit, then last-digit = number
; and rest-of-number = 0.  According to the code,

;                 (digits-in-increasing-order? number)

; returns #t, which is correct: the digits of a single digit number are
; sorted.


; The induction hypothesis is as expected: we assume

;                (digits-in-increasing-order? number)

; works correctly whenever number has no more than k digits.  In particular,
; we assume that the value returned is #t if the digits of number occur in increasing
; order, and #f otherwise. 


; The induction step can be organized as follows: suppose number
; has k+1 digits.  Then rest-of-number, computed as

;              (truncate (remainder number 10))

; has k digits.  Thus it follows from the induction hypothesis that 

;           (digits-in-increasing-order? rest-of-number)

; returns #t iff the digits of rest-of-number are in increasing order.  The task
; facing us is to sew this knowledge into a demonstration that

;              (digits-in-increasing-order? number)

; returns #t iff the digits of number are in increasing order.

; Suppose first that (digits-in-increasing-order? rest-of-number) returns
; #t.  We see from the code that next-to-last-digit is the least significant
; digit of rest-of-number, and that last-digit is the least significant digit
; of number.  So the digits of number are in increasing order exactly when
; the test (< next-to-last-digit last-digit) is #t - and in this case the
; program returns #t, the value of

;  (and (< next-to-last-digit last-digit)
;       (digits-in-increasing-order? rest-of-number))

; It is clear from this as well that the program returns #f if next-to-last-digit
; is not less than last-digit, and that this is the correct value for that case.

; What is left to consider?  Well, what if (digits-in-increasing-order? rest-of-number)
; returns #f, as it would - by our induction hypothesis - if the digits of
; rest-of-number are not properly sorted?  In this case, the call

;    (digits-in-increasing-order? number)

; returns the value of the and -- that is, #f -- as it clearly should: if rest-of-number's
; digits are out of order, then so of course are those of number. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; develop and certify an iterative scheme program to test whether the digits in a
; non-negative integer are in increasing order.


; if we suppress the abstraction inherent in the recursion and look instead at the
; 'step-by-step' picture of the process' progress, we might summarize our observations
; with a diagram such as the following


;       rest-of-number                  |   already-processed
;   -----------------------------------------------------------------
;    1  2  3   4   5   6                |    7  8  9
;   -----------------------------------------------------------------
;                      ^                     ^
;                      |                     |
;                      next-to-last-digit    last-digit
;
; As the computation proceeds, the already-processed segment (maintained only
; virtually by the program) is kept sorted, and the rest-of-number
; segment empties, with the boundary between them sliding to the left. 


; iterative

(define (iterative-digits-in-increasing-order? number)

  (define (iter result-so-far last-digit rest-of-number)
    (cond ((not result-so-far) #f)
	  ((zero? rest-of-number) result-so-far)
	  (else (let ((next-to-last-digit (remainder rest-of-number 10)))
		  (iter (< next-to-last-digit last-digit)
			next-to-last-digit
			(truncate (/ rest-of-number 10)))))))

  (iter #t (remainder number 10) (truncate (/ number 10))))


; certification by design roles


; let us suppose that we wish to summarize the situation after k digits have been processed.  At
; that point, we would have the situation suggested by the diagram, for the particular input
; 123456789.

; Of course, one cannot base the correctness argument entirely on the processing of a single
; input, so we need to try to capture a more general picture.


; Using a more formal notation, we give initial, or 'guess-', design roles for the program
; variables, after the first k digits of the input NUMBER have been processed:


; NUMBER = rest-of-number * 10^k + already-processed
;   this implicitly gives the design role of the 'virtual' variable, already-processed 


; last-digit = most significant digit of already-processed

; next-to-last-digit = least significant digit of rest-of-number


; these equations give the design roles of rest-of-number, last-digit and next-to-last-digit, but we
; still need to say something about result-so-far, which is a boolean:

; result-so-far = (digits of already-processed are in increasing order)


; That is, result-so-far is the truth value of 'digits of already-processed are in
; increasing order'.



; Now that we have them, what does one do with these initial guesses at design roles?  

; We need to show

; 1. that the design roles are achieved by the initial values
; of the program parameters;
;
; 2. that the design roles, if true on the mth
; call, are true on the (m+1)st call;

; 3. that the design roles, if maintained true through each call,
; and hence true when the program halts, are sufficient to show that
; the program is correct.



; One would normally start by checking 1 (that the design roles are initially achieved) and 3 (that
; the design roles are powerful enough to imply correctness if the program terminates) before checking
; 2 (that the design roles are preserved by calls to iter).  The reason for this is that
; 1 and 3 are usually easier to check - if these do not hold, we need to develop new design roles
; before ever bothering with their preservation.



; So, let's do it.

; Do the design roles hold on our initial call, 

;                  (iter #t (remainder number 10) (truncate (/ number 10)))

;                        ^           ^                   ^
;                                                        | 
;                                    |                   rest-of-number
;                                    last-digit
;                        |
;                        result-so-far
; ?


; At this point, the first digit has been processed - so we need to know

;    NUMBER = (truncate (/ NUMBER 10)) * 10 + (remainder number 10)

;    last-digit = least significant digit of NUMBER

;    next-to-last-digit = (remainder rest-of-number 10)

;    result-so-far = (digits of already-processed are in increasing order)
    
; You can easily check that all are true.




; Now for 3: if the design roles hold, and if the program stops, do we know that
; the returned value is correct?

; If the program stops because result-so-far is #f, then we know that the digits
; of already-processed are not in increasing order.  The program returns #f (look
; at the code), and this is the correct value.

; If the program stops because rest-of-number is 0, then NUMBER = already-processed from
; our first equation.  But then the equation for result-so-far guarantees that the value
; of that variable - which is the value returned by the program - is exactly the value
; of 'digits of NUMBER are in increasing order'.

; So the invariant (the logical and of the design roles) is strong enough to imply that
; the returned value is the right one.




; All that is left to do is to check that the truth of the invariant on the kth call
; of iter implies the truth of the invariant on the (k+1)st call of iter.

; I believe that, at this point, I can safely leave this verification to the student. 




; the termination argument is like that given for the recursive version


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Suggested for practice

; What about an invariant of the form

;  NUMBER is sorted if and only if
;   each of the following holds
;      result-so-far
;      rest-of-number is sorted
;      least significant digit of rest-of-number <= last

; This has the apparent virtue of eliminating the "virtual" variables from the invariant
; we discuss above.

; Does it work?  (Hint: no, not quite.  More needs to be said about the design role for
; result-so-far.)

; Another practice problem: can you rewrite the program to eliminate the parameter
; result-so-far, while remaining iterative?  (Hint: yes, but the proof needs to change
; as well.)



