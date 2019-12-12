; CSc 335
; Spring 2019

; March 7

; First Midterm Exam - 1.75 hours

; Professor Troeger

FIBONACCIE
RECURSIVE
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1
        (else (+(fib(- n 1)) (fib(- n 2)))))))


proof of induction on n

base case are n = 1 or 0


hypothesis: assuming n-1 >= 0 and n-2 >= 0, (fib (- n 1)) returnes the (n -1) nth fib gib # and fib(- n 2) reutrns the n-2 ad fib #


Iduction step
since n>=2 when control reaches the recursive calls the  IH applies. So (+ (fib (- n 1)) (fib (- n 2)))
nth fib #, by def if fib seq.


SOME OF DIGITS (Induction on number of digits)

total work =

logical and of design roles 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm 
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - final code (out of 10 points)
;;;; Problem 1 - development (out of 15 points)

;;;; Problem 2 - code (out of 15 points)

;;;; Problem 3 - final code (out of 20 points)
;;;; Problem 3 - development (out of 40 points)



;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no lists, no vectors, no strings, no assignment...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed in your backpack/briefcase.  They are not to leave the room, nor are
; they to be visible at any point during the exam.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  

; Problem 1 (25 points) Using an invariant, give a complete development for an iterative procedure aux
; to simulate a 3-digit one-button counter which rolls over from 999 to 000. Your procedure should
; have parameters hundreds, tens, and ones, perhaps among others.  The procedure aux is to be
; called by another function, count: the call (count start k) returns the 3-digit number formed
; from the digits hundreds, tens and ones which would result when the step button on the simulated counter
; is pressed k times with initial value start.  

; Thus (count 0 1) returns 1, (count 95 10) returns 105, and (count 995 10) should return 5.  You
; should give a specification for the function count as well as for your iterative function aux.  

; Of course I do not want you to use modulo -- again, we are thinking of aux as an old-fashioned single-button counter:
; press the button once and 000 goes to 001, press it again and the display goes to 002.  The parameters
; hundreds, tens and ones change as they would on the single-button counter.  

; The only difference is that our simulator cannot output leading zeros -- we are at present constrained to
; use scheme numbers.

; As in class, a complete development is one which shows all of the guess-invariants and guess-designs and guess-programs,
; along with the reasons (arguments) for changing these as you make progress, and which culminates in a working program.
; I want to see a record of your development process.  I also need to see working tests for each of your functions.  

((define count x y)
 (cond((> 999 (+ x y)) (+ x y))
      (())

; INSERT YOUR ANSWER HERE


              
; Problem 2 (15 points)  We are all familiar with mathematicians' use of functions defined
; by cases.  For example, the absolute value function (abs x) is defined to be x if x is
; greater than or equal to 0, and -x otherwise.  In this problem you are to build a function
; by-cases which inputs two unary functions f and g, as well as a unary predicate pred -- you
; may assume that f and g and pred all expect a number as input -- and which returns the function
; (by-cases f g pred) such that

;              ((by-cases (lambda (x) x) (lambda (x) (- x)) (lambda (x) (>= x 0))) -4)

; returns 4, as does

;              ((by-cases (lambda (x) x) (lambda (x) (- x)) (lambda (x) (>= x 0))) 4)

; No development or proof is expected for this problem, but I do need to see a working test.



; INSERT YOUR ANSWER HERE



; Problem 3 (60 points)  Without using iteration, give a complete development for a
; procedure which inputs two positive integers n and m, and which returns the largest number which can
; be formed from any m digits in n.

; For example, if n = 11111 and m = 3, then your (main) program will return 111.  If n = 14253 and m = 2,
; your program will return 54.  Of course m cannot exceed the number of digits in n.

; For this problem, whose solution almost certainly requires auxilliary functions, I want to see
; justification for the functional decomposition which leads to the auxilliary functions.  I want to see
; a specification and an induction argument for each recursive function, and I want your certification
; arguments to make full use of pre- and post-conditions for the auxilliary functions whenever these are
; called.  Give clear definitions of any terms you introduce to describe your functions.  

; You are perfectly free to use ("built-in") scheme primitives such as quotient and expt -- there is
; no need to build and prove an exponentiation function, for example.

; As always, I need to see working tests of your functions.  


; INSERT YOUR ANSWER HERE




