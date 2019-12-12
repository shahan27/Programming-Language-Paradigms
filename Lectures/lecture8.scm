


; CSc 335
; Lecture 8
; October 23 2019





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; here is another example of proving a program correct using induction on list length, which I 
; write out, to provide a template.  

;; pre: a is an atom, lat is a list of atoms (possibly empty)

;; post: (member? a lat) = #t precisely when the atom a occurs in lat


(define member?
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))


;(member? 'meat '(mashed potatoes and meat gravy)) = #t


; here is a correctness argument, noting that member? has recursive form

;; we show that if a is an atom and lat is a list of atoms, then
;; (member? a lat) is true precisely when a occurs in lat.  We argue
;; by induction on lat, that is, by induction on the length of lat.

;; if lat has length 0, then lat = (), and of course a does not occur in lat --
;; (member? a lat) correctly returns #f

;; now assume that the recursive call (member? a (cdr lat)) works correctly,
;; and consider (member? a lat).  If a = (car lat), (eq? a (car lat)) is #t,
;; and the (or ... ) returns #t.  This is then the value of the call 
;; (member? a lat), and as a does then occur in lat, it is clearly correct.

;; If on the other hand it is not the case that (car lat) is a, our function
;; computes (or #f (member? a (cdr lat))).  As now a belongs to lat if and
;; only if it occurs in (cdr lat), and as we assume that the recursive call
;; works correctly, we see that (or #f (member? a (cdr lat)) returns #t 
;; if a occurs in something other than the first position of lat, and that
;; it otherwise returns #f.  As these would then be the values returned by
;; (member? a lat), we have shown that (member? a lat) satisfies its
;; specification.

;; [Observe that this argument does not depend in any way on the order of 
;; evaluation of the arguments to or.]


; we can also give an iterative version

(define member?
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))


; an invariant: a belongs to LAT if and only if
;                 a belongs to lat

; You should confirm that this works! Again, LAT is the original value of lat: lat changes as
; the program runs, but LAT is always equal to the initial value.


; It is worth noting the essential similarity of the last two functions -- it should not be particularly surprising
; that the standard induction proof given for the recursive version carries over - pretty much unchanged - to the 
; iterative version. 

; In fact, one might want to ask whether the first version really is iterative, rather than recursive.
; Noting that the or function can here
; be replaced by cond, one wants to ask: what, precisely, is the deferred operation?  

; For our factorial program, earlier in the course, the multiplication could not be carried out until the recursion had
; finished.  But here, there is a sense in which the or evaluates to the value of the recursive call -- whatever it is --
; without needing to defer anything once it knows that its first argument is #f.  So there would not need to be any
; deferral, and the function is in fact iterative.  

; Hmmmmm.  So now the question becomes: is there a version of member? which is "truly" recursive?  

; Perhaps the answer has to do with knowing or not knowing the order of evaluation for or ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; in class: sketch the design of a function, firsts, for which

; (firsts l) = (apple plum grape bean)
; when l is 
;    '((apple peach pumpkin)
;      (plum pear cherry)
;      (grape raisin pea)
;      (bean carrot eggplant))

; (firsts '((a b) (c d) (e f))) = (a c e)

; we develop a specification -- for example, it appears that we do not
; need to require that the input sublists all have the same length --
; and sketch a correctness proof.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 
; next we consider the problem of removing a member of a list of atoms

;; pre: a is an atom, and lat is a list of atoms

;; post: (rember a lat) is the list lat', where lat' is 
;;       obtained from lat by removing the first occurrence -- if any --
;;       of a

; first attempt

(define rember1
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember1 a (cdr lat))))))))

; trying

; (rember1 'bacon '(bacon lettuce and tomato))

; (rember1 'and '(bacon lettuce and tomato))

; we see that there is a problem with this preliminary design


; use cons

(define rember2
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond 
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (rember2 a (cdr lat)))))))))

; carry out some testing

;(rember2 'and '(bacon lettuce and tomato))

; ...

; when everything seems to be right, work up a proof.  the idea is
; to exploit whatever techniques are available to increase one's 
; confidence in the code


; we develop an induction argument showing that rember2
; satisfies its specification

; as before, the induction is on the length of lat

; again, the basis step -- for lat of length 0 -- is clearly correct: 
;  (rember2 a ()) = () , exactly as it ought

; assuming that the recursive call (rember2 a (cdr lat)) works correctly,
; we now need to argue that the else clause computes the desired result.

; one begins by observing that there are two cases: either the atom a 
; occurs first in lat, or not.  If it does occur first, then the post
; condition is achieved if our function returns (cdr lat), as you can 
; see it does.  If the atom a does not occur first, then either it occurs
; later in lat or it does not occur at all.  In either case, the induction
; hypothesis (namely, that (rember2 a (cdr lat)) works correctly) guarantees
; that (rember2 a lat) works correctly.  

; how does this argument show that it is the _first_ occurrence
; which has been removed?  it may make sense to fill in some of the 
; detail in induction step:

;   If the atom a does not occur first, then either it occurs 
;   later in lat or it does not occur at all.  In the first case,
;   the first occurrence of a in (cdr lat) is the first occurrence
;   of a in lat, so it is enough to know that (rember2 a (cdr lat))
;   removes the first occurrence of a in (cdr lat), as is guaranteed
;   by the induction hypothesis.  If a does not occur at all, then
;   of course it does not occur in (cdr lat), and the induction 
;   hypothesis guarantees that ...


; an excellent exercise is to try to construct a similar proof for the first, incorrect,
; version of this program.  What goes wrong?  Precisely where does
; the induction break?


; and wait: do you see how our argument shows that the value returned is the
; original list without its first occurrence of the atom a? 





; returning to the code, we ask: can the function be simplified?

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))


; why not do this right away?  because the revised rember's structure
; does not coincide with its argument's structure, and it is frequently
; easier to develop scheme programs when their structure is in alignment
; with that of their data.  clarity and correctness should come first,
; and simplification second.

; what do we mean here by 'its argument's structure'?  Roughly, the shape
; of the Backus-Naur definition for lat

;     lat ::= () | (cons a lat)



; we go on to modify rember to remove all occurrences of a from lat,
; and to modify the proof as well.  Following the naming style used in
; The Little Schemer, this function can be called multirember.


;; in class








; Now consider 

(define (insertR lat occ new)
  ... )

; where the specification is 
;
;  pre:  lat is a list of atoms
;        occ is an atom, which may or may not occur in lat
;        new is an atom
;
;  post: given that lat = (a1 a2 ... am) and that aj is the first occurrence of occ in lat, 
;             (insertR lat occ new) =  (a1 a2 ... a(j-1) occ new a(j+1) ... am)
;
;        if occ does not occur in lat, then 
;             (insertR lat occ new) = lat


; one possible design

(define (insertR lat occ new)
  (cond ((null? lat) '())
	((eq? (car lat) occ) (cons (car lat) 
				   (cons new (cdr lat))))
	(else (cons (car lat) (insertR (cdr lat) occ new)))))



; Do you agree that the above specification is made easier to understand 
; if an example is included?  

; For instance, one might show 
;   (insertR '(a b c d c e f) 'c 'new) = '(a b c new d c e f)
; and
;   (insertR '(a b c d c e f) 'g 'new) = '(a b c d c e f)

; Do you agree that examples alone are unlikely to give the complete idea 
; for the specification?



; insertL is an easy modification:

;                     ... (cons new 
;                               (cons (car lat) (cdr lat)))
;                                                          ...

;; (can you see how to simplify the second line?)



; now try subst, another easy modification in which (subst new old lat) simply replaces the first
; occurrence of old in lat with new

;                     ... ((eq? (car lat) old)
;                           (cons new (cdr lat)) 
;                                             ...


; you should work out proofs for all of these, making sure, 
; that you understand how to prove that it is the _first_ occurrence of old which is 
; replaced

;; for all proofs not included in these notes: work them out on your own; when you have
;; something down on paper, compare your work to what we did in class; if questions remain,
;; or if in fact the argument you are interested in was not done in class, come see me --
;; we can look at your stuff together.

;; the same idea goes for omitted code, as well



; now try multisubst, replacing all occurrences of old in lat with new


(define (multisubst lat old new)
  (cond ((null? lat) '())
	((eq? (car lat) old) (cons new (multisubst (cdr lat) old new))) 
	(else (cons (car lat) (multisubst (cdr lat) old new)))))


; do you see the similarity of insertR, insertL, subst, and multisubst?

; could you convince yourself that you do --  by now writing out for yourself 
; designs of multi-insertR and multi-insertL?  

; make sure you see how to give specifications for the multi- functions!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
