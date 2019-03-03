
;;
;; a guide of lisp
;; 
;; goals:
;; 1. Do enough practices here to have a systemic understanding of common lisp.
;; 2. Try to find new patterns of language design via those practices.
;;
;;



;;
;; restart due to errors
;;

* (/ 3 0)

debugger invoked on a DIVISION-BY-ZERO:
  arithmetic error DIVISION-BY-ZERO signalled
Operation was SB-KERNEL::DIVISION, operands (3 0).

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(SB-KERNEL::INTEGER-/-INTEGER 3 0)
0] 0

*


;;
;; name of loop
;; 
;; loop [name-clause] {variable-clause}* {main-clause}* => result*
;; 
;; name-clause::= named name 
;;
* (loop "print-1"
	(print 1)
	(when 1 (return)))

1
nil


;; loop
;; loop [name-clause] {variable-clause}* {main-clause}* => result*
;;
;; variable-clause::= with-clause | initial-final | for-as-clause
;; main-clause::= unconditional | accumulation | conditional | termination-test | initial-final 
;;
;; sqrt advisor
;;
(defun sqrt-advisor ()
	(loop (format t "Number: ~%")  ; initially compound-form, function
	
		  ; initially compound-form, special operator
		  (let ((n (parse-integer (read-line) :junk-allowed t))) 
		  
		  ; main-clause, conditional macro
		  (when (not n) (return))  
		  
		  ; main-clause, finally compound-form, function
		  (format t "The square root of ~D is ~D.~%" n (sqrt n)))))  

SQRT-ADVISOR
* (sqrt-advisor)
Number: 
1
The square root of 1 is 1.0.
Number: 
2
The square root of 2 is 1.4142135.
Number: 
3
The square root of 3 is 1.7320508.
Number: 
Nil



;;
;; loop
;;
;; for-as-clause
;;
* (loop as n from -20 to 20
	if (and (> n 0) (oddp n)) collect n)

(1 3 5 7 9 11 13 15 17 19)


* (loop for n from -20 to 20
	when (and (> n 0) (oddp n)) collect n)

(1 3 5 7 9 11 13 15 17 19)

*
(loop for n from 10 downto 0
	when (< n 6)
	collect n)
	
(5 4 3 2 1 0)



;;
;; loop
;;
;; for-as-in-list
;; for-as-on-list
;;

* x

(1 2 3)

*
(loop
	for v in x
	when (> v 2)
	    do (print v))
	    
3

*
(loop
	as v in x
	when (> v 1)
	    collect v)	
		
(2 3)

*
(loop
	for v on x
	    do (print v))
	    
(1 2 3) 
(2 3) 
(3) 


;; Here, I found typing "as" is better than "for" 
;; and "if" and "when" is better than "unless",
;; and "when" is better than "if", because "if" is more abstract than "when",
;; "loop" should be hidden when the following "for-as-clause" is found.
;; 
*
(loop for v on x
	do (print 
		(loop for vv in v
			unless (> vv 2)
			collect vv)))

(loop as v on x
	do (print 
		(loop as vv in v
			when (<= vv 2)
			collect vv)))
				    		  	
(1 2) 
(2)


;;
;; How about below codes ?
;; however, "loop as" is tolerated as the consistency with common lisp
;;
(as v on x
	do (print 
		(as vv in v
			when (<= vv 2)
			collect vv)))
			
			




	    
