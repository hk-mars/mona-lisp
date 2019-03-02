
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


		
		

