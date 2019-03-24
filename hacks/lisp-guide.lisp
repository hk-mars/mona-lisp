
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
;; and "when" is better than "if", because "when" is more extensible than "if",
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
*
(as v on x
	do (print 
		(as vv in v
			when (<= vv 2)
			collect vv)))
			
			
			
;;
;; do ({@var}*) (end-test-form . result-form*) declaration* . {tag | statement}*
;; @var :: var | (var [init-form [step-form]])
;;


*
(do 
	((j 0 (+ j 1)))  ; @var
	
	; without end-test-form and result-form, do forever
	(nil)
	 
	 
	;; statement
	(format t "~%input ~D:" j)
	
	;; declaration . statement
	(let ((item (read)))  ; declaration
	
	;; statement
	(if (null item) (return)
		(format t "~%output ~D: ~S" j item))))


a
input 0:
output 0: A
b
input 1:
output 1: B
cd
input 2:
output 2: CD
123456
input 3:
output 3: 123456
nil
input 4:
NIL


*
(do
	;; @var
	((n 10)
	 (i 0 (+ i 1))
	 (sum 0))

	;; end-test-form . result-form
	((> i n) sum)
	
	;; statement
	(if (= i 0) (format t "~&sum of (1+2+...+~D) is: " n))
		(setf sum (+ sum i)))
		
sum of (1+2+...+10) is: 
55


*
(do
	;; @var
	((n 10)
	 (i 1 (+ i 1))
	 (sum 0))

	;; end-test-form . result-form
	((> i n) sum)
	
	;; statement
	;; here, "when" is better than "if"
	(when (= i 1) (format t "~&sum of (1+2+...+~D) is: " n))
	(setf sum (+ sum i)))

sum of (1+2+...+10) is: 
55



(defun list-reverse (list)
	(do ((x list (cdr x))
	
	     ;; put the front item to the last position
		 (y '() (cons (car x) y)))
		 
		 ;; end-test-form . result-form
		 ((endp x) y))

		 
* 
(list-reverse x)
(5 4 3 2 1)


;;
;; do* ({@var}*) (end-test-form . result-form*) declaration* . {tag | statement}*
;; @var :: var | (var [init-form [step-form]])
;;
;; The syntax is like do, but their @var lexical binding is clearly different.
;;

*
(do* ((x 1 (1+ x))
		
	   ;; y-form always gets the latest x evaluated from the step-form of x-form
	   ;; this binding is done in the lexical stage.  
       (y 0 (1+ x)))
            
      ((= 5 y) y)
      
      (print y))

0
3 
4 
5


;; macro
;;
;; return [result]
;; @result:: a from
;;

;;
;; special operator
;;
;; return-from name [result]
;; @name:: a block tag; not evaluated
;;

;; 
;; tips:
;; (return) ==  (return-from nil)
;; (return form) ==  (return-from nil form)  
;;


*
(block nil (return) 1)

nil

*
(block nil (return 1) 2)

1

*
;; the block-tag "one" is given, so the @name of return-from must be given too.
(block one (return-from one 1))

1

*
(block nil (return-from nil nil))

nil

*
;; "return form" is the same with "return-from nil form"
(block nil (return nil))

nil


*
;; "return-from nil" == "return"
(block nil (return-from nil))

nil


;;
;; if "return" is replaced by "out"
;; out: move away from a place
;; return: got back to place
;;
*
(block one (out-from one 1))
(block nil (out 1))



;;
;; macros
;;
;; dotimes (var count-form [result-form]) declaration* {tag | statement}*
;;
;; dolist (var list-form [result-form]) declaration* {tag | statement}*
;;

*
(defun list-sum(lst)
	(setf x 0)
	
	;; @n starts from 0
	(dotimes (n (length lst) x) (incf x (nth n lst))))

* (list-sum '(1 2 3 4))

10


*
(defun list-sum(lst)
	(setf x 0)
	(dolist (v lst x) (incf x v)))

* (list-sum '(1 2 3))

6


;;
;; macro
;;
;; cond {clause}* => result*
;; clause ::= (test-form form*)
;;


;;
;; below example shows "cond" is very useful in error-processing situation.
;; it is like the "switch + case" in C language, but more brief.
;;
(defun err-show (id)
	(cond ((= id 1) "err: device init")
		  ((= id 2) "err: memory full")))


* (err-show 1)
"err: device init"

* (err-show 2)
"err: memory full"

* (err-show 0)
NIL


		 
	 
		 





			
			




	    
