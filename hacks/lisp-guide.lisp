
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



