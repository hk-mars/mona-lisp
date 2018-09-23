## signaling errors

* (defun factorial (x)
	(cond ((or (not (typep x 'integer)) (minusp x))
		(error "~S is not a valid argument."  
			x))
		((zerop x) 1)
		(t (* x (factorial (- x 1))))))

FACTORIAL
* (factorial 20)

2432902008176640000
* (factorial -1)

debugger invoked on a SIMPLE-ERROR: -1 is not a valid argument.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(FACTORIAL -1)
0]



(defun factorial (x)
	(cond ((not (typep x 'integer))
		(error "~S is not a valid argument."  
			x))
		((minusp x)
		 (let ((x-cb (- x)))
			(cerror "compute -(~D!) instead."
				"(-~D)! is not defined." x-cb)
			(- (factorial x-cb))))
		((zerop x) 1)
		(t (* x (factorial (- x 1))))))

* (factorial -3)

debugger invoked on a SIMPLE-ERROR: (-3)! is not defined.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE] compute -(3!) instead.
  1: [ABORT   ] Exit debugger, returning to top level.

(FACTORIAL -3)
0] 0

-6


## trapping errors

* (ignore-errors (open filename :direction :input))

; in: IGNORE-ERRORS (OPEN FILENAME :DIRECTION :INPUT)
;     (OPEN FILENAME :DIRECTION :INPUT)
; 
; caught WARNING:
;   undefined variable: FILENAME
; 
; compilation unit finished
;   Undefined variable:
;     FILENAME
;   caught 1 WARNING condition

NIL
#<SB-INT:SIMPLE-FILE-ERROR "~@<~?: ~2I~_~A~:>" {11C838D9}>


* (setq filename "nosuchfile")

; in: SETQ FILENAME
;     (SETQ FILENAME "nosuchfile")
; 
; caught WARNING:
;   undefined variable: FILENAME
; 
; compilation unit finished
;   Undefined variable:
;     FILENAME
;   caught 1 WARNING condition

"nosuchfile"
* (handler-case (open filename :direction :input)
	(error (condition)
		(values nil condition)))

; in: HANDLER-CASE (OPEN FILENAME :DIRECTION :INPUT)
;     (OPEN FILENAME :DIRECTION :INPUT)
; 
; caught WARNING:
;   undefined variable: FILENAME
; 
; compilation unit finished
;   Undefined variable:
;     FILENAME
;   caught 1 WARNING condition

NIL
#<SB-INT:SIMPLE-FILE-ERROR "~@<~?: ~2I~_~A~:>" {11CC6F79}>


* (makunbound 'filename)

FILENAME
* (handler-case (open filename :direction :input)
	(file-error (condition)
		(values nil condition)))
; 
; caught WARNING:
;   undefined variable: FILENAME
; 
; compilation unit finished
;   Undefined variable:
;     FILENAME
;   caught 1 WARNING condition

debugger invoked on a UNBOUND-VARIABLE: The variable FILENAME is unbound.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

((LAMBDA ()))


### handling conditions

(defmacro without-arithmetic-errors (&body forms) 
  (let ((tag (gensym))) 
    `(block ,tag 
       (handler-bind ((arithmetic-error 
                         #'(lambda (c)     ;Argument c is a condition 
                             (return-from ,tag (values nil c))))) 
         ,@body)))) 
; in: DEFMACRO WITHOUT-ARITHMETIC-ERRORS
;     (DEFMACRO WITHOUT-ARITHMETIC-ERRORS (&BODY FORMS)
;       (LET ((TAG (GENSYM)))
;         `(BLOCK ,TAG
;            (HANDLER-BIND (#)
;              ,@BODY))))
; --> PROGN EVAL-WHEN SB-C::%DEFMACRO FUNCTION LET* 
; ==>
;   (LET* ((FORMS (CDR #:WHOLE0)))
;     (BLOCK WITHOUT-ARITHMETIC-ERRORS
;       (LET ((TAG #))
;         `(BLOCK ,TAG
;            (HANDLER-BIND #
;              ,@BODY)))))
; 
; caught STYLE-WARNING:
;   The variable FORMS is defined but never used.
; 
; caught WARNING:
;   undefined variable: BODY
; 
; compilation unit finished
;   Undefined variable:
;     BODY
;   caught 1 WARNING condition
;   caught 1 STYLE-WARNING condition
STYLE-WARNING:
   redefining COMMON-LISP-USER::WITHOUT-ARITHMETIC-ERRORS in DEFMACRO
WITHOUT-ARITHMETIC-ERROR



