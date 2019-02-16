
;;
;; macro definition
;;


* (defmacro nil! (var)
	(list `setq var nil))  ; 'setq as symbol

NIL!
* (nil! x)

nil

* x

NIL

* (defmacro len (var)
	(length var))

STYLE-WARNING:
   LEN is being redefined as a macro when it was previously defined to be a function.
STYLE-WARNING: redefining COMMON-LISP-USER::LEN in DEFMACRO

LEN
* (len '(1 2))

2


* `(a ,b c)

(A 2 C)
* b

2
* (list 'a b 'c)

(A 2 C)

* (defmacro nil! (var)
	`(setq ,var nil))
STYLE-WARNING: redefining COMMON-LISP-USER::NIL! in DEFMACRO

NIL!

* `(a ,b c ,d) 

(A 2 C 1)
* (list 'a b 'c d)

(A 2 C 1)
* `(,a ,b c ,d)

(10 2 C 1)
* c

debugger invoked on a UNBOUND-VARIABLE: The variable C is unbound.

* `(a ,b ,c ,d)

debugger invoked on a UNBOUND-VARIABLE: The variable C is unbound.


* (defmacro while (test &body body)
	; do is a macro
	`(do ()
	     ((not, test))
	    ,@body))

WHILE


 
