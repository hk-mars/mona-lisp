

;;
;; special forms
;;

* (quote 43)

43
* (setq a 43)

43

* (list (quote a))

(A)


* (defun adder (x) (function (lambda (y) (+ x y))))

ADDER

* (adder 1)

#<CLOSURE (LAMBDA (Y) :IN ADDER) {11D13EF5}>

* (funcall (adder 1) 2)

3

* (setq add1 (adder 1))

#<CLOSURE (LAMBDA (Y) :IN ADDER) {11D2E51D}>
* (funcall add1 2)


* (symbol-function 'push)

#<CLOSURE (LAMBDA (&REST SB-C::ARGS) :IN MACRO-FUNCTION) {10FA254D}>
* (symbol-function 'pop)

#<CLOSURE (LAMBDA (&REST SB-C::ARGS) :IN MACRO-FUNCTION) {10D5633D}>
* (symbol-function 'hack)

debugger invoked on a UNDEFINED-FUNCTION:
  The function COMMON-LISP-USER::HACK is undefined.
  
* x

NIL

* (boundp x)

T
* (boundp 'push)

NIL
* (boundp h)

debugger invoked on a UNBOUND-VARIABLE: The variable H is unbound.


* (fboundp 'push)

T


;;
;; assignment
;;

* (setq a 1 b 2)

2

* a

1
* b

2


;;
;; function invoation
;;

* (setq f '+

+
)
* (apply f '(1 2))

3
* (apply #'max '(1 2 3))

3


* (funcall f 1 2)

3


* call-arguments-limit

536870911


;;
;; simple sequencing
;;


* (progn (+ 1 2) (+ 3 4))

7
* (prog1 (+ 1 2) (+ 3 4))

3

* (prog2 (+ 1 2) (+ 3 4) (+ 5 6)) 

7
* (progn (+ 1 2) (prog1 (+ 3 4) (+ 5 6)))

7


