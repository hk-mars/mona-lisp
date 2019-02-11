

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



;;
;; variables bingding
;;

* (let ((x 1)
	(y 2)
	(z 3))
     (setq r (+ x y))
     r)

3


* x       

1
* (let ((x 2)
	(y (+ x 1))) ; x is 1
     y)

2

* (let* ((x 2)
	 (y (+ x 1))) ; x is 2
     y)

3

  

* (setq longlist '(4 4))

(4 4)
* (flet ((safesqrt (x) (sqrt (abs x)))) 
  ;; The safesqrt function is used in two places. 
  (safesqrt (apply #'+ (map 'list #'safesqrt longlist))))

2.0



;;
;; iteration
;;


* (loop
	(setq x 1)
	(when (eq x 1) (return)))

nil


* (setf x 10)

10

* (loop
	(when (eq x 0) (return))
	(print x)
	(setf x (- x 1)))

10 
9 
8 
7 
6 
5 
4 
3 
2 
1 
NIL

