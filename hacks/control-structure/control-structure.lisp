

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


* (do ((x 0 (+ x 1))
       (y 5 (- y 1)))
      ((= x 3) y)
      (print y)
      (print 'working))

5 
WORKING 
4 
WORKING 
3 
WORKING 
2



* (do ((x 0 (+ x 1))
       (y 5 (+ x 1)))
      ((= x 3) y)
      (print y)
      (print 'working))

5 
WORKING 
1 
WORKING 
2 
WORKING 
3


* (do* ((x 0 (+ x 1))
       (y 5 (+ x 1)))
       ((= x 3) y)
       (print y)
       (print 'working))

5 
WORKING 
2 
WORKING 
3 
WORKING 
4


* (dolist (x '(a b c))
	  (print x))

A 
B 
C 
NIL





* (mapcar #'abs '(-1 -2 -3 4))

(1 2 3 4)

* (maplist #'(lambda (x) (cons 'e x))
           '(a b c d))

((E A B C D) (E B C D) (E C D) (E D))


* (dotimes (x 10)
	   (print x))

0 
1 
2 
3 
4 
5 
6 
7 
8 
9 
NIL



;;
;; program feature
;;

* (tagbody
	(setf x 1)
    case-1
	(when (= x 0) (print x)) 

    case-2
	(when (= x 1) (print x)))

1



;;
;; multiple values
;;


* (defun polar (x y) 
  (values (sqrt (+ (* x x) (* y y))) (atan y x))) 

POLAR
* (multiple-value-bind (r theta) (polar 3.0 4.0) 
  (vector r theta)) 

#(5.0 0.9272952)
* (polar 3 4)

5.0
0.9272952
* (values 1 2 4)

1
2
4
* (values-list (list 1 2 3))

1
2
3
* multiple-values-limit

536870911
* (values-list '(1 2 3))

1
2
3
* (multiple-value-list (floor -3 4))

(-1 1)

* (multiple-value-call #'* (+ 1 2) (- 3 4))

-3
* (multiple-value-bind (x y z) (floor 5 3) (list x y z))

(1 2 NIL)

* (multiple-value-setq (x y) (floor 5 3))
1
* x

1
* y

2

* (nth-value 1 (floor 5 3))

2
* (nth 1 (multiple-value-list (floor 5 3)))

2


* (multiple-value-prog1 (abs -1) (abs -2) (abs -3))

1


