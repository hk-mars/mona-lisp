
;;
;; general type predicates 
;;

* (typep x 'number)

T
* (setf x 1)

1
* (setf y 1.0)

1.0
* (setf s "hello")

"hello"
* (setf tb '(1 2 3))

(1 2 3)
* (typep x 'number)

T
* (typep y 'float)

T
* (typep s 'string)

T
* (typep tb 'list)

T
* (typep x 'float)

NIL


* (subtypep '(array a) '(array b))

NIL
NIL
* (subtypep '(array a) 'array)

T
T


;;
;; specific data type predicates
;;

* (floatp x)

NIL
* (floatp y)

T
* (characterp s)

NIL
* (stringp s)

T

* (functionp 'lambda)

NIL
* (functionp 'functionp)

NIL

* (functionp 'list)

NIL
* (functionp 'a)

NIL
* (functionp 'funcall)

NIL

* (functionp (lambda (a b) (+ a (* b 3))))

T


;;
;; equality predicates
;;

* (eq x x)

T
* (eq 'a 'b)

NIL
* (eq 'a 'a)

T
* (eq 3.3 3.3)

NIL
* (eql x x)

T
* (eql 3.3 3.3)

T
* (eql 'a 'a)

T
* (eql #\a #\a)

T

* (eql "Foo" "Foo")

NIL
* (equal "Foo" "Foo")

T

* (equal "Foo" "foo")

NIL
* (equalp "Foo" "foo")

T


;;
;; logical operatiors
;;


* (setf x nil)

NIL
* (not x)

T
* (setf x 'a)

NIL
* (not x)

NIL

* (and t
	(princ "hi"))
hi
"hi"
* (and nil
	(printc "hi"))

NIL


