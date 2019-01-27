

* ((lambda (a b)
	(+ a (* b 3)))
   4 5)
	
19


;;
;; &optional
;;
* ((lambda (a &optional (b 2))
	(+ a (* b 3)))
   4 5)

19


* ((lambda (a &optional (b 2))
	(+ a (* b 3)))
   4)

10


;;
;; &key
;;

* ((lambda (a b &key c d) 
	(list a b c d))
   1 2)

(1 2 NIL NIL)
* ((lambda (a b &key c d) 
	(list a b c d))
   1 2 :c 6)

(1 2 6 NIL)
* ((lambda (a b &key c d) 
	(list a b c d))
   1 2 :d 8 :c 9)

(1 2 9 8)


;;
;; &rest
;; Not clear of its syntax and usage 
;;






