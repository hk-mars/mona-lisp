
;;
;; cons
;;

* (car '())

NIL
* (cdr '())

NIL
* (cdr '(a b c))

(B C)
* (car '(a b c))

A
* (car 'nil)

* '(nil)

(NIL)
* (car '(nil))

nil

* (cons 'a 'b)

(A . B)
* (cons 'a '(b c d))

(A B C D)


* (cons 'a (cons 'b ()))

(A B)



* (tree-equal '(a b c) '(a b c))

T
* (equal "abc" "abc")

T



;;
;; lists
;;

* (setf x '())

NIL
* (endp x)

T
* (setf x '(a))

(A)

* (endp x)

NIL


* 

(list-length '())

0
* (list-length '(a b c d))

4
* (list-length '(a (b c) d))

3
* (let ((x (list 'a 'b 'c)))
	(rplacd (last x) x)
	(list-length x))

NIL


* 
(defun list-len (x)
	(do ((n 0 (+ n 2))
	     (fast x (cddr fast))
	     (slow x (cdr slow)))
	(nil)
	(when (endp fast) (return n))
	(when (endp (cdr fast)) (return (+ n 1)))
	;; If fast pointer eventually equals slow pointer,
	;; then we must be stuck in a circular list.
	(when (and (eq fast slow) (> n 0)) (return nil))))

LIST-LEN
* (list-len '())

0
* (list-len '(a b c))

3
* (let ((x (list 'a 'b 'c)))
	(rplacd (last x) x)
	(list-length x))

NIL


* 
(setf x '(a b c))

(A B C)
* x

(A B C)
* (nth 0 x)

A
* (nth 1 x)

B
* (nth 3 x)

NIL
* 

* (first x)

A
* (tenth x)

NIL

* (rest x)

(B C)

* (nthcdr (- (list-length x) 1) x)

(C)

* (last x)

(C)


* (setf x '(a b c d))

(A B C D)
* (last x)

(D)
* (last x 3)

(B C D)

* (list (list 'a 'b) x)

((A B) (A B C D)


* x       

(A B C D)
* (list (list 'a 'b) x)

((A B) (A B C D))
* (list* 'a 'b 'c)

(A B . C)
* (cdr (list* 'a 'b 'c))

(B . C)
* (cdr (cdr (list* 'a 'b 'c)))


* (make-list 5)

(NIL NIL NIL NIL NIL)
* (make-list 3 :initial-element 'hk)

(HK HK HK)


* (append x '(d) '() x)  

(A B C D D A B C D))


* (setf x'(a b c (d (e f))))

(A B C (D (E F)))
* (copy-list x)

(A B C (D (E F)))


* x

(A B C)
* (setq y '(d e f))

(D E F)
* (nconc x y)

(A B C D E F)

* x

(A (B C) D)
* (push 1 (cadr x))

(1 B C)

* x

(A (1 B C) D)



* x

(A (1 B C) D)
* x

(A (1 B C) D)
* (pop x)

A
* x

((1 B C) D)


* x

((1 B C) D)
* x

((1 B C) D)
* (butlast x)

((1 B C))
* (butlast x 1)

((1 B C))
* (butlast x 2)

NIL
* (butlast x 0)

((1 B C) D)


* (nbutlast x)

((1 B C))
* x

((1 B C))


* x

(A B C D E)
* (setf y (cdddr x)
)

(D E)
* (ldiff x y)

(A B C)




