

* (make-array 5)

#(0 0 0 0 0)
* (make-array '(3 4))

#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))
* (make-array '(3 4) :element-type '(mod 16))

#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))
* (make-array 5 :element-type 'single-float)

#(0.0 0.0 0.0 0.0 0.0)


;;
;; vector vs list
* (vector 1 2 3)

#(1 2 3)
* (list 1 2 3)

(1 2 3)
* (car (vector 1 2 3))

debugger invoked on a TYPE-ERROR: The value #(1 2 3) is not of type LIST.


;;
;; aref
;;

* (setf x (make-array 5))
#(0 0 0 0 0)

* (aref x 1)

0
* (setf (aref x 0) 1)

1
* x

#(1 0 0 0 0)


;;
;; svref
;;

* (setf y (vector 1 2 3 4 5))

#(1 2 3 4 5)
* y

#(1 2 3 4 5)
* (setf (svref y 1) 0)

0
* y

#(1 0 3 4 5)


;;
;; array info. functions
;;

* (setf x (make-array '(1 2 3)))

;     (SETF X (MAKE-ARRAY '(1 2 3)))
; ==>
;   (SETQ X (MAKE-ARRAY '(1 2 3)))
; 
; caught WARNING:
;   undefined variable: X
; 
; compilation unit finished
;   Undefined variable:
;     X
;   caught 1 WARNING condition

#3A(((0 0 0) (0 0 0)))
* (array-dimensions x)

(1 2 3)
* (array-total-size x)

6
* (array-rank x)

3

* (adjustable-array-p x)

NIL

