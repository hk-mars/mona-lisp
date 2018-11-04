

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

#3A(((0 0 0) (0 0 0)))
* (array-dimensions x)

(1 2 3)
* (array-total-size x)

6
* (array-rank x)

3

* (adjustable-array-p x)

NIL



;;
;; bit-arrays functions
;;

* (setf x #*1100)

;     (SETF X #*1100)
; ==>
;   (SETQ X #*1100)
; 
; caught WARNING:
;   undefined variable: X
; 
; compilation unit finished
;   Undefined variable:
;     X
;   caught 1 WARNING condition

#*1100
* x

#*1100
* (setf y #*1010)

#*1010

* (array-total-size x)

4
* (bit x 1)

1
* (bit x 0)

1
* (bit x 2)

0
* (bit x 3)

0
* (bit x 4)

debugger invoked on a SB-INT:INVALID-ARRAY-INDEX-ERROR:
  Index 4 out of bounds for (SIMPLE-BIT-VECTOR 4), should be nonnegative and <4.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(SB-INT:INVALID-ARRAY-INDEX-ERROR #*1100 4 4 NIL)
0] 0

* (bit-and x y)

#*1000
* (bit-xor x y)

#*0110
* (bit-not x)

#*0011
* x       

#*1100

* (bit-not x y)

#*0011
* y

#*0011


