

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


