* (setf number-of-widgets 10)

* (make-hash-table :size (* number-of-widgets 200)
		   :rehash-size 1.5)

#<HASH-TABLE :TEST EQL :COUNT 0 {11DFFF69}>
)

#<HASH-TABLE :TEST EQL :COUNT 0 {11DFFF69}>
* (setf x (make-hash-table :size (* number-of-widgets 200)
		           :rehash-size 1.5))

#<HASH-TABLE :TEST EQL :COUNT 0 {11E2AF69}>
* x

#<HASH-TABLE :TEST EQL :COUNT 0 {11E2AF69}>
* (hash-table-p x)

T


* (incf (gethash 'id x 0) 1) 

1

* (setf (gethash 'id x)
	(incf (gethash 'id x 0) 1))

2

*(remhash 'id x)

T


* (setf (gethash 'id-1 x) 1)
* (setf (gethash 'id-2 x) 2)
* (setf (gethash 'id-3 x) 3)

* (maphash #'(lambda (key val)
		(if (minusp val)
			(remhash key x)
			(setf (gethash key x) (sqrt val))))
	   x) 

#<HASH-TABLE :TEST EQL :COUNT 3 {11E2AF69}>
* (gethash 'id-2 x)

1.1892071
T
* (gethash 'id-3 x)

1.316074
T


* (clrhash x)  

#<HASH-TABLE :TEST EQL :COUNT 0 {11E2AF69}

#<HASH-TABLE :TEST EQL :COUNT 0 {11E2AF69}>
* (hash-table-count x)

0


(with-hash-table-iterator (show-turtle turtles)
	(labels ((try (show-one &optional key value)
			(when show-one
				(when (eq (first value) 'ninja)
					(format t "~%~:(~A~): ~{~A~^, ~}"
						key (rest value)))
				(multiple-value-call #'try (show-turtle)))))
		(multiple-value-call #'try (show-turtle))))

Leonardo: LEADER, BLUE
Donatello: MACHINES, PURPLE
Raphael: COOL, RUDE, RED
Michaelangelo: PARTY-DUDE, ORANGE
NIL


* (hash-table-rehash-size x)   

1.5
* (hash-table-rehash-threshold x)

1.0
* (hash-table-size x)         

2000
* (hash-table-test x)

EQL

* (hash-table-test (make-hash-table :test #'equal))

EQUAL


;;
;; sxhash function
;;

* (setf x (make-hash-table :size 10))

#<HASH-TABLE :TEST EQL :COUNT 0 {11E97781}>
* x

#<HASH-TABLE :TEST EQL :COUNT 0 {11E97781}>
* (sxhash x)

316863442
* (setf y (make-hash-table :size 10))

#<HASH-TABLE :TEST EQL :COUNT 0 {11EAA781}>
* (sxhash y)

316863442
* (equal x y)

NIL
* (= (sxhash x) (sxhash y))

T

 
