
* (defstruct (starbulks)
	     "Coffee time!"
	     where
	     who
	     coffees
	     hack-lisp)

STARBULKS


* (setf star  (make-starbulks :who "m." :coffees "americano"))

#S(STARBULKS :WHERE NIL :WHO "m." :COFFEES "americano" :HACK-LISP NIL)


* (starbulks-who star)

"m."



