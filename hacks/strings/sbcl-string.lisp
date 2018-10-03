
* (char "F...hello" 0)

#\F
* (char "F...hello" 1)

#\.

* (aref (the string "F...hello") 1)

#\.

* (setf str "hello-world")

"hello-world"
* str

"hello-world"
* (setf (char str 0) #\H)

#\H
* str

"Hello-world"


*(string= "foo" "foo")

T
* (string= "foo" "Foo")

NIL
* (string= "foo" "bar")

NIL
* (string= "together" "frog" :start1 1 :end1 3 :start2 2)

T

* (string-equal "foo" "Foo")

T


* (string> "bbc" "abc")

0 ; as true

* 
NIL
* (string< "abc" "abb")

NIL ; as false




