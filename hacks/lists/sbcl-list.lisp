
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


