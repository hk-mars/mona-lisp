

#Introduction

A cons, or dotted pair, is a compound data object having two components called the
car and cdr. Each component may be any lisp object. A list is a chain of conses 
linked by cdr fields, which is terminated by some atom(a non-cons object).
An ordinary list is terminated by nil, the empty list(also written ()).
A list whose cdr chain is terminated by some non-nil atom is called a dotted list.

endp => predicate for testing for the end of a list.


## Functions

### car list

It returns the car of list, which must be a cons or ().
List must satisfy the predicate listp.

(car '()) => nil
(car '(a b c)) => a

### cdr list

This returns the cdr of list.

(cdr '()) => nil
(cdr '(a b c)) => (b c)

### more
caar list 
cadr list 
cdar list 
cddr list 
caaar list 
caadr list 
cadar list 
caddr list 
cdaar list 
cdadr list 
cddar list 
cdddr list 
caaaar list 
caaadr list 
caadar list 
caaddr list 
cadaar list 
cadadr list 
caddar list 
cadddr list 
cdaaar list 
cdaadr list 
cdadar list 
cdaddr list 
cddaar list 
cddadr list 
cdddar list 
cddddr list

e.g.:
(cddadr x) is the same as (cdr (cdr (car (cdr x))))

Any of these functions may be used to specify a place for setf.


### cons x y

cons is the primitive function to create a new cons whose car is x and whose cdr is y.
e.g.:
(cons 'a 'b) => (a . b)

cons may be thought of as creating a list by adding a new element to the front of a 
list.


### tree-equal x y &key :test :test-not

This is a predicate that is true if x and y are isomorphic trees with identical leaves, that is, if x and y are atoms that satisfy the test, or if they are both conses and
their car's are tree-equal and their cdr's are tree-equal.
See equal, which does recursively compare certain other structured objects, such as
strings.


