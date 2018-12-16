
## Introduction

The very name "lisp" is an abbreviation for "list processing".

## Functions

### endp object

The predicate endp is the recommended way to test for the end of a list.
It is false of cons, true of nil, and an error for all other arguments.

### list-length list

It returns, as an integer, the length of list.
list-length differs from length when the list is circular, length may fail to return,
whereas list-length will return nil.


### nth n list

It returns the nth element of list.

### first list
### second list
### third list
### fourth list
### fifth list
### sixth list
### seventh list
### ninth list
### tenth list

These functions are sometimes convenient for accessing particular elements of a list.

setf may be used with each of these functions to store into the indicated position of
a list.

### rest list

rest means the same as cdr but complements first.

### nthcdr n list

It returns the nth cdr of the list.

(car (nthcdr n x)) == (nth n x)

### last list

It returns the last cons of list.

### last list &optional (n 1)

last returns the tail of the list consisting of the last n cons of list.
The list may be a dotted list. It is an error if the list is circular.
The argument n must be a non-negative integer.

### list &rest args
list constructs and returns a list of its arguments.


### list* arg &rest others

list* is like lit except that the last cons of the constructed list is "dotted."
The last argument to the list* is used as the cdr of the last cons constructed.

(list* 'a 'b 'c) => (a b . c) => (a (b c))

This is like
(cons 'a (cons 'b 'c))


### make-list size &key :initial-element

This creates and returns a list containing @size elements, each of which is 
initialized to the :initial-element argument (which defaults to nil).
@size should be a non-negative integer.


### append &rest lists

The arguments to appended are lists. The resuslt is a list.

### copy-list list

This returns a list that is equal to list, but not eq. Only the top level of list 
structure is copied. That is , copy-list copies in the cdr direction but not in the
direction. If the list is "dotted", that is, (cdr (last list)) is a non-nil atom, 
this will be true of the returned list also.
The new list is not in another memory area of the stack.


### revappend x y
(revappend x y) is exactly the same as (append (reverse x) y).
Bothe x and y should be lists. x is copied, not destroyed.


### nconc &rest lists

It returns a list that is the arguments concatenated together.
The arguments are changed rather than copied.


### nreconc x y

It is the same as (nconc (nreverse x) y).


### butlast list &optional n

This crates and returns a list with the same elements as list, excepting the last n
elements.
n => default value is 1.


### nbutlast list &optional n

This is the destructive version of butlast, the list would be changed.


### ldiff list sublist


The sublist should be a sublist of list.
ldiff means "list difference".





## Macro

### push item place

The form @place should be the name of a generalized variable containing a list;


### pushnew item place &key :test :test-not :key

The form @place should be the name of a generalized variable containing a list.


### pop place

The form @place should be the name of a generalized variable containing a list.
The result of pop is the car of the contents of place, and the cdr of the contents 
is stored back into @place.






