
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

### 
first list
second list
third list
fourth list
fifth list
sixth list
seventh list
ninth list
tenth list

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


