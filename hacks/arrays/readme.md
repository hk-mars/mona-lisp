

# Introduction

An array is an object with components arranged according to a rectilinear coordinate
system.
In principle, an array in common lisp may have any number of dimensions, including
zero (A zero-dimensional array has exactly one element.)
In practice, an implementation may limit the number of dimensions supported, but
every common lisp implementation MUST support arrays of up to seven dimensions.


An array may be a general array, meaning each element could be any lisp object,
or it may be a specialized array, meaning each element MUST be of a given restricted
type.


# Create Array

## Functions

### make-array dimensions &key :element-type : initial-element :initial-contents 
###            :adjustable :fill-pointer :displaced-to :displaced-index-offset


### array-dimension-limit

The value of array-dimension-limit is a positive integer that is the upper exclusive 
bound on each individual dimension of an array. This bound depends on the implementation but will not be smaller than 1024.  

### vector &rest objects

It is a convenient means for creating a simple general vector with specified initial
contents.

(vector v1 v2 ... vn ) 
   == (make-array (list n) :element-type t 
             :initial-contents (list v1 v2 ... vn))

It is analogous to the function list.

# Access Array


## Functions

### aref array &rest subscripts

THis accesses and returns the element of array specified by the subscripts.
The number of subscripts must equal the rank of the array, and each subscript must
be a non-negative integer less than the corresponding array dimension.

aref is unusual among the functions that operate on arrays in that it completely 
ignores fill pointers.
aref can access without error any array element, whether active or not.

setf may be used with aref to destructively replace an array element with a new value.


### svref simple-vector index


svref is identical to aref except that it requires its first argument to be simple
vector.


# Array Info.

## Functions

### array-element-type array

It returns a type specifier for the set of objects that can be stored in the array.


### array-rank array

This returns the number of dimensions(axes) of array.

### array-dimension array axis-number

The length of dimension number axis-number of the array is returned.


### array-dimensions array

It returns a list whose elements are the dimensions of array.

### array-total-size array

It returns the total number of elements in the array for all the dimensions.


### adjustalbe-array-p array

This predicate is true if the argument is adjustable.


