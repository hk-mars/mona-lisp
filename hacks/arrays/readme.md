

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



