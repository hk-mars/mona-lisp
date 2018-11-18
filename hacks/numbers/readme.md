
## Introduction

Common lisp provides numbers divided into four categories: 
1. integers
2. ratios
3. floating-point numbers
4. complex numbers

Many numeric functions are generic for any kind of number, some other functions
accept only certain kinds of numbers.

In general, numbers in common lisp are not true objects.
eq cannot be counted upon to operate on them reliably.
In particular, it is possible that the expression:

(let ((x z) (y z)) (eq x y))

may be false rather than true if the value of z is a number.


## Functions

### zerop number

It is true if @number is zero.

zerop -0.0 is always true.

### plusp number

It is true if the @number is greater than zero.

### minusp number

Just like plusp, It is true if the @number is less than zero.

### oddp integer

It is true if the @integer is odd (not divisible by 2).

### evenp integer

It is true if the @integer is even (divisible by 2).


### = number &rest more-numbers
### /= number &rest more-numbers
### < number &rest more-numbers
### > number &rest more-numbers
### <= number &rest more-numbers
### >= number &rest more-numbers

@=            all the same
@/=           all different
@<            monotonically increasing
@>            monotonically decreasing
@<=           monotonically nondecreasing
@>=           monotonically nonincreasing

If these functons satisfy with above certain condition, then return true, otherwise
return false.


### max number &rest more-numbers
### min number &rest more-numbers

The @number may be any non-complex numbers.
max returns the argument that is greatest.
min returns the argument that is least.


