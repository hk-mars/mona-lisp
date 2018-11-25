
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

"="            all the same

"/="           all different

"<"            monotonically increasing

">"            monotonically decreasing

"<="           monotonically nondecreasing

">="           monotonically nonincreasing

If these functons satisfy with above certain condition, then return true, otherwise
return false.


### max number &rest more-numbers
### min number &rest more-numbers

The @number may be any non-complex numbers.
max returns the argument that is greatest.
min returns the argument that is least.

### + &rest numbers

This returns the sum of numbers.

### - number &rest more-numbers

The function -, when given only one argument, returns the negative of that argument.
The function -, when given more than one argument, successively subtracts from the 
first argument all the others,and returns the result. For example, (- 3 4 5) => -6.


### * number &rest more-numbers

This returns the product of the arguments. If there are no arguments, the result is 1.

### / number &rest more-numbers

successively divides the first argument by all the others and returns the result.

### 1+ number
### 1- number

(1+ x) is the same as (+ x 1).
(1- x) is the same as (- x 1).

### conjugate nunber

This returns the complex conjugate of number.

### gcd &rest integers

This returns the greatest common divisor of the intergers.

### lcm integer &rest more-integers

This returns the least common multiple of intergers.


### float number &optional other
This converts any non-complex number to a floating-point number. 

### rational number
### rationalize number
These functions converts any non-complex number to a rational number.

### numerator rational
### denominator rational
These functions take a rational number (an integer or ratio) and return as an integer
the numerator or denominator.


### floor number &optional divisor 
### ceiling number &optional divisor 
### truncate number &optional divisor 
### round number &optional divisor
floor converts its argument by truncating toward negative infinity;
ceiling converts its argument by truncating toward positive infinity;
truncate converts its argument by truncating toward zero;
round converts its argument by rounding to the nearest integer;


### mod number divisor 
### rem number divisor
mod performs the operation floor on its two arguments and returns the second result 
of floor.
similarly for rem.

(mod 13 4) => 1                 (rem 13 4) => 1

(mod -13 4) => 3                (rem -13 4) => -1 

(mod 13 -4) => -3               (rem 13 -4) => 1 

(mod -13 -4) => -1              (rem -13 -4) => -1 

(mod 13.4 1) => 0.4             (rem 13.4 1) => 0.4 

(mod -13.4 1) => 0.6            (rem -13.4 1) => -0.4


### complex realpart &optional imagpart
### realpart number
### imagpart number

Functions for creating complex number and get part of it.



### logior &rest integers
### logxor &rest integers
### logand &rest integers
### logeqv &rest integers

### lognand integer1 integer2 
### lognor integer1 integer2 
### logandc1 integer1 integer2 
### logandc2 integer1 integer2 
### logorc1 integer1 integer2 
### logorc2 integer1 integer2

These bit-wise logical operations on two integers are summarized in the following table:

----------------------------------------------------------------
integer1        0       0       1       1 
integer2        0       1       0       1       Operation Name 
----------------------------------------------------------------
logand          0       0       0       1       and 
logior          0       1       1       1       inclusive or 
logxor          0       1       1       0       exclusive or 
logeqv          1       0       0       1       equivalence (exclusive nor) 
lognand         1       1       1       0       not-and 
lognor          1       0       0       0       not-or 
logandc1        0       1       0       0       and complement of integer1 with integer2 
logandc2        0       0       1       0       and integer1 with complement of integer2 
logorc1         1       1       0       1       or complement of integer1 with integer2 
logorc2         1       0       1       1       or integer1 with complement of integer2 

## Macro

### incf place [delta]
### decf place [delta]

The number produced by the form delta is added to (incf) or subtracted from (decf) 
the number.


### logcount integer
The number of bits in integer is determined and returned.

(logcount 13) => 3      ;Binary representation is ...0001101

