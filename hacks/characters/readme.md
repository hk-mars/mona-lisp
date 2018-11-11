
# Introduction

Common lisp provides a character data type,objects of this type represent printed
symbols such as letters.

In general, characters in common lisp are not true objects,eq cannot be counted
upon to operate on them reliably.
In particular, it's possible that the expression:

(let ((x z) (y z) (eq x y))

may be false rather than true, if the value of z is a character.


# Functions

## standard-char-p char

standard-char-p is true if the argument is a ``standard character,'' that is, an object
of type standard-char.

## graphic-char-p char

graphic-char-p is true if the argument is a ``graphic'' (printing) character, and falseif it is a ``non-graphic'' (formatting or control) character. 

Any character with a non-zero bits attribute is non-graphic.

## alpha-char-p char

alpha-char-p is true if the argument is an alphabetic character, and otherwise is false

## upper-case-p char
## lower-case-p char

upper-case-p is true if the argument is an uppercase character, and otherwise is false.
lower-case-p is true if the argument is a lowercase character, and otherwise is false.

## digit-char-p char &optional (radix 10)

If char is not a digit of the radix specified by radix, then digit-char-p is false.

## alphanumericp char

alphanumericp is true if char is either alphabetic or numeric.

## char= character &rest more-characters 
## char/= character &rest more-characters 
## char< character &rest more-characters 
## char> character &rest more-characters 
## char<= character &rest more-characters 
## char>= character &rest more-characters

These functions compare the objects using the implementation-dependent total ordering
on characters,analogous to numeric comparisons by = and related functions.

## char-equal character &rest more-characters 
## char-not-equal character &rest more-characters 
## char-lessp character &rest more-characters 
## char-greaterp character &rest more-characters 
## char-not-greaterp character &rest more-characters 
## char-not-lessp character &rest more-characters


The predicate char-equal is like char=, and similarly for the others, except according 
to a different ordering such that differences of bits attributes and case are ignored, 
and font information is taken into account in an implementation-dependent manner. 


## char-upcase char
## char-downcase char

The @char must be a character object.
char-upcase attempts to convert the @char to an uppercase char, and char-downcase is
for converting to lowercase char.


## digit-char weight &optional (radix 10) (font 0)

It determines whether or not it is possible to construct a character object whose
font attribute is @font, and whose @code is such that the result character has the 
@weight when considered as digit of the radix(digit-char-p).
It returns such a character if that is possible, otherwise returns nil.

If @font is zero, radix is between 2 and 36 inclusive, and @weight is non-negative and
less than @radix.


## char-int char

char-int returns a non-negative encoding the character object.


## char-name char

If the character has a name, then that name (a string) is returned.


## name-char name

If the name is the same as the name of char object, that object is returned,
otherwise nil is returned.


