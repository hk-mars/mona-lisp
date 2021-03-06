
## Introduction

A predicate is a **function** that tests for some condition involving its arguments and 
returns nil if the condition is false, or some non-nil value if the condition is true.

One may think of a predicate as producing a Boolean value, where nil stands for false and
anything else stands for true. Conditional control structures such as **cond**, **if**,
**when**, and **unless** test such Boolean values.

By convention, the names of predicates usually end in the letter p ("predicate").


## Logical Values

The names **nil** and **t** are constants in Common Lisp.
Although they are symbols like any other symbols, and appear to be treated as variables
when evaluated.

- nil

The value of nil is always nil.
This object represents the logical false value and also the empty list. It can also be
written as ().

- t
The value of t is always t.

## Data Type Predicates

Those predicates that deal with data types, given a data object one can determine whether
or not it belongs to a given type, or one can compare two type specifiers.

### general type predicates

If a data type is viewed as the set of all objects belonging to the type, then the 
**typep** function is a set membership test, while **subtypep** is a subset test.

#### functions

- typep object type

The return is true if **object** is of type **type**.

Note that an object can be "of" more than one type, since one type can include another.

Let x be an object such that (typep x type) is true and type is one of the following:

array           float         package         sequence 
bit-vector      function      pathname        short-float 
character       hash-table    random-state    single-float 
complex         integer       ratio           stream 
condition       long-float    rational        string 
cons            null          readtable       symbol 
double-float    number        restart         vector


- subtypep type1 type2

The two type specifiers are compared, the return is true if **type1** is a subtype of 
**type2**, but if it is nil, then type1 **may or may not** be a subtype of type2.

There are three possible result combinations:

t    t    type1 is definitely a subtype of type2
nil  t    type1 is definitely not a subtype of type2
nil  nil  subtypep could not determine the relationship


### specific data type predicates

#### functions

- null object

- symbolp object

- atom object

- consp object

- listp object

- numberp object

- integerp object

- rationalp object

- floatp object

- realp object

- complexp object

- characterp object

- stringp object

- bit-vector-p object

- vectorp object

- simple-vector-p object

- simple-string-p object

- simple-bit-vector-p object

- arrayp object

- packagep object

- functionp object

- compiled-function-p object

It is true if its argument is any compiled code object.

- commonp object

It is true if its argument is any standard Common Lisp data type.


For above all predicates, they are evaluated by the same expression:

> (XXp obj) == (typep obj 'XX)


## Equality Predicates

### functions

- eq x y

It is true if and only if x and y are **the same identical object**.

Implementationally, x and y are usually eq if and only if they address the same identical 
**memory location**.

(eq 'a 'b) is false. 
(eq 'a 'a) is true. 
(eq 3 3) might be true or false, depending on the implementation. 
(eq 3 3.0) is false.
(eq #\A #\A) might be true or false, depending on the implementation. 

- eql x y

It is ture if its arguments are **eq**, or if they are numbers of the same type with the
same value, of if they are character objects that represent the same character.

(eql 'a 'b) is false. 

(eql 'a 'a) is true. 

(eql 3 3) is true. 

(eql 3 3.0) is false. 

(eql 3.0 3.0) is **true** (the same type with the same value)

(eql #\A #\A) is **true** (character objects that represent the same character)


- equal x y

It is true if its arguments are structurally similar(in form) objects.

Two objects are equal if and only if their printed representations are the same.


Two arrays are equal only if they are eq, with one exception: strings and bit-vectors 
are compared element-by-element. If either argument has a fill pointer, the fill pointer 
limits the number of elements examined by equal. Uppercase and lowercase letters in strings 
are considered by equal to be distinct. 
(**In contrast, equalp ignores case distinctions in strings**)

(equal 'a 'b) is false. 

(equal 'a 'a) is true. 

(equal 3 3) is true. 

(equal 3 3.0) is false. 

(equal 3.0 3.0) is true. 

(equal #\A #\A) is true. 

(equal "Foo" "Foo") is true.

(equal "FOO" "foo") is false.

- equalp x y

Two objects are equalp if they are **equal**.

Objects that have components are equalp if they are of the same type and corresponding 
components are equalp. 

Two symbols can be equalp only if they are eq, that is, the same identical object.

Two arrays are equalp if and only if they have the same number of dimensions.

To clarify that otherwise equalp never recursively descends any structure or data type 
other than the ones: conses, arrays (including bit-vectors and strings), and pathnames. 
Numbers are compared for numerical equality (see =), 
characters are compared as if by char-equal, 
and all other data objects are compared as if by eq.


(equalp 'a 'b) is false.

(equalp 'a 'a) is true. 

(equalp 3 3) is true. 

(equalp 3 3.0) is true. 

(equalp 3.0 3.0) is true.

(equalp #\A #\A) is true. 

(equalp "Foo" "Foo") is true. 

(equalp "Foo" (copy-seq "Foo")) is true. 

(equalp "FOO" "foo") is true.


## Logical Operators

### functions

- not x

not returns t if x is nil, otherwise returns nil.

### Macros

- and {form}*

(and form1 form2 ... ) evaluates each form, one at a time, from left to right. 
If any form evaluates to nil, the value nil is immediately returned without evaluating 
the remaining forms. 

```lisp
(and x y z ... w) == (cond ((not x) nil) 
			    ((not y) nil) 
			    ((not z) nil) 
			    ...
			    (t w))		   
```
			   
- or {form}*

(or form1 form2 ... ) evaluates each form, one at a time, from left to right. 
If any form other than the last evaluates to something other than nil, or immediately 
returns that non-nil value without evaluating the remaining forms. 

```lisp
(or x y z ... w) == (cond (x) (y) (z) ... (t w))
```



