

## Introduction

Lisp programs are organized as forms and functions.

Forms are evaluated to produce values and side effects.

Functions are invoked by applying them to arguments.

The most important kind of form performs a function call; conversely, a function
performs computation by evaluating forms.


## Forms

### Self-Evaluating Forms
All numbers, characters, strings, and bit-vectors are **self-evaluating forms**.
When such an object is evaluated, that object is returned as the value of the form.
The empty list (), which is also the false value nil, is also a self-evaluating form(the 
value of nil is nil).
Keywords(symbols written with a leading colon) also evaluate to themselves(the value of 
:start is :start).


### Variables
Symbols are used as names of variables in Common Lisp programs.
When a symbols is evaluated as a form, the value of the variable it names is produced.
For example, after doning(setq items 3), which assigns the value 3 to the variable named
@items, then @items => 3.
Variables can be assigned to, as by setq, or bound, as by let.
Any program construct that binds a variable effectively saves the old value of the 
variable and causes it to have a new value, and on exit from the construct the old value
is reinstated.

There are tow kinds of variables in Common Lisp, called lexical(or static) variables and
special(or dynamic) variables.

The distinction between them is one of scope and extent.


### Special Forms

If a list is to be evaluated as a form, the first step is to examine the first element
of the list. If the first element is one of the symbols appearing in list-1, then the
list is called a special form.

**List-1: Names of All Common Lisp Special Forms**
- block
- if
- progv
- catch
- labels
- quote
- let
- return-from
- declare
- let*
- setq
- eval-when
- macrolet
- tagbody
- flet
- multiple-value-call
- the
- multiple-value-prog1
- throw
- go
- progn
- unwind-protect


### Macros

If a form is a list and the first element is not the name of a special form, it may be
the name of a macro; if so, the form is said to be a macro call.
A macro is essentially a function from forms to forms that will, given a call to that
macro, compute a new form to be evaluated in place of the macro call.
This computation is sometimes referred to as macro expansion. 
For example, the macro named @return will take a form such as (return x) and from that
from compute a new form (return-from nil x), we say that the old form expands into the
new form. The new form is then evaluated in place of the original form; the value of the
new form is returned as the value of the original form.

There are a number of standard macros in Common Lisp, and the user can define more by
using **defmacro**.


### Function Calls

If a list is to be evaluated as a form and the first element is not a symbol that names
a special form or macro, then the list is assumed to be a **function call**.
The first element of the list is taken to name a function.
Any and all remaining elements of the list are forms to be evaluated; one value is 
obtained from each form, and these values become the **arguments** to the function.
The function is then **applied** to the arguments. 
The functional computation normally produces a value, but it may instead call for 
a non-local exit, see **throw**.
A function that does return may produce no value or several values, see **values**.


## Functions

### Named Functions

### Lambda-Expressions


## Top-Level Forms

### Defining Named Functions

### Declaring Global Variables and Named Constants

### Control of Time of Evaluation



