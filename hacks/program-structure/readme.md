

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



### Macros

### Function Calls


## Functions

### Named Functions

### Lambda-Expressions


## Top-Level Forms

### Defining Named Functions

### Declaring Global Variables and Named Constants

### Control of Time of Evaluation



