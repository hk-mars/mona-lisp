

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
There are two ways to name a function:
1. Use a symbol that names the function.
2. Use a lambda-expression, which is a list whose first element is the symbol **lambda**.

A lambda-expression is not a form; it cannot be meaningfully evaluated.

These two ways should be distinguished from the treatment of symbols and lambda-expressions
as function objects, objects that satisfy the predicate **functionp**, as when giving such
an object to **apply** or **funcall**.


### Named Functions

A name can be given to a function in one of two ways.
- a **global** name
- a **local** name


### Lambda-Expressions

A lambda-expression is a list with the following syntax:

```lisp
(lambda lambda-list . body)
```

The first element MUST be the symbol **lambda**.
The second element MUST be a list, called the **lambda-list**, specifies names for the
**parameters** of the function.

When the function denoted by the lambda-expression is applied to arguments, the arguments
are matched with the parameters specified by the lambda-list.
The **body** may then refer to the arguments by using the parameter names.
The **body** consists of any number of forms, these forms are evaluated in sequence, and
the results of the last form only are returned as the results of the application.

The complete syntax of a lambda-expression is:

```lisp
(lambda ({var}*
         [&optional {var | (var [initform [svar]])}*]
         [&rest var]
         [&key {var | ({var | (keyword var)} [initform [svar]])}*
                [&allow-other-keys]]
         [&aux {var | (var [initform])}*)]
   [[{declaration}* | documentation-string]]
   {form}*)
```
   
   



## Top-Level Forms

The standard way for the user to interact with a Common Lisp implementation is via a 
**real-eval-print** loop.

The system repeatedly reads a form from some input source, evaluates it, and prints the
value(s) to some output sink(screen or file). Any form is acceptable.

### Defining Named Functions

The **defun** special form is the usual means of defining named functions.

[**Macro**]
```lisp
defun name lambda-list [[ {declaration}* | doc-string]] {form}*
```

Evaluating a @defun form causes the symbol @name to be a global name for the function
specified by the lambda-expression:

 ```lisp
 (lambda lambda-list {declaration | doc-string}* {form}*)
 ```

### Declaring Global Variables and Named Constants

#### Macro
##### defvar name [initial-value] [documentation]
##### defparameter name initial-value [documentation]
##### defconstant name initial-value [documentation]
**defvar** is the recommended way to declare the use of a special variable in a program.

```lisp
(defvar variable)
```
proclaims **variable** to be **special**, and can perform other system-dependent bookkeeping
actions.

If the second argument **initial-value** is supplied, then **variable** is initialized to
the result of evaluating the form **initial-value** unless it already has a value.
The **initial-value** form is not evaluated unless it is used; this fact is useful if 
evaluation of the **initial-value** form does something expensive like creating a large
data structure.

**defparameter** is similar to **defvar**, but it prequires an **initial-value** form,
always evaluates the form, and assigns the result to the variable.

**defconstant** is like **defparameter** but does assert that the value of the variable
is **fixed**.

The **documentation** string is not evaluated but must appear as a literal string when 
the form is evaluated.

For example, the form:

```lisp
(defvar *avoid-registers* nil "Compilation control switch #43")
```
is legitimate, but

```lisp
(defvar *avoid-registers* nil
	(format nil "Compilation control switch #~D"
			(incf *compiler-switch-number*)))
```
is erroneous because the all to **format** is not a literal string.

On the other hand, the form

```lisp
(defvar *avoid-registers* nil
	#.(format nil "Compilation control switch #~D"
				(incf *compiler-switch-number*)))
```
be used to accomplish the same purpose, because the call to **format** is evaluated at
**read** time.


### Control of Time of Evaluation



