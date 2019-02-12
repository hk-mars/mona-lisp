
## Introduction

Common Lisp provides a variety of special structures for organizing programs.

Some have to with flow-of-control(control structures), while others control access to 
variables(environment strutures).

Some of these features are implemented as special forms; others are implemented as macros,
which typically expand into complex program fragments expressed in terms of special forms
or other macros.

## Constants and Variables

### reference

The value of an ordinary variable may be obtained simply by writing the name of the variable
as a form to be executed.

Whether this is treated as the name of a special variable or a lexical variable is 
determined by the presence of absence of an applicale **special** declaration.

#### special forms

- quote object

(quote x) simply returns x.

This **object** is not evaluated and may be any lisp object.

This construct allows any lisp object to be written as a constant value in a program.


- function fn

The value of **function** is always the functional interpretation of **fn**.

**fn** is interpreted as if it had appeared in the functional invocation.

#### functions

- symbol-value symbol

It returns the current global function definition named by **symbol**.

This function is **particularly useful** for implementing **interpreters** for languages
**embedded** in Lisp.


- boundp symbol

It is true if the dynamic (special) variable named by the **symbol** has a value.

- fboundp symbol

It is true if the symbol has a global definition.


### assignment

This facility allows the value of a variable to be altered.

#### special forms

- setq {var form}*

The special form **(setq var1 form1 var2 form2 ...)** is the "simple variable assignment 
statement" of Lisp. 


## Generalized Variables



## Function Invocation

The most primitive form for function in lisp has **no name**; any list that has no other
interpretation as a macro call or special form is taken to be a function call.


### functions

- apply function arg &rest more-args

This applies **function** is to a list of arguments.

- funcall fn &rest arguments

(funcall fn a1 a2 ... an) applies the function fn to the arguments a1, a2, ..., an.

The fn may not be a special form or a macro; this would not be meaningful.


The difference between funcall and an ordinary function call is that the function is 
obtained **by ordinary Lisp evaluation rather** than by the special interpretation of 
the function position.


### constant

- call-arguments-limit

The value of call-arguments-limit is a positive integer that is the upper exclusive bound
 on the number of arguments that may be passed to a function. 


## Simple Sequencing

Sequencing forms evaluates all the arguments forms in order.

### special forms

- progn {form}*

The **progn** construct takes a number of forms and evaluates them sequentially, in order,
from left to right; the values of all the forms but the last are discarded.

**progn** is the primitive control structure construct for "compound statements", as part
of their syntax each allows **many forms** to be written that are to be evaluated sequentially.

### macros

- prog1 first {form}*

*prog1* is similar to **progn**, but it returns the value of its **first** form.

- prog2 first second {form} *

**prog2** is similar to **prog1**, but it returns the value of its **second** form.

(prog2 a b c ... z) == (progn a (prog1 b c ... z))


## Variable Bindings

### special forms

```lisp
let ({var | (var value)}*) {declaration}* {form}*
```

A let form can be used to execute a series of forms with specified variables bound to
specified values.

More precisely, the form:

```lisp
(let ((var1 value1)
	  (var2 value2)
	  ...
	  (varm valuem))
	declaration1
	declaration2
	...
	declarationp
	body1
	body2
	...
	bodyn)	  
```

first evaluates the expressions value1, value2, and so on, in that order, saving the 
resulting values. Then all of the variables varj are bound to the corresponding values 
in parallel;

```lisp
let ({var | (var [value])}*) {declaration}* {form}*
```

This changes let to allow a list (var) to appear, meaning the same as simply var.

- let* ({var | (var value)}*) {declaration}* {form}*

**let*** is similar to let, but the bindings of variables are performed **sequentially 
rather than in parallel**.
 
This allows the expression for the value of a variable to refer to variables **previously**
bound in the let** form.

```lisp
(let* ((var1 value1) 
       (var2 value2) 
       ... 
       (varm valuem)) 
	declaration1
	declaration2
	...
	declarationp
	body1
	body2
	...
	bodyn)	  
```

first evaluates the expression value1, then binds the variable var1 to that value; then 
it evaluates value2 and binds var2; and so on.


```lisp
let* ({var | (var [value])}*) {declaration}* {form}*
```

This changes let* to allow a list (var) to appear, meaning the same as simply var. 


```lisp
progv symbols values {form}*
```

**progv** allows binding one or more dynamic variables whose names may be determined at 
run time.

The sequence of forms is evaluated with the dynamic variables whose names are in the list 
** symbols** bound to values from the list **values**.

The results of the progv form are those of the last form.

progv is **particularly useful** for writing **interpreters** for **languages embedded**
in lisp; it provides a handle on the mechanism for binding dynamic variables.


```lisp
flet ({(name lambda-list
         [[ {declaration}* | doc-string ]] {form}*)}*)
     {form}*
      
labels ({(name lambda-list
         [[ {declaration}* | doc-string ]] {form}*)}*)
     {form}*
      
macrolet ({(name varlist
         [[ {declaration}* | doc-string ]] {form}*)}*)
     {form}*
```

**flet** may be used to define **locally named functions**.

Function names matching those defined by the **flet** refer to the locally defined 
functions rather than to the global function.

**labels** construct is identical in form to the **flet** construct, it can be used to
**define mutually recursive functions**, but flet cannot.

**macrolet** is similar in form to flet but **defines local macros**, using the same format
by **defmacro**.


## Conditionals

The traditional conditional construct in lisp is **cond**, it is considered to be primitive
in common lisp, but common lisp also provides more convenient constructs like **case** and
**typecase**.


### special forms

```lisp
if test then [else]
```

### macros

```lisp
when test {form}*
```


```lisp
unless test {form}*
```

```lisp
cond {(test {forms}*)}*
```


```lisp
case keyform {({({key}* | key)} {}*)}*
```


```lisp
typecase keyform {(type {form}*)}*
```

## Blocks and Exits

Below constructs provides a lexical non-local exit feature.

### special forms

```lisp
block name {form}*
```


```lisp
return-from name [result]
```

#### macros

```lisp
return [result]
```

This is identical in meaning to (return-from nil form), used to exit properly in the 
iteration constructs like **do**.


## Iteration

Common Lisp provides a number of iteration constructs, like: 
- loop
- do
- do*
- dolist
- dotimes
- tagbody

### indefinite iteration

**[macro]**

```lisp
loop {form}*
```

The loop construct is the simplest iteration facility, it controls no variables, and 
simply executes its body(forms) repeatedly, and never returns a value.

We can use **return** or **throw** constructs to exit **loop**.


### general iteration

**do** and **do*** can recalculate the variables.


#### macros

```lisp
do ({(var [init [step]])}*)
	(end-test {result}*)
	{declaration}*
	{tag | statement}*
```

The **do** special form provides a iteration facility, with an arbitrary number of 
"index variables"; these variables are bound within the iteration and stepped in parallel
in specified ways.

More precisely, a **do** loop looks like this:

```lisp
(do ((var1 init1 step1) 
	(var2 init2 step2) 
	... 
	(varn initn stepn)) 
	(end-test . result) 
	{declaration}*
	. tagbody)
```


```lisp
do* ({(var [init [step]])}*)
	(end-test {result}*)
	{declaration}*
	{tag | statement}*
```

A **do*** loop looks exactly the same except that name do is replaced by **do***.

The do* form is to do as let* is to let.


```lisp
dolist (var listform [resultform])
       {declaration}* 
       {tag | statement}*
```

**dolist** provides iteration over the elements of a list simply.


```lisp
dotimes (var countform [resultform])
        {declaration}* 
        {tag | statement}*
```

**dotimes** provides iteration over a sequence of integers simply.


### mapping

Mapping is a type of iteration in which a function is successively applied to one or more
sequences.

The result of it is a sequence containing the respective results of the function applications.

#### functions

```lisp
mapcar function list &rest more-lists 
maplist function list &rest more-lists 
mapc function list &rest more-lists 
mapl function list &rest more-lists 
mapcan function list &rest more-lists 
mapcon function list &rest more-lists

```

**mapcar** operates the **function** to the **car** of each **list**, then to the **cadr**
of each list, and so on.

**maplist** is like mapcar except that the function is applied to the successive **cdr** 
of the list, the result of each operation is a list, so the whole result of 
**maplist** is a list-in-list list like:

```lisp
({()}*)
```

### program feature

In Common Lisp, these three operations have been separated into three distinct constructs:
let, block, and tagbody. These three constructs may be used independently as **building 
blocks** for other types of constructs.


#### special forms

```lisp
tagbody {tag | statement}*
```

The part of a **tagbody** is called the body:

```lisp
{tag | statement}*
```
A **tag** may be symbol or an integer.

A **statement** may be a list.

Each element of the body is processed form left to right; a tag is ignored; a statment is
evaluated, and its results are discarded; at the end of the body, tagbody returns nil.


## Multiple Values


## Dynamic Non-Local Exits

