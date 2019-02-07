
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


## Simple Sequencing


## Variable Bindings


## Conditional Construct


## Blocks and Exits


## Iteration


## Multiple Values


## Dynamic Non-Local Exits

