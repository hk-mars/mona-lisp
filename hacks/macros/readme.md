

## Introduction

Macros allows the user to define arbitrary functions that convert certain lisp forms into
different forms before evaluating or compiling them.

**This is done at the expression level, not at the character-string level** as in most
other languages.

Macros are important in the writing of good code: they make it possible to write code 
that is **clear and elegant** at the **user level** but that is converted to a more complex
or more efficient internal form for execution.

When **eval** is given a list whose **car** is a symbol, it looks for local definitions of
that symbol (by **flet, lables, and macrolet**); If that fails, it looks for a global 
definition. If the definition is a macro definition, then the original list is said to be 
a **macro call**.

**All any macro does is saving typing to make program elegant and brief.**



## Macro Definition

[**Function**]

```lisp
macro-function symbol
```

If the **symbol** has a global definition that is a macro, then the expansion function is
returned, otherwise, nil is returned.


[**Macro**]

```lisp
defmacro name lamda-list 
		[[ {declaration}* | doc-string]]
		{form}*
```

**defmacro** decompose the macro-call form in an **elegant and useful** way; it has
essentially the same syntax as **defun**:

> **name** is the symbol whose macro definition we are creating, **lambda-list** is similar
in form to a lambda-list, and the **forms** constitute the body of the expander function;
**defmacro** installs this expander function as the global macro definition of **name**.

> Like the lambda-list in a **defun**, it contains the keywords **&optional**, **&rest**,
**&key**, **&allow-other-keys**, and **&aux**.

For &optional and &key parameters, initialization forms and parameters may be specified.


The **name** is returned as the value of the **defmacro** form.



## Macro Expansion

### functions

```lisp
macroexpand-1 form &optional env
```

```lisp
macroexpand form &optional env
```


This function is used to expand a macro call; a hook is provided for a user function to
gain control during the expansion process.

If **form** is a macro call, then **macroexpand-1** will expand the macro call **once**
and return the **expansion** and **t**, otherwise **form** and nil are returned.

A form is considered to be a macro call only if it is a **cons** whose **car** is a symbol
that names a macro.

**macroexpand** is similar to macroexpand-1, but repeatedly expands form until it is no 
longer a macro call.

## Destructuring

[**Macro**]

```lisp
destructuring-bind lambda-list
					expression
					{form}*
```

This macro binds the variables specified in **lambda-list** to the corresponding values in
the tree structure resulting from evaluating the **expression**, then executes the 
**forms** as an implicit **progn**.



## Compiler Macros


## Environments


