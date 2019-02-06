

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


## Macro Definition


## Macro Expansion


## Destructuring


## Compiler Macros


## Environments


