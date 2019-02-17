

# Introduction

The evaluator is a function used to execute Lisp programs; it accepts a form and performs
the computation specified by the form.

> The evaluator is typically implemented **as an interpreter** that executes the given
form **recursively**; the theory of it is created by John McCarthy, who is well known as
the AI father(I also think so); the original paper of this theory refers to "Recursive 
Functions of Symbolic Expressions and Their Computation by Machine" so far written in 1960.


# Function

```lisp
eval form
```

The **form** is evaluated recursively.


