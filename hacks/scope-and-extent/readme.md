
## Introduction

Scope refers to the spatial region of the program within which references may occur.
Extent refers the interval of time during which references may occur.

There are a few kinds of scope and extent that are particularly useful in Common
Lisp:

- Lexical scope
  
The established entity can occur only within certain program portions that are
lexically(textually) contained within the establishing construct.
Typically the construct will have a part designated the @body, and the scope of
all entities established will include the body.

- Indefinite scope

It may occur anywhere, in any program.

- Dynamic extent

It may occur at any time in the interval between establishment of the entity and
the explicit disestablishment of the entity.

As a rule, the entity is disestablished when execution of the establishing
construct completes or is otherwise terminated. Therefore entities with dynamic
extent obey a stack-like discipline, paralleling the nested executions of their
establishing constructs.

The binding of a "special" variable has dynamic extent.

- Indefinite extent

The entity continues to exist as long as the possibility of reference remains.

It is free to destroy the entity if it can prove that reference to it is no longer
possible. GC(garbage collection) strategies implicitly employ such proofs.)

Most Common Lisp data objects have indefinite extent.


## Rules

- Variable bindings normally have lexical scope and indefinite extent.

- Variable bindings for which there is a dynamic-extent declaration also have
lexical scope and indefinite extent, but objects that are the values of such 
bindings may have dynamic extent.

The declaration is the programmer's grantee that the program will behave correctly
even if certain of the data objects have only dynamic extent rather than the 
indefinite extent.

- Variable bindings that are declared to be special have dynamic scope (indefinite
scope and dynamic extent).

- Bindings of function names established by @lables have lexical scope and indefinite extent.

- Bindings of function names for which there is a dynamic-extent declaration also
have lexical scope and indefinite extent, but function objects that are the values
of such bindings may have dynamic extent.

- Bindings of function names to @macros as established by macrolet have lexical 
scope and indefinite extent.

- A catcher established by a @catch or unwind-protect special form has dynamic scope

- An exit point established by a block construct has lexical scope and dynamic 
extent (established by @do, @prog, and other iteration constructs)

- The @go targets established by a @tagbody, named by the @tags in the @tagbody,
are referred bo by @go have lexical scope and dynamic extent.

- Named constants such as @nil and @pi have indefinite scope and indefinite extent.

The rules of lexical scope imply that lambda-expressions appearing in the function construct will result in "closures" over those non-special variables visible to the lambda-expression.
That is, the function represented by a lambda-expression may refer ANY lexically
apparent non-special variable and get the correct value, even if the construct that
established the binding has been exited in the course of execution.







 
