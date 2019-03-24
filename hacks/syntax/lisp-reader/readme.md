
## Introduction

The lisp reader parses objects form an input character stream, constructs objects and
returns it.


### reader algorithm

When dealing with tokens, the reader's basic function is to distinguish representations
of symbols from those of numbers; when a token is accumulated, it is assumed to represent
a number if it satisfies the syntax for number; if it does not represent a number, it is
then assumed to be a potential number if it satisfies the rules governing the syntax for
a potential number; if a valid token is neither a representation of a number nor a potential
number, it represents a symbol.
 

### functions

```lisp
read-from-string string &optional eof-error-p eof-value &key start end preserve-whitespace
=> object, position
```

@string:  a string.

@eof-error-p:  boolean, the default is true.

@eof-value:  an object, the default is nil.

@start, @end:  bounding index designators of string, default of @start is 0, default of @end
is nil.

@preserve-whitespace:  boolean, the default is false.

@object:  an object returned.

@postion: an  integer greater than or equal to zero, but less than or equal to one more 
than the length of the string.

 




