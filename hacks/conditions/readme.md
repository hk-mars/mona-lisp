
## Introduction
Often we find it useful to describe a function in terms of its behavior in ``normal situations.'' For example, we may say informally that the function + returns the sum of its arguments or that the function read-char returns the next available character on a given input stream.

Sometimes, however, an ``exceptional situation'' will arise that does not fit neatly into such descriptions. For example, + might receive an argument that is not a number, or read-char might receive as a single argument a stream that has no more available characters. This distinction between normal and exceptional situations is in some sense arbitrary but is often very useful in practice.

For example, suppose a function f were defined to allow only integer arguments but also guaranteed to detect and signal an error for non-integer arguments. Such a description is in fact internally inconsistent (that is, paradoxical) because the function's behavior is well-defined for non-integers. Yet we would not want this annoying paradox to force description of f as a function that accepts any kind of argument (just in case f is being called only as a quick way to signal an error, for example). Using the normal/exceptional distinction, we can say clearly that f accepts integers in the normal situation and signals an error in exceptional situations. Moreover, we can say that when we refer to the definition of a function informally, it is acceptable to speak only of its normal behavior. For example, we can speak informally about f as a function that accepts only integers without feeling that we are committing some awful fraud.

Not all exceptional situations are errors. For example, a program that is directing the typing of a long line of text may come to an end-of-line. It is possible that no real harm will result from failing to signal end-of-line to its caller because the operating system will simply force a carriage return on the output device, which will continue typing on the next line. However, it may still be interesting to establish a protocol whereby the printing program can inform its caller of end-of-line exceptions. The caller could then opt to deal with these situations in interesting ways at certain times. For example, a caller might choose to terminate printing, obtaining an end-of-line truncation. The important thing, however, is that the failure of the caller to provide advice about the situation need not prevent the printer program from operating correctly.

Mechanisms for dealing with exceptional situations vary widely. When an exceptional situation is encountered, a program may attempt to handle it by returning a distinguished value, returning an additional value, setting a variable, calling a function, performing a special transfer of control, or stopping the program altogether and entering the debugger.

For the most part, the facilities described in this chapter do not introduce any fundamentally new way of dealing with exceptional situations. Rather, they encapsulate and formalize useful patterns of data and control flow that have been seen to be useful in dealing with exceptional situations.

A proper conceptual approach to errors should perhaps begin from first principles, with a discussion of conditions in general, and eventually work up to the concept of an error as just one of the many kinds of conditions. However, given the primitive state of error-handling technology, a proper buildup may be as inappropriate as requiring that a beggar learn to cook a gourmet meal before being allowed to eat. Thus, we deal first with the essentials-error handling-and then go back later to fill in the missing details.

## Concepts

### signaling errors
Conceptually, signaling an error in a program is an admission by that program that it does not know how to continue and requires external intervention. Once an error is signaled, any decision about how to continue must come from the "outside".

### trapping errors
By default, a call to error will force entry into the debugger. You can override that behavior in a variety of ways.The simplest (and most blunt) tool for inhibiting entry to the debugger on an error is to use ignore-errors. In the normal situation, forms in the body of ignore-errors are evaluated sequentially and the last value is returned. If acondition of type error is signaled, ignore-errors immediately returns two values, namely nil and the condition that was signaled; the debugger is not entered and no error message is printed.


### handling conditions
Blind transfer of control to a handler-case is only one possible kind of recovery action that can be taken when a condition is signaled. The low-level mechanism offers great flexibility in how to continue once a condition has been signaled.

The basic idea behind condition handling is that a piece of code called the signaler recognizes and announces the existence of an exceptional situation using signal or some function built on signal (such as error).

The process of signaling involves the search for and invocation of a handler, a piece of code that will attempt to deal appropriately with the situation.

If a handler is found, it may either handle the situation, by performing some non-local transfer of control, or decline to handle it, by failing to perform a non-local transfer of control. If it declines, other handlers are sought.

Since the lexical environment of the signaler might not be available to handlers, a data structure called a condition is created to represent explicitly the relevant state of the situation. A condition either is created explicitly using make-condition and then passed to a function such as signal, or is created implicitly by a function such as signal when given appropriate non-condition arguments.

In order to handle the error, a handler is permitted to use any non-local transfer of control such as go to a tag in a tagbody, return from a block, or throw to a catch. In addition, structured abstractions of these primitives are provided for convenience in exception handling.


 
