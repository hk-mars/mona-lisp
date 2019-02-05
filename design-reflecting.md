
### language spec 
I think a programming language should be mostly defined in itself. A language spec should be divided into two parts, a small core of operators that play the role of axioms, 
and the rest of the language, which, like theorems, are defined in terms of the axioms.

### editor

@ _

e.g.:  quit monalisp

@ (q)


Comparing below codes in editor:

   **@** (eql "foo" "foo")

   **>** (eql "foo" "foo")

   * (eql "foo" "foo")

I think "@" is more elegant, but ">" and "*" is still better.


### design-hackable
Any ideas that make designing more easy to hack, try them in monalisp.
1. layout design.
2. interface design.
3. architecture design.
4. complex system design.
...

Consider more deeply about this.
