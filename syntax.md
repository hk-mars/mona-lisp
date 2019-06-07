

## Monalisp Syntax

### Basic Law-Rules

Syntax with extended BNF forms:

{A} means 0 or more as,

[A] means an optional A,


### Syntax

syntax-types ::= 

	whitespace | constituent | constituent*
	
	terminating | non-terminating |
	 
	macro | char | 
	
	single-escape | multiple-escape
	
						
whitespace ::=

	backspace | tab | newline | 
	
	linefeed | page | return | space
	
constituent ::=

	$ | % | & | * | + | - | . | / |
	
	0 -- 9 |
	
	: | < | = | > | @ | 
	
	A -- Z | a -- z |
	
	^ | _ | ~ | rubout
	
single escape ::= \

multiple escape ::= |

constituent* ::= characters reserved to the user

terminating ::= TBD

non-terminating ::= TBD

macro ::=
	macro-label-chars | 
	macro-func-chars |
	simple-vector-chars |
	signals-error-chars |
	bit-vector-chars |
	character-object-chars |
	TBD 
	
macro-label-chars ::= # #

macro-func-chars  ::= # '

simple-vector-chars ::= # (

signals-error-chars ::= # )

bit-vector-chars ::= # *

character-object-chars ::= # \

char ::= TBD

single-escape ::= TBD

multiple-escape ::= TBD


	

