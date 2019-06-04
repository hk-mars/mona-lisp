

## Monalisp Syntax


Syntax with extended BNF forms:

{A} means 0 or more as,

[A] means an optional A,


syntax-types ::= 

	whitespace | constituent | constituent*
	
	terminating | non-terminating |
	 
	macro | char | 
	
	single escape | multiple escape
	
			
			
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




	

