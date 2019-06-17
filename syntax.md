

## Monalisp Syntax

### Basic Law-Rules

Syntax with extended BNF forms:

{A} means 0 or more as,

[A] means an optional A,

A | B  means A or B,

A B means A and B,




### Syntax

syntax-character-types ::= 

	whitespace | constituent | constituent*
	
	single-escape | multiple-escape |
	
	macro
	
						
whitespace ::=

	backspace | tab | newline | 
	
	linefeed | page | return | space
	
constituent ::=

	$ | % | & | * | + | - | . | / |
	
	0 -- 9 |
	
	: | < | = | > | @ | 
	
	A -- Z | a -- z |
	
	^ | _ | ~ | rubout
	
single-escape ::= \

multiple-escape ::= |

constituent* ::= characters reserved to the user

macro ::=

	terminating | non-terminating

terminating ::= TBD

non-terminating ::= TBD

macro_chars ::=
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

 
numeric-token	::=
  
	integer |

	ratio |
	
	float
				          
integer	::=
  
	[sign] decimal-digit+ decimal-point |
	
	[sign] digit+
				         
ratio	::=
  
	[sign] {digit}+ slash {digit}+ 
				      
float	::=
  
	[sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent] | 
                    
    [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
				       
exponent	::=
  
	exponent-marker [sign] {digit}+	      

sign	::= + | -

slash	::= /

dot		::= .

exponent-marker	::= 

	e | s | f | d | l | E | S | F | D | L

decimal-digit	::= 

	radix-10-digit
		
digit	::= 

	0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 


Guides:
 
1. [Parsing of Numbers and Symbols](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node189.html#SECTION002612000000000000000).

2. [What the Read Function Accepts](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node188.html#SECTION002611000000000000000)

		
				   
				   



	

