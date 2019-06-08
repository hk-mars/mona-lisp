

## Monalisp Syntax

### Basic Law-Rules

Syntax with extended BNF forms:

{A} means 0 or more as,

[A] means an optional A,

A | B  means A or B,

A B means A and B,




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


Tip [Parsing of Numbers and Symbols] (http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node189.html#SECTION002612000000000000000).


		
				   
				   



	

