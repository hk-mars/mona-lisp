

## Monalisp Syntax

### Basic Law-Rules

Syntax with extended BNF forms:

{A}* means 0 or more A,

{A}+ means one or more A,

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

decimal-point	::= .

exponent-marker	::= 

	e | s | f | d | l | E | S | F | D | L

decimal-digit	::= 

	digit-as-10-radix
		
digit-as-10-radix	::= 

	0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
	
digit	::= 

	a digit in the current input radix 


Guides:
 
1. [Parsing of Numbers and Symbols](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node189.html#SECTION002612000000000000000).

2. [What the Read Function Accepts](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node188.html#SECTION002611000000000000000)

		
		
- Function +

Syntax:

> + &rest numbers => sum

@number: a number.

@sum: a number.

Returns the sum of numbers

[More...](http://www.lispworks.com/documentation/HyperSpec/Body/f_pl.htm)


- Function SET

setf {pair}* => result*

place---a place.

newvalue---a form.

results---the multiple values[2] returned by the storing form for the last place, 
or nil if there are no pairs.

[More...](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm#setf)



				   
				   
				   



	

