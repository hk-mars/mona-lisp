

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


- Macro SETF

setf {pair}* => result*

pair ::= place newvalue

place---a place.

newvalue---a form.

results---the multiple values[2] returned by the storing form for the last place, 
or nil if there are no pairs.

[More...](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm#setf)


- Special Form SETQ

setq {pair}* => result

pair::= var form 

var---a symbol naming a variable other than a constant variable.

form---a form.

result---the primary value of the last form, or nil if no pairs were supplied.
				   
				   
- Macro DEFCONSTANT

**defconstant** name initial-value [documentation] => name				   

name---a symbol; not evaluated.

initial-value---a form; evaluated.

documentation---a string; not evaluated.

Tip: show the documentation string of a variable:

> (documentation 'name 'variable)
	

- Function CONS

cons object-1 object-2 => cons

object-1---an object.

object-2---an object.

cons---a cons(a compound data object having two components called the car and the cdr.)


- Function CAR, CDR, CAAR, CADR, CDAR, CDDR

car x => object

cdr x => object

caar x => object

cadr x => object

cdar x => object

cddr x => object

(setf (car x) new-object)

(setf (cdr x) new-object)

(setf (caar x) new-object)

(setf (cadr x) new-object)

(setf (cdar x) new-object)

(setf (cddr x) new-object)


x---a list.

object---an object.

new-object---an object.


- Function LIST, LIST*

list &rest objects => list

list* &rest objects+ => result

object---an object.

list---a list.

result---an object.










	
	

