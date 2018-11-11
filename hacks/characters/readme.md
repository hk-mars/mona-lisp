
# Introduction

Common lisp provides a character data type,objects of this type represent printed
symbols such as letters.

In general, characters in common lisp are not true objects,eq cannot be counted
upon to operate on them reliably.
In particular, it's possible that the expression:

(let ((x z) (y z) (eq x y))

may be false rather than true, if the value of z is a character.


Table 13-1: Standard Character Labels, Glyphs, and Descriptions

                            SM05 @ commercial at        SD13 ` grave accent 
SP02 ! exclamation mark     LA02 A capital A            LA01 a small a 
SP04 " quotation mark       LB02 B capital B            LB01 b small b 
SM01 # number sign          LC02 C capital C            LC01 c small c 
SC03 $ dollar sign          LD02 D capital D            LD01 d small d 
SM02 % percent sign         LE02 E capital E            LE01 e small e 
SM03 & ampersand            LF02 F capital F            LF01 f small f 
SP05 ' apostrophe           LG02 G capital G            LG01 g small g 
SP06 ( left parenthesis     LH02 H capital H            LH01 h small h 
SP07 ) right parenthesis    LI02 I capital I            LI01 i small i 
SM04 * asterisk             LJ02 J capital J            LJ01 j small j 
SA01 + plus sign            LK02 K capital K            LK01 k small k 
SP08 , comma                LL02 L capital L            LL01 l small l 
SP10 - hyphen or minus sign LM02 M capital M            LM01 m small m 
SP11 . period or full stop  LN02 N capital N            LN01 n small n 
SP12 / solidus              LO02 O capital O            LO01 o small o 
ND10 0 digit 0              LP02 P capital P            LP01 p small p 
ND01 1 digit 1              LQ02 Q capital Q            LQ01 q small q 
ND02 2 digit 2              LR02 R capital R            LR01 r small r 
ND03 3 digit 3              LS02 S capital S            LS01 s small s 
ND04 4 digit 4              LT02 T capital T            LT01 t small t 
ND05 5 digit 5              LU02 U capital U            LU01 u small u 
ND06 6 digit 6              LV02 V capital V            LV01 v small v 
ND07 7 digit 7              LW02 W capital W            LW01 w small w 
ND08 8 digit 8              LX02 X capital X            LX01 x small x 
ND09 9 digit 9              LY02 Y capital Y            LY01 y small y 
SP13 : colon                LZ02 Z capital Z            LZ01 z small z 
SP14 ; semicolon            SM06 [ left square bracket  SM11 { left curly bracket 
SA03 < less-than sign       SM07 \ reverse solidus      SM13 | vertical bar 
SA04 = equals sign          SM08 ] right square bracket SM14 } right curly bracket 
SA05 > greater-than sign    SD15 ^ circumflex accent    SD19 ~ tilde 
SP15 ? question mark        SP09 _ low line        


# Functions

## standard-char-p char

standard-char-p is true if the argument is a ``standard character,'' that is, an object
of type standard-char.

## graphic-char-p char

graphic-char-p is true if the argument is a ``graphic'' (printing) character, and falseif it is a ``non-graphic'' (formatting or control) character. 

Any character with a non-zero bits attribute is non-graphic.

## alpha-char-p char

alpha-char-p is true if the argument is an alphabetic character, and otherwise is false

## upper-case-p char
## lower-case-p char

upper-case-p is true if the argument is an uppercase character, and otherwise is false.
lower-case-p is true if the argument is a lowercase character, and otherwise is false.

## digit-char-p char &optional (radix 10)

If char is not a digit of the radix specified by radix, then digit-char-p is false.

## alphanumericp char

alphanumericp is true if char is either alphabetic or numeric.

## char= character &rest more-characters 
## char/= character &rest more-characters 
## char< character &rest more-characters 
## char> character &rest more-characters 
## char<= character &rest more-characters 
## char>= character &rest more-characters

These functions compare the objects using the implementation-dependent total ordering
on characters,analogous to numeric comparisons by = and related functions.

## char-equal character &rest more-characters 
## char-not-equal character &rest more-characters 
## char-lessp character &rest more-characters 
## char-greaterp character &rest more-characters 
## char-not-greaterp character &rest more-characters 
## char-not-lessp character &rest more-characters


The predicate char-equal is like char=, and similarly for the others, except according 
to a different ordering such that differences of bits attributes and case are ignored, 
and font information is taken into account in an implementation-dependent manner. 


## char-upcase char
## char-downcase char

The @char must be a character object.
char-upcase attempts to convert the @char to an uppercase char, and char-downcase is
for converting to lowercase char.


## digit-char weight &optional (radix 10) (font 0)

It determines whether or not it is possible to construct a character object whose
font attribute is @font, and whose @code is such that the result character has the 
@weight when considered as digit of the radix(digit-char-p).
It returns such a character if that is possible, otherwise returns nil.

If @font is zero, radix is between 2 and 36 inclusive, and @weight is non-negative and
less than @radix.


## char-int char

char-int returns a non-negative encoding the character object.


## char-name char

If the character has a name, then that name (a string) is returned.


## name-char name

If the name is the same as the name of char object, that object is returned,
otherwise nil is returned.


