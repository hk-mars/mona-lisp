
## introduction

Symbols are Lisp data objects that serve several purposes and have several 
interesting characteristics.

Every symbol has a name called print name.

We can obtain its name in the form of string.

We can obtain symbol itself from the string of name.

Symbols have a componnent called the property list, or plist.

A symbol can be notated simply by writing its name:

FROBBOZ         ;The symbol whose name is FROBBOZ
frobboz         ;Another way to notate the same symbol 
fRObBoz         ;Yet another way to notate it 
unwind-protect  ;A symbol with a - in its name 
+$              ;The symbol named +$ 
1+              ;The symbol named 1+ 
+1              ;This is the integer 1, not a symbol 
pascal_style    ;This symbol has an underscore in its name 
b^2-4*a*c       ;This is a single symbol! 


In addition to letters and numbers, the following characters are normally
considered to be alphabetic for the purposes of notating symbols:

+ - * / @ $ % & ^ =_ < > ~ .


The following characters are also alphabetic by default but are explicitly 
reserved to the user for definition as reader macro characters:

?  !  [  ]  {  }

 

