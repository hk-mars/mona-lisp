

## Introduction

A string is a specialized vector whose elements are characters.
Any Lisp object may be tested for being a string by the predicate stringp.


## Functions

### char string index
### schar simple-string index

index => must be a non-negative integer less than the length of string, which must
be a string. The character at position index of the string is returned as a character
object.(this character will satisify the predicate string-char-p.)

setf may be used with char to destructively replace a character within a string.


### string= string1 string2 &key :start1 :end1 :start2 :end2

It compares two strings and is true if they are the same but is false if they are not.
The function equal calls string= if applied to two strings.

start => default to zero

end => default to the length of the strings.

### string-equal string1 string2 &key :start1 :end1 :start2 :end2

it is just like string= excep that differences in case are ignored. e.g.:
(string-equal "foo" "Foo") is true

### string< string1 string 2 &key :start1 :end1 :start2 :end2
### string> string1 string2 &key :start1 :end1 :start2 :end2 
### string<= string1 string2 &key :start1 :end1 :start2 :end2 
### string>= string1 string2 &key :start1 :end1 :start2 :end2 
### string/= string1 string2 &key :start1 :end1 :start2 :end2

These functions compare the two string arguments lexicographically, and the result is nil unless string1 is respectively less than, greater than, less than or equal to, greater than or equal to, or not equal to string2. 

A string a is less than a string b if in the first position in which they differ the
character of a is less than the corresponding character of b according to char<, or
if string a is a proper prefix of string b.

### string-lessp string1 string2 &key :start1 :end1 :start2 :end2
### string-greaterp string1 string2 &key :start1 :end1 :start2 :end2 
### string-not-greaterp string1 string2 &key :start1 :end1 :start2 :end2 
### string-not-lessp string1 string2 &key :start1 :end1 :start2 :end2 
### string-not-equal string1 string2 &key :start1 :end1 :start2 :end2

These are exactly like string<, string>, string<=, string>=, and string/=, respectively, except that distinctions between uppercase and lowercase letters are ignored.



