
## Introduction

An association list, or a-list, is a list of pairs of conses.
each pair is an association. The car of a pair if called the key, and the cdr is 
called the data. It is viewed as a mapping from keys to data.

Advantages:
- Adding new entries to the front.


## Functions

### acons key datum a-list
acons constructs a new association list by adding the pair to the old a-list.

(acons x y a) == (cons (cons x y) a)


### pairlis keys data &optional a-list

It takes two lists and makes an association list that associates elements of the
first list to corresponding elements of the second list.

It is an error if the two lists @keys and @data are not of the same length.
If the optional argument @a-list is given, then the new pairs are added to the
front of it.


### assoc item a-list &key :test :test-not :key
### assoc-if predicate a-list &key :key
### assoc-if-not predicate a-list &key :key

Searches the association list a-list.
The value is the first pair in the a-list such that the car of the pair satisfies the
test, or nil if no such pair in the a-list.

The two expressions

(assoc item list :test fn)
and

(find item list :test fn :key #'car)
are equivalent in meaning with one important exception: if nil appears in the a-list in
place of a pair, and the item being searched for is nil, find will blithely compute the
car of the nil in the a-list, find that it is equal to the item, and return nil, 
whereas assoc will ignore the nil in the a-list and continue to search for an actual 
pair (cons) whose car is nil.

### rassoc item a-list &key :test :test-not :key
### rassoc-if predicate a-list &key :key
### rassoc-if-not predicate a-list &key :key

rassoc is the reverse form of assoc.
e.g.:
(rassoc 'a '((a . b) (b . c) (c . a) (z . a))) => (c . a)

(rassoc item list :test fn)

and

(find item list :test fn :key #'cdr)

are equivalent in meaning, except when the item is nil and nil appears in place of
a pair in the a-list.
 
