
## Introdction

Allow a list of items to be treated as a set.
To add, remove, and search for items in a list.
There are also set union, intersection, and difference functions.


## Functions

### member item list &key :test :test-not :key
### member-if predicate list &key :key
### member-if-not predicate list &key :key

The list is searched for an element that satisfies the test.
If none if found, nil is returned, the tail of list beginning with the first element
that satisfied is returned.


### tailp sublist list

This predicate is true if sublist is a sublist of list.
(eql sublist (nthcdr n list))

### adjoin item list &key :test :test-not :key

adjoin is used to add an element to a set, provided that it is not already a member.

The same as:

(adjoin item list) == (if (member item list) list (cons item list))

### union list1 list2 &key :test :test-not :key 
### nunion list1 list2 &key :test :test-not :key

union takes two lists and returns a new list containing everything that is an element
of either of the lists. 

nunion is the destructive version of union.


### intersection list1 list2 &key :test :test-not :key
### nintersection list1 list2 &key :test :test-not :key

intersection takes two lists and returns a new list containing everything that is an 
element of both argument lists.

### set-difference list1 list2 &key :test :test-not :key
### nset-difference list2 list2 &key :test :test-not :key

set-difference returns a list of elements of list1 that do not appear in list2.


### set-exclusive-or list1 list2 &key :test :test-not :key 
### nset-exclusive-or list1 list2 &key :test :test-not :key

set-exclusive-or returns a list of elements that appear in exactly one of list1 and 
list2.


### subsetp list1 list2 &key :test :test-not :key

subsetp is a predicate that is true if every element of list1 is in list2.


