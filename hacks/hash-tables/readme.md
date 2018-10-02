
## Introduction
A hash table is a Lisp object that can efficiently map a given Lisp object to another Lisp object. Each hash table has a set of entries, each of which associates a particular key with a particular value. The basic functions that deal with hash tables can create entries, delete entries, and find the value that is associated with a given key. Finding the value is very fast, even if there are many entries, because hashing is used; this is an important advantage of hash tables over property lists.


## Functions
### make-hash-table
make-hash-table &key :test :size :rehash-size :rehash-threshold

This function creates and returns a new hash table.
:test => determines how keys are compared; it must be one of the three values #`eq, #`eql or #`equal, or on one of the three symbols eq, eql or equal. Default value is eql.

:size => sets the initial size of hash table, in entries. (The actual size may be
rounded up from the size you specify to the next "good" size, for example to make
it a prime number.)
It does serve as a hint to the implementation of approximately how many entries
you intend to store.

:rehash-size => specifies how much to increase the size of the hash table when it
becomes full.

:rehash-threshold => argument specifies how full the hash table can get before it
must grow. This can be an integer greater than zero and less than the :rehash-size.


### hash-table-p
hash-table-p is true if tis argument is a hash table, and ohterwise is false.


### gethash key hash-table &optional default
Finds the entry in hash-table whose key is @key and returns the associated value.
If there is no such entry, gethash returns default, which is nil if not specified.

setf may be used with gethash to make new entries in a hash table.
If an entry with the specified key already exists, it is removed before the new entry
is added.The default argument may be specified in this context; it is ignored by setf
but may be useful in such macros as incf that are related to setf:
(incf (gethash a-key table 0)
(setf (gethash a-key table)
      (incf (gethash a-key table 0)))


### remhash key hash-table
Removes any entry for key in hash-table.

### maphash function hash-table

For each entry in hash-table, maphash calls function on two arguments: the key of 
entry and the value of the entry; maphash then returns nil.
If entries are added to or deleted from the hash table while a maphash is in progress,
the results are unpredictable, with one exception: if the function calls remhash to remove the entry currently being processed by the function, or performs a setf of 
gethash on that entry to change the associated value, then those operations will have the intended effect. For example:

;;; Alter every entry in MY-HASH-TABLE, replacing the value with 
;;; its square root.  Entries with negative values are removed. 
(maphash #'(lambda (key val) 
             (if (minusp val) 
                 (remhash key my-hash-table) 
                 (setf (gethash key my-hash-table) (sqrt val)))) 
         my-hash-table)


### clrhash hash-table

Removes all the entries from hash-table and returns the hash table itself.


### hash-table-count hash-table

This returns the number of entries in the hash-table.
When a hash table is first created or has been cleared, the number of entries is 0.

## hash-table-rehash-size hash-table 
## hash-table-rehash-threshold hash-table 
## hash-table-size hash-table 
## hash-table-test hash-table
hash-table-rehash-size returns the current rehash size of a hash table.

hash-table-rehash-threshold returns the current rehash threshold.

hash-table-size returns the current size of a hash table.

hash-table-test returns the test used for comparing keys. 
If the test is one of the standard test functions, then the result will always be a 
symbol, even if the function itself was specified when the hash-table was created. 
For example:

(hash-table-test (make-hash-table :test #'equal)) => equal


## Macro

### with-hash-table-iterator (mname hash-table) {form}*

mname => is bound and defined as if by macrolet, with the body forms as its lexical
scope, to be a "generator macro" such that successive invocations (mname) will return
entries, one by one, from the hash table that is the value of the expression 
hash-table (which is evaluated exactly once).

This facility is a bit more flexible than maphash. It makes possible a portable and
efficient implementation of loop clauses for iterating over hash tables.





