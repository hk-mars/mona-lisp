
A hash table is a Lisp object that can efficiently map a given Lisp object to another Lisp object. Each hash table has a set of entries, each of which associates a particular key with a particular value. The basic functions that deal with hash tables can create entries, delete entries, and find the value that is associated with a given key. Finding the value is very fast, even if there are many entries, because hashing is used; this is an important advantage of hash tables over property lists.

