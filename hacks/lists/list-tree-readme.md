
## Introduction

A number of functions are provided for performing subsitutions within a tree.
All take a tree and a description of nold subexpressions to be replaced by new ones.


## Functions

### subst new old tree &key :test :test-not :key
### subst-if new test tree &key :key
### subst-if-not new test tree &key :key

(subst new old tree) makes a copy of tree, substituting new for every subtree of leaf
of tree such that @old and the subtree or leaf satisfy the test.
It returns modified copy of tree.
The original tree is unchanged, but the result tree may share with parts of the 
argument tree.


### sublis alist tree &key :test :test-not :key

sublis makes substitutions for objects in a tree(a structure of conses).
This first argument to sublis is an association list.
The second argument is the tree in which substitutions are to be made.
sublis looks at all subtrees and leaves of the tree.
if a subtree or leaf appears as a key in the association list, it is replaced by
the object with which it is associated.
This operation is non-destructive.


### nsublis a list tree &key :test :test-not :key

nsublis is like sublis but destructively modifies the relevant parts of the tree.


