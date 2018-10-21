
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



