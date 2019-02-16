

# Introduction

Common Lisp provides a facility for creating named record structures with named components.


# Defstruct

[**Macro**]

```lisp
defstruct name-and-options [doc-string]
		{slot-description}+
```

The same as:

```lisp
(defstruct (name option-1 option-2 ... option-m) 
           doc-string 
           slot-description-1 
           slot-description-2 
           ... 
           slot-description-n) 
```

The **name** must be a symbol; it is the name of a new data type consisting of all instances
of the structure; it is returned as the value of the **defstruct**.

Each slot-description-j is of the form

```lisp
(slot-name default-init 
 slot-option-name-1 slot-option-value-1 
 slot-option-name-2 slot-option-value-2 
 ... 
 slot-option-name-kj slot-option-value-kj)
```

(defstruct (starbulks)
	     "Coffee time!"
	     where
	     who
	     coffees
	     hack-lisp)

