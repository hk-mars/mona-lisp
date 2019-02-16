
# Introduction

Common Lisp provides the **package system** to fix the accidental name collisions.

A package is a data structure that establishes a mapping from print names (strings) to symbols.


# An example

```lisp
;;;; Lisp init file for I. Newton. 
 
;;; Set up the USER package the way I like it. 
 
(load "calculus")               ; I use CALCULUS a lot; load it. 
(use-package 'calculus)         ; Get easy access to its 
                                ; exported symbols. 
 
(load "newtonian-mechanics")    ; Ditto for NEWTONIAN-MECHANICS 
(use-package 'newtonian-mechanics) 
 
;;; I just want a few things from RELATIVITY, 
;;; and other things conflict. 
;;; Import only what I need into the USER package. 
 
(load "relativity") 
(import '(relativity:speed-of-light 
          relativity:ignore-small-errors)) 
 
;;; These are worth loading, but I will use qualified names, 
;;; such as PHLOGISTON:MAKE-FIRE-BOTTLE, to get at any symbols 
;;; I might need from these packages. 
 
(load "phlogiston") 
(load "alchemy") 

```


```lisp
(cl:unless (cl:find-package "PHLOGISTON") 
  (cl:load "phlogiston-package"))

```