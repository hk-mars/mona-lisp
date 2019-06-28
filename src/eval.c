

#include "eval.h"

#include "debug.h"

#include "mem.h"



/** 
 * A Common Lisp system evaluates forms with respect to lexical, dynamic, and global
 * environments. 
 *
 * Evaluation can be understood in terms of a model in which an interpreter recursively 
 * traverses a form performing each step of the computation as it goes.
 *
 * Forms fall into three categories: symbols, conses, and self-evaluating objects.
 */

