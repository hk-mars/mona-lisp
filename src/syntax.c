

/**
 * Concepts
 *
 * The abstract syntactic tree (AST) is a tree representation of the source, 
 * strongly tied to the source language(language frontend).
 * A tree is a type of graph.
 *
 * The first intermediate representation(IR1)
 * > Should be brief(remove high-level  node in AST tree) and 
 *   optimizable(preserve enough information of the source).
 * > Tradeoffs in the level of abstraction are necessary.
 * > Can be represented as tree or list or graph.
 * > Also known as ICR(the Implicit Continuation Represenation).
 * 
 * The second intermediate representation(IR2)
 * > also known as VMR, or the Virtual Machine Representation.
 *
 * Compiler (language back-end)
 * > Hides the algorithms used to map Lisp semantics onto the operations supplied 
 *   by the VM.
 * > Exports the mechanisms used for defining the VM.
 * 
 * Depth first order(DFO)
 * This is a linearization of the flow graph, obtained by a depth-first walk.  
 * Iterative flow analysis algorithms work better when blocks are processed in DFO 
 * (or reverse DFO.)
 * 
 */


/**
 * Schedule to implement a demo:
 * 1. Build the abstract syntactic tree(AST) form a BNF syntax file.
 * 2. Check the syntax of the forms of codes.
 * 3. Evaluate the forms which the syntax has been checked to be right.
 * 4. Optimize the AST tree.
 * 5. Make part of the optimized AST tree be consts to save the heap memory.
 * 6. Parse some macro codes and update the AST tree dynamically.
 * 
 */


#include "syntax.h"

#include "error.h"

#include "debug.h"

#include "rules.h"





syntax_rt_t
syntax_init(void)
{
    func_s();


    func_ok();
    return SYNTAX_OK;
}


syntax_rt_t
syntax_check(form_s *form)
{
    func_s();


    func_ok();
    return SYNTAX_OK;
}
