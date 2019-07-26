

/**
 * Concepts
 *
 * The abstract syntactic tree (AST) is a tree representation of the source, 
 * strongly tied to the source language(language frontend).
 * A tree is a type of graph.
 *
 * The first intermediate representation(IR1)
 * @ Should be brief(remove high-level  node in AST tree) and 
 *   optimizable(preserve enough information of the source).
 * @ Tradeoffs in the level of abstraction are necessary.
 * @ Can be represented as tree or list or graph.
 * @ Also known as ICR(the Implicit Continuation Represenation).
 * 
 * The second intermediate representation(IR2)
 * @ also known as VMR, or the Virtual Machine Representation.
 *
 * Compiler (language back-end)
 * @ Hides the algorithms used to map Lisp semantics onto the operations supplied 
 *   by the VM.
 * @ Exports the mechanisms used for defining the VM.
 * 
 * Depth first order(DFO)
 * This is a linearization of the flow graph, obtained by a depth-first walk.  
 * Iterative flow analysis algorithms work better when blocks are processed in DFO 
 * (or reverse DFO.)
 * 
 */


/* An abstract semantic graph (ASG) or term graph is a form of abstract syntax 
in which an expression of a formal or programming language is represented by a 
graph whose vertices are the expression's subterms. An ASG is at a higher level 
of abstraction than an abstract syntax tree (or AST), which is used to express 
the syntactic structure of an expression or program. 
ASGs are more complex and concise than ASTs because they may contain shared subterms 
(also known as "common subexpressions").[1] Abstract semantic graphs are often used 
as an intermediate representation by compilers to store the results of performing 
common subexpression elimination upon abstract syntax trees. ASTs are trees and are 
thus incapable of representing shared terms. ASGs are usually directed acyclic graphs. 
However, they may contain cycles, particularly in the field of graph rewriting. Graphs 
that contain cycles may represent recursive expressions which are commonly used to 
express iteration in functional programming languages without looping constructs.
*/



/**
 * Try below steps to implement a demo:
 * @ Create a part of common lisp syntax based on BNF rule.
 *   The BNF file should be text file like format *.txt or *.json.
 *
 * @ Build the AST tree from a BNF syntax file.
 * @ Transform AST tree to ASG graph.
 * @ Remove duplicate nodes in ADG and do term-graph-rewriting as rules.
 *   more: https://encyclopedia.thefreedictionary.com/Graph+rewriting
 * @ Check the syntax of codes given via ASG.
 * @ Evaluate the forms which the syntax has been checked to be right.
 * @ Transform the ASG to IR1 rules tree or list.
 * @ Transform the IR1 rules to rules based on C code.
 * @ Parse some macro codes and update the AST, ASG and IR1 dynamically.
 * @ Create the complete syntax of monalisp based on BNF rule.
 * @ Create the baby parser of monalisp.
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
