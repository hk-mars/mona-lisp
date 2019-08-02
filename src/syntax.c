

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



static hash_table_s syntax_htab;


syntax_rt_t
syntax_init(void)
{
    func_s();


    func_ok();
    return SYNTAX_OK;
}


static syntax_rt_t
find_path(tr_node_s *root, lisp_list_s *path)
{
    /* a path is a pattern sequence of which the element is constructed by lisp-characters.
     * a syntax tree includes all the paths for syntax patterns of a language.
     * if the path is found, then the syntax of given codes is valid.
     *
     * the leafs of the tree should be determinded.
     * a leaf is an element.
     * all the elements should be saved together in a hash table.
     *
     * a node may be:
     * @ a leaf
     * @ a pattern node
     * @ a syntax-object node
     * @ a temporary node.
     * 
     * if a pattern node or a syntax-object node has been found, then its sub-nodes
     * can be ignored to track, this may work in macro-related context.
     * 
     */
    
    func_s();


    func_ok();
    return SYNTAX_OK;
}


static syntax_rt_t
check_func_form_syntax(form_s *form)
{
    syntax_rt_t rt;
    lisp_list_s *l;
    
    func_s();

    
    if (!form->list->next) {

	debug("null form \n");
	return SYNTAX_ERR;
    }

    l = form->list->next;
    
    debug("%s \n", l->obj.token.value.symbol);

    htab_entry_s *item = pop_syntax_htab(l->obj.token.value.symbol);
    if (!item) goto FAIL;

#if 0    
    l = l->next;

    while (l && l != form->list) {

	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");

	    form_s *subform = l->obj.sub;
	    if (subform) {
		debug("sub_form \n");
	    }
	    
	    
	}
	else if (l->obj.type == OBJ_TYPE) {

	    debug("OBJ_TYPE \n");
	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);
	}

	
	l = l->next;
    }
#endif
    
    
    /* track the tree to find the given path
     */
    lisp_list_s *path = l;
    rt = find_path((tr_node_s*)item->data, path);

    
    func_ok();
    return SYNTAX_OK;

  FAIL:
    func_fail();
    return SYNTAX_ERR;
}


syntax_rt_t
syntax_check(form_s *form)
{
    syntax_rt_t rt;
    
    func_s();

    if (!form) return SYNTAX_ERR;

    form_s *f = form->next;

    while (f && f != form) {

	switch (f->type) {

	case COMPOUND_FUNCTION_FORM:
	    debug("COMPOUND_FUNCTION_FORM \n");

	    rt = check_func_form_syntax(f);
	    if (rt != SYNTAX_OK) return rt;
	    
	    break;

	case SYMBOL_FORM:
	    debug("SYMBOL_FORM \n");
	    break;

	default:
	    break;
	}
	
	    
	f = f->next;
    }
    

    func_ok();
    return SYNTAX_OK;
}


static void
show_path(tr_node_s *s)
{
    fs();
  
    while(s) {
	debug("%s \n", s->key);
	s = s->next;
    }
  
    fe();
}


int
create_syntax_htab(int cnt)
{
    int rt;
  
    if (syntax_htab.table) return 0;
  
    rt = hcreate(&syntax_htab, cnt);
  
    return rt;
}


hash_table_s* 
get_syntax_htab(void)
{
    if (!syntax_htab.table) return NULL;
  
    return &syntax_htab;
}


int 
push_syntax_htab(char *key, tr_node_s *root)
{
    ENTRY item, *rti;
  
    if (!syntax_htab.table) return 0;

    item.key = key;
    item.data = root;
    rti = hsearch(&syntax_htab, item, ENTER); 
  
    return !!rti;
}


ENTRY*
pop_syntax_htab(char *key)
{
    ENTRY item, *rti;

    func_s();
  
    if (!syntax_htab.table) return NULL;

  
  
    memset(&item, 0, sizeof(ENTRY));
    item.key = key;
    rti = hsearch(&syntax_htab, item, FIND);

    func_ok();
    return rti;
}
