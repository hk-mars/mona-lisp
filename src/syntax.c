

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
 * @ Remove duplicate nodes in ASG and do term-graph-rewriting as rules.
 *   more: https://encyclopedia.thefreedictionary.com/Graph+rewriting
 * @ Check the syntax of given codes via ASG.
 * @ Evaluate the forms of which the syntax has been checked to be valid.
 * @ Transform the ASG to IR1 rules tree or list.
 * @ Transform the IR1 rules to rules based on C code.
 * @ Parse given macro codes and update the AST, ASG and IR1 dynamically.
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



    
static tr_node_s*
search_end(tr_node_s *root)
{
    tr_node_s *rtn;
  
    if (!root) return NULL;

    //func_s();
    
    if (strcmp(root->key, "@") == 0) {
  
#if 1
	debug("%s \n", root->key);
	if (root->right) debug("still has a right: %s. \n", root->right->key);
	if (root->left) debug("still has a left: %s. \n", root->left->key);
	if (root->left && root->left->left) debug("still has a left left: %s. \n", 
						  root->left->left->key);
	if (root->sub) debug("still has a sub: %s. \n", root->sub->key);
#endif
    
	if (!root->right && !root->left && !root->sub) {
	    debug("@e. \n");
	    return root;
	}
    }
  
    if (root->is_token) {
	debug("end as a token: %s \n", root->key);
	return root;
    }
  
    if (root->sub) {
	//debug("sub %s \n", root->sub->key);
	rtn = search_end(root->sub);
	if (rtn) return rtn;
    }
  
    if (root->left) {
	//debug("left %s \n", root->left->key);
	rtn = search_end(root->left);
	if (rtn) return rtn;
    } 
  
    if (root->right) {
	//debug("right %s \n", root->right->key);
	rtn = search_end(root->right);
	if (rtn) return rtn;
    }
  
    return NULL;
}


static tr_node_s*
find_subset(tr_node_s *root, char *key)
{
    tr_node_s *rtn;

    if (!root) return NULL;

    //debug("%s %s  \n", __func__,  key);

    if (!root->key) {
	debug("root key is null \n");
	return NULL;
    }
    else {
	debug("%s %s %s \n", __func__, root->key, key);
    }
    
    if (!strcmp(root->key, "<tmp>")) {


	if (root->left) {

	    
	    rtn = find_subset(root->left, key);
	    if (rtn) return rtn;
	}
    
	if (root->right) {
	
	    rtn = find_subset(root->right, key);
	    if (rtn) return rtn;
	}	
    }
    else {
    
    
	if (!strcmp(root->key, key)) {

	    debug("found subset %s \n", key);
	    return root;
	}

	if (root->sub) {
	
	    rtn = find_subset(root->sub, key);
	    if (rtn) return rtn;
	}
	
    }
    
  

    return NULL;
}



static char*
get_token_name(token_s *tk)
{
    char *name;

    name = NULL;
    switch (tk->type) {

    case TOKEN_SYMBOL:
   
	debug("TOKEN_SYMBOL: %s \n", tk->value.symbol);

	name = tk->value.symbol;
	break;

    case TOKEN_NUM_INT:
   
	//debug("TOKEN_NUM_INT \n");
	//name = "number-token ::=";
	name = "token ::=";
	break;

    case TOKEN_NUM_FLOAT:
   
	//debug("TOKEN_NUM_FLOAT \n");
	//name = "number-token ::=";
	name = "token ::=";
	break;

    case TOKEN_NUM_RATIO:
   
	//debug("TOKEN_NUM_RATIO \n");
	//name = "number-token ::=";
	name = "token ::=";
	break;

    default:
	debug("unknown token \n");
	break;
    }

    return name;
}


static char*
get_leaf_name(object_s *obj)
{
    char *name;

    name = NULL;
    switch (obj->type) {

    case OBJ_CHARACTER:

	//debug("OBJ_CHARACTER \n");
	name = obj->character;

	//debug("character %s \n", name);
	break;
	
    case OBJ_LIST:

	//debug("OBJ_LIST \n");
	name = "list ::=";
	
	break;

    case OBJ_ARRAY:

	//debug("OBJ_ARRAY \n");
	break;
	    
    case OBJ_SEQUENCE:

	//debug("OBJ_SEQUENCE \n");
	break;
	    
    case OBJ_TYPE:

	//debug("OBJ_TYPE \n");

	name = get_token_name(&obj->token);
	
	break;
	    
    case OBJ_INPUT_STREAM:

	//debug("OBJ_INPUT_STREAM \n");
	break;
	    
    case OBJ_OUTPUT_STREAM:

	//debug("OBJ_OUTPUT_STREAM \n");
	break;
	    
    case OBJ_CLASS:

	//debug("OBJ_CLASS \n");
	break;
	    
    default:

	//debug("unkown object \n");
	break;
    }

    return name;
}


lisp_list_s *m_list_head = NULL;

static tr_node_s*
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
    
    tr_node_s *rtn, *nd;
    ENTRY *rti;
    lisp_list_s *lst, *sl, *el;


    //func_s();
    
    nd = NULL;
  
    if (!root) return NULL;
  
    if (root->is_token) {
	debug("token node: %s \n", root->key);

	if (path->obj.type == OBJ_LIST) {

	    debug("subform \n");
	    form_s *subform = path->obj.sub;
	    list_show(subform->next->list);

	    char *name = subform->next->list->next->next->obj.token.value.symbol;
	    debug("find syntax object: %s \n", name);
	    rti = pop_syntax_htab(name);
	    if (!rti) {
		debug("syntax tree not found for: %s \n", name);
		return NULL;
	    }
	    
	    rtn = find_path(rti, subform->next->list->next);
	    if (!rtn) return NULL;

	    debug("subform found \n");
	    nd = rtn;
	    debug("node: %s \n", root->key);
	    
	}
	else {


	    if (path->obj.token.type == TOKEN_SYMBOL) {

		if (!strcmp(root->key, "symbol-token ::=")) {

		    debug("found token: %s \n", root->key);
		    goto FOUND;
		}
	    }
		
	    char *name = get_leaf_name(&path->obj);

	    //debug("%s, %s \n", root->key, name);
	    if (strcasecmp(root->key, name)) {

		return NULL;
		
		//tr_node_s *lex_tree = get_lex_tree();
		//if (lex_tree) debug("search lex tree \n");
	    
		/* check if it's a subset of leaf
		 */	    
		//if (!find_subset(root, name)) return NULL;
		    
		//debug("found %s token's subset: %s \n", root->key, name);		
	    }
	    else {

		debug("found token: %s \n", root->key);
	    }
   	}

      FOUND:
	/* the end token, check the path if it's at the end.
	 */	
	if (path->next->is_head) {

	    debug("the end of path \n");

	    if (!root->sub && !root->left && !root->right) return root;
      
	    rtn = search_end(root->sub);
	    if (rtn) return root;
      
	    rtn = search_end(root->left);
	    if (rtn) return root;
      
	    rtn = search_end(root->right);
	    if (rtn) return root;
      
	    debug("no end path. \n");
	    return NULL;
	}
	else {

	    path = path->next;
	    debug("find next: %s \n", get_leaf_name(&path->obj));
	}

    }
  
  
    if (root->loop) {
      //debug("loop node: %s \n", root->key);
	//nd = root->loop;
    }
  
  
    if (root->is_in_syntax_tree && !nd) {
  
	/* get the root of sub tree from hash table.
	 */
	rti = pop_syntax_htab(root->key);
	if (!rti) return NULL;
	if (!rti->data) {
	    debug("sub tree not found: %s \n", root->key);
	    return NULL;
	}
    
	nd = rti->data;
	debug("syntax sub tree found: %s \n", root->key);
    }
  
    if (nd) {
  
	/* make new token list from the trail of old list, 
	 * and search the sub solution path again and again until done.
	 */
	sl = path;
	el = NULL;
	while (1) {
    
	    rtn = find_path(nd, sl);
      
	    /* reconnect the list
	     */
	    lst = sl;
	    while (lst) {
		if (lst->next->is_head) break;
		lst = lst->next;
	    }
	    if (el) lst->next = el;
      
	    if (rtn) {
		//debug("en token: %s \n", lst->tk.key);
		//if (el) debug("sn token: %s \n\n", el->tk.key);
		if (!el) return rtn;
		path = el;
        
		break;
	    }
      
	    el = lst;
	    if (el == sl) return NULL;
      
	    /* cut the list from the trail.
	     */
	    lst = sl;
	    while (lst) {
		if (lst->next == el) break;
		lst = lst->next;
	    }
	    lst->next = NULL;
	}	
    }
  
  
    if (root->back) {
	debug("go back to node: %s from %s \n", root->back->key, root->key);
	rtn = find_path(root->back, path);
	if (rtn) return rtn;
    }
  
    if (root->sub) {
	//debug("sub: %s \n", root->sub->key);
	root->next = root->sub;
	rtn = find_path(root->sub, path);
	if (rtn) return rtn;
    }
  
    if (root->left) {
	//debug("left: %s \n", root->left->key);
	root->next = root->left;
	rtn = find_path(root->left, path);
	if (rtn) return rtn;
    }
  
    if (root->right) {
	//debug("right: %s \n", root->right->key);
	root->next = root->right;
	rtn = find_path(root->right, path);
	if (rtn) return rtn;
    }

    
    return NULL;
}


static syntax_rt_t
check_list_form_syntax(form_s *form)
{
    syntax_rt_t rt;
    lisp_list_s *l;
    
    func_s();


    if (!form->list) return SYNTAX_INVALID;
    
    if (!form->list->next) {

	debug_err("null list \n");	
	return SYNTAX_ERR;
    }


    debug("list form \n");    
    list_show(form->list);

    l = form->list->next->next;
    if (!l) {

	debug_err("function name not found in list form \n");
	ml_err_signal(ML_ERR_SYNTAX);
	goto FAIL;
    }


    char *syntax_obj_name = l->obj.token.value.symbol;
    debug("find syntax object: %s \n", syntax_obj_name);
    
    htab_entry_s *item = pop_syntax_htab(syntax_obj_name);
    if (!item) {

	debug_err("syntax object \"%s\" is not created \n", syntax_obj_name);
	goto FAIL;
    }
    
    if (!item->data) {

	debug_err("AST tree of \"%s\" is not constructed \n", syntax_obj_name);
	goto FAIL;
    }


    debug("found \n");
     
    /* track the tree to find the given path
     */
    lisp_list_s *path = form->list->next;
    tr_node_s *nd = find_path((tr_node_s*)item->data, path);
    if (!nd) goto FAIL;
    
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
	    //debug("COMPOUND_FUNCTION_FORM \n");

	    if (f->list) {
		
		rt = check_list_form_syntax(f);
		if (rt != SYNTAX_OK) return rt;
	    }
	    else {
	    }

	    
	    break;

	case COMPOUND_SPECIAL_FORM:
	    //debug("COMPOUND_SPECIAL_FORM \n");

	    if (f->list) {
		
		rt = check_list_form_syntax(f);
		if (rt != SYNTAX_OK) return rt;
	    }
	    else {
	    }

	    
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
    if (!rti) {

	func_fail();
	return NULL;
    }
    
    func_ok();
    return rti;
}
