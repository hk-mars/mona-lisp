

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

#include "ast_tree.h"


#define SYNTAX_DEBUG_ON true
#if !SYNTAX_DEBUG_ON
#undef debug
#define debug(...) ;
#endif



static hash_table_s syntax_htab;


/* check S-Function
 */
static bool is_atom(form_s *form);
static bool is_eq(form_s *form);
static bool is_cons(form_s *form);
static bool is_car(form_s *form);
static bool is_cdr(form_s *form);
static bool is_list(form_s *form);
static bool is_print(form_s *form);
static bool is_num_add(form_s *form);


/* check Special Form
 */
static bool is_setq(form_s *form);
static bool is_if(form_s *form); 




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
  
    if (is_token_node(root)) {
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

    //debug("%s \n", root->key);

    if (!strcmp(root->key, "<tmp>")) {

	if (!root->sub && !root->left && !root->right) {

	    return root;
	}
    }
    
    return NULL;
}


static char*
get_token_name(token_s *tk, bool is_specified_type, char *key)
{
    char *name;

    name = NULL;
    switch (tk->type) {

    case TOKEN_SYMBOL:
   
	//debug("TOKEN_SYMBOL: %s \n", tk->value.symbol);

	name = tk->value.symbol;
	break;

    case TOKEN_NUM_INT:
   
	//debug("TOKEN_NUM_INT \n");

	if (is_specified_type) {
	    name = "number-token ::=";
	}
	else {
	    if (strcmp(key, "number-token ::=")) {
		name = "token ::=";
	    }
	    else {
		name = "number-token ::=";
	    }
	}
	
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
get_leaf_name(object_s *obj, char *key)
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

	name = get_token_name(&obj->token, obj->is_specified_type, key);
	
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


static void
show_path(lisp_list_s *start, lisp_list_s *end)
{
    func_s();

    if (!start || !end) return;

    while (1) {

	if (start->front == end) break;
	    
	obj_show(&start->obj);

	start = start->next;
    };
    
    
    
    func_ok();
}


static tr_node_s*
find_path(tr_node_s *root, lisp_list_s *path, lisp_list_s *path_end)
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


    /* Create a sequence leaf list. All sublists should be merged into this leaf list.
       Search all the leafs in AST, if all matched, then a solution found.
       If more than one solution were found, then the AST tree is wrong. */

       
    tr_node_s *rtn, *nd;
    ENTRY *rti;
    lisp_list_s *sl, *el;


    //func_s();
    
    nd = NULL;
  
    if (!root) return NULL;

#if SYNTAX_DEBUG_ON
    char *name1 = get_leaf_name(&path->obj, root->key);
    if (root->key[strlen(root->key)-1] == '=') {
      debug("cur node: %s, find node: %s \n", root->key, name1);

      //tree_show_node(root);
	
    }
    //token_show(&path->obj.token);

    if (!strcmp(root->key, "@")) {

      debug("cur node: %s, find node: %s \n", root->key, name1);

      tree_show_node(root->left);
    }

#endif
    
    if (is_token_node(root)) {
	//debug("token node: %s \n", root->key);

	//obj_show(&path->obj);
		
	char *name = get_leaf_name(&path->obj, root->key);

	//debug("%s, %s \n", root->key, name);

	if (is_char_node(root) || path->obj.token.type != TOKEN_SYMBOL) {
	    if (strcmp(root->key, name)) {

		return NULL;		
	    }
	    else {

		debug("found token: %s \n", root->key);
		//token_show(&path->obj.token);
		goto FOUND;
	    }

	}
	else {

	    if (is_keyword_node(root)) {

		if (!strcasecmp(root->key, name)) {
	       
		    debug("found token: %s keyword\n", root->key);
		    goto FOUND;
		}
		    
	    }
	    else {

		if (path->obj.token.type == TOKEN_SYMBOL && !ast_lex_is_keyword(name)) {
		    debug("found token: %s variable \n", name);
		    token_show(&path->obj.token);
		    goto FOUND;
		}
		
	    }
	}	
	

	return NULL;

      FOUND:
	/* the end token, check the path if it's at the end.
	 */	
	if (path == path_end) {

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
	    debug("find next: %s \n", get_leaf_name(&path->obj, root->key));
	    obj_show(&path->obj);

	    //tree_show(root, 5);
	    
	    if (is_outside_loop_node(root)) {
		debug("go to outside-loop-node: %s \n", root->key);
		rtn = find_path(root, path, path_end);
		if (rtn) return rtn;
	    }

	    if (!root->left && !root->right && !root->sub) {

		debug("end node \n");
		//debug_suspend();
		
	    }
	    
	    goto NEXT0;
	}

    }
  
 
    if (path->obj.type == OBJ_LIST) {
	    
	debug("subform \n");
	form_s *subform = path->obj.sub;
	if (!subform->next) {
	    debug_err("subform is null \n");
	    return NULL;
	}
	
	list_show(subform->next->list);

	//char *name = subform->next->list->next->next->obj.token.value.symbol;

	if (root->loop) {

	    
	    debug("find subform in loop node %s \n", root->key);

	    
	    rtn = find_path(root->loop,
			    subform->next->list->next,
			    subform->next->list->front);
	    if (rtn) {
		
		if (root->loop) debug("subform found in loop node %s \n", root->loop->key);
		goto FIND_SUBFORM_DONE; 
	    }
	
	}

	
	rtn = find_path(root->sub,
			subform->next->list->next,
			subform->next->list->front);
	if (rtn) {

	    if (root->sub) debug("subform found in sub node %s \n", root->sub->key);
	    goto FIND_SUBFORM_DONE;
	}
	
	rtn = find_path(root->left,
			subform->next->list->next,
			subform->next->list->front);
	if (rtn) {

	    if (root->left) debug("subform found in left node %s \n", root->left->key);
	    root = root->left;
	    goto FIND_SUBFORM_DONE;
	}

	rtn = find_path(root->right,
			subform->next->list->next,
			subform->next->list->front);
	if (rtn) {

	    if (root->right) debug("subform found in right node %s \n", root->right->key);
	    root = root->right;
	    goto FIND_SUBFORM_DONE;
	}
	else {
	    goto LOOP_NODE;
	}

	

      FIND_SUBFORM_DONE:
	debug("subform found \n");

	
	debug("rtn: %s \n", rtn->key);

	debug("find next: %s \n", get_leaf_name(&path->next->obj, root->key));

	if (rtn->loop) {

	    debug("ignore loop node \n");
	}
	
	if (path == path_end) {

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
	      
	    tree_show(root->father, 9);

	    /* 
	     * Here, if father of the current node is an external loop node, then we shuould
	     * directly keep on searching from its father, because the external loop node 
	     * includes all solution of its childs.  
	     */	  
	    if (is_outside_loop_node(root->father)) {
		//root = root->father;
		debug("go to outside-loop-node: %s \n", root->key);
		rtn = find_path(root->father, path, path_end);
		if (rtn) return rtn;

		//debug("found from %s \n", root->key);
		//debug_suspend();
	    }
	    
	    goto NEXT0;
	}
    } 


  LOOP_NODE:
    if (root->loop) {
	debug("loop node: %s \n", root->key);
	nd = root->loop;

	rtn = find_path(nd, path, path_end);
	if (rtn) return rtn;
    }
  

    
    if (is_in_syntax_tree(root) && !nd) {
  
	/* get the root of sub tree from hash table.
	 */
	rti = pop_syntax_htab(root->key);
	if (!rti) return NULL;
	if (!rti->data) {
	    debug("sub tree not found: %s \n", root->key);
	    return NULL;
	}

	if (!tree_search_node(rti->data, root)) {
	    nd = rti->data;
	    debug("syntax sub tree found: %s \n", root->key);

	    tree_show(nd, 25);
	    
	    //debug_suspend();


	    /* ... a b c d e f g
	     * sub path:
	     * ... a 
	     * ... a b
	     * ... a b c 
	     * ... a b c d
	     * ... a b c d e 
	     * ... a b c d e f
	     * ... a b c d e f g
	     *
	     * create a new path to be searched.
	     *
	     */

	    sl = path;
	    el = path_end;
	 
	    show_path(sl, el);
	    //debug_suspend();

	    tr_node_s *node = NULL;
	    
	    while (1) {

		debug("search path: \n");
		show_path(sl, el);
		
		rtn = find_path(nd, sl, el);
		
		if (rtn) {

		    if (!node) node = root;
		    show_path(sl, el);
		    //debug_suspend();

		    if (el->next->is_head) {

			debug("solution found \n");
			return rtn;
		    }

			    
		    /* go on searching from the next node
		     */
		    
		    debug("go on searching \n");
		    path = el->next;
		    show_path(path, path_end);
		    tree_show(root, 10);
		    //debug_suspend();
		    break;		    
	
		}

		if (el == sl) {

		    debug("the last sub path \n");

		    return NULL;		    
		}

		debug("path not found: \n");
		show_path(sl, el);


		el = el->front;
		
	    }
	}
    }


  NEXT0:

    if (root->back) {

	if (root->loop) {

	    debug("loop node \n");
	    debug("back node: %s from %s \n", root->back->key, root->key);
	    
	    goto NEXT;
	}
	
	/* TBD:
	 */
	if (!memcmp(root->back->key, root->key, strlen(root->back->key))) {

	    debug("back-node: %s of %s \n", root->back->key, root->key);

	    tree_show(root->father->father, 5);
	    
	    if (!is_outside_loop_node(root->father->father)) {
		
		goto NEXT;
	
	    }
	    
	    debug("father-fhater is outside-loop-node: %s \n", root->father->father->key);
	}
	
	debug("go back to node: %s from %s \n", root->back->key, root->key);
	rtn = find_path(root->back, path, path_end);
	if (rtn) return rtn;
    }

   NEXT:    
    if (root->sub) {
	//debug("sub: %s \n", root->sub->key);
	root->next = root->sub;
	rtn = find_path(root->sub, path, path_end);
	if (rtn) return rtn;
    }
  
    if (root->left) {
	//debug("left: %s \n", root->left->key);
	root->next = root->left;
	rtn = find_path(root->left, path, path_end);
	if (rtn) return rtn;
    }
  
    if (root->right) {
	//debug("right: %s \n", root->right->key);
	root->next = root->right;
	rtn = find_path(root->right, path, path_end);
	if (rtn) return rtn;
    }

    
    return NULL;
}


static syntax_rt_t
check_list_form_syntax(form_s *form)
{
    lisp_list_s *l;
    lisp_list_s *path;
    
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

    if (!strcmp(syntax_obj_name, "+")) {
	syntax_obj_name = "num-add ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, "<")) {
	syntax_obj_name = "num-less-than ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, "<=")) {
	syntax_obj_name = "num-less-or-equal-than ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, ">")) {
	syntax_obj_name = "num-greater-than ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, ">=")) {
	syntax_obj_name = "num-greater-or-equal-than ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, "=")) {
	syntax_obj_name = "num-equal-than ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, "/=")) {
	syntax_obj_name = "num-not-equal-than ::=";

	list_mark_type_specified(form->list);
    }
    else if (!strcmp(syntax_obj_name, "!=")) {
	syntax_obj_name = "num-not-equal-than ::=";

	list_mark_type_specified(form->list);
    }

    
    debug("find syntax object: %s \n", syntax_obj_name);

    htab_entry_s *item = pop_syntax_htab(syntax_obj_name);
    if (!item) {

	debug_err("syntax object \"%s\" is not created \n", syntax_obj_name);


	if (form->type == COMPOUND_MACRO_FORM) {

	    /* TODO: adding the macro syntax defined by user into the AST tree
	     */
	    debug("a possible macro name: %s \n", syntax_obj_name);
	    debug("The syntax would be checked during evaluating. \n ");

	    goto DONE;
	    
	}
	else if (form->type == COMPOUND_FUNCTION_FORM) {
	    
	    debug("a function call: %s \n", syntax_obj_name);
	    
	    debug("The syntax would be checked during evaluating. \n ");

	    goto DONE;
	    
	}

	goto FAIL;
    }
    else {

	path = form->list->next;
    }
    
    if (!item->data) {

	debug_err("AST tree of \"%s\" is not constructed \n", syntax_obj_name);
	goto FAIL;
    }
    else {

	tree_show(item->data, 20);

	debug_suspend();
    }


    debug("found \n");

     
    /* track the tree to find the given path
     */
    tr_node_s *nd = find_path((tr_node_s*)item->data, path, form->list->front);
    if (!nd) goto FAIL;
    //bool rt = is_atom(path, form->list->front);
    //if (!rt) goto FAIL;

  DONE:
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

	form_show_type(f);
	
	switch (f->type) {

	case SELF_EVALUATING_FORM:

	    /* syntax of self-evaluating objects have been checked during lexcial parsing.
	     */
	    break;
	    
	case COMPOUND_FUNCTION_FORM:

	    if (is_atom(f)) goto NEXT;
	    if (is_eq(f)) goto NEXT;
	    if (is_cons(f)) goto NEXT;
	    if (is_car(f)) goto NEXT;
	    if (is_cdr(f)) goto NEXT;
	    if (is_list(f)) goto NEXT;
	    if (is_print(f)) goto NEXT;
	    if (is_num_add(f)) goto NEXT;

	    
	    if (f->list) {
		
		rt = check_list_form_syntax(f);
		if (rt != SYNTAX_OK) return rt;

	    }
	    
	    break;

	case COMPOUND_SPECIAL_FORM:

	    if (is_setq(f)) goto NEXT;
	    if (is_if(f)) goto NEXT;
	    
	    if (!f->obj) {
		
		rt = check_list_form_syntax(f);
		if (rt != SYNTAX_OK) return rt;
	    }
	    else {

		/* TODO: check the syntax like Single-Quote form.
		 */
		obj_show(f->obj);
		//debug_suspend();
		goto NEXT;
	    }

	    
	    break;
	    
	case SYMBOL_FORM:
	    
	    break;

	case COMPOUND_MACRO_FORM:
	    
	    if (f->list) {
		
		rt = check_list_form_syntax(f);
		if (rt != SYNTAX_OK) return rt;
	    }
	    
	    break;	    

	default:

	    break;
	}
	
      NEXT:
	f = f->next;
    }
    

    out(ok, SYNTAX_OK);
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

    if (rti) mark_in_syntax_tree(root);
    
    //debug("%s, key: %s \n", __func__, key);
    //debug_suspend();
    
    return !!rti;
}


ENTRY*
pop_syntax_htab(char *key)
{
    ENTRY item, *rti;
    char buf[128];

    func_s();
  
    if (!syntax_htab.table) return NULL;

    memset(&item, 0, sizeof(ENTRY));
    
    unsigned long len = strlen(key);
    if (len > strlen(" ::=")) {

	len -= strlen(" ::=");
	if (strcmp(key+len, " ::=")) {

	    memset(buf, 0, sizeof(buf));
	    strcat(strcat(buf, key), " ::=");
	    item.key = buf;
	}
	else {

	    item.key = key;
	}
    }
    else {

	memset(buf, 0, sizeof(buf));
	strcat(strcat(buf, key), " ::=");
	item.key = buf;
    }
    
    
    
    rti = hsearch(&syntax_htab, item, FIND);
    if (!rti) {
	
	func_fail();
	return NULL;
    }

    debug("%s, found key: %s \n", __func__, key);
    //debug_suspend();
    
    func_ok();
    return rti;
}


/**
Check the syntax of the fundamental Recursive Functions of Symbolic Expressions:

a. A Class of Symbolic Expressions. We shall now define the S-expressions 
(S stands for symbolic). They are formed by using the special characters
·
)
(

a1. S-expressions are then defined as follows:
1. Atomic symbols are S-expressions.
2. If e1 and e2 are S-expressions, so is (e1 · e2).
Examples of S-expressions are
AB
(A·B)
((AB · C) · D)
An S-expression is then simply an ordered pair, the terms of which may be atomic symbols 
or simpler S-expressions.

a2. We can can represent a list of arbi- trary length in terms of S-expressions as follows. 
The list
    (m1,m2,···,mn) 
is represented by the S-expression
    (m1 ·(m2 ·(···(mn ·NIL)···)))
Here NIL is an atomic symbol used to terminate lists.


b. Elementary S-functions and Predicates:

b1. atom. atom[x] has the value of T or F according to whether x is an atomic symbol. 
Thus: 
atom [X] = T 
atom [(X · A)] = F

b2. eq. eq [x;y] is defined if and only if both x and y are atomic.(TODO) 
eq [x; y] = T if x and y are the same symbol, and eq [x; y] = F otherwise.

eq [X; X] = T
eq [X; A] = F
eq [X; (X · A)] is undefined.

b3. car. car[x] is defined if and only if x is not atomic. car [(e1 · e2)] = e1. 
Thus car [X] is undefined.

car [(X · A)] = X
car [((X · A) · Y )] = (X · A)

b4. cdr. cdr [x] is also defined when x is not atomic. We have cdr [(e1 · e2)] = e2. 
Thus cdr [X] is undefined.

cdr [(X ·A)] = A cdr [((X ·A)·Y)] = Y

b5. cons. cons [x; y] is defined for any x and y. We have cons [e1;e2] =
(e1 · e2). Thus

cons [X; A] = (X A)
cons [(X · A); Y ] = ((X · A)Y)

car, cdr, and cons are easily seen to satisfy the relations
car [cons [x; y]] = x
cdr [cons [x; y]] = y
cons [car [x]; cdr [x]] = x, provided that x is not atomic.

c. Recursive S-functions. We get a much larger class of functions (in fact, 
all computable functions) when we allow ourselves to form new functions of 
S-expressions by conditional expressions and recursive definition. 

REFERENCES: 
1. Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I
John McCarthy, Massachusetts Institute of Technology, Cambridge, Mass.
April 1960

*/


static bool
is_atom(form_s *form)
{
    if (form->subtype != S_FUNCTION_ATOM) return false;
    
    func_s();

    /* head -> ( -> atom -> object -> ) -> head
     */
    lisp_list_s *l = form->list;
  
    l = l->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
    
    if (num <= 0) {

	debug_err("no argument in atom form \n");       	
	goto FAIL;
    }
    else if (num > 1) {

	debug_err("%d arguments in atom form, only one requied \n", num);       	
	goto FAIL;	
    }
    
   
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_ATOM);
    out(fail, false);
}    


static bool
is_eq(form_s *form)
{
    if (form->subtype != S_PREDICATE_EQ) return false;
    
    func_s();

    /* head -> ( -> eq -> object -> object -> ) -> head
     */    
    lisp_list_s *l = form->list;

    l = l->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
    
    if (num < 1) {

	debug_err("no argument in eq form, 2 requied. \n");       	
	goto FAIL;
    }
    else if (num == 1 || num > 2) {

	debug_err("%d arguments in eq form, 2 requied. \n", num);       	
	goto FAIL;	
    }
    
   
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_EQ);
    out(fail, false);
}


static bool
is_cons(form_s *form)
{
    if (form->subtype != S_FUNCTION_CONS) return false;
    
    func_s();

    /* head -> ( -> cons -> object -> object -> ) -> head
     */    
    lisp_list_s *l = form->list;

    l = l->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
    
    if (num < 1) {

	debug_err("no argument in cons form, 2 requied. \n");       	
	goto FAIL;
    }
    else if (num == 1 || num > 2) {

	debug_err("%d arguments in cons form, 2 requied. \n", num);       	
	goto FAIL;	
    }
    
   
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_CONS);
    out(fail, false);
}


static bool
is_car(form_s *form)
{
    if (form->subtype != S_FUNCTION_CAR) return false;
    
    func_s();

    /* head -> ( -> car -> a CONS -> ) -> head
     */    
    lisp_list_s *l = form->list;

    l = l->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
    
    if (num < 1) {

	debug_err("no argument in car form, 1 requied. \n");       	
	goto FAIL;
    }
    else if (num > 1) {

	debug_err("%d arguments in car form, 1 requied. \n", num);       	
	goto FAIL;	
    }
    
   
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_CAR);
    out(fail, false);
}


static bool
is_cdr(form_s *form)
{
    if (form->subtype != S_FUNCTION_CDR) return false;
    
    func_s();

    /* head -> ( -> cdr -> a CONS -> ) -> head
     */    
    lisp_list_s *l = form->list;

    l = l->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	    
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
    
    if (num < 1) {

	debug_err("no argument in cdr form, 1 requied. \n");       	
	goto FAIL;
    }
    else if (num > 1) {

	debug_err("%d arguments in cdr form, 1 requied. \n", num);       	
	goto FAIL;	
    }


    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_CDR);
    out(fail, false);
}


static bool
is_list(form_s *form)
{
    if (form->subtype != S_FUNCTION_LIST) return false;
    
    func_s();

    /* head -> ( -> list -> object* -> ) -> head
     */    
    lisp_list_s *l = form->list->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	    
	num++;
    }

    debug("arguments count: %d \n", num);

    form->obj_count = num;
    
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_LIST);
    out(fail, false);
}


static bool
is_setq(form_s *form)
{
    if (form->subtype != SPECIAL_FORM_SETQ) return false;
    
    func_s();

    /* head -> ( -> setq -> pair* -> ) -> head
     * pair: var form
     * var: a symbol
     */    
    lisp_list_s *l = form->list->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	    
	num++;
    }

    debug("arguments count: %d \n", num);

    form->obj_count = num;

    if (num%2 == 1) {

	debug_err("%d arguments in setq form, even number requied. \n", num);
	goto FAIL;
    }
    
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_SETQ);
    out(fail, false);
}


static bool
is_if(form_s *form)
{
    if (form->subtype != SPECIAL_FORM_IF) return false;
    
    func_s();

    /* head -> ( -> if -> test-form -> then-form -> [then-form ->] ) -> head
     */    
    lisp_list_s *l = form->list->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	    
	num++;
    }

    debug("arguments count: %d \n", num);

    form->obj_count = num;

    if (num <= 1 || num > 3) {

    	debug_err("%d arguments in if form, 2 or 3 requied. \n", num);
	//syntax_show(SYNTAX_FORM_IF);
    	goto FAIL;
    }
    
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_IF);
    out(fail, false);
}


static bool
is_print(form_s *form)
{
    if (form->subtype != S_FUNCTION_PRINT) return false;
    
    func_s();

    /* head -> ( -> print -> object -> [output-stream ->] ) -> head
     */    
    lisp_list_s *l = form->list->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
    
    if (num < 1) {

	debug_err("no argument in car form, 1 requied. \n");       	
	goto FAIL;
    }
    else if (num > 1) {

	debug_err("%d arguments in car form, 1 requied. \n", num);       	
	goto FAIL;	
    }
    
   
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_PRINT);
    out(fail, false);
}


static bool
is_num_add(form_s *form)
{
    if (form->subtype != S_FUNCTION_NUM_ADD) return false;
    
    func_s();

    /* head -> ( -> + -> token-number* -> ) -> head
     */    
    lisp_list_s *l = form->list->next->next->next;

    
    /* check the number of arguments
     */
    int num = 0;
    while (l) {

	if (l->obj.sub) {

	    debug("subform \n");

	    if (syntax_check(l->obj.sub) != SYNTAX_OK) {

		goto FAIL;
	    }
	}
	
	l = l->next;
	if (list_is_head(l)) break;
	
	num++;
    }

    debug("arguments count: %d \n", num);
    form->obj_count = num;
   
    out(ok, true);

  FAIL:
    ml_err_signal(ML_ERR_SYNTAX_NUM_ADD);
    out(fail, false);
}











