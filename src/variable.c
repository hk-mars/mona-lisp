

#include "variable.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "list.h"

#include "eval.h"

#include "hsearch.h"

#include "mem.h"


/* TODO: bin-tree is more extensible than hash-table when there are frequent
 * deletion and resize operations.
 */
static hash_table_s m_lexical_htab;  /* hash table for lexical variables */
static hash_table_s m_dynamic_htab;  /* hash table for dynamic variables */
static hash_table_s m_constant_htab;  /* hash table for constants */


static bool binding_setq(variable_s *var, void *context, eval_value_s *result);

static bool binding_defconstant(variable_s *var, void *context, eval_value_s *result);

const var_binder_s m_binders[] =
{
    { "setq", binding_setq },
    
    { "setf", NULL },
    
    { "let", NULL },
    
    { "let*", NULL },
    
    { "defvar", NULL },
    
    { "defconstant", binding_defconstant },
    
    { "lambda", NULL },

     /* useful when writing interpreters for languages embedded in Lisp */
    { "progv", NULL },

    /* TODO: add more binders */
    
};


static void
show_setq_pair(pair_s *pair)
{
    bool found;

    func_s();

    
    
    switch (pair->val.type) {

    case OBJ_CHARACTER:

	//debug("OBJ_CHARACTER \n");
	
	debug("pair= var: %s, character, value: 0x%x \n",
	      pair->var_name,
	      pair->val.character[0]);

	found = true;
	break;

    default:
	found = false;
	break;
    }

    if (found) return;

    //debug("check token \n");
    
    switch (pair->val.token.type) {

    case TOKEN_NUM_INT:
	debug("pair= var: %s, value: %d \n",
	      pair->var_name,
	      pair->val.token.value.num_int);
	
	break;

    case TOKEN_NUM_FLOAT:
	debug("pair= var: %s, value: %f \n",
	      pair->var_name,
	      pair->val.token.value.num_float);
	
	break;

    case TOKEN_NUM_RATIO:
	debug("pair= var: %s, value: %d/%d \n",
	      pair->var_name,
	      pair->val.token.value.num_ratio.int_up,
	      pair->val.token.value.num_ratio.int_down);
	
	break;

	
    case TOKEN_SYMBOL:
	debug("pair= var: %s, value: %s \n",
	      pair->var_name,
	      pair->val.token.value.symbol);
	break;
	
    default:

	debug_err("unkown var pair type \n");
	break;

    }
}


void
var_show(variable_s *var)
{
    bool found;

    func_s();
    
    switch (var->val.type) {

    case OBJ_CHARACTER:
	//debug("OBJ_CHARACTER \n");
	
	debug("var: %s, value: %s \n",
	      var->name,
	      var->val.character);

	found = true;
	break;

    default:
	found = false;
	break;
    }

    if (found) return;

    //debug("check token \n");
     
    switch (var->val.token.type) {

    case TOKEN_NUM_INT:
	//debug("TOKEN_NUM_INT \n");
	
	debug("var: %s, value: %d \n",
	      var->name,
	      var->val.token.value.num_int);
	
	break;

    case TOKEN_NUM_FLOAT:
	debug("var: %s, value: %f \n",
	      var->name,
	      var->val.token.value.num_float);
	
	break;

    case TOKEN_NUM_RATIO:
	debug("var: %s, value: %d/%d \n",
	      var->name,
	      var->val.token.value.num_ratio.int_up,
	      var->val.token.value.num_ratio.int_down);
	
	break;

	
    case TOKEN_SYMBOL:
	//debug("TOKEN_SYMBOL \n");
	debug("var: %s, value: %s \n",
	      var->name,
	      var->val.token.value.symbol);
	break;
	
    default:

	debug_err("unkown var type \n");
	break;

    }
}


static bool
binding_setq(variable_s *var, void *context, eval_value_s *result)
{
    lisp_list_s *head = (lisp_list_s*)context;
    lisp_list_s *l;
    pair_s pair;
    int i;
 
    func_s();

    i = 1;
    l = head->next->next;
    while (l) {

	i++;
	
	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");

	    if (i%2 != 0) {

	        debug_err("should be symbol but not list form \n");
		ml_err_signal(ML_ERR_BIND_VARIABLE);
		return false;
	    }

	    form_s *subform = l->obj.sub;
	    if (subform) {
		debug("eval sub_form \n");
	    }
	    
	    memset(result, 0, sizeof(eval_value_s));
	    eval_rt_t rt = eval(subform, result);
	    if (rt != EVAL_OK) return false;

	    debug("eval sub_form done \n");

	    
	    if (result->obj_out.type != OBJ_UNKNOWN) {

		memcpy(&pair.val,
		       &result->obj_out,
		       sizeof(object_s));	      
		obj_show(&pair.val);
	    }
	    else {

		list_show(&result->list);
		list_copy(&pair.val_list, &result->list);
		list_show(&pair.val_list);
	    }
	  	    
	    pair.var_name = l->front->obj.token.value.symbol;
	}
	else if (l->obj.type == OBJ_TYPE) {

	    debug("OBJ_TYPE \n");

	    if (i%2 == 0) {

		memcpy(&pair.val, &l->obj, sizeof(var_value_s));
			    
		pair.var_name = l->front->obj.token.value.symbol;
		
	    }
	}
	else if (l->obj.type == OBJ_CHARACTER) {

	    debug("OBJ_CHARACTER \n");

	    if (i%2 == 0) {

		memcpy(&pair.val, &l->obj, sizeof(var_value_s));
			    
		pair.var_name = l->front->obj.token.value.symbol;
		//debug("%s %s \n", pair.var_name, pair.val.character);
	    }	    

	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);

	    ml_err_signal(ML_ERR_BIND_VARIABLE);

	    return false;
	}


	if (i%2 == 0) {

	    show_setq_pair(&pair);

	    var->name = pair.var_name;
	    var->type = VAR_LEXICAL;
	    
	    if (pair.val.type == OBJ_UNKNOWN) {
		
		
		list_copy(&var->val_list, &pair.val_list);
		list_show(&var->val_list);
	    }
	    else {

		memcpy(&var->val, &pair.val, sizeof(var_value_s));
		obj_show(&var->val);
		
	    }
	    
	    if (!var_add(var)) {

		if (!var_update(var)) {

		    return false;
		}
	    }
	    
	}
	
	l = l->next;
	if (l->next && l->next->is_head) break;
    }    
    

    func_ok();
    return true;
}


static bool
binding_defconstant(variable_s *var, void *context, eval_value_s *result)
{
    lisp_list_s *head = (lisp_list_s*)context;
    lisp_list_s *l;
    int i;
 
    func_s();

    i = 1;
    l = head;
    while (l) {

	i++;
	
	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");

	    if (i%2 != 0) {

	        debug_err("should be symbol but not list form \n");
		ml_err_signal(ML_ERR_BIND_VARIABLE);
		return false;
	    }

	    form_s *subform = l->obj.sub;

	    memset(result, 0, sizeof(eval_value_s));	    
	    eval_rt_t rt = eval(subform, result);
	    if (rt != EVAL_OK) return false;

	    var->name = l->front->obj.token.value.symbol;
	    memcpy(&var->val.token, &l->obj.token, sizeof(token_s));   
	}
	else if (l->obj.type == OBJ_TYPE) {

	    debug("OBJ_TYPE \n");

	    if (i%2 == 0) {
		
		var->name = l->front->obj.token.value.symbol;
		memcpy(&var->val.token, &l->obj.token, sizeof(token_s));
	    }
	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);

	    ml_err_signal(ML_ERR_BIND_VARIABLE);

	    return false;
	}
	
	l = l->next;

	if (l->next && l->next->is_head) break;
    }    

    
    var->type = VAR_CONSTANT;
    if (!var_add(var)) {

	return false;
    }
    
    
    func_ok();
    return true;
}



const var_binder_s*
var_match_binder(const char *defined_name)
{
    func_s();
    
    int len = ARR_LEN(m_binders);
    for (int i = 0; i < len; i++) {

	if (!strcasecmp(m_binders[i].defined_name, defined_name)) {

	    func_ok();	    
	    return &m_binders[i];
	}
    }

    return NULL;
}



var_rt_t
var_init(void)
{
    int rt;
    
    func_s();

    memset(&m_lexical_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&m_lexical_htab, 1024);
    if(!rt) return VAL_ERR_CREATE_HTAB;    

    memset(&m_dynamic_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&m_dynamic_htab, 1024);
    if(!rt) return VAL_ERR_CREATE_HTAB;       

    memset(&m_constant_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&m_constant_htab, 1024);
    if(!rt) return VAL_ERR_CREATE_HTAB;


    /* add the constants already defined in syntax
     * e.g.:  t, nil
     */
    variable_s var;
    var.name = "t";
    var.val.type = OBJ_TYPE;
    var.val.subtype = OBJ_SUBTYPE_BOOL_TRUE;  
    if (!var_add(&var)) goto FAIL;
    var.name = "T";
    if (!var_add(&var)) goto FAIL;

    var.name = "nil";
    var.val.subtype = OBJ_SUBTYPE_BOOL_FALSE;
    if (!var_add(&var)) goto FAIL;
    var.name = "NIL";
    if (!var_add(&var)) goto FAIL;   
 
    
    func_ok();
    return VAR_OK;

  FAIL:
    func_fail();
    return VAR_ERR;
}


/** 
 * add variable
 */
bool
var_add(variable_s *var)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;

    func_s();
    
    debug("var->name: %s \n", var->name);
    
    entry.key = var->name;
    entry_rt = hsearch(&m_lexical_htab, entry, FIND);
    if (entry_rt) {

	debug_err("varible %s has existed \n", var->name);

	return false;
    }

    variable_s *v = (variable_s*)ml_malloc(sizeof(variable_s));
    if (!v) return false;
    
    memcpy(v, var, sizeof(variable_s));
      
    entry.key = v->name;
    entry.data = v;
    entry_rt = hsearch(&m_lexical_htab, entry, ENTER);
    if (!entry_rt) {

	debug_err("push varible %s into hash table, failed \n", v->name);

	ml_free(v);
	return false;
    }
    
    var_get(v->name);
    
    func_ok();
    return true;
}


/** 
 * delete variable as name
 */
bool
var_delete(char *name)
{

    func_ok();
    return true;
}


/** 
 * get variable as name
 */
variable_s*
var_get(char *name)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;
    variable_s *var;
    
    func_s();
    
    debug("name: %s, %dbytes\n", name, strlen(name));
    
    
    entry.key = name;
    
    entry_rt = hsearch(&m_lexical_htab, entry, FIND);
    if (entry_rt) {

	goto FOUND;
    }
    
    entry_rt = hsearch(&m_constant_htab, entry, FIND);
    if (entry_rt) {
   
	goto FOUND;
    }

    return NULL;

  FOUND:

    var = (variable_s*)entry_rt->data;
    
    var_show(var);
    
    func_ok();
    return entry_rt->data;
}


/** 
 * update value of variable
 */
bool
var_update(variable_s *var_new)
{
    func_s();
    
    variable_s *var = var_get(var_new->name);
    if (!var) return false;

    memcpy(&var->val, &var_new->val, sizeof(var_value_s));   

    var_show(var);
    
    func_ok();
    return true;
}


bool
var_is_bound(char *name)
{
    if (!name) return false;
    
    return !!var_get(name);
}

