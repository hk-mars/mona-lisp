

#include "variable.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "list.h"

#include "eval.h"



static bool binding_setq(variable_s *var, void *context);

const var_binder_s m_binders[] =
{
    { "setq", binding_setq },
    
    { "setf", NULL },
    
    { "let", NULL },
    
    { "let*", NULL },
    
    { "defvar", NULL },
    
    { "defconstant", NULL },
    
    { "lambda", NULL },

     /* useful when writing interpreters for languages embedded in Lisp */
    { "progv", NULL },

    /* TODO: add more binders */
    
};


static void
show_setq_pair(pair_s *pair)
{
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


static bool
binding_setq(variable_s *var, void *context)
{
    lisp_list_s *head = (lisp_list_s*)context;
    lisp_list_s *l;
    pair_s pair;
    int i;
 
    func_s();

    i = 1;
    l = head->next->next->next;
    while (l && l != head) {

	i++;
	
	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");

	    if (i%2 != 0) {

	        debug_err("should be symbol but not list form \n");
		ml_err_signal(ML_ERR_BIND_VARIABLE);
		return false;
	    }

	    form_s *subform = l->obj.sub;

	    eval_rt_t rt = eval(subform);
	    if (rt != EVAL_OK) return false;

	    memcpy(&pair.val.token,
		   &subform->next->list->obj.token,
		   sizeof(token_s));

	    pair.var_name = l->front->obj.token.value.symbol;
	}
	else if (l->obj.type == OBJ_TYPE) {

	    debug("OBJ_TYPE \n");

	    if (i%2 == 0) {

		memcpy(&pair.val.token, &l->obj.token, sizeof(token_s));
			    
		pair.var_name = l->front->obj.token.value.symbol;
	    }
	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);

	    ml_err_signal(ML_ERR_BIND_VARIABLE);

	    return false;
	}


	if (i%2 == 0) {
	    show_setq_pair(&pair);
	}
	
	l = l->next;
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


