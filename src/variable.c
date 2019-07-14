

#include "variable.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "list.h"



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

	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");
	    
	}
	else if (l->obj.type == OBJ_TYPE) {

	    i++;
	    
	    debug("OBJ_TYPE \n");

	    if (i%2 == 0) {

		pair.var_name = l->front->obj.token.value.symbol;
		pair.form = NULL;
		debug("pair.symbol: %s \n", pair.var_name);
	    }
	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);

	    ml_err_signal(ML_ERR_BIND_VARIABLE);

	    return false;
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


