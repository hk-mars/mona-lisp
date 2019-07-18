

#include "eval.h"

#include "debug.h"

#include "mem.h"

#include "error.h"

#include "list.h"

#include "token.h"

#include "variable.h"



/** 
 * A Common Lisp system evaluates forms with respect to lexical, dynamic, and global
 * environments. 
 *
 * Evaluation can be understood in terms of a model in which an interpreter  
 * recursively traverses a form performing each step of the computation as it goes.
 *
 * Forms fall into three categories: symbols, conses, and self-evaluating objects.
 */


/**
 *  The rules of evaluation:
 *  1. rules of evaluating a compound form, which is classified as:
 *     a special form, a macro form, a function form, or a lambda form.
 *
 *
 */


static bool
arithmetic_add(void *left, void *right)
{
    func_s();

    eval_value_s  *l = left;
    eval_value_s  *r = right;


    if (l->type  == TOKEN_UNKOWN) {

	l->value.num_int = r->value.num_int;
	l->type = r->type;
    }
    else {

	l->value.num_int += r->value.num_int;
    }
    

    debug("cur val: %d \n", l->value.num_int);
    
    return true;
}


static bool
arithmetic_minus(void *left, void *right)
{
    func_s();

    eval_value_s  *l = left;
    eval_value_s  *r = right;


    if (l->type  == TOKEN_UNKOWN) {

	l->value.num_int = r->value.num_int;
	l->type = r->type;
    }
    else {

	l->value.num_int -= r->value.num_int;
    }
    

    debug("cur val: %d \n", l->value.num_int);
    
    return true;
}


static bool
arithmetic_product(void *left, void *right)
{
    func_s();

    eval_value_s  *l = left;
    eval_value_s  *r = right;


    if (l->type  == TOKEN_UNKOWN) {

	l->value.num_int = r->value.num_int;
	l->type = r->type;
    }
    else {

	l->value.num_int *= r->value.num_int;
    }
    

    debug("cur val: %d \n", l->value.num_int);
    
    return true;
}


static bool
arithmetic_divide(void *left, void *right)
{
    func_s();

    eval_value_s  *l = left;
    eval_value_s  *r = right;


    if (l->type  == TOKEN_UNKOWN) {

	l->value.num_int = r->value.num_int;
	l->type = r->type;
    }
    else {

	if (r->value.num_int == 0) {
	    
	    debug_err("error: divide zero \n");
	    ml_err_signal(ML_ERR_EVAL_DIVIDE_ZERO);
	    return false;
	}
	
	l->value.num_int /= r->value.num_int;
    }
    

    debug("cur val: %d \n", l->value.num_int);
    
    return true;
}


static bool
eval_list(void *left, void *right)
{
    func_s();


    func_ok();
    return true;
}


static bool
eval_car(void *left, void *right)
{
    func_s();


    func_ok();
    return true;
}


static bool
eval_cdr(void *left, void *right)
{
    func_s();


    func_ok();
    return true;
}


static bool
eval_cons(void *left, void *right)
{
    func_s();


    func_ok();
    return true;
}


typedef bool (*eval_func_f)(void *left, void *right);

typedef struct
{
    char *name;
    
    eval_func_f f;
    
} eval_func_s;


static const eval_func_s m_funcs[] =
{
    { "+", arithmetic_add },
    { "-", arithmetic_minus },
    { "*", arithmetic_product },
    { "/", arithmetic_divide },

    { "list", eval_list},
    { "car", eval_car},
    { "cdr", eval_cdr},
    { "cons", eval_cons},

    

};



eval_func_f
match_func(char *name)
{
    int len = (int)sizeof(m_funcs) / sizeof(m_funcs[0]);
    for (int i = 0; i < len; i++) {

	if (!strcmp(name, m_funcs[i].name)) return m_funcs[i].f;
	
    }

    return NULL;
}



/**
 * The rule of evaluating a function form:
 * The subforms in the cdr of the original form are evaluated in left-to-right 
 * order in the current lexical and dynamic environments. The primary value of 
 * each such evaluation becomes an argument to the named function; any additional 
 * values returned by the subforms are discarded.
 *
 */
static eval_rt_t
eval_function_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;

    func_s();

    
    if (!form->list->next) {

	debug("null form \n");
	return EVAL_ERR;
    }

    l = form->list->next;
    
    debug("%s \n", l->obj.token.value.symbol);

    eval_func_f f = match_func(l->obj.token.value.symbol);
    if (!f) {

	debug_err("undefined function \n");
	
	return EVAL_ERR;
    }

    l = l->next;
    
    while (l && l != form->list) {

	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");

	    form_s *subform = l->obj.sub;
	    if (subform) {
		debug("eval sub_form \n");
	    }
	    
	    eval_value_s value;
	    memset(&value, 0, sizeof(eval_value_s));
	    
	    eval_rt_t rt = eval_function_form(subform->next, &value);
	    if (rt != EVAL_OK) return rt;

	    debug("val: %d, val2: %d \n", val->value.num_int, value.value.num_int);
	    f(val, &value);
	}
	else if (l->obj.type == OBJ_TYPE) {

	    debug("OBJ_TYPE \n");
	    
	    f(val, &l->obj.token);

	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);
	}

	
	l = l->next;
    }

    
    func_ok();

    return EVAL_OK;
}


eval_rt_t
eval_symbol_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;

    func_s();

    l = form->list->next;

    debug("%s \n", l->obj.token.value.symbol);
    
    const var_binder_s *binder = var_match_binder(l->obj.token.value.symbol);
    if (!binder) {

	debug_err("undefined binder \n");
	
	return EVAL_ERR;
    }

    if (!binder->bind) {

	debug_err("binding function is null \n");
	
	return EVAL_ERR;
    }

    variable_s var;
    binder->bind(&var, form->list);  
    
    func_ok();

    return EVAL_OK;
}



eval_rt_t
eval(form_s *forms)
{
    eval_rt_t rt;
    eval_value_s value;
    
    if (!forms) return EVAL_ERR_NULL;

    func_s();

    form_s *f = forms->next;

    while (f && f != forms) {
	
	memset(&value, 0, sizeof(eval_value_s));
    
	switch (f->type) {

	case COMPOUND_FUNCTION_FORM:
	    
	    rt = eval_function_form(f, &value);
	    if (rt != EVAL_OK) {
		
		ml_err_signal(ML_ERR_EVAL);
		return EVAL_ERR;
	    }

	    memcpy(&f->list->obj.token, &value, sizeof(eval_value_s));
	    
	    break;

	case SYMBOL_FORM:

	    rt = eval_symbol_form(f, &value); 
	    if (rt != EVAL_OK) {
		
		ml_err_signal(ML_ERR_EVAL);
		return EVAL_ERR;
	    }

	    memcpy(&f->list->obj.token, &value, sizeof(eval_value_s));

	    break;
	    
	default:
	    break;
	    
	}

	f = f->next;
    }
    


    func_ok();

    return EVAL_OK;
}
