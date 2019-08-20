

#include "eval.h"

#include "debug.h"

#include "mem.h"

#include "error.h"

#include "list.h"

#include "token.h"

#include "variable.h"

#include "printer.h"



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

    token_s *l = &((eval_value_s*)left)->obj_out.token;
    token_s *r = &((eval_value_s*)right)->obj_in->token;


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

    token_s *l = &((eval_value_s*)left)->obj_out.token;
    token_s *r = &((eval_value_s*)right)->obj_in->token;

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

    token_s *l = &((eval_value_s*)left)->obj_out.token;
    token_s *r = &((eval_value_s*)right)->obj_in->token;

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

    token_s *l = &((eval_value_s*)left)->obj_out.token;
    token_s *r = &((eval_value_s*)right)->obj_in->token;

    
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

    lisp_list_s *list = &((eval_value_s*)left)->list;
    object_s *obj = ((eval_value_s*)right)->obj_in;

    if (!obj) {

	//debug_err("%s: object is null \r", __func__);
	//return false;
    }
    
    if (list->obj.type == OBJ_UNKNOWN) {

	list->obj.type = OBJ_LIST;
	list->is_head = true;
    }
    
    
    /* add an object into the list
     */
    if (!list_add_object(list, obj)) {

	func_fail();
	return false;
    }

    list_show(list);

    func_ok();
    return true;
}


static bool
eval_car(void *left, void *right)
{
    func_s();

    object_s *obj_out = &((eval_value_s*)left)->obj_out;
    lisp_list_s *list_in = &((eval_value_s*)right)->list;


    if (obj_out->type == OBJ_TYPE) {

	ml_err_signal(ML_ERR_SYNTAX_CAR);
	return false;
    }

    if (!list_in) {

	debug_err("NULL list \n");
	return false;
    }

    if (!list_in->next) {

	debug_err("nil list \n");
	return true;
    }
   
    
    debug("car of list is: ");
    debug("%d \n", list_in->next->next->obj.token.value.num_int);
    
    memcpy(obj_out, &list_in->next->next->obj, sizeof(object_s));
	    
    func_ok();
    return true;
}



static bool
eval_cdr(void *left, void *right)
{
    func_s();

    lisp_list_s *list_out = &((eval_value_s*)left)->list;
    lisp_list_s *list_in = &((eval_value_s*)right)->list;

    if (!list_in) {

	debug_err("NULL list \n");
	return false;
    }

    if (!list_in->next) {

	debug_err("nil list \n");
	return true;
    }

    lisp_list_s *l = list_in->next->next->next;
    
    while (l && l != list_in) {
	
	if (!list_add_object(list_out, &l->obj)) {

	    func_fail();
	    return false;
	}

	l = l->next;

	if (l->next && l->next->is_head) break;
    }

    list_show(list_out);
     
    func_ok();
    return true;
}


static bool
eval_cons(void *left, void *right)
{
    func_s();

    lisp_list_s *list_out = &((eval_value_s*)left)->list;
    lisp_list_s *list_in = &((eval_value_s*)right)->list;

    if (!list_in) {

	debug_err("NULL list \n");
	return false;
    }

    if (!list_in->next) {

	debug_err("nil list \n");
	return true;
    }

    lisp_list_s *l = list_in->next;
    
    while (l && l != list_in) {
	
	if (!list_add_object(list_out, &l->obj)) {

	    func_fail();
	    return false;
	}

	l = l->next;
    }

    list_show(list_out);
    
    func_ok();
    return true;
}


typedef bool (*eval_func_f)(void *left, void *right);
typedef bool (*get_result_f)(void *in, void *out);

typedef struct
{
    char *name;
    
    eval_func_f eval;

    get_result_f get_result;
    
    
} eval_func_s;


static const eval_func_s m_funcs[] =
{
    { "+", arithmetic_add, NULL},
    { "-", arithmetic_minus, NULL},
    { "*", arithmetic_product, NULL},
    { "/", arithmetic_divide, NULL},

    { "list", eval_list, NULL},
    { "car", eval_car, NULL},
    { "cdr", eval_cdr, NULL},
    { "cons", eval_cons, NULL},

    

};



const eval_func_s*
match_func(char *name)
{
    int len = (int)sizeof(m_funcs) / sizeof(m_funcs[0]);
    for (int i = 0; i < len; i++) {

	if (!strcmp(name, m_funcs[i].name)) return &m_funcs[i];
	
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
    eval_value_s value;
    
    func_s();


    
    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    debug("list form \n");
    list_show(form->list);

    /* get the function name
     */
    l = l->next;
    if (!l) {
      
	debug_err("function name not found \n");
	return EVAL_ERR_NULL;
    }

  
    char *func_name = l->obj.token.value.symbol;
    const eval_func_s *eval_func = match_func(func_name);
    if (!eval_func) {

	debug_err("undefined function: %s \n", func_name);
	
	return EVAL_ERR;
    }

    debug("function: %s \n", func_name);

    
    /* evaluate the list
     */
    l = l->next;
    val->list.is_head = true;
    while (l) {

	if (l->obj.type == OBJ_LIST) {

	    debug("OBJ_LIST \n");

	    form_s *subform = l->obj.sub;
	    if (subform) {
		debug("eval sub_form \n");
	    }
	    
	    memset(&value, 0, sizeof(eval_value_s));
	    
	    eval_rt_t rt = eval_function_form(subform->next, &value);
	    if (rt != EVAL_OK) return rt;

	    debug("eval sub_form done \n");
	    
	    debug("add sublist to the result \n");

	    debug("eval %s \n", func_name);
	    value.obj_in = &value.list.obj;
	    eval_func->eval(val, &value);
	}
	else if (l->obj.type == OBJ_TYPE) {

	    debug("OBJ_TYPE \n");

	    char *sym = obj_get_symbol(&l->obj);
	    if (sym && !var_is_bound(sym)) {

		debug_err("symbol %s is unbound \n", sym);
		ml_err_signal( ML_ERR_VARIABLE_UNBOUND);
		return EVAL_ERR;		
	    }
	    
	    value.obj_in = &l->obj;
	    eval_func->eval(val, &value);
	}
	else {

	    debug("unkown object, type: %d \n", l->obj.type);
	}

	
	l = l->next;

	/* break if it's ")" as the end of list */
	if (l && l->next == form->list) break;
    }

    if (val->obj_out.type != OBJ_UNKNOWN) {
	debug("result is an object with type: %d \n", val->obj_out.type);
	obj_show(&val->obj_out);
    }
    else {

	debug("add ( \n");
	list_add_char_obj(val->list.next, "(");
    
	debug("add ) \n");
	list_add_char_obj(&val->list, ")");

	list_show(&val->list);
    }
    
    
    if (eval_func->get_result) {

	eval_func->get_result(val, &value);
    }
	    
    func_ok();

    return EVAL_OK;
}


eval_rt_t
eval_symbol_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;

    func_s();

    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    debug("list form \n");
    list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = l->obj.token.value.symbol;
    debug("name: %s \n", name);
    
    const var_binder_s *binder = var_match_binder(name);
    if (!binder) {

	debug_err("undefined binder: %s \n", name);
	
	return EVAL_ERR;
    }

    if (!binder->bind) {

	debug_err("binding function is null \n");
	
	return EVAL_ERR;
    }

    variable_s var;
    binder->bind(&var, l);  
    
    func_ok();

    return EVAL_OK;
}


eval_rt_t
eval_special_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;

    func_s();

    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    debug("list form \n");
    list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = l->obj.token.value.symbol;
    debug("name: %s \n", name);
    
    const var_binder_s *binder = var_match_binder(name);
    if (!binder) {

	debug_err("undefined binder: %s \n", name);
	
	return EVAL_ERR;
    }

    if (!binder->bind) {

	debug_err("binding function is null \n");
	
	return EVAL_ERR;
    }

    variable_s var;
    binder->bind(&var, l);  
    
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

	case COMPOUND_SPECIAL_FORM:

	    rt = eval_special_form(f, &value);
	    if (rt != EVAL_OK) goto FAIL;
	    
	    break;
	    
	case COMPOUND_FUNCTION_FORM:
	    
	    rt = eval_function_form(f, &value);
	    if (rt != EVAL_OK) goto FAIL;

	    printer_print(&value, OBJ_LIST);
	    
	    break;

	case SYMBOL_FORM:
	    
	    rt = eval_symbol_form(f, &value); 
	    if (rt != EVAL_OK) goto FAIL;
	 
	    break;

	case SELF_EVALUATING_FORM:

	    if (f->subtype == NIL_LIST_FORM) {
		debug("() \n");
	    }
	    
	    break;
	    
	default:
	    break;
	    
	}

	f = f->next;
    }
    


    func_ok();

    return EVAL_OK;


  FAIL:
    ml_err_signal(ML_ERR_EVAL);
    return EVAL_ERR;    
}
