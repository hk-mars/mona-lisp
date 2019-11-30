

#include "eval.h"

#include "debug.h"

#include "mem.h"

#include "error.h"

#include "list.h"

#include "token.h"

#include "variable.h"

#include "printer.h"

#include "function.h"

#include "macro.h"

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


typedef bool (*eval_call_f)(void *left, void *right);
typedef bool (*get_result_f)(void *in, void *out);

typedef struct
{
    char *name;
    
    eval_call_f eval;

    get_result_f get_result;
    
    
} eval_call_s;


static eval_rt_t eval_macro_function_form(form_s *form, eval_value_s *val);


void
eval_result_show(eval_value_s *result)
{
    if (!result) return;

    func_s();
    
    if (result->obj_out.type != OBJ_UNKNOWN) {

	obj_show(&result->obj_out);
    }
    else {

	list_show(&result->list);
    }

    func_ok();
}


static bool
arithmetic_add(void *left, void *right)
{
    func_s();

    object_s *obj_l = &((eval_value_s*)left)->obj_out;   
    token_s *l = &obj_l->token;
    token_s *r = &((eval_value_s*)right)->obj_in->token;


    if (l->type  == TOKEN_UNKOWN) {

	l->value.num_int = r->value.num_int;
	l->type = r->type;

	obj_l->type = OBJ_TYPE;

	
	debug("add, v0: ");
	token_show_fixnum(l->value.num_int);
    }
    else {

	l->value.num_int += r->value.num_int;

	debug("add, result: ");
	token_show_fixnum(l->value.num_int);
    }
    
    obj_show(obj_l);
    
    
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
    

    debug("cur val: ");
    token_show_fixnum(l->value.num_int);
    
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
    

    debug("cur val: ");
    token_show_fixnum(l->value.num_int);
    
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
    

    debug("cur val: ");
    token_show_fixnum(l->value.num_int);
    
    return true;
}

static bool num_compare(void *left, void *right, eval_call_f call);

static bool
num_less_than(void *left, void *right)
{
    func_s();
    
    return num_compare(left, right, num_less_than);
}


static bool
num_less_or_equal_than(void *left, void *right)
{
    func_s();

    return num_compare(left, right, num_less_or_equal_than);
}


static bool
num_greater_than(void *left, void *right)
{
    func_s();

    return num_compare(left, right, num_greater_than);
}


static bool
num_greater_or_equal_than(void *left, void *right)
{
    func_s();

    return num_compare(left, right, num_greater_or_equal_than);
}


static bool
num_equal_than(void *left, void *right)
{
    func_s();

    return num_compare(left, right, num_equal_than);
}

static bool
num_not_equal_than(void *left, void *right)
{
    func_s();

    return num_compare(left, right, num_not_equal_than);
}


static bool
num_compare(void *left, void *right, eval_call_f call)
{
    bool flag = false;
    
    func_s();

    token_s *l = &((eval_value_s*)left)->obj_out.token;
    token_s *r = &((eval_value_s*)right)->obj_in->token;
    object_s *obj = &((eval_value_s*)left)->obj_out;

    //obj_show(obj);
    if (obj->type  == OBJ_UNKNOWN) {

	l->value.num_int = r->value.num_int;
	l->type = r->type;
	obj->type = OBJ_TYPE;
	debug("init val: ");
	token_show_fixnum(l->value.num_int);
    }
    else {
	
	if (call == num_less_than) {

	    flag = l->value.num_int < r->value.num_int;
	}
	else if (call == num_less_or_equal_than) {

	    flag = l->value.num_int <= r->value.num_int;
	}
	else if (call == num_greater_than) {

	    flag = l->value.num_int > r->value.num_int;
	}
	else if (call == num_greater_or_equal_than) {

	    flag = l->value.num_int >= r->value.num_int;
	}
	else if (call == num_equal_than) {

	    flag = l->value.num_int == r->value.num_int;
	}
	else if (call == num_not_equal_than) {

	    flag = l->value.num_int != r->value.num_int;
	}
	else {	    
	    goto FAIL;
	}
	
	obj->token.type = TOKEN_UNKOWN;
	obj->subtype = (flag ? OBJ_SUBTYPE_BOOL_TRUE : OBJ_SUBTYPE_BOOL_FALSE);

	if (obj_is_true(obj)) {

	    debug("T \n");
	}
	else {
	    debug("nil \n");
	}
    }
    
    return true;

  FAIL:
    ml_err_signal(ML_ERR_NUM_COMPARE);
    return false;
}



static bool
eval_list(void *left, void *right)
{
    func_s();

    lisp_list_s *list = &((eval_value_s*)left)->list;
    object_s *obj = ((eval_value_s*)right)->obj_in;
    lisp_list_s *list_in = ((eval_value_s*)right)->list_in;
    
    if (!obj) {

	//debug_err("%s: object is null \r", __func__);
	//return false;
    }

    //obj_show(obj);
    
    if (list->obj.type == OBJ_UNKNOWN) {

	list->obj.type = OBJ_LIST;
	list->is_head = true;
    }

    if (obj) {
	
	/* add an object into the list
	 */
	if (!list_add_object(list, obj)) {

	    func_fail();
	    return false;
	}
    }
    else if (list_in) {

	debug("a list x as an element of a list y \n");

	list_show(list);
	
	if (!list_add_list(list, list_in)) {

	    goto FAIL;
	}

	//debug_suspend();
    }
    else {
	goto DONE;
    }
    
    
    list_show(list);

    
  DONE:
    func_ok();
    return true;

  FAIL:
    out(fail, false);
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
	debug_suspend();
	return false;
    }


    if (!list_is_head(list_in)) {

	debug_err("nil list \n");
	obj_set_nil(obj_out);
	
	goto DONE;	
    }
    

    if (list_is_head(list_in->next->next->next)) {

	debug("nil list \n");
	debug("car of list is: nil \n");

	obj_set_nil(obj_out);
	
	goto DONE;
    }

    
    debug("car of list is: ");
    obj_show(&list_in->next->next->obj);
    
    obj_clone(obj_out, &list_in->next->next->obj);
    
 DONE:
    func_ok();
    return true;
}



static bool
eval_cdr(void *left, void *right)
{
    func_s();

    lisp_list_s *list_out = &((eval_value_s*)left)->list;
    lisp_list_s *list_in = &((eval_value_s*)right)->list;
    object_s *obj_out = &((eval_value_s*)left)->obj_out;

    if (!list_in || !list_in->next) {

	debug_err("NULL list \n");
	return false;
    }

    lisp_list_s *l = list_in->next->next->next;
    if (list_is_head(l)) {

	debug("nil list \n");
	debug("cdr of nil list is: nil \n");

	obj_set_nil(obj_out);       
	
	goto DONE;
    }
    

    if (list_is_head(l->next)) {
	
	debug("only one element, so cdr of the list is nil\n");
	obj_set_nil(obj_out);

	goto DONE;
    }
    
    while (l && l != list_in) {
	
	if (!list_add_object(list_out, &l->obj)) {

	    func_fail();
	    return false;
	}

	l = l->next;

	if (l->next && l->next->is_head) break;
    }

    list_show(list_out);

  DONE:
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


static bool
eval_eq(void *left, void *right)
{
    variable_s *var1, *var2;
    
    func_s();

    object_s *obj_l = &((eval_value_s*)left)->obj_out;
    object_s *obj_r = ((eval_value_s*)right)->obj_in;
    lisp_list_s *list_in = &((eval_value_s*)right)->list;

    //if (!obj_l || !obj_r) goto FAIL;
    
    if (!obj_r && !list_in) goto FAIL;

    if (!obj_r && list_in) {

	if (obj_l->type == OBJ_UNKNOWN) {
	    
	    obj_l->type = OBJ_LIST;
	}
	else {

	    obj_l->type = OBJ_TYPE;
	    obj_l->subtype = OBJ_SUBTYPE_BOOL_FALSE;
	    
	}
	
	goto DONE;
    }
      
    
    if (obj_l->type == OBJ_UNKNOWN) {

	memcpy(obj_l, obj_r, sizeof(object_s));
	goto DONE;
    }
    

    if (obj_is_symbol(obj_l)) {

	 var1 = var_get(obj_get_symbol(obj_l));
	 if (var1) {
	     obj_l = &var1->val;
	 }
    }

    if (obj_is_symbol(obj_r)) {

	var2 = var_get(obj_get_symbol(obj_r));
	 if (var2) {
	     obj_r = &var2->val;
	 }	
    }   

    
    if (obj_l->type != obj_r->type) {

	debug_err("object type is inconsistent \n");
	
	obj_l->type = OBJ_TYPE;
	obj_l->subtype = OBJ_SUBTYPE_BOOL_FALSE;

	goto DONE;
    }


    
    bool result;
    
    switch (obj_l->type) {

    case OBJ_CHARACTER:

	result = (obj_l->character[0] == obj_r->character[0]);
	break;

    case OBJ_TYPE:

	//debug("OBJ_TYPE \n");
	obj_show(obj_l);
	if (obj_l->token.type == TOKEN_NUM_INT) {

	    debug("TOKEN_NUM_INT \n");
	    result = (obj_l->token.value.num_int == obj_r->token.value.num_int);	    
	}
	else if (obj_l->subtype == OBJ_SUBTYPE_BOOL_TRUE ||
	    obj_l->subtype == OBJ_SUBTYPE_BOOL_FALSE) {

	    result = (obj_l->subtype == obj_r->subtype);
	}
	else {

	    goto FAIL;
	}
	
	break;
	
    default:
	
	goto FAIL;
	break;
    }

    
    debug("result: %d \n", result);

    obj_l = &((eval_value_s*)left)->obj_out;
    memset(obj_l, 0, sizeof(object_s));
    obj_l->type = OBJ_TYPE;
    obj_l->subtype = (result ? OBJ_SUBTYPE_BOOL_TRUE : OBJ_SUBTYPE_BOOL_FALSE);
    

  DONE:
    obj_show(obj_l);
 
    func_ok();
    return true;

  FAIL:
    ml_err_signal(ML_ERR_EVAL_EQ);
    
    func_fail();
    return false;
}


static bool
eval_print(void *left, void *right)
{
    
    func_s();

  
    
    object_s *obj_l = &((eval_value_s*)left)->obj_out;
    object_s *obj_r = ((eval_value_s*)right)->obj_in;
    
    if (obj_l->type == OBJ_UNKNOWN) {

	memcpy(obj_l, obj_r, sizeof(object_s));
	goto DONE;
    }

    goto FAIL;


  DONE:
    
    obj_show(obj_l);

    stream_s stream;
    char buf[1024];
    
    memset(&stream, 0, sizeof(stream_s));
    stream.type = STREAM_OUTPUT;
    stream.buf = buf;
    stream.is_default_terminal = true;
    stream.max_buf_len = sizeof(buf);
    printer_print(obj_l, &stream);

    
    func_ok();
    return true;

  FAIL:
    ml_err_signal(ML_ERR_EVAL);
    
    func_fail();
    return false;
}


static const eval_call_s m_funcs[] =
{
    { "+", arithmetic_add, NULL},
    { "-", arithmetic_minus, NULL},
    { "*", arithmetic_product, NULL},
    { "/", arithmetic_divide, NULL},

    { "<", num_less_than, NULL},
    { "<=", num_less_or_equal_than, NULL},
    { ">", num_greater_than, NULL},
    { ">=", num_greater_or_equal_than, NULL},
    { "=", num_equal_than, NULL},
    { "/=", num_not_equal_than, NULL},
    { "!=", num_not_equal_than, NULL},
    
    { "list", eval_list, NULL},
    { "car", eval_car, NULL},
    { "cdr", eval_cdr, NULL},
    { "cons", eval_cons, NULL},

    { "eq", eval_eq, NULL},
    //{ "eql", eval_eql, NULL},
    //{ "equal", eval_equal, NULL},

    { "print", eval_print, NULL},
};



static const eval_call_s*
match_eval_call(char *name)
{
    int len = (int)sizeof(m_funcs) / sizeof(m_funcs[0]);
    for (int i = 0; i < len; i++) {

	if (!strcmp(name, m_funcs[i].name)) return &m_funcs[i];
	
    }

    return NULL;
}


static eval_rt_t
eval_user_func_form(form_s *form, lisp_list_s *val_in, eval_value_s *val_out)
{
    func_s();
    
    list_show(val_in);

    form_show(form);


    /* lambda-list as arguments */
    lisp_list_s *args = form->list->next->next->next->next;  

    //obj_show(&args->obj);

    form_s *args_form = args->obj.sub;
    if (!args_form) goto FAIL;

    form_show(args_form);

    
    lisp_list_s *l = args_form->next->list->next->next;
    lisp_list_s *ll = val_in->next->next->next;
    while (l) {

	debug("get the self of a value \n");
	ll->obj.self = &l->obj;
	

	l = l->next;
	ll = ll->next;

	if (l->next && l->next->is_head) break;
	if (ll->next && ll->next->is_head) goto FAIL;
    }

    
    val_out->list_in = val_in;
	
    
    /* evaluate all forms of the function
     */
    l = args->next;
    form_s *f;
    while (l && !l->next->is_head) {

	debug("a form \n");

	f = l->obj.sub;
	form_show(f);

	 
	eval_rt_t rt = eval(f, val_out);
	if (rt != EVAL_OK) goto FAIL;

	l = l->next;
    }

    val_out->list_in = NULL;

    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
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
    const eval_call_s *eval_call = match_eval_call(func_name);
    if (!eval_call) {

	function_s *user_func = func_get(func_name);
	if (user_func) {
	    debug("eval user function: %s \n", func_name);
	    eval_rt_t rt = eval_user_func_form(user_func->form, form->list, val);
	    if (rt != EVAL_OK) goto FAIL;
	    
	    goto DONE;
	}
	else {
	
	    debug_err("undefined function: %s \n", func_name);
	
	    return EVAL_ERR;
	}
    }

    debug("function: %s \n", func_name);

    l = l->next;
    if (!l) goto FAIL;
    
    val->list.is_head = true;
    memset(&value, 0, sizeof(eval_value_s));

    if (l->next == form->list) {

	value.obj_in = NULL;
	eval_call->eval(val, &value);
	
	debug("add ( \n");
	list_add_char_obj(&val->list, "(");
    
	debug("add ) \n");
	list_add_char_obj(&val->list, ")");

	list_show(&val->list);
	
	goto EVAL_END;
    }
    
    
    /* evaluate the list
     */  
    while (l) {

	switch (l->obj.type) {

	case OBJ_LIST:

	    debug("OBJ_LIST \n");

	    form_s *subform = l->obj.sub;
	    if (subform) {
		debug("eval sub_form \n");
	    }
	    
	    memset(&value, 0, sizeof(eval_value_s));
	    value.list_in = val->list_in;
	    
	    eval_rt_t rt = eval(subform, &value);
	    if (rt != EVAL_OK) return rt;

	    debug("eval sub_form done \n");
	   
	    debug("eval %s \n", func_name);
	    if (value.obj_out.type != OBJ_UNKNOWN) {
		
		value.obj_in = &value.obj_out;
	    }
	    else {

		if (list_is_head(&value.list)) {

		    /* the head of a list */
		    //value.obj_in = &value.list.obj;

		    value.list_in = &value.list;
		}
		else {
		    goto FAIL;
		}
		
		//obj_show(value.obj_in);
	    }
	    
	    eval_call->eval(val, &value);

	    break;

	case OBJ_TYPE:

	    debug("OBJ_TYPE \n");

	    char *sym = obj_get_symbol(&l->obj);
	    if (sym) {

		if (val->list_in) {

		    object_s *obj = var_get_val_from_list(val->list_in, sym);
		    if (obj) {

			value.obj_in = obj;
			goto NEXT;
		    }
		}
	
		variable_s *var = var_get(sym);
		if (!var) {

		    debug_err("symbol %s is unbound \n", sym);
		    //ml_err_signal( ML_ERR_VARIABLE_UNBOUND);
		    //return EVAL_ERR;
		    value.obj_in = &l->obj;
		}
		else {
		    value.obj_in = &var->val;
		    
		}

		
	
	    }
	    else {

		if (l->obj.subtype == OBJ_SUBTYPE_MACRO_COMMNA) {

		    /* get the form from ",form", and evaluate it.
		     */
		    debug("OBJ_SUBTYPE_MACRO_COMMNA \n");

		    l = l->next;
		    sym = obj_get_symbol(&l->obj);
		    debug("%s \n", sym);

		    if (val->list_in) {

			object_s *obj = var_get_val_from_list(val->list_in, sym);
			if (obj) {

			    value.obj_in = obj;

			    form_s *f = (form_s*)obj->sub;
			    form_show(f);

			    memset(&value, 0, sizeof(eval_value_s));
			    value.list_in = val->list_in;
	    
			    eval_rt_t rt = eval(f, &value);
			    if (rt != EVAL_OK) return rt;

			    debug("%d, eval sub_form done \n", __LINE__);
	   
			    if (value.obj_out.type != OBJ_UNKNOWN) {

				value.obj_in = &value.obj_out;
			    }
			    else {
				value.obj_in = &value.list.obj;
			    }
			    
			    goto NEXT;
			}
		    }
		}		
		else {
		    value.obj_in = &l->obj;
		}
	    }

	NEXT:
	    //obj_show(value.obj_in);
	    
	    eval_call->eval(val, &value);	    
	    break;
	    
	case OBJ_CHARACTER:

	    debug("OBJ_CHARACTER \n");

	    value.obj_in = &l->obj;
	    eval_call->eval(val, &value);
	    break;

	    break;

	default:

	    debug("unkown object, type: %d \n", l->obj.type);
	    break;
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
    

  EVAL_END:
    if (eval_call->get_result) {

	eval_call->get_result(val, &value);
    }


  DONE:
    func_ok();

    return EVAL_OK;

  FAIL:
    return EVAL_ERR;
}


static eval_rt_t
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
    
    char *name = obj_get_symbol(&l->obj);
    debug("name: %s \n", name);
    
    const var_binder_s *binder = var_match_binder(name);
    if (!binder) {

	if (func_get(name)) {
	    
	    form_set_type(form, COMPOUND_FUNCTION_FORM);
	    
	    debug("eval function form \n");
	    eval_rt_t rt = eval_function_form(form, val);
	    if (rt != EVAL_OK) goto FAIL;
	    
	    goto DONE;
	}
	else if (macro_get(name)) {
	    
	    form_set_type(form, COMPOUND_MACRO_FORM);
	    
	    debug("eval macro form \n");
	    eval_rt_t rt = eval_macro_function_form(form, val);
	    if (rt != EVAL_OK) goto FAIL;

	    goto DONE;
	}
	

	ml_err_signal(ML_ERR_UNKNOWN_CALL);
	
	goto FAIL;
    }

    if (!binder->bind) {

	debug_err("binding function is null \n");
	
	goto FAIL;
    }

    variable_s var;
    binder->bind(&var, l, val);  

  DONE:
    func_ok();

    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}


static eval_rt_t
eval_binder_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;

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

	debug("%s is not binder's name \n", name);
	
	return EVAL_ERR;
    }

    func_s();

    if (!binder->bind) {

	debug_err("binding function is null \n");
	
	return EVAL_ERR;
    }

    variable_s var;
    bool rt = binder->bind(&var, l, val);
    if (!rt) goto FAIL;

    if (var.val.type == OBJ_UNKNOWN) {

	list_copy(&val->list, &var.val_list);
	list_show(&val->list);
    }
    else {
	memcpy(&val->obj_out, &var.val, sizeof(object_s));
  
	obj_show(&val->obj_out);
    }
    
    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}


static eval_rt_t
eval_if_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;
    eval_value_s result;
    eval_rt_t rt;
    variable_s *var;

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


    /* evaluating test-form
     */
    debug("eval test-form \n"); 
    l = l->next;
    form_s *subform = l->obj.sub;
    if (subform) {
	
	debug("eval sub_form \n");
	
	memset(&result, 0, sizeof(eval_value_s));
	result.list_in = val->list_in;
	
	rt = eval(subform, &result);
	if (rt == EVAL_ERR) goto FAIL;
	obj_show(&result.obj_out);	
    }
    else {

	obj_show(&l->obj);


	char *sym = obj_get_symbol(&l->obj);
	if (sym) {

	    var = var_get(sym);
	    if (var) {

	        var_show(var);
		memcpy(&result.obj_out, &var->val, sizeof(object_s));
	    }
	}

	/* self-evaluating obj
	 */
	//rt = eval_myself(&l->obj, &result);
	//if (rt == EVAL_ERR) goto FAIL;
	
    }

    eval_result_show(&result);

 
    /* evaluating then-form and else-form
     */ 
    if (obj_is_true(&result.obj_out)) {
	
	debug("t, eval then-form \n");
	l = l->next;
    }
    else {

	debug("nil, eval else-form \n");
	l = l->next->next;
    }
    
    subform = l->obj.sub;
    if (subform) {
	debug("eval sub_form \n");

	memset(&result, 0, sizeof(eval_value_s));
	rt = eval(subform, val);
	if (rt == EVAL_ERR) goto FAIL;
    }
    else {

	obj_show(&l->obj);
	
	/* self-evaluating obj
	 */
	//rt = eval_myself(&l->obj, &result);
	//if (rt == EVAL_ERR) goto FAIL;

	memcpy(&val->obj_out, &l->obj, sizeof(object_s));
    }

    eval_result_show(val);
  

    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}



static eval_rt_t
eval_return_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;
    eval_rt_t rt;
    variable_s *var;

    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    //debug("list form \n");
    list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = l->obj.token.value.symbol;   
    if (!name) return EVAL_ERR;

    if (strcasecmp(name, "return")) {

	return EVAL_ERR;
    }
       
    func_s();
    debug("name: %s \n", name);


    /* evaluating form in the [form] syntax
     */
    l = l->next;
    if (l->obj.type == OBJ_LIST) {

	form_s *subform = l->obj.sub;
	
	debug("eval form \n");

	rt = eval(subform, val);
	if (rt == EVAL_ERR) goto FAIL;
	obj_show(&val->obj_out);	
    }
    else {

	debug("self-evaluating form \n");

	char *sym = obj_get_symbol(&l->obj);
	if (sym) {

	    if (val->list_in) {

		object_s *obj = var_get_val_from_list(val->list_in, sym);
		if (obj) {

		    memcpy(&val->obj_out, obj, sizeof(object_s));
		    goto NEXT;
		}
	    }
		    
	    var = var_get(sym);
	    if (!var) {

		debug_err("unbound variable %s \n", sym);
		goto FAIL;
	    }
	    var_show(var);
	    memcpy(&val->obj_out, &var->val, sizeof(object_s));

	}
	else {

	    memcpy(&val->obj_out, &l->obj, sizeof(object_s));
	}
	
		
    }

  NEXT:
    l = l->next;
    if (!l->next->is_head) {

	debug_err("error syntax of return form \n");
	goto FAIL;
    }

    val->is_return = true;
    
    eval_result_show(val);

    
    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}


static eval_rt_t
eval_loop_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;
    eval_value_s value;


    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    //debug("list form \n");
    //list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = l->obj.token.value.symbol;
    //debug("name: %s \n", name);
    if (!name) return EVAL_ERR;

    if (strcasecmp(name, "loop")) {

	return EVAL_ERR;
    }

    func_s();
    
    debug("name: %s \n", name);

    list_show(form->list);
    
    
    /* evaluate all compound-forms in the loop form
     */
    l = l->next;
    while (l) {

	switch (l->obj.type) {

	case OBJ_LIST:

	    debug("OBJ_LIST \n");

	    form_s *subform = l->obj.sub;
	    if (subform) {
		debug("eval sub_form \n");
	    }
	    
	    eval_rt_t rt = eval(subform, val);
	    if (rt != EVAL_OK) goto FAIL;

	    debug("eval sub_form done \n");

	    break;

	case OBJ_TYPE:

	    debug("OBJ_TYPE \n");

	    //obj_show(&l->obj);

	    if (l->obj.subtype == OBJ_SUBTYPE_MACRO_COMMNA) {

		/* get the form from ",form", and evaluate it.
		 */
		debug("OBJ_SUBTYPE_MACRO_COMMNA \n");

		l = l->next;
		char *sym = obj_get_symbol(&l->obj);
		debug("%s \n", sym);

		if (val->list_in) {

		    object_s *obj = var_get_val_from_list(val->list_in, sym);
		    if (obj) {

			form_s *f = (form_s*)obj->sub;
			form_show(f);

			memset(&value, 0, sizeof(eval_value_s));
			value.list_in = val->list_in;
	    
			eval_rt_t rt = eval(f, &value);
			if (rt != EVAL_OK) goto FAIL;

			debug("line %d, eval sub_form done \n", __LINE__);
		    }
		}
	    }
	    break;
	    
	case OBJ_CHARACTER:

	    //debug("OBJ_CHARACTER \n");

	    break;

	default:

	    debug("unkown object, type: %d \n", l->obj.type);
	    break;
	}

	
	if (val->is_return) {

	    debug("returning \n");
	    val->is_return = false;
	  
	    obj_show(&val->obj_out);
	    break;
	}
	
	l = l->next;
		
	if (l == form->list) l = l->next;
    }

    eval_result_show(val);  

    
    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}


static eval_rt_t
eval_defun_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;

    if (!val) return EVAL_ERR;
    
    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    //debug("list form \n");
    //list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = obj_get_symbol(&l->obj);
    //debug("name: %s \n", name);
    if (!name) return EVAL_ERR;

    if (strcasecmp(name, "defun")) {

	return EVAL_ERR;
    }

    func_s();
    
    debug("name: %s \n", name);

    list_show(form->list);


    /* get the function name
     */
    l = l->next;
    char *func_name = obj_get_symbol(&l->obj);
    debug("function name: %s \n", func_name);

    function_s func;
    func.name = func_name;
    func.form = form;
    if (!func_get(func_name)) {

	if (!func_add(&func)) {

	    debug_err("add function failed \n");
	    goto FAIL;
	}
    }
    else {
	
	if (!func_update(&func)) {

	    debug_err("update function failed \n");
	    goto FAIL;
	}
    }

    
    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}


static eval_rt_t
eval_special_form(form_s *form, eval_value_s *val)
{
    func_s();

    if (eval_binder_form(form, val) == EVAL_OK) goto DONE;

    if (eval_if_form(form, val) == EVAL_OK) goto DONE;
    
    
  DONE:
    func_ok();
    return EVAL_OK;
}


static eval_rt_t
eval_backquote_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;


    if (!val) return EVAL_ERR;
    
    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    //debug("list form \n");
    //list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = obj_get_symbol(&l->obj);
    //debug("name: %s \n", name);
    if (!name) return EVAL_ERR;

    if (strcasecmp(name, "defmacro")) {

	return EVAL_ERR;
    }

    func_s();
    
    debug("name: %s \n", name);

    list_show(form->list);


    /* get the macro name
     */
    l = l->next;
    char *macro_name = obj_get_symbol(&l->obj);
    debug("macro name: %s \n", macro_name);


    macro_s m;
    m.name = macro_name;
    m.form = form;
    if (!macro_get(macro_name)) {

	if (!macro_add(&m)) {

	    debug_err("add macro failed \n");
	    goto FAIL;
	}
    }
    else {
	
	if (!macro_update(&m)) {

	    debug_err("update macro failed \n");
	    goto FAIL;
	}
    }

    
    
    func_ok();
    return EVAL_OK;

  FAIL:
    func_fail();
    return EVAL_ERR;
}


static eval_rt_t
eval_user_macro_func(form_s *form, lisp_list_s *val_in, eval_value_s *val_out)
{
    func_s();
    
    list_show(val_in);

    form_show(form);


    /* lambda-list as arguments */
    lisp_list_s *args = form->list->next->next->next->next;  

    //obj_show(&args->obj);

    form_s *args_form = args->obj.sub;
    if (!args_form) goto FAIL;

    debug("@ arguments form: \n");
    form_show(args_form);


    lisp_list_s *l = args_form->next->list->next->next;
    lisp_list_s *ll = val_in->next->next->next;
    while (l) {

	debug("get the self of a value \n");
	//obj_show(&l->obj);
	ll->obj.self = &l->obj;
	

	l = l->next;
	ll = ll->next;

	if (l->next && l->next->is_head) break;
	if (ll->next && ll->next->is_head) goto FAIL;
    }

    
    val_out->list_in = val_in;
	
    
    /* evaluate all forms of the macro
     */
    l = args->next;
    form_s *f;
    while (l && !l->next->is_head) {

	debug("a form \n");

	//obj_show(&l->obj);
	if (!obj_is_char(&l->obj)) goto FAIL;

	

	if (!strcmp(l->obj.character, "`")) {
	    /* `form
	     */

	    debug("`form \n");
	    
	}
	else if (!strcmp(l->obj.character, ",")) {

	    /* ,form
	     */

	    debug(",form \n");
	    
	}
	else {

	    debug_err("unknown form \n");
	    goto FAIL;
	}
	
	
	f = l->obj.sub;
	form_show(f);

	
	eval_rt_t rt = eval(f, val_out);
	if (rt != EVAL_OK) goto FAIL;

	l = l->next;
    }

    val_out->list_in = NULL;


    out(ok, EVAL_OK);

  FAIL:
    out(fail, EVAL_ERR);
}


static eval_rt_t
eval_macro_function_form(form_s *form, eval_value_s *val)
{
    lisp_list_s *l;
    eval_rt_t rt;


    if (!form->list->next) {

	debug("null form \n");	
	return EVAL_ERR;
    }

    l = form->list->next;
    
    //debug("list form \n");
    //list_show(form->list);


    /* ignore '(' */
    l = l->next;
    
    char *name = obj_get_symbol(&l->obj);
    //debug("name: %s \n", name);
    if (!name) return EVAL_ERR;

    macro_s *user_macro = macro_get(name);
    if (!user_macro) {
	return EVAL_ERR;
    }  

    func_s();
    
    debug("macro name: %s \n", name);
    
    rt = eval_user_macro_func(user_macro->form, form->list, val);
    if (rt != EVAL_OK) goto FAIL;    


    out(ok, EVAL_OK);
    
  FAIL:
    out(fail, EVAL_ERR);
}


static eval_rt_t
eval_macro_form(form_s *form, eval_value_s *val)
{
    func_s();

    if (eval_macro_function_form(form, val) == EVAL_OK) goto DONE;
    
    if (eval_backquote_form(form, val) == EVAL_OK) goto DONE;

    if (eval_return_form(form, val) == EVAL_OK) goto DONE;

    if (eval_loop_form(form, val) == EVAL_OK) goto DONE;

    if (eval_defun_form(form, val) == EVAL_OK) goto DONE;


    out(fail, EVAL_ERR);
    
  DONE:
    func_ok();
    return EVAL_OK;
}


eval_rt_t
eval(form_s *form, eval_value_s *result)
{
    eval_rt_t rt;
    //eval_value_s value;
    
    if (!form) return EVAL_ERR_NULL;

    func_s();

    form_s *f = form->next;

    while (f && f != form) {

	if (!result->list_in) {
	    memset(result, 0, sizeof(eval_value_s));
	}
	
	switch (f->type) {

	case COMPOUND_SPECIAL_FORM:

	    rt = eval_special_form(f, result);
	    if (rt != EVAL_OK) goto FAIL;	    

	    eval_result_show(result);
	    
	    break;
	    
	case COMPOUND_FUNCTION_FORM:
	    
	    rt = eval_function_form(f, result);
	    if (rt != EVAL_OK) goto FAIL;

	    eval_result_show(result);
	   
	    break;

	case SYMBOL_FORM:
	    
	    rt = eval_symbol_form(f, result); 
	    if (rt != EVAL_OK) goto FAIL;
	   
	    break;

	case SELF_EVALUATING_FORM:

	    if (f->subtype == NIL_LIST_FORM) {	       

		debug("nil list form \n");
		
		stream_s stream;
		char buf[1024];  
		memset(&stream, 0, sizeof(stream_s));
		stream.type = STREAM_OUTPUT;
		stream.buf = buf;
		stream.is_default_terminal = true;
		stream.max_buf_len = sizeof(buf);
		printer_print_nil(&stream);
	    }
	    
	    break;

	case COMPOUND_MACRO_FORM:
	    
	    rt = eval_macro_form(f, result);
	    if (rt != EVAL_OK) goto FAIL;	    

	    eval_result_show(result);
	    
	    break;
	    
	default:

	    debug_err("unknown form type %d \n", f->type);
	    break;
	    
	}

	f = f->next;
    }

    func_ok();

    return EVAL_OK;


  FAIL:
    func_fail();
    ml_err_signal(ML_ERR_EVAL);    
    return EVAL_ERR;    
}



