

#include "form.h"

#include "debug.h"

#include "mem.h"

#include "util.h"

#include "error.h"

#include "list.h"

#include "obj.h"



/**
 * Conses as Forms
 * 
 * A cons that is used as a form is called a compound form.
 *
 */


/**
 * Symbols as Forms
 * 
 * 1. If a form is a symbol, then it is either a symbol macro or a variable.
 * 
 * If a form is a symbol that is not a symbol macro, then it is the name of a 
 * variable, and the value of that variable is returned. There are three kinds 
 * of variables: lexical variables, dynamic variables, and constant variables. 
 * A variable can store one object. The main operations on a variable are to 
 * read[1] and to write[1] its value.
 *
 */


form_s*
form_create(void)
{
    form_s *f;

    f = ml_malloc(sizeof(form_s));
    if (!f) goto FAIL;

    
    f->list = ml_malloc(sizeof(lisp_list_s));
    if (!f->list) goto FAIL;

    f->list->is_head = true;
    f->list->obj.type = OBJ_LIST;

    f->type = UNKNOWN_FORM;

    
    out(ok, f);

 FAIL:
    out(fail, NULL);
}


form_s*
form_create_as_self_eval_form(void)
{
    form_s *f;

    func_s();
    
    f = ml_malloc(sizeof(form_s));
    if (!f) goto FAIL;

    
    f->obj = ml_malloc(sizeof(object_s));
    if (!f->obj) goto FAIL;

    f->type = SELF_EVALUATING_FORM;
    f->obj->type = OBJ_TYPE;

    
    out(ok, f);

 FAIL:
    out(fail, NULL);
}


form_s*
form_create_as_quoted_expression(void)
{
    form_s *f;

    func_s();
    
    f = ml_malloc(sizeof(form_s));
    if (!f) goto FAIL;

    
    f->obj = ml_malloc(sizeof(object_s));
    if (!f->obj) goto FAIL;

    f->type = COMPOUND_SPECIAL_FORM;
    f->subtype = SPECIAL_FORM_QUOTE;
    f->obj->type = OBJ_TYPE;
    f->obj->subtype = OBJ_SUBTYPE_QUOTE_EXPRESSION;
    
    out(ok, f);

 FAIL:
    out(fail, NULL);
}


form_s*
form_create_symbol_form(void)
{
    form_s *f;

    func_s();
    
    f = ml_malloc(sizeof(form_s));
    if (!f) goto FAIL;

    f->type = SYMBOL_FORM;
    
    //form->symbol = ml_malloc(sizeof(symbol_s));
    //if (!form->symbol) return false;

    f->obj = ml_malloc(sizeof(object_s));
    if (!f->obj) goto FAIL;

    out(ok, f);

 FAIL:
    out(fail, NULL);
}


void
form_free(form_s *form)
{
    /* TODO: free list 
     */
    //ml_free(form->list);
    
    ml_free(form);
}



bool
form_add_front(form_s *forms, form_s *new_form)
{
    if (!forms || !new_form) return false;

    if (!forms->front) {

	forms->next = new_form;
	forms->front = new_form; 
	new_form->front = forms;
	new_form->next = forms;
    }
    else {
	
	forms->front->next = new_form;
	new_form->front = forms->front;
	new_form->next = forms;
	forms->front = new_form;
    }

    func_ok();
    return true;
}


void
form_set_type(form_s *form, form_t type)
{
    debug("%s, type: %d \n", __func__, type);
    
    form->type = type;
}


bool
form_is_unkown(form_s *form)
{
    return (form->type == UNKNOWN_FORM);
}


void
form_show(form_s *form)
{
    if (!form) return;

    func_s();

    bool showing_one = false;
    
    form_s *f;
    if (form->type == UNKNOWN_FORM) {
	f = form->next;
    }
    else {
	f = form;
	showing_one = true;
    }
    
    
    while (f) {

	switch (f->type) {

	case SELF_EVALUATING_FORM:
	    debug("SELF_EVALUATING_FORM \n");
	    debug("subtype: ");
	    if (f->subtype == SELF_EVAL_FORM_NUMBER) {
		debug("SELF_EVAL_FORM_NUMBER \n");
	    }
	    else if (f->subtype == SELF_EVAL_FORM_BOOL) {
		debug("SELF_EVAL_FORM_BOOL \n");
	    }
	    
	    obj_show(f->obj);
	    
	    break;
	    
	case SYMBOL_FORM:
	    
	    debug("SYMBOL_FORM \n");
	    list_show(f->list);
	    
	    break;

	case COMPOUND_FUNCTION_FORM:
	    
	    debug("COMPOUND_FUNCTION_FORM \n");
	    if (f->subtype == S_PREDICATE_EQ) {
		debug("subtype: S_PREDICATE_EQ \n");
	    }
	    list_show(f->list);
	    break;

	case COMPOUND_SPECIAL_FORM:
	    
	    debug("COMPOUND_SPECIAL_FORM \n");
	    
	    break;

	case COMPOUND_MACRO_FORM:
	    
	    debug("COMPOUND_MACRO_FORM \n");
	    list_show(f->list);
	    break;
	    
	default:
	    debug("unkown form, type %d \n", f->type);
	    break;

	}
	
	if (showing_one) break;
	
	f = f->next;
	if (f == form) break;
    }

    func_ok();
}


form_s*
form_clone(form_s *form)
{
    form_s *f = NULL;
    
    func_s();

    if (!form) goto FAIL;
    

    func_ok();
    return f;

  FAIL:
    func_fail();
    return NULL;
}


bool
form_add_token(form_s *form, token_s *token)
{
    func_s();
    
    
    if (form->list) {
	    
	/* clone the token into the list form */
	list_add_token(form->list, token);	
    }
    else if (form->type == SELF_EVALUATING_FORM) {

	/* clone the token into the self-evaluating form */
	obj_clone_token(form->obj, token);
    }
    else if (form->type == SYMBOL_FORM) {

	/* clone the token into the symbol form */
	obj_clone_token(form->obj, token);
    }    
    else {

	goto FAIL;
    }

    
    out(ok, true);

  FAIL:
    out(fail, false);
}


void
form_show_type(form_s *form)
{
    switch (form->type) {

    case SELF_EVALUATING_FORM:
	debug("SELF_EVALUATING_FORM \n");	
	if (form->subtype == SELF_EVAL_FORM_NUMBER) {
	    debug("SELF_EVAL_FORM_NUMBER \n");
	}
	else if (form->subtype == SELF_EVAL_FORM_BOOL) {
	    debug("SELF_EVAL_FORM_BOOL \n");
	}
	else {

	    debug("unknown \n\n");
	}
	    
	break;
	    
    case SYMBOL_FORM:
	    
	debug("SYMBOL_FORM \n");
	
	break;

    case COMPOUND_FUNCTION_FORM:
	    
	debug("COMPOUND_FUNCTION_FORM \n");
	break;

    case COMPOUND_SPECIAL_FORM:
	    
	debug("COMPOUND_SPECIAL_FORM \n");

	if (form->subtype == S_PREDICATE_EQ) {
	    debug("subtype: S_PREDICATE_EQ \n");
	}	
	break;

    case COMPOUND_MACRO_FORM:
	    
	debug("COMPOUND_MACRO_FORM \n");
	
	break;
	    
    default:
	debug("unkown form, type %d \n", form->type);
	break;

    }

    debug("\n");
}


form_s*
form_create_as_character_obj(char *character)
{
    form_s *form = form_create_as_self_eval_form();
    if (!form) goto FAIL;

    form->subtype = SELF_EVAL_FORM_CHARACTER;	    
    form->obj->type = OBJ_CHARACTER;
    form->obj->character = character;

    return form;

  FAIL:
    out(fail, false);
}


form_s*
form_create_nil(void)
{
    form_s *form = form_create_as_self_eval_form();
    if (!form) goto FAIL;

    form->subtype = SELF_EVAL_FORM_BOOL;	    
    obj_set_nil(form->obj);

    return form;

  FAIL:
    out(fail, false);
}

bool
form_is_quote_form(form_s *form)
{
    
    bool flag =  (form->type == COMPOUND_SPECIAL_FORM &&
		  form->subtype == SPECIAL_FORM_QUOTE);

    if (flag) func_ok();

    return flag;
}




