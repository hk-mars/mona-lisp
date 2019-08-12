

#include "form.h"

#include "debug.h"

#include "mem.h"

#include "util.h"

#include "error.h"


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
    if (!f) return NULL;

    f->list = ml_malloc(sizeof(lisp_list_s));
    if (!f->list) return NULL;

    f->list->is_head = true;
    f->list->obj.type = OBJ_LIST;

    f->type = UNKNOWN_FORM;
    
    return f;
}


bool
form_create_symbol(form_s *form)
{
    form->symbol = ml_malloc(sizeof(symbol_s));
    if (!form->symbol) return false;
    
    return true;
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

    form_s *f = form->next;
    
    while (f != form) {

	switch (f->type) {

	case SYMBOL_FORM:
	    debug_err("SYMBOL_FORM \n");
	    
	    break;

	case COMPOUND_FUNCTION_FORM:
	    debug_err("COMPOUND_FUNCTION_FORM \n");
	    
	    break;

	    
	default:
	    debug_err("unkown form \n");
	    break;

	}
	
	
	f = f->next;
    }

    func_ok();
}

