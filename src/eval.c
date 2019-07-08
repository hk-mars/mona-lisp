

#include "eval.h"

#include "debug.h"

#include "mem.h"

#include "error.h"



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


/**
 * The rule of evaluating a function form:
 * The subforms in the cdr of the original form are evaluated in left-to-right 
 * order in the current lexical and dynamic environments. The primary value of 
 * each such evaluation becomes an argument to the named function; any additional 
 * values returned by the subforms are discarded.
 *
 */
static eval_rt_t
eval_function_form(form_s *form)
{
    func_s();
    

    func_ok();

    return EVAL_OK;
}


eval_rt_t
eval(form_s *forms)
{
    eval_rt_t rt;
    
    if (!forms) return EVAL_ERR_NULL;

    func_s();

    form_s *f = forms->next;

    while (f && f != forms) {

	switch (f->type) {

	case COMPOUND_FUNCTION_FORM:

	    rt = eval_function_form(f);
	    if (rt != EVAL_OK) {
		
		ml_err_signal(ML_ERR_EVAL);
		return EVAL_ERR;
	    }
	    
	    break;


	default:
	    break;
	    
	}

	f = f->next;
    }
    


    func_ok();

    return EVAL_OK;
}
