

#include "function.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "hsearch.h"

#include "mem.h"

#include "lex.h"



static hash_table_s m_func_htab;


func_rt_t
func_init(void)
{
    int rt;
    
    func_s();

    memset(&m_func_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&m_func_htab, 1024);
    if(!rt) return FUNC_ERR_CREATE_HTAB;

    func_ok();
    return FUNC_OK;
}


bool
func_add(function_s *func)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;

    func_s();
    
    debug("func->name: %s \n", func->name);
    
    entry.key = func->name;
    entry_rt = hsearch(&m_func_htab, entry, FIND);
    if (entry_rt) {

	debug_err("func %s has existed \n", func->name);

	return false;
    }

    function_s *f = (function_s*)mm_malloc(sizeof(function_s));
    if (!f) return false;
    
    memcpy(f, func, sizeof(function_s));
    
    /* clone the form here(TODO: it's not a good idea to copy it)
     * if the form is not saved, we have to load it from the codes every time, 
     * it means we have to save the codes of function.
     */
    //f->form = form_clone(func->form);

    
    /* clone the codes of function
     */
    f->code = (char*)mm_malloc(func->form->code_sz);
    f->code_sz = func->form->code_sz;
    memcpy(f->code, func->form->code, func->form->code_sz);
    
    //ml_util_show_buf(f->code, f->code_sz);
    //debug_suspend();

    /* mark the form as NULL first, and load it from the codes when we need it 
     * as to restrict the use of memory.
     */
    f->form = NULL;
    
    
    entry.key = ml_util_str_clone(f->name, mm_malloc);  /* clone the name */
    entry.data = f;
    entry_rt = hsearch(&m_func_htab, entry, ENTER);
    if (!entry_rt) {

	debug_err("push varible %s into hash table, failed \n", f->name);

	//function_free(f);
	goto FAIL;
    }
    
    //func_get(f->name);
    
    func_ok();
    return true;

  FAIL:
    out(fail, false);
}


/** 
 * get a function as name
 */
function_s*
func_get(char *name)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;
    function_s *f;
    
    func_s();
    
    debug("name: %s, %dbytes \n", name, strlen(name));
    
    
    entry.key = name;
    
    entry_rt = hsearch(&m_func_htab, entry, FIND);
    if (entry_rt) {

	goto FOUND;
    }

    return NULL;

  FOUND:

    f = (function_s*)entry_rt->data;
    
    //func_show(f);
    
    /* if the form is not created, then loading it from codes. 
     */
    //if (!f->form || !gc_search_mem(f->form)) {
    if (1) {

	debug("load function form \n");

	lex_s lex;
	code_s cd;
	memset(&lex, 0, sizeof(lex_s));
	cd.code = f->code;
	cd.code_sz = f->code_sz;
	lex_rt_t lex_rt = ml_lex(&lex, &cd);
	if (lex_rt != LEX_OK) goto FAIL;

	debug("load function form done \n");
	form_show(lex.forms.next);
	f->form = lex.forms.next;
	//debug_suspend();

    }
    
    
    form_show(f->form);
    func_ok();
    return f;


  FAIL:
    out(fail, NULL);
}


bool
func_update(function_s *new_func)
{
    func_s();
    
    function_s *f = func_get(new_func->name);
    if (!f) return false;

    ml_free(f->name);
    ml_free(f->form);

    f->name = new_func->name;
    f->form = new_func->form;
    
    //func_show(var);

    func_get(f->name);
    
    func_ok();
    return true;
}


function_s*
func_new(void)
{
    func_s();

    /* create a new gc object for the management of memory of a new funcion
     */

    
    out(ok, FUNC_OK);

  FAIL:
    out(fail, FUNC_ERR);
}


func_rt_t
func_free(char *name)
{
    func_s();

    /* free all memories of this func malloced by gc.
     *
     * get the func object from hash-table as name
     * get the gc id from func object
     * free the gc memories as gc id
     */

    out(ok, FUNC_OK);

  FAIL:
    out(fail, FUNC_ERR);
}


bool
func_exist(char *name)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;
    function_s *f;
    
    func_s();
    
    debug("name: %s, %dbytes \n", name, strlen(name));
    
    
    entry.key = name;
    
    entry_rt = hsearch(&m_func_htab, entry, FIND);
    if (entry_rt) {

	goto FOUND;
    }

    return false;

  FOUND:

    //f = (function_s*)entry_rt->data;
    
    //func_show(f);
   
    //form_show(f->form);

    
    func_ok();
    return true;


  FAIL:
    out(fail, false);
}



