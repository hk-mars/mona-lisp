

#include "macro.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "hsearch.h"

#include "mem.h"

#include "lex.h"



static hash_table_s m_macro_htab;


macro_rt_t
macro_init(void)
{
    int rt;
    
    func_s();

    memset(&m_macro_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&m_macro_htab, 1024);
    if(!rt) return MACRO_ERR_CREATE_HTAB;

    func_ok();
    return MACRO_OK;
}


macro_s*
macro_new(void)
{
    func_s();

    /* create a new gc object for the management of memory of a new macro
     */

    out(ok, MACRO_OK);

  FAIL:
    out(fail, MACRO_ERR);
}


macro_rt_t
macro_free(char *name)
{
    func_s();

    /* free all memories of this macro malloced by gc.
     *
     * get the macro object from hash-table as name
     * get the gc id from macro object
     * free the gc memories as gc id
     */

    out(ok, MACRO_OK);

  FAIL:
    out(fail, MACRO_ERR);
}


bool
macro_add(macro_s *macro)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;

    func_s();
    
    debug("macro->name: %s \n", macro->name);
    
    entry.key = macro->name;
    entry_rt = hsearch(&m_macro_htab, entry, FIND);
    if (entry_rt) {

	debug_err("macro %s has existed \n", macro->name);

	return false;
    }

    macro_s *m = (macro_s*)mm_malloc(sizeof(macro_s));
    if (!m) return false;
    
    memcpy(m, macro, sizeof(macro_s));

    /* clone the form here(TODO: it's not a good idea to copy it)
     * if the form is not saved, we have to load it from the codes every time, 
     * it means we have to save the codes of macro.
     */
    //m->form = form_clone(macro->form);

    
    /* clone the codes of macro
     */
    m->code = (char*)mm_malloc(macro->form->code_sz);
    m->code_sz = macro->form->code_sz;
    memcpy(m->code, macro->form->code, macro->form->code_sz);
    

    //ml_util_show_buf(m->code, m->code_sz+1);
    //debug_suspend();

    /* mark the form as NULL first, and load it from the codes when we need it 
     * as to restrict the use of memory.
     */
    m->form = NULL;
    

    entry.key = strdup(m->name);  /* clone the name */
    entry.data = m;
    entry_rt = hsearch(&m_macro_htab, entry, ENTER);
    if (!entry_rt) {

	debug_err("push varible %s into hash table, failed \n", m->name);

	mm_free(m);
	return false;
    }
    
    //macro_get(m->name);
    //if (!macro_is_defined(m->name)) return false; 
    
    func_ok();
    return true;    
}


/** 
 * get a macro as name
 */
macro_s*
macro_get(char *name)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;
    macro_s *m;
    
    func_s();
    
    debug("name: %s, %dbytes \n", name, strlen(name));
    
    
    entry.key = name;
    
    entry_rt = hsearch(&m_macro_htab, entry, FIND);
    if (entry_rt) {

	goto FOUND;
    }

    return NULL;

  FOUND:

    m = (macro_s*)entry_rt->data;
    m->name = name;

    //macro_show(m);

    /* if the form is not created, then loading it from codes. 
     */
    //if (!m->form || !gc_search_mem(m->form)) {
    if (1) {

	debug("load macro form \n");

	lex_s lex;
	code_s cd;
	memset(&lex, 0, sizeof(lex_s));
	cd.code = m->code;
	cd.code_sz = m->code_sz;
	lex_rt_t lex_rt = ml_lex(&lex, &cd);
	if (lex_rt != LEX_OK) return NULL;

	debug("load macro form done \n");
	form_show(lex.forms.next);
	m->form = lex.forms.next;
	//debug_suspend();

    }
    
    form_show(m->form);
    func_ok();
    return m;    
}


bool
macro_update(macro_s *new_macro)
{
    func_s();
    
    macro_s *f = macro_get(new_macro->name);
    if (!f) return false;

    //ml_free(f->name);
    ml_free(f->form);

    //f->name = new_macro->name;
    f->form = new_macro->form;
    
    //macro_show(var);

    macro_get(f->name);
    
    func_ok();
    return true;
}



bool
macro_is_defined(char *name)
{
    htab_entry_s *entry_rt;
    htab_entry_s entry;
    macro_s *m;
    
    func_s();
    
    debug("name: %s, %dbytes \n", name, strlen(name));
    
    
    entry.key = name;
    
    entry_rt = hsearch(&m_macro_htab, entry, FIND);
    if (entry_rt) {

	goto FOUND;
    }

    return false;

  FOUND:

    m = (macro_s*)entry_rt->data;
    m->name = name;

    //macro_show(m);

    form_show(m->form);
    func_ok();
    return true;    
}
