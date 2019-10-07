

#include "macro.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "hsearch.h"

#include "mem.h"



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

    /* clone the form here.
     * if the form is not saved, we have to load it from the codes every time, 
     * it means we have to save the codes of macro.
     */
    //m->form = form_clone(macro->form);
      
    entry.key = strdup(m->name);
    entry.data = m;
    entry_rt = hsearch(&m_macro_htab, entry, ENTER);
    if (!entry_rt) {

	debug_err("push varible %s into hash table, failed \n", m->name);

	mm_free(m);
	return false;
    }
    
    macro_get(m->name);
    
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
    //m->form = form_load(codes);
    
    
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
       
