

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

    macro_s *f = (macro_s*)ml_malloc(sizeof(macro_s));
    if (!f) return false;
    
    memcpy(f, macro, sizeof(macro_s));
      
    entry.key = f->name;
    entry.data = f;
    entry_rt = hsearch(&m_macro_htab, entry, ENTER);
    if (!entry_rt) {

	debug_err("push varible %s into hash table, failed \n", f->name);

	ml_free(f);
	return false;
    }
    
    macro_get(f->name);
    
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
    macro_s *f;
    
    func_s();
    
    debug("name: %s, %dbytes \n", name, strlen(name));
    
    
    entry.key = name;
    
    entry_rt = hsearch(&m_macro_htab, entry, FIND);
    if (entry_rt) {

	goto FOUND;
    }

    return NULL;

  FOUND:

    f = (macro_s*)entry_rt->data;
    
    //macro_show(f);

    form_show(f->form);
    func_ok();
    return f;    
}


bool
macro_update(macro_s *new_macro)
{
    func_s();
    
    macro_s *f = macro_get(new_macro->name);
    if (!f) return false;

    ml_free(f->name);
    ml_free(f->form);

    f->name = new_macro->name;
    f->form = new_macro->form;
    
    //macro_show(var);

    macro_get(f->name);
    
    func_ok();
    return true;
}
       
