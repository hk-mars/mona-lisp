

#include "function.h"

#include "debug.h"

#include "error.h"

#include "util.h"

#include "hsearch.h"

#include "mem.h"


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

    function_s *f = (function_s*)ml_malloc(sizeof(function_s));
    if (!f) return false;
    
    memcpy(f, func, sizeof(function_s));
      
    entry.key = f->name;
    entry.data = f;
    entry_rt = hsearch(&m_func_htab, entry, ENTER);
    if (!entry_rt) {

	debug_err("push varible %s into hash table, failed \n", f->name);

	ml_free(f);
	return false;
    }
    
    func_get(f->name);
    
    func_ok();
    return true;    
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

    form_show(f->form);
    func_ok();
    return f;    
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
       
