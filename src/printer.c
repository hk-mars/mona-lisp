

#include "printer.h"

#include "debug.h"

#include "mem.h"

#include "error.h"



/* Rule of the lisp printer:
 * 1. Common Lisp provides a representation of most objects in the form of 
 * printed text called the printed representation
 *
 * 2. Reading a printed representation typically produces an object that is equal 
 * to the originally printed object.
 *
 */



printer_rt_t
printer_print(object_s *obj, object_t type)
{
    func_s();

    if (!obj) return PRINTER_ERR_NULL;

    if (type != OBJ_UNKNOWN) {
	
	debug("strong type: %d \b", type);
    }
    

    func_ok();
    return PRINTER_OK;
}
