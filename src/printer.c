

#include "printer.h"

#include "debug.h"

#include "mem.h"

#include "error.h"



/**
 * Introduction of Common Lisp printer, see:
 * http://www.lispworks.com/documentation/HyperSpec/Body/22_a.htm
 * Copyright 1996-2005, LispWorks Ltd. All Rights Reserved.
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
