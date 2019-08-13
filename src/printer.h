

#ifndef __PRINTER__
#define __PRINTER__


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "obj.h"


typedef enum
{
    PRINTER_OK = 0,
    PRINTER_ERR = 1,
    PRINTER_ERR_NULL = 2,
    
} printer_rt_t;


printer_rt_t printer_print(object_s *obj, object_t type);


#endif /*  __PRINTER__ */


