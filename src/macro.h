
#ifndef __MACRO_H__
#define __MACRO_H__


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "form.h"


typedef enum
{
    MACRO_OK = 0,
    MACRO_ERR = 1,
    MACRO_ERR_CREATE_HTAB = 2, /* error when creating the hash-table */
    
} macro_rt_t;


typedef struct
{
    char *name;

    form_s *form;
    
} macro_s;



macro_rt_t macro_init(void);


/** 
 * add a macro
 */
bool macro_add(macro_s *macro);

/** 
 * delete the macro as name
 */
//bool macro_delete(char *name);

/** 
 * get a macro as name
 */
macro_s* macro_get(char *name);

/** 
 * update the value of macro
 */
bool macro_update(macro_s *new_macro);


macro_s* macro_new(void);

macro_rt_t macro_free(char *name);


#endif /* __MACRO_H__ */


