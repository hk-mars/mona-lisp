
#ifndef __FUNCTION_H__
#define __FUNCTION_H__


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "form.h"


typedef enum
{
    FUNC_OK = 0,
    FUNC_ERR = 1,
    FUNC_ERR_CREATE_HTAB = 2, /* error when creating the hash-table */
    
} func_rt_t;


typedef struct
{
    char *name;

    form_s *form;
    
} function_s;



func_rt_t func_init(void);


/** 
 * add a function
 */
bool func_add(function_s *func);

/** 
 * delete the function as name
 */
//bool func_delete(char *name);

/** 
 * get a function as name
 */
function_s* func_get(char *name);

/** 
 * update the value of function
 */
bool func_update(function_s *new_func);


#endif /* __FUNCTION_H__ */


