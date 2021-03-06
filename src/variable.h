

#ifndef ML_VARIABLE_H
#define ML_VARIABLE_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>


#include "config.h"

#include "obj.h"

#include "form.h"

#include "eval.h"


typedef enum
{
    VAR_OK = 0,
    VAR_ERR = 1,
    VAL_ERR_CREATE_HTAB = 2, /* error when creating the hash-table */
    
} var_rt_t;


typedef enum
{
    VAR_UNKOWN = 0, 
    VAR_LEXICAL = 1,
    VAR_DYNAMIC = 2,
    VAR_CONSTANT = 3,
    
} variable_t;


typedef struct
{
    
} var_scope_t;



typedef object_s var_value_s;


typedef struct
{
    variable_t type;
    
    char *name;

    var_scope_t scope;
    
    var_value_s val;

    /* lisp_list_s val_list; */
    
    
} variable_s;


typedef bool (*bind_f) (variable_s *var, void *context, eval_value_s *result);

typedef struct
{
    char *defined_name;

    bind_f bind;
    
} var_binder_s;


typedef struct
{
    char *var_name;

    var_value_s val;

    lisp_list_s val_list;
  
} pair_s;



var_rt_t var_init(void);


const var_binder_s* var_match_binder(const char *defined_name);


/** 
 * add a variable
 */
bool var_add(variable_s *var);

/** 
 * delete the variable as name
 */
bool var_delete(char *name);

/** 
 * get a variable as name
 */
variable_s* var_get(char *name);

/** 
 * update the value of variable
 */
bool var_update(variable_s *var_new);


bool var_is_bound(char *name);

void var_show(variable_s *var);

object_s* var_get_val_from_list(lisp_list_s *list, char *name);


variable_s* var_new(void);

var_rt_t var_free(char *name);

#endif /* ML_VARIABLE_H */



