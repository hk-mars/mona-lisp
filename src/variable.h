

#ifndef ML_VARIABLE_H
#define ML_VARIABLE_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>


#include "config.h"

#include "obj.h"

#include "form.h"


typedef enum
{
    VAR_UNKOWN = 0, 
    VAR_LEXICAL = 1,
    VAR_DYNAMIC = 2,
    VAR_CONSTANT = 3,
    
} variable_t;


typedef object_s var_value_s;


typedef struct
{
    variable_t type;
    
    char *name;
    
    var_value_s val;
    
} variable_s;


typedef bool (*bind_f) (variable_s *var, void *context);

typedef struct
{
    char *defined_name;

    bind_f bind;
    
} var_binder_s;


typedef struct
{
    char *var_name;

    var_value_s val;
  
} pair_s;


const var_binder_s* var_match_binder(const char *defined_name);



#endif /* ML_VARIABLE_H */



