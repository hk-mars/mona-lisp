

#ifndef ML_EVAL_H
#define ML_EVAL_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "form.h"

#include "token.h"

#include "list.h"


typedef enum
{
    EVAL_OK = 0,
    EVAL_ERR = 1,
    EVAL_ERR_NULL = 2,
    
} eval_rt_t;


typedef struct
{
    lisp_list_s list;

    object_s obj_out;

    object_s *obj_in;
    
} eval_value_s;


eval_rt_t eval(form_s *forms, eval_value_s *result);

void eval_result_show(eval_value_s *result);


#endif /* ML_EVAL_H */

