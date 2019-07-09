

#ifndef ML_EVAL_H
#define ML_EVAL_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "form.h"

#include "token.h"


typedef enum
{
    EVAL_OK = 0,
    EVAL_ERR = 1,
    EVAL_ERR_NULL = 2,
    
} eval_rt_t;


typedef token_s eval_value_s;


eval_rt_t eval(form_s *forms);


#endif /* ML_EVAL_H */

