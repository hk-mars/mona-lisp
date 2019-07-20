

#ifndef ML_SYNTAX_H
#define ML_SYNTAX_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


#include "config.h"

#include "form.h"


typedef enum
{
    SYNTAX_OK = 0,
    SYNTAX_ERR = 1,
    SYNTAX_ERR_FUNC = 2,
    SYNTAX_ERR_ARG1 = 3,
    SYNTAX_ERR_ARG2 = 4,
    SYNTAX_ERR_ARG3 = 5,

    
} syntax_rt_t;


syntax_rt_t syntax_init(void);

syntax_rt_t syntax_check(form_s *form);


#endif /* ML_SYNTAX_H */

