

#ifndef ML_OBJ_H
#define ML_OBJ_H


#include <stdint.h>
#include <stdio.h>

#include "config.h"

#include "token.h"


typedef struct s_object
{
    unsigned int type;
    
    token_s *tokens;

    struct s_object *self;
} object_s;



#endif /* ML_OBJ_H */


