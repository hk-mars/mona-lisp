

#ifndef ML_OBJ_H
#define ML_OBJ_H


#include <stdint.h>
#include <stdio.h>

#include "config.h"

#include "token.h"


typedef enum
{
    OBJ_UNKOWN = 0,
    OBJ_LIST = 1,
    OBJ_ARRAY = 2,
    OBJ_SEQUENCE = 3,
    OBJ_TYPE = 4,
    OBJ_INPUT_STREAM = 5,
    OBJ_OUTPUT_STREAM = 6,
    OBJ_CLASS = 7,
    
} object_t;

typedef struct s_object
{
    object_t type;
    
    token_s token;

    void *self;

    void *sub;
    
} object_s;



#endif /* ML_OBJ_H */


