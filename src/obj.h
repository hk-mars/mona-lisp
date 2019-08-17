

#ifndef ML_OBJ_H
#define ML_OBJ_H


#include <stdint.h>
#include <stdio.h>

#include "config.h"

#include "token.h"


typedef enum
{
    OBJ_UNKNOWN = 0,
    
    OBJ_CHARACTER = 1,
    
    OBJ_LIST = 2,
    OBJ_ARRAY = 3,
    OBJ_SEQUENCE = 4,
    
    OBJ_TYPE = 5,
    OBJ_TYPE_FUNCTION = 6,
    
    OBJ_INPUT_STREAM = 7,
    OBJ_OUTPUT_STREAM = 8,
    OBJ_CLASS = 9,
    
} object_t;


typedef struct s_object
{
    object_t type;

    char *character;
    
    token_s token;

    void *self;

    void *sub;
    
} object_s;


void obj_show(object_s *obj);



#endif /* ML_OBJ_H */


