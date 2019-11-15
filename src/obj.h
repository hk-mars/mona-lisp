

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


typedef enum
{
    OBJ_SUBTYPE_BOOL_TRUE = 1,
    OBJ_SUBTYPE_BOOL_FALSE = 2,

    OBJ_SUBTYPE_MACRO_BACKQUOTE = 3,
    OBJ_SUBTYPE_MACRO_COMMNA = 4,

    OBJ_SUBTYPE_EXPRESSION = 5,
    
} object_sub_t;

typedef struct s_object
{
    object_t type;
    object_sub_t subtype;

    char *character;
    
    token_s token;

    void *self;

    void *sub;

    bool is_specified_type;
    
} object_s;


void obj_show(object_s *obj);

bool obj_is_symbol(object_s *obj);

char* obj_get_symbol(object_s *obj);

bool obj_is_true(object_s *obj);
bool obj_is_nil(object_s *obj);

bool obj_is_char(object_s *obj);

bool obj_update(object_s *obj, object_s *new);

bool obj_clone_token(object_s *obj, token_s *token);

#endif /* ML_OBJ_H */


