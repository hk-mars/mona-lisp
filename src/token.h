

#ifndef ML_TOKEN_H
#define ML_TOKEN_H


#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"



typedef enum
{
    TOKEN_UNKOWN = 0,
    TOKEN_SYMBOL = 1,
    TOKEN_NUM_INT = 2,
    TOKEN_NUM_FLOAT = 3,
    TOKEN_NUM_RATIO = 4,
    
} token_t;


typedef struct
{
    int int_up;

    int int_down;
    
} ratio_s;


typedef union
{
    char *symbol;

    int num_int;

    float num_float;

    ratio_s num_ratio;
} token_value_s;


typedef struct
{
    token_t type;

    char *name;

    token_value_s value;
    
} token_s;


token_s* token_create(void);

token_s* token_clone(token_s *token);

void token_show(token_s *token);

#endif /* ML_TOKEN_H */

