

#ifndef ML_TOKEN_H
#define ML_TOKEN_H


#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "config.h"



typedef enum
{
    TOKEN_NUMBER = 0,
    TOKEN_SYMBOL = 1,
    
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

    token_value_s value;
    
} token_s;


typedef struct s_token_list
{
    token_s tk;

    struct s_token_list *next;
    
} token_list_s;


token_s* token_make_number(const char *code, size_t code_sz);

#endif /* ML_TOKEN_H */

