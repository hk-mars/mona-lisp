
#ifndef ML_LEX_H
#define ML_LEX_H


#include <stdint.h>
#include <stdio.h>

#include "config.h"
#include "chars.h"


typedef enum
{
    LEX_OK = 0,
    LEX_ERR = 1,
    LEX_ERR_NULL = 2,
    LEX_ERR_ARGU = 3,
    
    
} lex_rt_t;


typedef enum
{
    TOKEN_NUMERIC = 0,
    TOKEN_SYMBOL = 1,
    
} token_t;


typedef struct
{
    const char *code;
    size_t code_sz;
    
} code_s;


typedef struct
{
    token_t type;
    
} token_s;


typedef struct
{
    token_s *tokens[TOKEN_MAX_CNT];
    int token_cnt;
    
} lex_s;


typedef struct
{
    
} value_list_s;


typedef struct
{
    int vint;
    float vfloat;
    value_list_s vlist;
    
} values_s;



/* Initialize lexer */
lex_rt_t ml_lex_init(void);


/* Lexical analyzing for the code */
lex_rt_t ml_lex(lex_s *lex, code_s *cd);



#endif /* ML_LEX_H */

