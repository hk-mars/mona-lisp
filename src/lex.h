
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
    
    
} lex_rt_t;


typedef enum
{
    TOKEN_NUMERIC = 0,
    TOKEN_SYMBOL = 1,
    
} token_t;


typedef struct
{
    token_t type;
    
} token_s;


typedef struct
{
    const char *code;

    token_s *tokens[TOKEN_MAX_CNT];
    int token_cnt;

    
    
} lex_s;


/* Initialize lexer */
lex_rt_t lex_init(void);


/* Analyse lexical for the code */
lex_rt_t lex(lex_s *lex, const char *code);



#endif /* ML_LEX_H */

