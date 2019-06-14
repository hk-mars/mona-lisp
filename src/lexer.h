
#ifndef __LEXER_H__
#define __LEXER_H__


#include <stdint.h>
#include <stdio.h>


typedef enum
{
    LEX_OK = 0,
    LEX_ERR = 1,
    
} lex_rt_t;


/* initialize lexer */
lex_rt_t lexer_init(void);




#endif /* __LEXER_H__ */

