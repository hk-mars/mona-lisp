
#ifndef __LEXER_H__
#define __LEXER_H__


#include <stdint.h>
#include <stdio.h>


typedef enum
{
    LEXER_OK = 0,
    LEXER_ERR = 1,
    
} lexer_rt_t;


/* initialize lexer */
lexer_rt_t lexer_init(void);




#endif /* __LEXER_H__ */

