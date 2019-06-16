
#include "lex.h"

#include "debug.h"




/**
 * Initialize lexer 
 */
lex_rt_t
lex_init(void)
{
    func_s();

    

    func_ok();
    
    return LEX_OK;
}


/**
 * Analyse lexical for the code 
 */
lex_rt_t
lex(lex_s *lex, const char *code)
{
    func_s();

    if (!lex || !code) return LEX_ERR_NULL;

    

    lex->code = code;
    
    func_ok();
    
    return LEX_OK;
}
