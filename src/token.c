

#include "token.h"

#include "debug.h"

#include "mem.h"



token_s*
token_create(void)
{
    token_s *t = (token_s*)ml_malloc(sizeof(token_s));
    if (!t) return NULL;

    func_ok();
    return t;
}


token_s*
token_clone(token_s *token)
{
    token_s *t = (token_s*)ml_malloc(sizeof(token_s));
    if (!t) return NULL;
    
    memcpy(t, token, sizeof(token_s));

    func_ok();
    return t;
}

