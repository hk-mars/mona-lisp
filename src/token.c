

#include "token.h"

#include "debug.h"

#include "mem.h"



token_s*
token_make_number(const char *code, size_t code_sz)
{
    token_s *t = (token_s*)ml_malloc(sizeof(token_s));
    if (!t) return NULL;


    func_ok();
    return t;
}


