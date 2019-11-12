

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


void
token_show(token_s *token)
{
    //func_s();

    switch (token->type) {

    case TOKEN_NUM_INT:
	debug("int:  %ld \n", (long)token->value.num_int);
	
	break;

    case TOKEN_NUM_FLOAT:
	debug("float:  %f \n", token->value.num_float);
	
	break;

    case TOKEN_NUM_RATIO:
	debug("ratio:  %d/%d \n",
	      token->value.num_ratio.int_up,
	      token->value.num_ratio.int_down);
	
	break;

	
    case TOKEN_SYMBOL:
	debug("symbol: %s \n", token->value.symbol);
	
	break;
	
    default:

	debug_err("unkown token  \n");
	break;

    }
    
    //func_e();
}


void
token_show_fixnum(fixnum_t num)
{
    
#if OS_64BIT_ENABLE
    debug("%lld \n", num);
#else
    debug("%d \n", num);    
#endif
    
}


bool
token_print_fixnum(fixnum_t num, char *buf, long buf_len)
{

#if OS_64BIT_ENABLE
    snprintf(buf, buf_len, "%lld\n", num);
#else
    snprintf(buf, buf_len, "%d\n", num);
#endif

    return true;
}
