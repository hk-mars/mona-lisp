

#include "error.h"

#include "debug.h"

#include "system.h"


void
ml_err_proc_mem_full(void)
{
    debug_err("err: oops, heap memory is full");

    
    /* TODO: process for out of memory */
    while (1);
}


static void
proc_illegal_char(void)
{
    func_s();

    /* TODO: process for illegal lex char */
    while (1);
}


static void
proc_stack_overflow(void)
{
    func_s();
    
    /* TODO: process for stack overflow */
    while (1);
}


static void
proc_buf_overflow(void)
{
    func_s();
    
    /* TODO: process for buffer overflow */
    while (1);
}


static void
proc_divide_zero(void)
{
    func_s();
    
    /* TODO: process for deviding zero */
    while (1);
}


void
ml_err_signal(ml_err_t err)
{
    debug("%s, err:%d \n", __func__, err);

    switch (err) {
    case ML_ERR_ILLEGAL_CHAR:

	proc_illegal_char();
	break;

    case ML_ERR_MEM_FULL:

	ml_err_proc_mem_full();
	break;

    case ML_ERR_STACK_OVERFLOW:

	proc_stack_overflow();
	break;


    case ML_ERR_BUF_OVERFLOW:

	proc_buf_overflow();
	break;

    case ML_ERR_EVAL_DIVIDE_ZERO:

	proc_divide_zero();
	break;

    case ML_ERR_SYNTAX_CAR:

	debug_err("illegal car syntax \n");
	break;
	
    case ML_ERR_SYNTAX_CDR:

	debug_err("illegal cdr syntax \n");
	break;	

    case ML_ERR_SYNTAX_CONS:

	debug_err("illegal cons syntax \n");
	break;

    case ML_ERR_VARIABLE_UNBOUND:
	debug_err("unbound variable \n");
	break;

    case ML_ERR_SYNTAX_EQ:
	debug_err("ML_ERR_SYNTAX_EQ \n");
	break;

    case ML_ERR_UNKNOWN_CALL:
	debug_err("ML_ERR_UNKNOWN_CALL \n");
	break;


    case ML_ERR_NUM_OVERFLOW:
	debug_err("ML_ERR_NUM_OVERFLOW \n");
	break;
	
    default:
	debug_err("unkown error \n");
	break;
    }


    sys_show_status();
    
    
    #if 1
    while (1) {
    }
    #endif
    
}


#if DEBUG_ENABLE

void
debug_err(const char *fmt, ...)
{   
    va_list argp;
    
    char buf[512];
    unsigned long len;

    memset(buf, 0, sizeof(buf));

    va_start(argp, fmt);

    len = vsprintf(buf, fmt, argp);
    
    if (len > sizeof(buf)) {

	va_end(argp);
	
	printf("err: stack buffer overflow in %s \n", __func__);
	
	sys_set_status(SYS_STATUS_ERROR);
	ml_err_signal(ML_ERR_BUF_OVERFLOW);

	return;
    }
    
    va_end(argp);

    printf("err: %s", buf);
}

#endif

