

#include "error.h"

#include "debug.h"


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
}



void
ml_err_signal(ml_err_t err)
{
    show("%s, err:%d ", __func__, err);

    switch (err) {
    case ML_ERR_ILLEGAL_CHAR:

	proc_illegal_char();
	break;

    case ML_ERR_MEM_FULL:

	ml_err_proc_mem_full();
	break;

    default:
	debug_err("unkown error");
	break;
    }

}
