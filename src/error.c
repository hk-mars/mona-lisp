

#include "error.h"

#include "debug.h"


void
ml_err_proc_mem_full(void)
{
    debug_err("err: oops, heap memory is full");

    
    /* TODO: process for out of memory */
    while (1);
}
