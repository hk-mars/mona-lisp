

#include "mem.h"

#include "debug.h"

#include "error.h"

#include "gc.h"


void*
ml_malloc(size_t sz)
{
    void *m = malloc(sz);

    if (!m) {
	ml_err_proc_mem_full();

	return NULL;
    }


    return m;
}


void
ml_free(void *mem)
{
    if (!mem) {

	debug_err("%s, @mem is NULL", __func__);
	return;
    }
    
    free(mem);
}

