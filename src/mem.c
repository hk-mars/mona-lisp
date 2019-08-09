

#include "mem.h"

#include "debug.h"

#include "error.h"

#include "gc.h"



void*
ml_malloc(size_t sz)
{
    void *m = malloc(sz);

    if (!m) {
      
	ml_err_signal(ML_ERR_MEM_FULL);

	return NULL;
    }

    //debug("sz: %d \n", sz);
    memset(m, 0, sz);

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

