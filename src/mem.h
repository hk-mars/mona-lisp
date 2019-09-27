

#ifndef ML_MEM_H
#define ML_MEM_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "config.h"


void* ml_malloc(size_t sz);

void ml_free(void *mem);

void* mm_malloc(size_t sz);

void mm_free(void *mem);

void mm_show(void);

#endif /* ML_MEM_H */

