
#ifndef __GC_H__
#define __GC_H__


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"


int gc_new(void);

void* gc_malloc(size_t size);

void gc_free(void);

void gc_show(void);

bool gc_debug(void);


#endif /* __GC_H__ */

