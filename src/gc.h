
#ifndef __GC_H__
#define __GC_H__


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"



typedef enum
{
    GC_OBJ = 0,
    GC_VAR = 1,
    GC_FUNC = 2,
    GC_MACRO = 3,

} gc_t;



typedef struct
{
    long block_cnt;
    long all_blocks_size;   
} gc_status_s;


typedef struct
{
    gc_t type;
    long id;
    gc_status_s st;
    
} gc_s;


gc_s gc_new(void);

void* gc_malloc(size_t size);

void gc_free(void);

//void gc_free_as_id(long id);

void gc_show(void);

bool gc_is_valid(void);

bool gc_debug(void);


#endif /* __GC_H__ */

