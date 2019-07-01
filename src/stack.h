

#ifndef ML_STACK_H
#define ML_STACK_H

#include <stdint.h>
#include <stdio.h>

#include "config.h"


#define MAX_STACK_ITEM_CNT 2048

#define ml_stack_t int


typedef enum
{
    STACK_OK = 0,
    STACK_ERR = 1,
    STACK_ERR_INIT = 2,
    STACK_ERR_NULL = 3,
    STACK_ERR_OVERFLOW = 4,
    
} stack_rt_t;


typedef struct
{
    ml_stack_t *data;
    size_t data_sz;
    
} stack_data_t;



stack_rt_t stack_init(size_t sz);

stack_rt_t stack_push(void *data, size_t data_cnt);

stack_rt_t stack_pop(stack_data_t *mydata);


#endif /* ML_STACK_H */



