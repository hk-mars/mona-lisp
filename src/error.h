

#ifndef ML_ERROR_H
#define ML_ERROR_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>


typedef enum
{
    ML_OK = 0,
    ML_ERR = 1,
    ML_ERR_NULL = 2,
    ML_ERR_MEM_FULL = 3,
    ML_ERR_ILLEGAL_CHAR = 4,
    
    
} ml_err_t;



/* process for the out-of-memory error */
void ml_err_proc_mem_full(void);

/* signal an error for processing */
void ml_err_signal(ml_err_t err);


#endif /* ML_ERROR_H */

