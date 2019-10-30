

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
    ML_ERR_BUF_OVERFLOW = 5,
    ML_ERR_STACK_OVERFLOW = 6,
    ML_ERR_EVAL = 7,
    ML_ERR_EVAL_DIVIDE_ZERO = 8,
    ML_ERR_BIND_VARIABLE = 9,
    ML_ERR_SYNTAX = 10,
    ML_ERR_SYNTAX_CAR = 11,
    ML_ERR_SYNTAX_CDR = 12,
    ML_ERR_SYNTAX_CONS = 13,
    ML_ERR_VARIABLE_UNBOUND = 14,
    ML_ERR_SYNTAX_EQ = 15,
    ML_ERR_UNKNOWN_CALL = 16,
    
} ml_err_t;



/* process for the out-of-memory error */
void ml_err_proc_mem_full(void);

/* signal an error for processing */
void ml_err_signal(ml_err_t err);


#endif /* ML_ERROR_H */

