

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
    ML_ERR_SYNTAX_ATOM = 11,
    ML_ERR_SYNTAX_EQ = 12,
    ML_ERR_SYNTAX_CAR = 13,
    ML_ERR_SYNTAX_CDR = 14,
    ML_ERR_SYNTAX_CONS = 15,
    ML_ERR_SYNTAX_LIST,
    ML_ERR_SYNTAX_SETQ,
    ML_ERR_SYNTAX_IF,
    ML_ERR_SYNTAX_PRINT,
    ML_ERR_SYNTAX_NUM_ADD,
    ML_ERR_SYNTAX_NUM_COMPARE,
    ML_ERR_SYNTAX_MACRO_LOOP,
    ML_ERR_SYNTAX_MACRO_RETURN,
    ML_ERR_VARIABLE_UNBOUND,
    ML_ERR_UNKNOWN_CALL,
    ML_ERR_NUM_COMPARE,
    ML_ERR_NUM_OVERFLOW,
    ML_ERR_EVAL_CONS,
    ML_ERR_EVAL_CAR,
    ML_ERR_EVAL_CDR,
    ML_ERR_EVAL_EQ,
    ML_ERR_EVAL_LIST,
    ML_ERR_EVAL_SETQ,
    ML_ERR_SYMBOL_UNBOUND,
    ML_ERR_EVAL_IF,
    ML_ERR_EVAL_PRINT,
    ML_ERR_EVAL_NUM_ADD,
    ML_ERR_EVAL_NUM_COMPARE,
    ML_ERR_EVAL_NUM_GREATER_THAN
    
} ml_err_t;


#define err_signal(id, info)					\
    if (!(info)) {						\
	ml_err_signal_x(id, __FUNCTION__, __LINE__);		\
    }								\
    else {							\
	ml_err_signal_info(id, __FUNCTION__, __LINE__, info);	\
    }


/* process for the out-of-memory error */
void ml_err_proc_mem_full(void);

/* signal an error for processing */
void ml_err_signal(ml_err_t err);

void ml_err_signal_x(ml_err_t err, const char *func_name, int line);
void ml_err_signal_info(ml_err_t err, const char *func_name, int line, void *info);

#endif /* ML_ERROR_H */

