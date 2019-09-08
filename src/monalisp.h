

#ifndef MONALISP_H
#define MONALISP_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>


typedef enum
{
    LISP_OK = 0,
    LISP_ERR = 1,
    LISP_ERR_LEX = 2,
    LISP_ERR_SYNTAX = 3,
    LISP_ERR_PARSER = 4,
    LISP_ERR_READER = 5,
    LISP_ERR_STACK = 6,
    LISP_ERR_VAR = 7,
    LISP_ERR_FUNC = 8,
    
} lisp_rt_t;


/* Get the version of monalisp */
const char* ml_get_version(void);

/* Initialize monalisp */
lisp_rt_t ml_init(void);





#endif /* MONALISP_H */


