

#ifndef __MONALISP_H__
#define __MONALISP_H__


#include <stdint.h>
#include <stdio.h>
#include <string.h>


typedef enum
{
    LISP_OK = 0,
    LISP_ERR = 1,
    LISP_ERR_LEXER = 2,
    
    
} lisp_rt_t;


/* get the version of monalisp */
const char* ml_get_version(void);

/* initialize monalisp */
lisp_rt_t ml_init(void);





#endif /* __MONALISP_H__ */


