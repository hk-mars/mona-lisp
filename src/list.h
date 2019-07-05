

#ifndef ML_LIST_H
#define ML_LIST_H


#include <stdint.h>
#include <stdio.h>

#include "config.h"

#include "obj.h"


typedef struct s_lisp_list
{
    object_s obj;

    struct s_lisp_list *next;
    
} lisp_list_s;


#endif /* ML_LIST_H */

