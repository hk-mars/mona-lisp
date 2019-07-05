

#ifndef ML_FORM_H
#define ML_FORM_H


#include <stdint.h>
#include <stdio.h>

#include "config.h"

#include "symbol.h"
#include "obj.h"
#include "list.h"


typedef enum
{
    SYMBOL_FORM = 0,

    /* 1. a non-empty list.
     * 2. a special form.
     * 3. a lambda form.
     * 4. a macro form.
     * 5. a function form.
     *
     */
    COMPOUND_FORM = 1,

    SELF_EVALUATING_FORM = 2,
    
    OPERATOR_FORM = 3,
    
} form_t;


/* a form is any object meant to be evaluated.
 */
typedef struct s_form
{
    form_t type;

    symbol_s *symbol;
    
    lisp_list_s *list;

    object_s *self_eval;
    
    struct s_form *next;

} form_s;


#endif /* ML_FORM_H */

