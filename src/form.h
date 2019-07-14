

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
    UNKOWN_FORM = 0,
    
    SYMBOL_FORM = 1,

    /* a compound non-empty-list form:
     * 1. a special form.
     * 2. a lambda form.
     * 3. a macro form.
     * 4. a function form.
     *
     */
    COMPOUND_SPECIAL_FORM = 2,
    COMPOUND_LAMBDA_FORM = 3,
    COMPOUND_MACRO_FORM = 4,
    COMPOUND_FUNCTION_FORM = 5,

    SELF_EVALUATING_FORM = 6,

} form_t;


typedef enum
{
    SYMBOL_MACRO_FORM = 1,
    SYMBOL_VARIABLE_FORM = 2,
    
} symbol_form_t;


/* a form is any object meant to be evaluated.
 */
typedef struct s_form
{
    form_t type;
    unsigned char sub_type; /* the sub type of this form */

    symbol_s *symbol;
    
    lisp_list_s *list;

    object_s *self_eval;
    
    
    struct s_form *next; /* next form */
    struct s_form *front; /* front form */
    struct s_form *sub; /* sub form */
    

} form_s;


form_s* form_create(void);

bool form_create_symbol(form_s *form);

void form_free(form_s *form);

bool form_add_front(form_s *forms, form_s *new_form);

void form_set_type(form_s *form, form_t type);

bool form_is_unkown(form_s *form);

void form_show(form_s *form);


#endif /* ML_FORM_H */

