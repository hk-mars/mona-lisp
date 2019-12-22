

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
    UNKNOWN_FORM = 0,
    
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

    
    /** 
     *  All numbers, characters, strings, and bit-vectors are self-evaluating forms. 
     *  When such an object is evaluated, that object (or possibly a copy in the case of
     *  numbers or characters) is returned as the value of the form. The empty list (), 
     *  which is also the false value nil, is also a self-evaluating form: the value of 
     *  nil is nil. Keywords (symbols written with a leading colon) also evaluate to 
     *  themselves: the value of :start is :start.
     *
     *  Reference: 
     *  Common Lisp the Language, 2nd Edition
     *  5.1.1. Self-Evaluating Forms
     */
    SELF_EVALUATING_FORM = 6,

    /* backquote form is a subform of the defmacro form.
     */
    //BACKQUOTE_FORM = 7,

} form_t;


typedef enum
{
    SYMBOL_MACRO_FORM = 1,
    SYMBOL_VARIABLE_FORM = 2,
    
} symbol_form_t;


typedef enum
{
    SELF_EVAL_FORM_NUMBER = 1,
    SELF_EVAL_FORM_CHARACTER = 2,
    SELF_EVAL_FORM_STRING = 3,
    SELF_EVAL_FORM_BIT_VECTOR = 4,
    SELF_EVAL_FORM_BOOL = 5,
    SELF_EVAL_FORM_EMPTY_LIST = 6,
    SELF_EVAL_FORM_KEYWORD = 7,
    
} self_evaluating_form_t;


/* elementary S-functions and Predicates
 */
typedef enum
{
    S_FUNCTION_ATOM = 1,
    S_FUNCTION_CONS = 2,
    S_FUNCTION_CAR = 3,
    S_FUNCTION_CDR = 4,
    S_PREDICATE_EQ = 5, /* a predicate is still a type of function */
    
    S_FUNCTION_LIST = 6,
    S_FUNCTION_PRINT = 7,
    
    S_FUNCTION_NUM_ADD,
    S_FUNCTION_NUM_GREATER_THAN,
    
} s_function_t;


typedef enum
{
    SPECIAL_FORM_QUOTE = 1,
    SPECIAL_FORM_SETQ = 2,
    SPECIAL_FORM_IF = 3,
    SPECIAL_FORM_LET = 4,
    SPECIAL_FORM_BLOCK = 5,
    
} special_form_t;


typedef enum
{
    MACRO_LOOP = 1,
    MACRO_RETURN = 2,
    MACRO_DEFUN = 3,
    MACRO_DEFMACRO = 4,
    
} macro_t;



/* a form is any object meant to be evaluated.
 */
typedef struct s_form
{
    form_t type;
    unsigned char subtype; /* subtype of the form */

    symbol_s *symbol;
    
    lisp_list_s *list;

    object_s *obj;
    
    struct s_form *next; /* next form */
    struct s_form *front; /* front form */

    char *code; /* source code of the form */
    size_t code_sz;

    size_t obj_count;
    
} form_s;


form_s* form_create(void);

form_s* form_create_as_self_eval_form(void);

form_s* form_create_as_quoted_expression(void);

form_s* form_create_as_character_obj(char *character);

form_s* form_create_nil(void);

form_s* form_create_symbol_form(void);

void form_free(form_s *form);

bool form_add_front(form_s *forms, form_s *new_form);

void form_set_type(form_s *form, form_t type);

bool form_is_unkown(form_s *form);

void form_show(form_s *form);

void form_show_type(form_s *form);

form_s* form_clone(form_s *form);

bool form_add_token(form_s *form, token_s *token);

bool form_is_quote_form(form_s *form);

#endif /* ML_FORM_H */

