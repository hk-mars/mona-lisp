

#ifndef ML_LIST_H
#define ML_LIST_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "obj.h"

#include "token.h"


typedef struct s_lisp_list
{
    object_s obj;

    struct s_lisp_list *next;
    struct s_lisp_list *front;

    bool is_head;
    bool is_nil;
    
} lisp_list_s;

    

lisp_list_s* list_new(void);

bool list_add_token(lisp_list_s *list, token_s *token);

bool list_add_object(lisp_list_s *list, object_s *obj);

bool list_add_char_obj(lisp_list_s *list, char *ch);

void list_show(lisp_list_s *list);

void list_mark_type_specified(lisp_list_s *list);

bool list_copy(lisp_list_s *dst, lisp_list_s *src);

bool list_is_head(lisp_list_s *node);


bool list_add_list(lisp_list_s *list, lisp_list_s *list_element);


#endif /* ML_LIST_H */

