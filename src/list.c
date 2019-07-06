

#include "list.h"

#include "debug.h"

#include "mem.h"

#include "token.h"

#include "obj.h"




bool
list_add_token(lisp_list_s *list, token_s *token)
{
    if (!list || !token) return false;

    lisp_list_s *node = (lisp_list_s*)ml_malloc(sizeof(lisp_list_s));
    if (!node) return false;

    memcpy(&node->obj.token, token, sizeof(token_s));
    
    if (!list->front) {

	list->next = node;
	node->front = list;
	node->next = list;
    }
    else {
	
	list->front->next = node;
	node->front = list->front;
	node->next = list;
    }

    func_ok();
    return true;
}








