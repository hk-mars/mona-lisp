

#include "list.h"

#include "debug.h"

#include "mem.h"

#include "token.h"

#include "obj.h"

#include "util.h"



bool
list_add_token(lisp_list_s *list, token_s *token)
{
    if (!list || !token) return false;

    lisp_list_s *node = (lisp_list_s*)ml_malloc(sizeof(lisp_list_s));
    if (!node) return false;

    node->obj.type = OBJ_TYPE;
    
    memcpy(&node->obj.token, token, sizeof(token_s));
    
    if (!list->front) {

	list->next = node;
	list->front = node;
	node->front = list;
	node->next = list;
    }
    else {
	
	list->front->next = node;
	node->front = list->front;
	node->next = list;
	list->front = node;
    }

    func_ok();
    return true;
}


bool
list_add_object(lisp_list_s *list, object_s *obj)
{
    if (!list || !obj) return false;

    func_s();
    
    lisp_list_s *node = (lisp_list_s*)ml_malloc(sizeof(lisp_list_s));
    if (!node) return false;

    debug("new node \n");
    memcpy(&node->obj, obj, sizeof(object_s));

    
    if (!list->front) {

	debug("null list \n");
	
	list->next = node;
	list->front = node;
	node->front = list;
	node->next = list;
    }
    else {

	debug("new list \n");
	list->front->next = node;
	node->front = list->front;
	node->next = list;
	list->front = node;
    }


    func_ok();
    return true;
}


bool
list_add_char_obj(lisp_list_s *list, char *ch)
{
    object_s obj;
    
    memset(&obj, 0, sizeof(object_s));
    
    obj.type = OBJ_CHARACTER;
    obj.character = ch;
    
    return list_add_object(list, &obj);    
}


void
list_show(lisp_list_s *list)
{
    if (!list) return;

    func_s();

    lisp_list_s *l = list->next;

    while (l && l != list) {

	switch (l->obj.type) {

	case OBJ_CHARACTER:

	    debug("OBJ_CHARACTER \n");
	    break;

	case OBJ_LIST:

	    debug("OBJ_LIST \n");
	    break;

	case OBJ_ARRAY:

	    debug("OBJ_ARRAY \n");
	    break;
	    
	case OBJ_SEQUENCE:

	    debug("OBJ_SEQUENCE \n");
	    break;
	    
	case OBJ_TYPE:

	    debug("OBJ_TYPE \n");
	    break;
	    
	case OBJ_INPUT_STREAM:

	    debug("OBJ_INPUT_STREAM \n");
	    break;
	    
	case OBJ_OUTPUT_STREAM:

	    debug("OBJ_OUTPUT_STREAM \n");
	    break;
	    
	case OBJ_CLASS:

	    debug("OBJ_CLASS \n");
	    break;
	    
	default:

	    debug("unkown object \n");
	    break;

	}

	l = l->next;
    }

    func_ok();
}




