

#include "obj.h"

#include "debug.h"

#include "token.h"

#include "list.h"



void
obj_show(object_s *obj)
{
    //func_s();

    
    switch (obj->type) {

    case OBJ_CHARACTER:

      debug("OBJ_CHARACTER \n");

      debug("%s \n", obj->character);
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

      if (obj->subtype == OBJ_SUBTYPE_BOOL_TRUE ||
	  obj->subtype == OBJ_SUBTYPE_BOOL_FALSE) {

	  debug("%s \n", obj->subtype == OBJ_SUBTYPE_BOOL_TRUE ? "bool: t" : "bool: nil");
      }
      else if (obj->subtype == OBJ_SUBTYPE_EXPRESSION) {

	  debug("symbol quote expression: %s \n", obj_get_symbol(obj));
      }
      else if (obj->subtype == OBJ_SUBTYPE_LIST_AS_ELEMENT) {

	  debug("list as object element \n");

	  list_show(obj->list);
      }
      else {
	
	  token_show(&obj->token);
      }
      
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

    if (obj->sub) {

	//debug("subform: \n");
    }
    
    
    //func_ok();
}


bool
obj_is_symbol(object_s *obj)
{
    return (obj->token.type == TOKEN_SYMBOL);
}


char*
obj_get_symbol(object_s *obj)
{
    if (!obj_is_symbol(obj)) return NULL;
    
    return obj->token.value.symbol;
}


bool
obj_is_true(object_s *obj)
{
    return (obj->subtype == OBJ_SUBTYPE_BOOL_TRUE);
}


bool
obj_is_nil(object_s *obj)
{
    return (obj->subtype == OBJ_SUBTYPE_BOOL_FALSE);
}


bool
obj_is_char(object_s *obj)
{
    return (obj->type == OBJ_CHARACTER);
}


bool
obj_update(object_s *obj, object_s *new)
{
    func_s();
    
    switch (obj->type) {
    case OBJ_TYPE:

	memcpy(&obj->token, &new->token, sizeof(token_s));
	break;

    default:
	goto FAIL;	
    }

    obj_show(obj);
    
    func_ok();
    return true;

  FAIL:
    func_fail();
    return false;
}


bool
obj_clone_token(object_s *obj, token_s *token)
{
    func_s();
    
    if (!obj || !token) return false;

    memcpy(&obj->token, token, sizeof(token_s));

    out(ok, true);
}


bool
obj_clone(object_s *to_obj, object_s *from_obj)
{
    func_s();

    if (!from_obj || !to_obj) out(fail, false);

    memcpy(to_obj, from_obj, sizeof(object_s));
    
    if (from_obj->list) {

	to_obj->list = (void*)list_new();
	if (!to_obj->list) out(fail, false);
	
	list_copy(to_obj->list, from_obj->list);
    }
    

    out(ok, true);
}


bool
obj_set_nil(object_s *obj)
{
    func_s();

    if (!obj) out(fail, false);

    obj->type = OBJ_TYPE;
    obj->subtype = OBJ_SUBTYPE_BOOL_FALSE;

    out(ok, true);
}


bool
obj_set_t(object_s *obj)
{
    func_s();

    if (!obj) out(fail, false);

    obj->type = OBJ_TYPE;
    obj->subtype = OBJ_SUBTYPE_BOOL_TRUE;

    out(ok, true);
}




