

#include "obj.h"

#include "debug.h"

#include "token.h"



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

	  debug("%s \n", obj->subtype == OBJ_SUBTYPE_BOOL_TRUE ? "t" : "nil");
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
obj_update(object_s *obj, object_s *new)
{
    func_s();
    
    switch (obj->type) {
    case OBJ_TYPE:

	memcpy(&obj->token, &new->token, sizeof(token_s));
	break;

    default:
	break;
    }

    obj_show(obj);
    
    func_ok();
    return true;

  FAIL:
    func_fail();
    return false;
}

