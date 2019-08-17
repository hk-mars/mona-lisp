

#include "obj.h"

#include "debug.h"

#include "token.h"



void
obj_show(object_s *obj)
{
    func_s();

 
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

      token_show(&obj->token);
      
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

    func_ok();
}

