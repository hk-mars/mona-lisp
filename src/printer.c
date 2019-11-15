

#include "printer.h"

#include "debug.h"

#include "mem.h"

#include "error.h"


#define PRINTER_DEBUG_DISABLE true
#if PRINTER_DEBUG_DISABLE
#undef debug
#define debug(...) ;
#endif	


/**
 * Introduction of Common Lisp printer, see:
 * http://www.lispworks.com/documentation/HyperSpec/Body/22_a.htm
 * Copyright 1996-2005, LispWorks Ltd. All Rights Reserved.
 */



static void
print_token(token_s *token, stream_s *stream)
{
    //func_s();

    switch (token->type) {

    case TOKEN_NUM_INT:
	debug("int:  %d \n", token->value.num_int);

	token_print_fixnum(token->value.num_int, stream->buf, stream->max_buf_len);
	
	break;

    case TOKEN_NUM_FLOAT:
	debug("float:  %f \n", token->value.num_float);
	
	sprintf(stream->buf, "%f\n", token->value.num_float);
	
	break;

    case TOKEN_NUM_RATIO:
	debug("ratio:  %d/%d \n",
	      token->value.num_ratio.int_up,
	      token->value.num_ratio.int_down);

	snprintf(stream->buf, stream->max_buf_len, "%d/%d\n",
		 token->value.num_ratio.int_up,
		 token->value.num_ratio.int_down);
		
	break;
	
    case TOKEN_SYMBOL:
	debug("symbol: %s \n", token->value.symbol);

	if (token->value.symbol[0] == '\'') {
	    
	    snprintf(stream->buf, stream->max_buf_len, "%s\n", token->value.symbol+1);
	}
	else {

	    snprintf(stream->buf, stream->max_buf_len, "%s\n", token->value.symbol);
	}
	
	break;
	
    default:

	debug_err("unkown token  \n");
	break;

    }
    
    //func_e();
}



printer_rt_t
printer_print(object_s *obj, stream_s *stream)
{
    
    func_s();

    if (!obj) return PRINTER_ERR_NULL;
    if (!stream) return PRINTER_ERR_NULL;
    if (!stream->buf) return PRINTER_ERR_NULL;

   
    /* print an object to the output-stream. 
     * the output-stream as codes, we can check its syntax via related APIs.
     */

    switch (obj->type) {

    case OBJ_CHARACTER:

	debug("OBJ_CHARACTER \n");

	if (strlen(obj->character) == 1) {
	    
	    snprintf(stream->buf, stream->max_buf_len, "#\\%c\n", obj->character[0]);
	}
	else {
	    
	    snprintf(stream->buf, stream->max_buf_len, "%s\n", obj->character);
	}
	
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
	    
	    snprintf(stream->buf, stream->max_buf_len,
		     "%s\n", obj->subtype == OBJ_SUBTYPE_BOOL_TRUE ? "t" : "nil");
	}
	else {
	
	    print_token(&obj->token, stream);
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
	goto FAIL;

    }

    if (obj->sub) {

	//debug("subform: \n");
    }

    
    if (stream->is_default_terminal) {
	
	printf("%s", stream->buf);
    }
    else {

	//stream->show(stream);
    }
    
    
    func_ok();
    return PRINTER_OK;

  FAIL:
    func_fail();
    return PRINTER_ERR;
}


