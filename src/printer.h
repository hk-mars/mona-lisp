

#ifndef __PRINTER__
#define __PRINTER__


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "obj.h"


typedef enum
{
    PRINTER_OK = 0,
    PRINTER_ERR = 1,
    PRINTER_ERR_NULL = 2,
    
} printer_rt_t;


typedef enum
{
    STREAM_UNKNOWN = 0,
    STREAM_OUTPUT = 1,
    STREAM_INPUT = 2,
    
} stream_t;

typedef struct
{
    stream_t type;
    
    char *buf;
    size_t max_buf_len;

    bool is_default_terminal;

    //show callback
    
} stream_s;


printer_rt_t printer_print(object_s *obj, stream_s *stream);


#endif /* __PRINTER__ */


