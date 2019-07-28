

#ifndef _PARSER_H_
#define _PARSER_H_

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

//#include "stack.h"


typedef enum
{
    PARSER_OK = 0,
    PARSER_ERR = 1,
    PARSER_ERR_NULL = 2,

} parser_rt_t;


parser_rt_t parser_init(void);


//s_object* parse(char *sql);
//int msql(void);

#endif /* _PARSER_H_ */


