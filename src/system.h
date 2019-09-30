
#ifndef __SYSTEM_H__
#define __SYSTEM_H__


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"


typedef enum
{
    SYS_STATUS_UNKNOWN = 0,
    SYS_STATUS_INIT = 1,
    SYS_STATUS_RUNNING = 2,
    SYS_STATUS_ERROR = 3,
    SYS_STATUS_DEBUGGING = 4,
    SYS_STATUS_RECOVERING = 5,
    SYS_STATUS_UPGRADING = 6,
    
} sys_status_t;



void sys_set_status(sys_status_t st);

sys_status_t  sys_get_status(void);

void sys_show_status(void);


#endif /* __SYSTEM_H__ */


