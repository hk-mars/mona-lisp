

#include "system.h"

#include "debug.h"

#include "error.h"

#include "util.h"


static sys_status_t m_sys_status;


void
sys_set_status(sys_status_t st)
{
    m_sys_status = st;
}


sys_status_t
sys_get_status(void)
{
    return m_sys_status;
}


void
sys_show_status(void)
{
    switch (m_sys_status) {

    case SYS_STATUS_INIT:

	debug("SYS_STATUS_INIT \n");

	break;

    case SYS_STATUS_RUNNING:

	debug("SYS_STATUS_RUNNING \n");

	break;	

    case SYS_STATUS_ERROR:

	debug("SYS_STATUS_ERROR \n");

	break;


    case SYS_STATUS_DEBUGGING:

	debug("SYS_STATUS_DEBUGGING \n");

	break;

    case SYS_STATUS_RECOVERING:

	debug("SYS_STATUS_RECOVERING \n");

	break;

    case SYS_STATUS_UPGRADING:

	debug("SYS_STATUS_UPGRADING \n");

	break;

	
    default:

	debug("SYS_STATUS_UNKNOWN \n");

	break;
	
    }   
}


