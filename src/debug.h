

#ifndef ML_DEBUG_H
#define ML_DEBUG_H

#include <stdint.h>
#include <stdio.h>

#include "config.h"


#if DEBUG_ENABLE

#define debug printf
#define debug_err printf

#else

#define debug ;
#define debug_err ;

#endif /* DEBUG_ENABLE */


#define func_s() debug("%s \r\n", __func__)
#define func_e() debug("%s end \r\n", __func__)
#define func_ok() debug("%s ok \r\n", __func__)
#define func_fail() debug("%s failed \r\n", __func__)


#define show printf



#endif /* ML_DEBUG_H */


