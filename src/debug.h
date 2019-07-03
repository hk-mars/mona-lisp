

#ifndef ML_DEBUG_H
#define ML_DEBUG_H

#include <stdint.h>
#include <stdio.h>

#include "config.h"


#if DEBUG_ENABLE

#define debug printf
#define debug_err printf

#define func_s() debug("\n%s \n", __func__)
#define func_e() debug("%s end \n\n", __func__)
#define func_ok() debug("%s ok \n\n", __func__)
#define func_fail() debug("%s failed \n\n", __func__)

#define show_func_line() debug("%s: %dL \n", __func__, __LINE__)

#else

#define debug(...) ;
#define debug_err(...) ;

#define func_s() ;
#define func_e() ;
#define func_ok() ;
#define func_fail() ;
#define show_func_line() ;

#endif /* DEBUG_ENABLE */

#define fs() func_s()
#define fe() func_e()


#define show printf




#endif /* ML_DEBUG_H */


