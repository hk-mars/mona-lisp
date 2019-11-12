

#ifndef _NUM_LIMITED_H_
#define  _NUM_LIMITED_H_

#include <stdint.h>
#include <stdio.h>
#include <limits.h>

#include "config.h"



#if OS_64BIT_ENABLE

#define fixnum_t int64_t
#define FIXNUM_MAX INT64_MAX
#define FIXNUM_MIN INT64_MIN

#else 

#define fixnum_t int32_t
#define FIXNUM_MAX INT32_MAX
#define FIXNUM_MIN INT32_MIN

#endif


#endif /*  _NUM_LIMITED_H_ */

