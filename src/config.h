

#ifndef ML_CONFIG_H
#define ML_CONFIG_H


#include <stdint.h>
#include <stdio.h>


#define YES 1
#define NO  0


#ifndef size_t
#define size_t int
#endif

#ifndef bool
#define bool unsigned char
#endif

#ifndef true
#define true YES
#endif

#ifndef false
#define false NO
#endif



#define DEBUG_ENABLE YES



#endif /* ML_CONFIG_H */

