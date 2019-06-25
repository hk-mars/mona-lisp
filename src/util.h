

#ifndef ML_UTIL_H
#define ML_UTIL_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"


char* ml_util_strdup(const char *str);

void ml_util_show_buf(char *buf, size_t size);


#endif /* ML_UTIL_H */


