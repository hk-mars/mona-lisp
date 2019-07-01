

#ifndef ML_UTIL_H
#define ML_UTIL_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"


char* ml_util_strdup(const char *str);

void ml_util_show_buf(char *buf, size_t size);

unsigned int ml_util_arr2int(char *arr, size_t len);

#endif /* ML_UTIL_H */


