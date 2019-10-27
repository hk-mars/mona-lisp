

#ifndef ML_UTIL_H
#define ML_UTIL_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


#include "config.h"


#define ARR_LEN(array) (sizeof(array) / sizeof(array[0]))


char* ml_util_buf2str(const char *buf, size_t len);

char* ml_util_strdup(const char *str);

char* ml_util_str_clone(const char *str, void* (f_malloc) (size_t));

bool ml_util_strbufcmp(const char *str, char *buf, size_t buf_len);

void ml_util_show_buf(char *buf, size_t size);

unsigned int ml_util_arr2int(char *arr, size_t len);

char* ml_util_clone_str_as_upper(char *str);


/* API of *read_base* :
 * The value of *read-base* controls the interpretation of tokens by read as 
 * being integers or ratios.
 */
void ml_util_set_read_base(unsigned char read_base);
unsigned char ml_util_get_read_base(void);


bool ml_util_fwrite(char *name, char *line);


#endif /* ML_UTIL_H */


