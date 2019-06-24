

#include "util.h"

#include "debug.h"

#include "mem.h"


char*
ml_strdup(const char *str)
{
    if (!str) return NULL;

    int len = strlen(str) + 1;
    
    char *s = ml_malloc(len);
    if (!s) return NULL;

    memcpy(s, str, len);

    return s;
}



