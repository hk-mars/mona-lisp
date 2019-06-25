

#include "util.h"

#include "debug.h"

#include "mem.h"


char*
ml_util_strdup(const char *str)
{
    if (!str) return NULL;

    int len = strlen(str) + 1;
    
    char *s = ml_malloc(len);
    if (!s) return NULL;

    memcpy(s, str, len);

    return s;
}


void
ml_util_show_buf(char *buf, size_t size)
{
    show("%s, %d bytes:\n", __func__, size);
    buf += (size - 1);
    while(--size >= 0) show("%c", *(buf - size));
    show("\n");    
}


