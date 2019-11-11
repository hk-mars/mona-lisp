

#include "util.h"

#include "debug.h"

#include "mem.h"

#include "error.h"


char*
ml_util_buf2str(const char *buf, size_t len)
{
    if (!buf) return NULL;
    if (len <= 0) return NULL;
    
    char *s = ml_malloc(len+1);
    if (!s) return NULL;

    memcpy(s, buf, len);
    s[len] = 0;

    return s;
}


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


char*
ml_util_str_clone(const char *str, void* (f_malloc) (size_t))
{
    if (!str) return NULL;

    int len = strlen(str) + 1;

    if (!f_malloc) {

	f_malloc = ml_malloc;
    }
 
    char *s = f_malloc(len);
    if (!s) return NULL;

    memcpy(s, str, len);

    return s;
}



bool
ml_util_strbufcmp(const char *str, char *buf, size_t buf_len)
{
    if (buf_len <= 0) return false;
    
    if ((size_t)strlen(str) > buf_len) return false;

    while (*str) {

	//debug("%c \n", *str);
	
	if (tolower(*str) != tolower(*buf)) return false;
	str++;
	buf++;
    }

    return true;
}


void
ml_util_show_buf(char *buf, size_t size)
{
    show("%s, %d bytes:\n", __func__, size);
    buf += (size - 1);
    while(--size >= 0) show("%c", *(buf - size));
    show("\n");    
}


unsigned int
ml_util_arr2int(char *arr, size_t len)
{
    unsigned int x;

    char buf[64];

    if (len+1 >= (int)sizeof(buf)) {
	
	debug_err("err: buffer overflow \n");
	ml_err_signal(ML_ERR_BUF_OVERFLOW);
	return 0;
    }
	
    memcpy(buf, arr, len);
    buf[len] = 0;

    x = atoi(buf);

    return x;
}


/* *read-base* value may be any integer from 2 to 36 (inclusive) and 
 * is normally 10 (decimal radix)
 */
static unsigned char m_read_base = 10;

void
ml_util_set_read_base(unsigned char read_base)
{
    m_read_base = read_base;
}


unsigned char
ml_util_get_read_base(void)
{
    return m_read_base;
}


char*
ml_util_clone_str_as_upper(char *str)
{
    if (!str) return NULL;
    
    int len = strlen(str);

    char *s = (char*)ml_malloc(len+1);

    char *ss = s;
    char *e = s + len;
    while (ss <= e) {

	*ss++ = toupper(*str++);
    }

    *ss = 0;

    return s;
}


bool
ml_util_fwrite(char *name, char *line)
{
    FILE *f;
    int rt;
  
    fs();

    f = fopen(name, "a");
    if (!f) return false;

    rt = fputc('\t', f);
    if (!rt) return false;
    
    rt = fputc('\"', f);
    if (!rt) return false;

    
    rt = fwrite(line, strlen(line), 1, f);
    if (!rt)  {
	debug("write [%s] failed. \n", line);
	return false;
    }

    rt = fputc('\"', f);
    if (!rt) return false;

    rt = fputc(',', f);
    if (!rt) return false;    
    
    rt = fputc('\n', f);
    if (!rt) return false;

    fclose(f);

    out(ok, true);
}


bool
ml_util_fwrite_buf(char *name, char *buf, size_t len)
{
    char *s = ml_util_buf2str(buf, len);
    if (!s) return false;
    
    bool rt = ml_util_fwrite(name, s);

    ml_free(s);
    return rt;
}













