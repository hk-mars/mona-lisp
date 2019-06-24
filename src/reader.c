

#include "reader.h"

#include "debug.h"

#include "lex.h"

#include "util.h"



/**
 * The purpose of the Lisp reader is to accept characters, interpret them as the printed 
 * representation of a Lisp object, and construct and return such an object.
 *
 * The reader cannot accept everything that the printer produces; for example, the printed 
 * representations of compiled code objects cannot be read in.
 * The reader is also parameterized in such a way that it can be used as a lexical analyzer for
 * a more general user-written parser.
 */


/**
 * The reader is organized as a recursive-descent parser. Broadly speaking, the reader operates 
 * by reading a character from the input stream and treating it in one of three ways:
 *
 * 1. Whitespace characters serve as separators but are otherwise ignored.
 * 2. Constituent and escape characters are accumulated to make a token, which is then 
 *    interpreted as a number or symbol.
 * 3. Macro characters trigger the invocation of functions (possibly user-supplied) that can 
 *    perform arbitrary parsing actions, including recursive invocation of the reader.
 */



typedef struct s_file_info
{
    const char *f_name;
    FILE *f;
    int f_sz;
    char *f_buf;
    int buf_sz;
    char *buf_e;
  
} file_info;


static void
show_buf(char *buf, int size, file_info *fi)
{
    if (fi) {
       if (buf + size - 1 > fi->buf_e) return;
    }
  
    debug("%d bytes:\n", size);
    buf += (size - 1);
    while(--size >= 0) debug("%c", *(buf - size));
    debug("\n");
}


static char* 
dump_trail(char *s, char *e)
{
  while(s <= e) {
    if (*e != ' ') break;
    e--;
  }
  
  if (s > e) return NULL;
  
  *(e+1) = '\0';

  return e;
}


static long 
get_file_size(FILE *f) 
{
  long cur;
  long size; 
  
  cur = ftell(f);
  fseek(f, 0L, SEEK_END); 
  size = ftell(f); 
  fseek(f, cur, SEEK_SET);
  
  return size; 
}


static int 
load_file(file_info *fi)
{
    fs();
  
    fi->f = fopen(fi->f_name,"rb");
    if (!fi->f) return 0;
  
    fi->f_sz = get_file_size(fi->f);
    debug("file %s opened, size %d bytes.\n", fi->f_name, fi->f_sz);
  
    fi->f_buf = (char*)malloc(fi->f_sz);
    if (!fi->f_buf) return 0;
  
    fi->buf_sz = fread(fi->f_buf, 1, fi->f_sz, fi->f);
    if (fi->buf_sz < fi->f_sz) {
	free(fi->f_buf);
	fi->f_buf = NULL;
	return 0;
    }
  
    fi->buf_e = fi->f_buf + fi->buf_sz - 1;
  
    //filter_bnf_buf(fi);
  
    fe();
    return 1;
}


reader_rt_t
ml_reader_init(void)
{
    func_s();

    
    

    func_ok();
    
    return READER_OK;
}


reader_rt_t
ml_reader_load_file(const char *fname)
{
    func_s();

    if (!fname) return READER_ERR_NULL;
    
    file_info fi;

    memset(&fi, 0, sizeof(fi));

    fi.f_name = ml_strdup(fname);

    if (!load_file(&fi)) return READER_ERR;

    

    

    func_ok();

    return READER_OK;
}



