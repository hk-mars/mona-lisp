

#include "reader.h"

#include "debug.h"

#include "lex.h"

#include "syntax.h"

#include "util.h"

#include "mem.h"

#include "eval.h"


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
load_file(file_info_s *f)
{
    func_s();
  
    f->f = fopen(f->name,"rb");
    if (!f->f) return 0;
  
    f->f_sz = get_file_size(f->f);
    show("file %s opened, size %d bytes.\n", f->name, f->f_sz);
  
    f->buf = (char*)ml_malloc(f->f_sz);
    if (!f->buf) return 0;
  
    f->buf_sz = fread(f->buf, 1, f->f_sz, f->f);
    if (f->buf_sz < f->f_sz) {
	
	ml_free(f->buf);
	f->buf = NULL;
	return 0;
    }
  
    f->buf_e = f->buf + f->buf_sz - 1;
  
    func_e();
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
ml_reader_load_file(reader_s *reader, const char *fname)
{
    func_s();

    if (!reader || !fname) return READER_ERR_NULL;

    memset(reader, 0, sizeof(reader_s));
    
    file_info_s *f = &reader->f;

    f->name = ml_util_strdup(fname);

    /* load codes from file 
     */
    if (!load_file(f)) return READER_ERR;
    ml_util_show_buf(f->buf, f->buf_sz);

    reader->cd.code = f->buf;
    reader->cd.code_sz = f->buf_sz;
    

    /* lexical analyzing
     */
    lex_rt_t lex_rt = ml_lex(&reader->lex, &reader->cd);
    if (lex_rt != LEX_OK) return READER_ERR_LEX;


    /* syntax check
     */
    syntax_rt_t syntax_rt = syntax_check(&reader->lex.forms);
    if (syntax_rt != SYNTAX_OK) return READER_ERR_SYNTAX;
    

    /* eval
     */
    eval_rt_t eval_rt = eval(&reader->lex.forms);
    if (eval_rt != EVAL_OK) return READER_ERR_EVAL;

    
    func_ok();
    return READER_OK;
}



