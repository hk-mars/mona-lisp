

#include "reader.h"

#include "debug.h"

#include "lex.h"

#include "syntax.h"

#include "util.h"

#include "mem.h"

#include "eval.h"


/**
 * Introduction of Common Lisp reader, see:
 * http://www.lispworks.com/documentation/HyperSpec/Body/23_.htm
 * Copyright 1996-2005, LispWorks Ltd. All Rights Reserved
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
    if (!f->f) {
	debug_err("cannot open file: %s \n", f->name);
	return 0;
    }
    
    f->f_sz = get_file_size(f->f);
    show("file %s opened, size %d bytes.\n", f->name, f->f_sz);
  
    f->buf = (char*)ml_malloc(f->f_sz);
    if (!f->buf) return 0;
  
    f->buf_sz = fread(f->buf, 1, f->f_sz, f->f);
    if (f->buf_sz < f->f_sz) {

	debug_err("error when reading file: %s \n", f->name);
	
	//ml_free(f->buf);
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
    eval_value_s *result = ml_malloc(sizeof(eval_value_s));
    if (!result) return READER_ERR;
    eval_rt_t eval_rt = eval(&reader->lex.forms, result);
    if (eval_rt != EVAL_OK) return READER_ERR_EVAL;

    
    func_ok();
    return READER_OK;
}



