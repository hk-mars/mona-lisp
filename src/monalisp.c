
#include "monalisp.h"

#include "debug.h"

#include "lex.h"

#include "syntax.h"

#include "parser.h"

#include "reader.h"

#include "stack.h"

#include "variable.h"

#include "function.h"

#include "macro.h"

#include "gc.h"

#include "mem.h"

#include "system.h"



#define VER "1.0.0"


int
main (int argc, char **argv)
{
#if 0
    char code[512];
#endif
    
    if (!argv || argc < 2) {	    
	;
    }
        
    show("Hello Monalisp \n");
    show("Ver: %s \n\n", ml_get_version());

    
#if GC_SELF_CHECK_ENABLE
    if (!gc_debug()) return -1;
#endif
    
    
    lisp_rt_t rt = ml_init();
    if (rt != LISP_OK) {

	sys_set_status(SYS_STATUS_ERROR);
	return -1;
    }
    
    sys_set_status(SYS_STATUS_RUNNING);


    reader_s reader;
    reader_rt_t reader_rt;
    gc_s gc;
    
#if 1    
    gc = gc_new();
    if (gc.id < 0) {

	debug_err("create gc object failed \n");
	return LISP_ERR_GC;
    }
    
    reader_rt = ml_reader_load_file(&reader, "demo_func.lisp");
    if (reader_rt != READER_OK) return -1;

    gc_show();
    gc_free();
    mm_show();
#endif

    
#if 1   
    gc = gc_new();
    if (gc.id < 0) {

	debug_err("create gc object failed \n");
	return LISP_ERR_GC;
    }
    
    reader_rt = ml_reader_load_file(&reader, "demo.lisp");
    if (reader_rt != READER_OK) return -1;

    gc_show();
    gc_free();
    mm_show();
#endif    
    

#if 0
    gc = gc_new();
    if (gc.id < 0) {

	debug_err("create gc object failed \n");
	return LISP_ERR_GC;
    }
    
    reader_rt = ml_reader_load_file(&reader, "init.lisp");
    if (reader_rt != READER_OK) return -1;

    gc_show();
    gc_free();
    mm_show();
#endif
    
    
 #if 0
    
    while (1) {
	show("\n@ ");

	fflush(stdout);
	fgets(code, sizeof(code) -1, stdin);

	if (strlen(code) <= 1) continue;
	if (!strcasecmp(code, "(q)\n") || !strcasecmp(code, "(quit)\n")) {
	    show("\nquit\n");
	    break;
	}

	show("\n");

	/* delete '\n' at the end of the string */
	code[strlen(code) - 1] = ' ';

	debug("%s \n", code);

	
	memset(&lex_obj, 0, sizeof(lex));
	lex_rt_t lex_rt = ml_lex(&lex_obj, code);
	if (lex_rt != LEX_OK) continue;       	
	
    }
 #endif

    
    
    return 0;
}


/**
 * Get the version of monalisp 
 */
const char*
ml_get_version(void)
{
    return VER;
}


/**
 * Initialize monalisp 
 */
lisp_rt_t
ml_init(void)
{
    func_s();

    //mm_show();
    //debug_suspend();

    sys_set_status(SYS_STATUS_INIT);
    
    stack_rt_t stack_rt = stack_init(1024);
    if (stack_rt != STACK_OK) {
	
	debug_err("err: %d, stack init failed \n", stack_rt);
	return LISP_ERR_STACK;
    }


    var_rt_t var_rt = var_init();
    if (var_rt != VAR_OK) {

	debug_err("err: %d, var_init failed \n", var_rt);
	return LISP_ERR_VAR;
    }


    func_rt_t func_rt = func_init();
    if (func_rt != FUNC_OK) {

	debug_err("err: %d, func_init failed \n", func_rt);
	return LISP_ERR_FUNC;
    } 
    
    macro_rt_t macro_rt = macro_init();
    if (macro_rt != MACRO_OK) {

	debug_err("err: %d, macro_init failed \n", macro_rt);
	return LISP_ERR_MACRO;
    } 

    
    lex_rt_t lex_rt = ml_lex_init();
    if (lex_rt != LEX_OK) return LISP_ERR_LEX;


    syntax_rt_t syntax_rt = syntax_init();
    if (syntax_rt != SYNTAX_OK) return LISP_ERR_SYNTAX;

    parser_rt_t parser_rt = parser_init();
    if (parser_rt != PARSER_OK) return LISP_ERR_PARSER;
    //TODO: reduce the use of memory while loading the syntax objects from BNF file(pratical).
    //mm_show();
    //debug_suspend();
    
    reader_rt_t reader_rt = ml_reader_init();
    if (reader_rt != READER_OK) return LISP_ERR_READER;

    //mm_show();
    //debug_suspend();
    
    func_ok();

    return LISP_OK;
}


