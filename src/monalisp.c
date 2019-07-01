
#include "monalisp.h"

#include "debug.h"

#include "lex.h"

#include "reader.h"

#include "stack.h"



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

    
    lisp_rt_t rt = ml_init();
    if (rt != LISP_OK) return -1;

    reader_s reader;
    reader_rt_t reader_rt = ml_reader_load_file(&reader, "test/demo_1.lisp");
    if (reader_rt != READER_OK) return -1;
    
    
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

    stack_rt_t stack_rt = stack_init(1024);
    if (stack_rt != STACK_OK) {
	
	debug_err("err: %d, stack init failed", stack_rt);
	return LISP_ERR_STACK;
    }
	
    lex_rt_t rt = ml_lex_init();
    if (rt != LEX_OK) return LISP_ERR_LEX;


    reader_rt_t reader_rt = ml_reader_init();
    if (reader_rt != READER_OK) return LISP_ERR_READER;


    func_ok();
    
    return LISP_OK;
}


