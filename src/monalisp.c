
#include "monalisp.h"

#include "debug.h"

#include "lexer.h"



#define VER "1.0.0"


int
main (int argc, char **argv)
{
    char code[512];

    
    if (!argv || argc < 2)
    {	    
	debug("No user input found \n");
    }
    
    debug("hello monalisp \n");
    debug("ver: %s \n", ml_get_version());


    lisp_rt_t rt = ml_init();
    if (rt != LISP_OK) return -1;

    
    while (1)
    {
	debug("\n@ ");

	fflush(stdout);
	fgets(code, sizeof(code) -1, stdin);

	if (strlen(code) <= 1) continue;
	if (!strcasecmp(code, "(q)\n") || !strcasecmp(code, "(quit)\n"))
	{
	    debug("\nquit\n");
	    break;
	}

	debug("\n");
	
	code[strlen(code) - 1] = ' ';

	
    }
  
    
    return 0;
}


/**
 * get the version of monalisp 
 */
const char*
ml_get_version(void)
{
    return VER;
}


/**
 * initialize monalisp 
 */
lisp_rt_t
ml_init(void)
{
    func_s();

    lex_rt_t rt = lexer_init();
    if (rt != LEX_OK) return LISP_ERR_LEXER;

    


    func_ok();
    
    return LISP_OK;
}


