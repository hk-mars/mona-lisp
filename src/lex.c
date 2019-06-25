
#include "lex.h"

#include "debug.h"

#include "chars.h"

#include "error.h"

#include "util.h"



#define is_illegal_char(x) 0

#define is_whitespace_char(x) ( \
        (x) == BACKSPACE ||    \
	(x) == TAB ||	       \
	(x) == NEWLINE ||      \
	(x) == LINEFEED ||     \
	(x) == RETURN_CR )
	
#define is_macro_char(x) 0

#define is_escape_char(x) 0

#define is_multiple_escape_char(x) 0









/**
 * Initialize lexer 
 */
lex_rt_t
ml_lex_init(void)
{
    func_s();

    

    func_ok();
    
    return LEX_OK;
}


/**
 * Lexical analyzing for the code
 */
lex_rt_t
ml_lex(lex_s *lex, const char *code, size_t code_sz)
{
    func_s();

    if (!lex || !code) return LEX_ERR_NULL;

    if (code_sz <= 0) return LEX_ERR_ARGU;


    ml_util_show_buf((char*)code, code_sz);
    
    
    /* identify the tokens
     */
    char x, y;
    while (1) {

	/* step-1: if at end of code, break.
	 */
	if (code_sz == 0) break;

	x = *code; /* read one character */

	show("0x%x \n", x);
	
	/* step-2: if x is an illegal character, signal an error. 
	 */
	if (is_illegal_char(x)) {
	
	    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
	    return LEX_ERR;
	}

	
	/* step-3: if x is a whitespace character, then discard it and go back to step-1. 
	 */
	if (is_whitespace_char(x)) {

	    code++;
	    code_sz--;
	    continue;
	}

	
	/* step-4: if x is a macro character, then execute the function associated with 
	 * that character. The function may return zero values or one value.
	 */
	if (is_macro_char(x)) {

	}


	/* step-5: 
	 * if x is a single escape character, then read the next character and call it y.
	 */
	if (is_escape_char(x)) {

	    y = *++code;
	    code_sz--;

	    if (code_sz == 0) {
		
		ml_err_signal(ML_ERR_ILLEGAL_CHAR);
		return LEX_ERR;
	    }

	    goto STEP_8;
	}


	/* step-6: if x is a multiple escape character, then begin a token and go to step-9.
	 */
	if (is_multiple_escape_char(x)) {

	    goto STEP_9;
	}


	/* step-7: 
	 */
	

	STEP_8:
	;


	STEP_9:
	;

	
	code++;
	code_sz--;
	
    } /* end of while */

    
    
    func_ok();
    
    return LEX_OK;
}
