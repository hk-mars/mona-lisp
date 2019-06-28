
#include "lex.h"

#include "debug.h"

#include "chars.h"

#include "error.h"

#include "util.h"

#include "eval.h"

#include "mem.h"



/** 
 * The function read is called recursively to read successive objects until a right parenthesis
 * is found to be next in the code. A list of the objects read is returned.
 */
       
static code_s
read_list(const char *code, size_t code_sz, values_s *v)
{
    debug("0x%02x %c \n", *code, *code);
    
    if (*code == ')') {

	func_ok();

	code_s cd = { code, code_sz };
	return cd;
    }

    if (code_sz == 0) {

	ml_err_signal(ML_ERR_ILLEGAL_CHAR);

	code_s cd = { NULL, 0 };
	return cd;
    }

    return read_list(++code, --code_sz, v);
}


static values_s*
read_macro(code_s *cd)
{
    values_s *v = NULL;

    if (!cd || !cd->code || !cd->code_sz) return NULL;
    
    func_s();

    char c = *cd->code;
    debug("0x%02x %c \n", c, c);
    
    /* compound form: a non-empty list which is a form: a special form, a lambda form, 
     * a macro form, or a function form.
     */


    /* The left-parenthesis character initiates reading of a list. The function read is called
       recursively to read successive objects until a right parenthesis is found to be next in
       the code. A list of the objects read is returned. */
      
    if (c == '(') {

	cd->code++;
	cd->code_sz--;

        v = (values_s*)ml_malloc(sizeof(values_s));
	if (!v) return NULL;
	
        code_s icode = read_list(cd->code, cd->code_sz, v);
	if (!icode.code) return NULL;

	cd->code = icode.code;
	cd->code_sz = icode.code_sz;
	
    }
    else {
	
    }

    
    func_e();
    return v;
}



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
ml_lex(lex_s *lex, code_s *cd)
{
    const char *code;
    size_t code_sz;
    
    func_s();

    if (!lex || !cd || !cd->code) return LEX_ERR_NULL;

    code = cd->code;
    code_sz = cd->code_sz;
    
    if (code_sz <= 0) return LEX_ERR_ARGU;


    ml_util_show_buf((char*)code, code_sz);
    
    
    
    /* identify the tokens
     *
     * rules:
     * 1. A single escape character never stands for itself but always serves to cause the 
     *    following character to be treated as a simple alphabetic character. A single escape 
     *    character can be included in a token only if preceded by another single escape 
     *    character.
     * 2. A multiple escape character also never stands for itself. The characters between a 
     *    pair of multiple escape characters are all treated as simple alphabetic characters, 
     *    except that single escape and multiple escape characters must nevertheless be preceded
     *    by a single escape character to be included.
     */
    char x, y, z;
    while (1) {

	/* step 1: if at end of code, break.
	 */
	if (code_sz == 0) break;

	x = *code; /* read one character */

	debug("0x%02x %c \n", x, x);
	
	/* step 2: if x is an illegal character, signal an error. 
	 */
	if (is_illegal_char(x)) {

	    show_func_line();
	    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
	    return LEX_ERR;
	}

	
	/* step 3: if x is a whitespace character, then discard it and go back to step 1. 
	 */
	if (is_whitespace_char(x)) {

	    debug("whitespace char \n");
	    
	    code++;
	    code_sz--;
	    continue;
	}

	
	/* step 4: if x is a macro character, then execute the function associated with that
	 * character. The function may return zero values or one value.
	 */
	if (is_macro_char(x)) {

	    debug("macro char \n");

	    cd->code = code;
	    cd->code_sz = code_sz;	    
	    values_s *v = read_macro(cd);
	    if (!v) return LEX_ERR;
	    
	    if (cd->code_sz == 0) break;
	    
	    code = ++cd->code;
	    code_sz = --cd->code_sz;
	    continue;
	}


	/* step 5: if x is a single escape character, then read the next character and call it y.
	 */
	if (is_escape_char(x)) {

	    debug("escape char \n");
	    
	    y = *++code;
	    code_sz--;

	    if (code_sz == 0) {

		debug_err("end of code \n");
		ml_err_signal(ML_ERR_ILLEGAL_CHAR);
		return LEX_ERR;
	    }

	    goto STEP_8;
	}


	/* step 6: if x is a multiple escape character, then begin a token and go to step 9.
	 */
	if (is_multiple_escape_char(x)) {

	    show("multiple escape char \n");
	    goto STEP_9;
	}


	/* step 7: if x is a constituent character, then it begins an extended token.
	 * After the entire token is read in, it will be interpreted either as representing 
	 * a Lisp object such as a symbol or number, or as being of illegal syntax.
	 */
	if (is_constituent_char(x)) {

	    debug("constituent char \n");
	    
	    /* use x to begin a token, and go on to step 8.
	     */
	    goto STEP_8;
	}
	

	/* step 8: at this point a token is being accumulated, and an even number of multiple 
	 * escape characters have been encountered.
	 * If at end of code, go to step 10, otherwise, read a character (call it y), 
	 * and perform one of the following actions according to its syntactic type:
	 *
	 * 1. If y is a constituent or non-terminating macro, then do the following:
	 *    Append y to the token being built, and repeat step 8.
	 * 2. If y is a single escape character, then read the next character and call it z(but 
	 *    if at end of code, signal an error instead). Ignore the usual syntax of z and
	 *    pretend it is a constituent whose only attribute is alphabetic.
	 *    Append z to the token being built, and repeat step 8.
	 * 3. If y is a multiple escape character, then go to step 9.
	 * 4. If y is an illegal character, signal an error.
	 * 5. If y is a terminating macro character, it terminates the token. First ``unread'' 
	 *    the character y, then go to step 10.
	 * 6. If y is a whitespace character, it terminates the token. First ``unread'' y if 
	 *    appropriate, then go to step 10.
	 */
      STEP_8:
	  ;


	/* step 9: at this point a token is being accumulated, and an odd number of multiple 
	 * escape characters have been encountered.
	 * If at end of file, signal an error. Otherwise, read a character (call it y), and
	 * perform one of the following actions according to its syntactic type:
	 *
	 * 1. If y is a constituent, macro or whitespace character, then ignore the usual syntax 
	 *    of that character and pretend it is a constituent whose attribute is alphabetic.
	 *    Append y to the token being built, and repeat step 9.
	 * 2. If y is a single escape character, then read the next character and call it z(but 
	 *    if at end of file, signal an error instead). Ignore the usual syntax of z and
	 *    pretend it is a constituent whose only attribute is alphabetic.
	 *    Append z to the token being built, and repeat step 9.
	 * 3. If y is a multiple escape character, then go to step 8.
	 * 4. If y is an illegal character, signal an error.
	 */
      STEP_9:
	  ;


	/* step 10: an entire token has been accumulated.
	 * Interpret the token as representing a Lisp object and return that object as the result 
	 * of the read operation, or signal an error if the token is not of legal syntax.
	 */
      STEP_10:
	  ;


	  
	code++;
	code_sz--;
	
    } /* end of while */

    
    
    func_ok();
    
    return LEX_OK;
}
