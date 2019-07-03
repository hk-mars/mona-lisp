
#include "lex.h"

#include "debug.h"

#include "chars.h"

#include "error.h"

#include "util.h"

#include "eval.h"

#include "mem.h"

#include "config.h"

#include "stack.h"


#define next_code(code, code_sz) \
    (code)++;			 \
    (code_sz)--;


typedef bool (*identify_token_f) (const char **code, size_t *code_sz);



/* When a double-quote is encountered, characters are read from the input stream
 * and accumulated until another double-quote is encountered. If a single escape
 * character is seen, the single escape character is discarded, the next character 
 * is accumulated, and accumulation continues.
 */
static bool
read_string(const char **code, size_t *code_sz)
{
    bool single_escape_flag;
    
    func_s();

    if (!check_char(**code, '\"')) return false;

    single_escape_flag = false;
    while (*code_sz > 0) {

	next_code(*code, *code_sz);
	
	debug("0x%02x %c \n", **code, **code);

	if (is_escape_char(**code)) {
	    
	    single_escape_flag = true;
	    continue;
	}
	
	if (!single_escape_flag && check_char(**code, '\"')) {
	    
	    next_code(*code, *code_sz);

	    func_ok();
	    return true;
	}

	if (single_escape_flag) single_escape_flag = false;
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}


/* A single-quote introduces an expression to be "quoted". Single-quote followed 
 * by an expression exp is treated by the Lisp reader as an abbreviation for and is 
 * parsed identically to the expression (quote exp) or 'exp.
 */
static bool
read_expression_with_single_quote(const char **code, size_t *code_sz)
{
    func_s();

    if (!check_char(**code, '\'')) return false;
    next_code(*code, *code_sz);

    /* TODO: reading expression 
     */
    while (*code_sz > 0) {

	debug("0x%02x %c \n", **code, **code);
	
	if (check_char(**code, LINEFEED)) {
	    
	    next_code(*code, *code_sz);

	    func_ok();
	    return true;
	}

	next_code(*code, *code_sz);
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}


/**
 * Semicolon is used to write comments.     
 * The semicolon and all characters up to and including the next newline are ignored. 
 * Thus a comment can be put at the end of any line without affecting the reader.
 */
static bool
read_comments(const char **code, size_t *code_sz)
{
    func_s();
    
    if (!check_char(**code, ';')) return false;
    next_code(*code, *code_sz);

    while (*code_sz > 0) {

	if (check_char(**code, NEWLINE) || check_char(**code, LINEFEED)) {
	    
	    next_code(*code, *code_sz);

	    func_ok();
	    return true;
	}

	next_code(*code, *code_sz);
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}



static bool
like_func(const char *code, size_t code_sz)
{
    if (is_sign(*code)) return true;

    if (check_char(*code, '*')) return true;

    if (check_char(*code, '/')) return true;

    
    return false;
}


static bool
like_num_token(const char *code, size_t code_sz)
{
    if (is_sign(*code)) return true;

    if (is_digit(*code)) return true;

    if ((*code == 'e' || *code == 'E') && (code_sz-1) > 0) {

	if (is_sign(*(code+1))) return true;

	if (is_digit(*(code+1))) return true;
    }

    return false;
}


static int
identify_digits(const char **code, size_t *code_sz)
{
    int k = 0;

    func_s();
    char *c = (char*)*code;
    
    while (*code_sz > 0) {

	if (!check_char_as_func(**code, is_digit)) {

	    debug("\n");
	    if (k > 0) {
		debug("digits len: %d \n", k);

		unsigned int x = ml_util_arr2int(c, k);
		debug("%d \n", x);
	    }
	    else {

		ml_err_signal(ML_ERR_ILLEGAL_CHAR);
	    }
	    
	    return k;
	}

	debug("%c ", **code);
	
	k++;
	next_code(*code, *code_sz);
    }

    return k;
}


static bool
is_number_token(const char **code, size_t *code_sz)
{
    if (is_sign(**code)) {

	next_code(*code, *code_sz);
    }

    int k = identify_digits(code, code_sz);
    if (k > 0) {

	func_ok();
	return true;
    }
	

    return false;
}


static bool
is_num_operator_func(const char **code, size_t *code_sz)
{
    if (!check_char(**code, '+') && !check_char(**code, '-') &&
	!check_char(**code, '*') && !check_char(**code, '/')) {

	return false;
    }
    
    next_code(*code, *code_sz);

    if (*code_sz == 0) goto DONE;
    

    if (check_char(**code, SPACE)) {
	
	next_code(*code, *code_sz);
	goto DONE;
    }
    
    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	goto DONE;
    }
     
    return false;
    
  DONE:
    
    func_ok();
    return true;
}


static bool
identify_token_as_func(const char **code, size_t *code_sz, identify_token_f f)
{
    bool found = false;
    
    code_s mycode = { *code, *code_sz };
    
    stack_push(&mycode, sizeof(code_s));
   
    if (f(code, code_sz)) {
	
	found = true;
    }
	
    stack_pop(&mycode);

    if (!found) {
	
	*code = mycode.code;
	*code_sz = mycode.code_sz;
	debug("0x%02x %c \n", *mycode.code, *mycode.code);
    }

    return found;
}


/** 
 * The function read is called recursively to read successive objects until a right
 * parenthesis is found to be next in the code. A list of the objects read is returned.
 */
       
static code_s
read_list(const char *code, size_t code_sz, values_s *v, lex_s *lex)
{
    bool found;
    
    func_s();
    
    while (code_sz > 0) {

	debug("0x%02x %c \n", *code, *code);
	
	if (*code == ')') {

	    next_code(code, code_sz);
	    code_s cd = { code, code_sz };
	    return cd;
	}

	if (like_func(code, code_sz)) {

	    found = identify_token_as_func(&code, &code_sz, is_num_operator_func);
	    if (found) continue;
	}
	
	
	if (like_num_token(code, code_sz)) {

	    debug("like number token \n");

	    found = identify_token_as_func(&code, &code_sz, is_number_token);
	    if (found) continue;
	}
	

	if (*code == '(') {
	
	    code_s cd = read_list(++code, --code_sz, v, lex);
	    if (!cd.code) return cd;

	    code = cd.code;
	    code_sz = cd.code_sz;
	    
	    continue;
	}

	next_code(code, code_sz);
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);

    code_s cd = { NULL, 0 };
    return cd;
}


static bool
read_macro(code_s *cd, lex_s *lex)
{
    bool found = false;
    
    values_s *v = NULL;

    if (!cd || !cd->code || !cd->code_sz) return NULL;
    
    func_s();

    char c = *cd->code;
    debug("0x%02x %c \n", c, c);

    switch (c) {

    case '(':

	/* The left-parenthesis character initiates reading of a list. 
	 */
	
	next_code(cd->code, cd->code_sz);

	v = (values_s*)ml_malloc(sizeof(values_s));
	if (!v) return false;
	
	code_s icode = read_list(cd->code, cd->code_sz, v, lex);
	if (!icode.code) return false;

	cd->code = icode.code;
	cd->code_sz = icode.code_sz;

	found = true;
	break;

    case '\'': 

	/* A single-quote introduces an expression to be "quoted" 
	 */
	
	found = read_expression_with_single_quote(&cd->code, &cd->code_sz);
	break;
	    
    case ';':

	/* A semicolon introduces characters to be ignored, such as comments. 
	 */
	
	found = read_comments(&cd->code, &cd->code_sz);
	break;

    case '\"':

        /* The double-quote is used to begin and end a string. 
	 */

	found = read_string(&cd->code, &cd->code_sz);
	break;

    case '`':

        /* The backquote introduces a template of a data structure to be built. 
	 */

	/* TODO: backquote syntax */
	
	break;


    case '#':

	/* Sharpsign is a non-terminating dispatching macro character. It uses 
	 * a character to select a function to run as a reader macro function.
	 */

	/* TODO: sharpsign syntax */
	
	break;

    default:
	break;

    }

    
    func_e();
    return found;
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
    
    if (cd->code_sz <= 0) return LEX_ERR_ARGU;

    ml_util_show_buf((char*)cd->code, cd->code_sz);
    

    code = cd->code;
    code_sz = cd->code_sz;
    
    
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
	    
	    next_code(code, code_sz);
	    continue;
	}

	
	/* step 4: if x is a macro character, then execute the function associated with that
	 * character. The function may return zero values or one value.
	 */
	if (is_macro_char(x)) {

	    debug("macro char \n");

	    cd->code = code;
	    cd->code_sz = code_sz;	    
        
	    if (!read_macro(cd, lex)) return LEX_ERR;

	    debug("remain code_sz:%d \n",cd->code_sz);
	    if (cd->code_sz == 0) break;

	    code = cd->code;
	    code_sz = cd->code_sz;   
	    continue;
	}


	/* step 5: if x is a single escape character, then read the next character and call it y.
	 */
	if (is_escape_char(x)) {

	    debug("escape char \n");

	    next_code(code, code_sz);
	    y = *code;

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


	  
	  next_code(code, code_sz);
	
    } /* end of while */

    
    
    func_ok();
    
    return LEX_OK;
}
