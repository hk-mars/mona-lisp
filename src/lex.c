
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


typedef bool (*identify_f) (const char **code, size_t *code_sz, void *context);



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
read_symbol(const char **code, size_t *code_sz)
{
    func_s();

    while (*code_sz > 0) {

	debug("0x%02x %c \n", **code, **code);
	
	if (check_char(**code, SPACE) || check_char(**code, ')')) {

	    if (check_char(**code, SPACE)) {
		next_code(*code, *code_sz);
	    }
	    
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


/* A token is a potential number if it satisfies all of the following requirements:
 * 
 * 1. The token consists entirely of digits, signs, ratio markers, decimal points
 *    (.), extension characters (^ or _), and number markers. 
 * 2. The token contains at least one digit(letters may be considered to be digits).
 * 3. The token begins with a digit, sign, decimal point, or extension character, 
 *    but not a package marker.
 * 4. The token does not end with a sign.
 * 
 */
static bool
like_num_token(const char *code, size_t code_sz)
{
    if (is_sign(*code)) goto FIND_ONE_DIGIT;

    if (is_digit(*code)) goto CHECK_MORE;
    
    if (check_char(*code, '.')) goto FIND_ONE_DIGIT;

    if (check_char(*code, '^')) goto FIND_ONE_DIGIT;
    if (check_char(*code, '_')) goto FIND_ONE_DIGIT;

    
  FIND_ONE_DIGIT:

    next_code(code, code_sz);
    while (code_sz > 0) {

	if (check_char(*code, SPACE) || check_char(*code, ')')) return false;

	if (is_digit(*code)) goto CHECK_MORE;

        next_code(code, code_sz);
    }

    return false;

  CHECK_MORE:

    next_code(code, code_sz);
    while (code_sz > 0) {

	if (check_char(*code, SPACE) || check_char(*code, ')')) {

	    /* The token does not end with a sign. */
	    if (is_sign(*(code-1))) return false;

	    return true;
	}

	if (is_digit(*code) || is_sign(*code) ||
	    check_char(*code, '^') || check_char(*code, '_') ||
	    check_char(*code, '.') || is_ratio_marker(*code) ||
	    is_exponent_maker(*code)) {

	    next_code(code, code_sz);

	    continue;
	}

	return false;
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

	if (!is_digit(**code)) {

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
identify_number_token(const char **code, size_t *code_sz, void *context)
{
    token_s *t = (token_s*)context;
    
    bool is_minus = false;

    if (is_sign(**code)) {

	is_minus = (**code == '-');
	next_code(*code, *code_sz);
    }
   
    int k = 0;

    char *c = (char*)*code;
    
    while (*code_sz > 0) {

	if (!is_digit(**code)) {

	    debug("\n");
	    if (k > 0) {
		debug("digits len: %d \n", k);

		unsigned int x = ml_util_arr2int(c, k);
		debug("%d \n", x);

		/* [sign] {digit}+ decimal-point
		 */
		if (check_char(**code, '.')) {
		    next_code(*code, *code_sz);

		    if (is_digit(**code)) {

			/* float */
			t->type = TOKEN_NUM_FLOAT;
		    }
		    else if (is_exponent_maker(**code)) {

			/* float */
			t->type = TOKEN_NUM_FLOAT;
		    }
		    else {

			/* integer */
			t->type = TOKEN_NUM_INT;
		    }
		}
		else if (check_char(**code, '/')) {  
		    next_code(*code, *code_sz);

		    if (is_digit(**code)) {

			/* ratio */
			t->type = TOKEN_NUM_RATIO;
			
		    }
		    else {
			
			return false;
		    }
			
		}		
		else {

		    /* integer */
		    t->type = TOKEN_NUM_INT;
		}
		
		break;
		
	    }
	    else {

		/* float */
		t->type = TOKEN_NUM_FLOAT;
	    }
	    
	    break;
	}

	debug("%c ", **code);
	
	k++;
	next_code(*code, *code_sz);
    }    
    
    if (t->type != TOKEN_UNKOWN) {

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
identify_code_as_func(const char **code, size_t *code_sz, identify_f f, void *context)
{
    bool found = false;
    
    code_s mycode = { *code, *code_sz };
    
    stack_push(&mycode, sizeof(code_s));

    if (f(code, code_sz, context)) {
	
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
read_list(const char *code, size_t code_sz, lex_s *lex, form_s *form)
{
    bool found;
    
    func_s();
    
    while (code_sz > 0) {

	debug("0x%02x %c \n", *code, *code);

	if (is_whitespace_char(*code)) {

	    next_code(code, code_sz);
	    continue;
	}

	if (check_char(*code, SPACE)) {

	    /* next token */
	    next_code(code, code_sz);
	    continue;
	}
	
	if (*code == ')') {

	    next_code(code, code_sz);
	    code_s cd = { code, code_sz };
	    return cd;
	}

	if (like_func(code, code_sz)) {

	    found = identify_code_as_func(&code, &code_sz, is_num_operator_func, NULL);
	    if (found) {

		if (form_is_unkown(form)) {
		    form_set_type(form, COMPOUND_FUNCTION_FORM);
		}
		
		form_add_front(&lex->forms, form);
		continue;
	    }
	}

	if (like_num_token(code, code_sz)) {

	    token_s *token = token_make_number(code, code_sz);
	    found = identify_code_as_func(&code, &code_sz, identify_number_token, token);
	    if (found) {

		/* add the number token into the list form */
		list_add_token(form->list, token);
		
		continue;
	    }
	}

	if (*code == '(') {

	    form_s *f = form_create_list();

	    code_s cd = read_list(++code, --code_sz, lex, f);
	    if (!cd.code) return cd;

	    code = cd.code;
	    code_sz = cd.code_sz;
	}
	else {

	    if (form_is_unkown(form)) {
		form_set_type(form, SYMBOL_FORM);
	    }
	    
	    found = read_symbol(&code, &code_sz);
	}
	
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);

    code_s cd = { NULL, 0 };
    return cd;
}


static bool
read_macro(code_s *cd, lex_s *lex)
{
    bool found = false;

    if (!cd || !cd->code || !cd->code_sz) return false;
    
    func_s();

    char c = *cd->code;
    debug("0x%02x %c \n", c, c);

    switch (c) {

    case '(':

	/* The left-parenthesis character initiates reading of a list. 
	 */
	
	next_code(cd->code, cd->code_sz);

	form_s *form = form_create_list();
	if (!form) return false;
	
	code_s icode = read_list(cd->code, cd->code_sz, lex, form);
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
