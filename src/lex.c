
#include "lex.h"

#include "debug.h"

#include "chars.h"

#include "error.h"

#include "util.h"

#include "eval.h"

#include "mem.h"

#include "config.h"

#include "stack.h"


static size_t m_offset = 0;

static const char *m_c = NULL;
static size_t m_sz = 0;

static const char *m_stack_code = NULL;
static size_t m_stack_code_sz = 0;


#define move_code(code, code_sz, offset)		\
    (code) += offset;					\
    (code_sz) -= offset;


#define next_code(code, code_sz) \
    (code)++;			 \
    (code_sz)--;

#define backup_context(code, code_sz) \
    m_stack_code = code; \
    m_stack_code_sz = code_sz; \
    m_c = code; \
    m_sz = code_sz


#define resume_context(code, code_sz) \
    code = m_stack_code; \
    code_sz = m_stack_code_sz;



static bool
is_like_num_token(const char *code, size_t code_sz)
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
identify_digits(void)
{
    unsigned int n = 0;
    int k = 0;

    const char *c = m_c;

    
    while (m_sz > 0) {

	if (!check_char_as_func(*m_c, is_digit)) {

	    debug("\n");
	    if (k > 0) {
		debug("digits len: %d \n", k);

		unsigned int x = ml_util_arr2int(c, k);
		debug("%d \n", x);
	    }
	    
	    return k;
	}

	debug("%c ", *m_c);
	
	k++;
	next_code(m_c, m_sz);
    }

    return k;
}


static bool
identify_number(lex_s *lex)
{
    func_s();
    
    while (m_sz > 0) {

	if (is_sign(*m_c)) {

	    next_code(m_c, m_sz);
	}

	int k = identify_digits();
	if (k > 0) {
	    
	    return true;
	}

	next_code(m_c, m_sz);
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);

    return false;
}


static bool
is_plus_func(const char **code, size_t *code_sz)
{
    if (!check_char(**code, '+')) return false;
    next_code(*code, *code_sz);

    if (*code_sz == 0) return true;

    if (check_char(**code, SPACE)) {

	next_code(*code, *code_sz);
	return true;
    }

    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	return true;
    }

    return false;
}




/** 
 * The function read is called recursively to read successive objects until a right parenthesis
 * is found to be next in the code. A list of the objects read is returned.
 */
       
static code_s
read_list(const char *code, size_t code_sz, values_s *v, lex_s *lex)
{
    func_s();
    
    while (code_sz > 0) {

	debug("0x%02x %c \n", *code, *code);
	
	if (*code == ')') {

	    code_s cd = { code, code_sz };
	    return cd;
	}

	//stack_data_t sdata = { code, 1 };
	//stack_push(&sdata, 2);
	
	if (is_plus_func(&code, &code_sz)) {

	    debug("plus function \n");

	    //stack_pop(&sdata);
	    
	    //next_code(code, code_sz);
	    continue;
	}
	
	//stack_pop(&sdata);

	
	if (is_like_num_token(code, code_sz)) {

	    debug("like number token \n");

	    backup_context(code, code_sz);
	    if (identify_number(lex)) {
		
		code = m_c++;
		code_sz = m_sz--;

		continue;
		
	    }
	    resume_context(code, code_sz);

	    debug("not number \n");
	}

	if (*code == '(') {
	
	    code_s cd = read_list(++code, --code_sz, v, lex);
	    if (!cd.code) return cd;

	    code = cd.code + 1;
	    code_sz = cd.code_sz - 1;
	    
	    continue;
	}

	next_code(code, code_sz);
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);

    code_s cd = { NULL, 0 };
    return cd;
}


static values_s*
read_macro(code_s *cd, lex_s *lex)
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
	
        code_s icode = read_list(cd->code, cd->code_sz, v, lex);
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
	    values_s *v = read_macro(cd, lex);
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
