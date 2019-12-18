
#include "lex.h"

#include "debug.h"

#include "chars.h"

#include "error.h"

#include "util.h"

#include "eval.h"

#include "mem.h"

#include "config.h"

#include "stack.h"

#include "token.h"

#include "variable.h"

#include "macro.h"
#include "function.h"



/**
 * Introduction of Common Lisp syntax, see:
 * http://www.lispworks.com/documentation/HyperSpec/Body/02_.htm
 * Copyright 1996-2005, LispWorks Ltd. All Rights Reserved.
 */


#define next_code(code, code_sz) \
    (code)++;			 \
    (code_sz)--;


#define move_code(code, code_sz, sz_moved)	\
    int i = sz_moved;				\
    while ((code_sz) > 0 && i > 0) {		\
	next_code(code, code_sz);		\
	i--;					\
    }


#define ignore_end_chars(code, code_sz)		\
    while (1) {					\
	if (eq(**code, NEWLINE) ||		\
	    eq(**code, LINEFEED) ||		\
	    **code == SPACE) {			\
	    next_code(*code, *code_sz);		\
	    if (*code_sz <= 0) break;		\
	    continue;				\
	}					\
	break;					\
    }


typedef bool (*identify_f) (char **code, size_t *code_sz, form_s *form);


static code_s
read_list(char *code, size_t code_sz, form_s *form_head, form_s *form);


static bool
read_symbol(char **code, size_t *code_sz, form_s *form);


static bool
read_string(char **code, size_t *code_sz, form_s *form)
{
    token_s token;
    
    bool single_escape_flag;
    
    func_s();

    const char *s;
    
    if (!eq(**code, '\"')) return false;
    s = *code;

    single_escape_flag = false;
    while (*code_sz > 0) {

	next_code(*code, *code_sz);
	
	debug("0x%02x %c \n", **code, **code);

	if (is_escape_char(**code)) {
	    
	    single_escape_flag = true;
	    continue;
	}
	
	if (!single_escape_flag && eq(**code, '\"')) {

	    next_code(*code, *code_sz);

	    memset(&token, 0, sizeof(token_s));
	    token.value.symbol = ml_util_buf2str(s, *code - s);
	    if (token.value.symbol) {

		debug("symbol: %s \n", token.value.symbol);
		token.type = TOKEN_SYMBOL;
	    }
	    
	    if (token.type != TOKEN_UNKOWN) {

		//token_s *t = token_clone(&token);

		/* add the symbol token into the list form */
		list_add_token(form->list, &token);	
	
		func_ok();
		return true;
	    }
	    

	    func_ok();
	    return true;
	}

	if (single_escape_flag) single_escape_flag = false;
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}


static bool
read_quote_expression(char **code, size_t *code_sz, form_s *form)
{
    object_s *obj;
    char *ss = NULL;
    int pair;
	    
    func_s();

    if (!eq(**code, '\'')) return false;
    next_code(*code, *code_sz);

    
    /* syntax:
     *     quote <expression>
     * or  ' <expression>
     * The <expression> is an expression of object type.
     * A textual notation used to notate an object in a source file.
     *
     */

    char *s = *code;
    char *e = s + *code_sz - 1;
    while (s <= e) {

	debug("0x%02x %c \n", *s, *s);

	if (*s == LINEFEED || *s == NEWLINE || *s == SPACE) {

	    s++;
	    continue;
	}
	

	/* list object
	 * TODO: move those codes to syntax.c ?
	 */
	if (*s == '(') {

	    ss = s+1;
	    pair = 1;
	    while (ss <= e) {

		if (*ss == '(') {

		    pair++;
		    ss++;
		    continue;
		}

		if (*ss == ')') {

		    pair--;
		    ss++;
		    if (pair == 0) {

			debug("a list object found \n");			
			goto FOUND;
		    }
		    continue;
		}

		ss++;
	    }	    
	}

	
	/* symbol, it could be the codes of a character, a number, a string or any object.
	 */
	ss = s+1;
	while (ss <= e) {

	    debug("0x%02x %c \n", *ss, *ss);

	    if (is_whitespace_char(*ss)) {

		debug("a symbol found \n");
	       	
		goto FOUND;
	    }
	    else if (*ss == '(' || *ss == ')') {

		debug("a symbol found \n");
		
		goto FOUND;
	    }
	    
	    ss++;
	}
	
	goto ERR;
    }
 
    goto ERR;

    
  FOUND:
    ml_util_show_buf(s, ss - s);
    
    if (form->obj) {

	obj = form->obj;	
    }
    else if (form->list) {

	/* object in list form 
	 */
	object_s new_obj;
	memset(&new_obj, 0, sizeof(object_s));
	new_obj.type = OBJ_TYPE;
	new_obj.subtype = OBJ_SUBTYPE_QUOTE_EXPRESSION;
	
	if (!list_add_object(form->list, &new_obj)) goto ERR;
	obj = &form->list->front->obj;
    }
    else {
	goto ERR;
    }
  
    if (!obj_clone_symbol(obj, s, ss - s)) goto ERR;

    move_code(*code, *code_sz, ss - s);

    out(ok, true);

    
  ERR:    
    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    out(fail, false); 
}


static bool
read_comments(char **code, size_t *code_sz)
{
    func_s();
    
    if (!eq(**code, ';')) return false;
    next_code(*code, *code_sz);

    while (*code_sz > 0) {

	if (eq(**code, NEWLINE) || eq(**code, LINEFEED)) {
	    
	    next_code(*code, *code_sz);

	    ignore_end_chars(code, code_sz);

	    func_ok();
	    return true;
	}

	next_code(*code, *code_sz);
    }

    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}


static bool
read_symbol(char **code, size_t *code_sz, form_s *form)
{
    bool found;
    token_s token;
     
    const char *buf;
    
    func_s();


    ignore_end_chars(code, code_sz);
    
    switch (**code) {

    case ';':

	/* A semicolon introduces characters to be ignored, such as comments. 
	 */
	
	found = read_comments(code, code_sz);
	if (!found) goto ERR;
	return true;
	break;
	
    case '\"':

        /* The double-quote is used to begin and end a string. 
	 */

	found = read_string(code, code_sz, form);
	if (!found) goto ERR;
	return true;
	break;

    }


    /* read the non-string symbol
     */
    buf = *code;  
    while (*code_sz > 0) {

	debug("0x%02x %c \n", **code, **code);
	
	if (eq(**code, SPACE) || **code == LINEFEED || eq(**code, ')')) {

	    memset(&token, 0, sizeof(token_s));
	    token.value.symbol = ml_util_buf2str(buf, *code - buf);
	    if (token.value.symbol) {

		debug("symbol: %s \n", token.value.symbol);
		token.type = TOKEN_SYMBOL;
	    }
	  
			    
	    if (eq(**code, SPACE) || **code == LINEFEED) {
		next_code(*code, *code_sz);
	    }


	    if (token.type != TOKEN_UNKOWN) {

		if (!form_add_token(form, &token)) goto ERR;	       
		
		func_ok();
		return true;
	    }
	    
	    return false;
	}

	next_code(*code, *code_sz);
    }

  ERR:
    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}


static bool
read_template(char **code, size_t *code_sz, form_s *form)
{
    func_s();

    if (**code == '`') {
	
	next_code(*code, *code_sz);

	/* Add a template form
	 */
	list_add_char_obj(form->list, "`");
    }
    else if (**code == ',') {

	next_code(*code, *code_sz);

	//list_add_char_obj(form->list, ",");
	
	object_s obj;
	memset(&obj, 0, sizeof(object_s));
	obj.type = OBJ_TYPE;
	obj.subtype = OBJ_SUBTYPE_MACRO_COMMNA;
	list_add_object(form->list, &obj);
	goto DONE;
	
    }
    else {

	return false;	
    }
    
 		
    switch (**code) {
    case '(':

	/* parse the list subform in the template form
	 */

	next_code(*code, *code_sz);

	form_s *f_head = form_create();
	form->list->front->obj.sub = (void*)f_head;
	    
	form_s *f = form_create();
	if (!f) goto ERR;
	
	list_add_char_obj(f->list, "(");
		
	code_s icode = read_list(*code, *code_sz, f_head, f);
	if (!icode.code) goto ERR;

	list_add_char_obj(f->list, ")");
	
	*code = icode.code;
	*code_sz = icode.code_sz;
	break;

    default:
	goto ERR;
	break;
    }
    

  DONE:
    out(ok, true);

  ERR:
    ml_err_signal(ML_ERR_ILLEGAL_CHAR);

    out(fail, false);
}



static bool
like_arithmetic_func(char *code)
{
    switch (*code) {
    case '+':
    case '-':
    case '*':
    case '/':
    case '<':
    case '>':
    case '=':
    case '!':
	return true;
    }
    
    return false;
}


static bool
like_list_func(char *code)
{
    if (eq(*code, 'l') || !eq(*code, 'L')) return true;

    return false;
}


static bool
like_other_func(char *code)
{
    /* atom */
    if (eq(*code, 'a') || eq(*code, 'A')) return true;
    
    /* car, cdr */
    if (eq(*code, 'c') || eq(*code, 'C')) return true;

    /* eq */
    if (eq(*code, 'e') || eq(*code, 'E')) return true;

    /* print */
    if (*code =='p' || *code =='P') return true;
       
    return false;
}


static bool
like_num_token(char *code, size_t code_sz)
{
    if (is_sign(*code)) goto FIND_ONE_DIGIT;

    if (is_digit(*code)) goto CHECK_MORE;
    
    if (eq(*code, '.')) goto FIND_ONE_DIGIT;

    if (eq(*code, '^')) goto FIND_ONE_DIGIT;
    if (eq(*code, '_')) goto FIND_ONE_DIGIT;

    
  FIND_ONE_DIGIT:

    next_code(code, code_sz);
    while (code_sz > 0) {

	if (eq(*code, SPACE) || eq(*code, ')')) return false;

	if (is_digit(*code)) goto CHECK_MORE;

        next_code(code, code_sz);
    }

    return false;

  CHECK_MORE:

    debug("check digits \n");

#if 0
    next_code(code, code_sz);
    while (code_sz > 0) {

	if (eq(*code, SPACE) || eq(*code, ')')) {

	    /* The token does not end with a sign. */
	    if (is_sign(*(code-1))) return false;

	    func_ok();
	    return true;
	}

	if (is_digit(*code) || is_sign(*code) ||
	    eq(*code, '^') || eq(*code, '_') ||
	    eq(*code, '.') || is_ratio_marker(*code) ||
	    is_exponent_maker(*code)) {

	    next_code(code, code_sz);

	    continue;
	}

	return false;
    }
    
    
    return false;
#endif

    out(ok, true);
}



static bool
identify_number_token(char **code, size_t *code_sz, form_s *form)
{
    bool is_negative = false;
    char *s = (char*)*code;

    token_s token;
    memset(&token, 0, sizeof(token_s));

    if (*code_sz <= 0) return false;
     
    if (**code == '+') {
	
	s++;
	next_code(*code, *code_sz);
    }
    else if (**code == '-') {

	is_negative = true;
	next_code(*code, *code_sz);
    }
    else {
    }
	

    
    char c;
    int k = 0;

    func_s();

    while (*code_sz > 0) {

	c = **code;
	if (!is_digit(c)) break;

	//debug("%c ", c);
	
	k++;
	next_code(*code, *code_sz);
    }
    //debug("\n");

    
    if (k > 0) {
	debug("digits len: %d \n", k + is_negative);

	/* [sign] {digit}+ decimal-point
	 */
	if (c == '.') {
	    next_code(*code, *code_sz);

	    if (is_digit(c)) {

		/* float */
		token.type = TOKEN_NUM_FLOAT;
	    }
	    else if (is_exponent_maker(c)) {

		/* float */
		token.type = TOKEN_NUM_FLOAT;
	    }
	    else {

		/* integer */
		token.type = TOKEN_NUM_INT;
	    }
	}
	else if (is_ratio_marker(c)) {  
	    next_code(*code, *code_sz);

	    if (is_digit(c)) {

		/* ratio */
		token.type = TOKEN_NUM_RATIO;
			
	    }
	    else {
			
		return false;
	    }
			
	}		
	else {

	    if (!is_whitespace_char(c) &&
		!eq(c, SPACE) &&
		!eq(c, ')')) {

		return false;
	    }
		
	    /* integer */
	    token.type = TOKEN_NUM_INT;
	    
	    token.value.num_int = ml_util_arr2fixnum(s, k + is_negative);
	    token_show_fixnum(token.value.num_int);
	}
	       		
    }
    else {

	
	/* float */
	//token.type = TOKEN_NUM_FLOAT;
    }

	    
    if (token.type != TOKEN_UNKOWN) {

	if (form->obj) form->subtype = SELF_EVAL_FORM_NUMBER;
    	
	if (!form_add_token(form, &token)) goto FAIL;

	//form_show(form);	
	
	out(ok, true);
    }

    
  FAIL:
    out(fail, false);
}


static bool
identify_arithmetic_operator(char **code, size_t *code_sz, form_s *form)
{
    token_s *t, tk;
    char *s;
    int len;

    switch (**code) {

    case '+':
	s = "+";
	len = 1;
	form->subtype = S_FUNCTION_NUM_ADD;
	break;
	
    case '-':
	s = "-";
	len = 1;
	break;
	
    case '*':
	s = "*";
	len = 1;
	break;
	
    case '/':
	if (*code_sz-1 == 0) return false;
	
	if (*(*code+1) == '=') {
	    s = "/=";
	    len = 2;
	}
	else {
	    return false;	    
	}
	break;
	
    case '<':
	if (*code_sz-1 == 0) return false;
	
	if (*(*code+1) == '=') {
	    s = "<=";
	    len = 2;
	}
	else {
	    s = "<";
	    len = 1;	    
	}
	break;
	
    case '>':
	if (*code_sz-1 == 0) return false;
	
	if (*(*code+1) == '=') {
	    s = ">=";
	    len = 2;
	}
	else {
	    s = ">";
	    len = 1;	    
	}
	break;

    case '!':
	if (*code_sz-1 == 0) return false;
	
	if (*(*code+1) == '=') {
	    s = "!=";
	    len = 2;
	}
	else {
	    return false;	    
	}
	break;
	
    default:
	
	return false;
    }


    move_code(*code, *code_sz, len);
    if (*code_sz == 0) goto DONE;

    if (eq(**code, SPACE)) {
	
	next_code(*code, *code_sz);
	goto DONE;
    }
    
    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	goto DONE;
    }
 
    if (**code == ')') {
	
	next_code(*code, *code_sz);
	goto DONE;
    }    
     
    return false;
    
  DONE:
    
    memset(&tk, 0, sizeof(token_s));
    t = &tk;
    //t = token_create();
    //if (!t) return false;
    
    
    t->type = TOKEN_SYMBOL;
    t->value.symbol = s;

        
    if (form_is_unkown(form)) {
	form_set_type(form, COMPOUND_FUNCTION_FORM);
    }
    
    list_add_token(form->list, t);
  
    func_ok();
    return true;
}


static bool
identify_list_func(char **code, size_t *code_sz, form_s *form)
{
    token_s *t, tk;
    
    if (!ml_util_strbufcmp("list", *code, *code_sz)) return false;
    
    move_code(*code, *code_sz, 4);
    
    if (*code_sz == 0) goto DONE;

    if (eq(**code, SPACE)) {
	
	next_code(*code, *code_sz);
	goto DONE;
    }
    
    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	goto DONE;
    }
 
    if (**code == ')') {
	
	goto DONE;
    }    
     
    return false;
    
  DONE:
    memset(&tk, 0, sizeof(token_s));
    t = &tk;
    
    t->type = TOKEN_SYMBOL;
    t->value.symbol = "list";

        
    if (form_is_unkown(form)) {
	form_set_type(form, COMPOUND_FUNCTION_FORM);
	form->subtype = S_FUNCTION_LIST;
    }
    
    list_add_token(form->list, t);
  
    func_ok();
    return true;
}


static bool
identify_other_func(char **code, size_t *code_sz, form_s *form)
{
    token_s *t, tk;
    char *str;
    int len;

    if (ml_util_strbufcmp("atom", *code, *code_sz)) {

	str = "atom";
	form->subtype = S_FUNCTION_ATOM;
    }    
    else if (ml_util_strbufcmp("car", *code, *code_sz)) {

	str = "car";
	form->subtype = S_FUNCTION_CAR;
    }
    else if (ml_util_strbufcmp("cdr", *code, *code_sz)) {

	str = "cdr";
	form->subtype = S_FUNCTION_CDR;
    }
    else if (ml_util_strbufcmp("cons", *code, *code_sz)) {

	str = "cons";
	form->subtype = S_FUNCTION_CONS;
    }
    else if (ml_util_strbufcmp("eq", *code, *code_sz)) {

	str = "eq";
	form->subtype = S_PREDICATE_EQ;
    }
    else if (ml_util_strbufcmp("print", *code, *code_sz)) {

	str = "print";
	form->subtype = S_FUNCTION_PRINT;
    }
    else {

	return false;
    }

    len = strlen(str);
    debug("%s \n", str);
    
    move_code(*code, *code_sz, len);   
    if (*code_sz == 0) goto DONE;

    if (eq(**code, SPACE)) {
	
	next_code(*code, *code_sz);
	goto DONE;
    }
    
    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	goto DONE;
    }
 
    if (**code == ')') {
	
	next_code(*code, *code_sz);
	goto DONE;
    }    
     
    return false;
    
  DONE:
    memset(&tk, 0, sizeof(token_s));
    t = &tk;
    
    t->type = TOKEN_SYMBOL;
    t->value.symbol = str;
        
    if (form_is_unkown(form)) {
	form_set_type(form, COMPOUND_FUNCTION_FORM);
    }
    
    list_add_token(form->list, t);

    out(ok, true);
}


static bool
identify_code_as_func(char **code, size_t *code_sz,
		      identify_f func, form_s *form)
{
    bool found;
    
    code_s icode = { *code, *code_sz };
    
    stack_push(&icode, sizeof(code_s));

    found = func(code, code_sz, form);
	
    stack_pop(&icode);

    if (!found) {
	
	*code = icode.code;
	*code_sz = icode.code_sz;
	//debug("0x%02x %c \n", *icode.code, *icode.code);
    }

    return found;
}



static char*
read_character(char **code, size_t *code_sz)
{
    char *s, *ss;
    
    if (**code != '\\') return false;
    next_code(*code, *code_sz);

    func_s();
    
    if (*code_sz == 0) goto FAIL;
    

    /* if the x is more than one character long, the x must be a symbol 
     * with no embedded package markers. 
     * the character name is (string-upcase x).
     * if the name is invalid, then signaling a syntax error.
     */
    size_t len = 0;
    ss = *code;
    while (*code_sz > 1) {

	if (**code == SPACE || **code == NEWLINE ||  **code == LINEFEED) {
	    
	    next_code(*code, *code_sz);
	    break;
	}

	if (**code == ')') {
	    
	    break;
	}	

	next_code(*code, *code_sz);
	len++;
    }

    if (len == 0) {

	debug_err("character name is null \n");
	goto FAIL;
	
    }
    
    s = (char*)ml_malloc(2);
    if (!s) return NULL;

    if (len == 1) {
	
	s[0] = *ss;
	s[1] = 0;
    }
    else {

	char *name = ml_util_buf2str(ss, len);	
	debug("character name: %s \n", name);
	
	char c = char_get(name);
	if (c == CHAR_UNKNOWN) {

	    debug_err("unknown character name: %s \n", name);
	    free(name);
	    goto FAIL;
	}
	
	ml_free(name);
	
	s[0] = c;
	s[1] = 0;
    }

    debug("character: %s\n", s);
    
    out(ok, s);
    
  FAIL:
    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return NULL;
}


static bool
identify_special_form(char **code, size_t *code_sz, form_s *form)
{
    token_s *t, tk;
    int len;
    char *name;

    
    if (ml_util_strbufcmp("if", *code, *code_sz)) {

	len = 2;
	name = "if";
	form->subtype = SPECIAL_FORM_IF;
    }
    else if (ml_util_strbufcmp("setq", *code, *code_sz)) {

	len = 4;
	name = "setq";
	form->subtype = SPECIAL_FORM_SETQ;
    }    
    else {

	return false;
    }
        
    move_code(*code, *code_sz, len);
    
    if (*code_sz == 0) goto DONE;

    if (eq(**code, SPACE)) {
	
	next_code(*code, *code_sz);
	goto DONE;
    }
    
    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	goto DONE;
    }
 
    if (**code == ')') {
	
	next_code(*code, *code_sz);
	goto DONE;
    }    
     
    return false;
    
  DONE:
    memset(&tk, 0, sizeof(token_s));
    t = &tk;
   
    
    t->type = TOKEN_SYMBOL;
    t->value.symbol = name;

        
    if (form_is_unkown(form)) {
	form_set_type(form, COMPOUND_SPECIAL_FORM);
    }
    
    list_add_token(form->list, t);
  
    func_ok();
    return true;
}


static bool
identify_macro_form(char **code, size_t *code_sz, form_s *form)
{
    token_s *t, tk;
    int len;
    char *name;

    
    if (ml_util_strbufcmp("loop", *code, *code_sz)) {

	len = 4;
	name = "loop";
    }
    else if (ml_util_strbufcmp("return", *code, *code_sz)) {

	len = 6;
	name = "return";
    }
    else if (ml_util_strbufcmp("defun", *code, *code_sz)) {

	len = 5;
	name = "defun";
    }
    else if (ml_util_strbufcmp("defmacro", *code, *code_sz)) {

	len = 8;
	name = "defmacro";
    }    
    else {

	return false;
    }
        
    move_code(*code, *code_sz, len);
    
    if (*code_sz == 0) goto DONE;

    if (eq(**code, SPACE)) {
	
	next_code(*code, *code_sz);
	goto DONE;
    }
    
    if (is_whitespace_char(**code)) {

	next_code(*code, *code_sz);
	goto DONE;
    }
 
    if (**code == ')') {
	
	next_code(*code, *code_sz);
	goto DONE;
    }    

    func_fail();
    return false;
    
  DONE:
    memset(&tk, 0, sizeof(token_s));
    t = &tk;
   
    
    t->type = TOKEN_SYMBOL;
    t->value.symbol = name;

    debug("%s \n", name);
        
    if (form_is_unkown(form)) {
	form_set_type(form, COMPOUND_MACRO_FORM);
    }
    
    list_add_token(form->list, t);
  
    func_ok();
    return true;
}

 
static code_s
read_list(char *code, size_t code_sz, form_s *form_head, form_s *form)
{
    bool found;
    bool is_nil_list;
    
    func_s();

    is_nil_list = true;
    while (code_sz > 0) {

	debug("0x%02x %c \n", *code, *code);

	if (is_whitespace_char(*code)) {

	    next_code(code, code_sz);
	    continue;
	}

	if (*code == SPACE) {

	    /* next token */
	    next_code(code, code_sz);
	    continue;
	}
	
	if (*code == ')') {

	    debug(") \n");
	    
	    /* empty list: () 
	     */
	    if (form->list->next->next == form->list) {
		form->list->is_nil = true;
		form_add_front(form_head, form);		
		form_set_type(form, SELF_EVALUATING_FORM);
		form->subtype = SELF_EVAL_FORM_EMPTY_LIST;
	    }

	    next_code(code, code_sz);
	    break;
	}

	is_nil_list = false;

	if (*code == '\'') {

	    if (form_is_unkown(form)) goto ERR;
	    
	    found = read_quote_expression(&code, &code_sz, form);
	    if (found) {

		continue;
	    }
	}

	
	if (*code == '`' || *code == ',') {

	    found = read_template(&code, &code_sz, form);
	    if (found) continue;
	    
	}

	if (like_arithmetic_func(code)) {
	    
	    found = identify_code_as_func(&code, &code_sz,
					  identify_arithmetic_operator, form);
	    if (found) {

		form_add_front(form_head, form);
		continue;
	    }
	}

	if (like_list_func(code)) {

	    found = identify_code_as_func(&code, &code_sz,
					  identify_list_func, form);
	    if (found) {

		form_add_front(form_head, form);
		continue;
	    }
	}

	if (like_other_func(code)) {

	    found = identify_code_as_func(&code, &code_sz,
					  identify_other_func, form);
	    if (found) {

		form_add_front(form_head, form);

		form_show(form);
		continue;
	    }
	}

	if (tolower(*code) == 'i' ||
	    tolower(*code) == 's') {

	    found = identify_special_form(&code, &code_sz, form);
	    
	    if (found) {

		form_add_front(form_head, form);
		
		continue;
	    }
	    
	}

	if (tolower(*code) == 'l' || tolower(*code) == 'r' ||
	    tolower(*code) == 'd'){

	    found = identify_macro_form(&code, &code_sz, form);
	    
	    if (found) {

		form_add_front(form_head, form);
		
		continue;
	    }
	    
	}	
	

	if (like_num_token(code, code_sz)) {

	    found = identify_code_as_func(&code, &code_sz,
					  identify_number_token, form);
	    if (found) {
		
		continue;
	    }
	}


	if (*code == '#') {
	 
	    char *character = char_get_name_as_code(code, code_sz);
	    if (character) {

		move_code(code, code_sz, strlen(character));
		//debug_suspend();
	    }
	    else {
		
		next_code(code, code_sz);
		character = read_character(&code, &code_sz);
	    }
	    
	    if (character) {

		list_add_char_obj(form->list, character);
		
		continue;
	    }
	}

	if (*code == '(') {

	    token_s tk;
	    memset(&tk, 0, sizeof(token_s));
	    list_add_token(form->list, &tk);
	    form->list->front->obj.type = OBJ_LIST;
	 
	    form_s *f_head = form_create();
	    form->list->front->obj.sub = (void*)f_head;

	    form_s *f = form_create();

	    f->code = code;
	    list_add_char_obj(f->list, "(");

	    //form_show(form);
	    //debug_suspend();
	    
	    code_s cd = read_list(++code, --code_sz, f_head, f);
	    
	    if (!cd.code) return cd;

	    list_add_char_obj(f->list, ")");

	    f->code_sz = cd.code - f->code + 1;

	    ml_util_show_buf(f->code, f->code_sz);
	    //form_show(form);
	    //debug_suspend();
	    
	    code = cd.code;
	    code_sz = cd.code_sz;
	}
	else {

	    found = read_symbol(&code, &code_sz, form);
	    if (!found) goto ERR;

	    
	    char *sym = obj_get_symbol(&form->list->front->obj);
	    if (var_match_binder(sym)) {
		
		if (form_is_unkown(form)) {
		    
		    form_set_type(form, COMPOUND_SPECIAL_FORM);
		    form_add_front(form_head, form);
		}
	    }
	    else {

		if (func_exist(sym)) {

		    if (form_is_unkown(form)) {
		    
			form_set_type(form, COMPOUND_FUNCTION_FORM);
			form_add_front(form_head, form);
		    }		    
		}		
		else if (macro_exist(sym)) {

		    debug("macro call: %s \n", sym);
		    
		    if (form_is_unkown(form)) {
		    
			form_set_type(form, COMPOUND_MACRO_FORM);
			form_add_front(form_head, form);
		    }		    
		}
		else {
		
		    if (form_is_unkown(form)) {
		    
			form_set_type(form, SYMBOL_FORM);
			form_add_front(form_head, form);
		    }
		}
	    }
	    
	}
	
    }

    code_s cd = { code, code_sz };
    return cd;   
    
  ERR:
    ml_err_signal(ML_ERR_ILLEGAL_CHAR);

    return (code_s){ NULL, 0};
}


static bool
read_macro(code_s *cd, lex_s *lex)
{
    bool found = false;
    form_s *form;
    char *character;
    
    if (!cd || !cd->code || !cd->code_sz) return false;
    
    func_s();

    char c = *cd->code;
    debug("0x%02x %c \n", c, c);

    switch (c) {

    case '(':

	/* The left-parenthesis character initiates reading of a list. 
	 */

	
	form = form_create();
	if (!form) return false;

	form->code = cd->code;
	
	next_code(cd->code, cd->code_sz);
	
	
	list_add_char_obj(form->list, "(");
		
	code_s icode = read_list(cd->code, cd->code_sz, &lex->forms, form);
	if (!icode.code) return false;

	list_add_char_obj(form->list, ")");

	form->code_sz = icode.code - form->code + 1;

	//ml_util_show_buf(form->code, form->code_sz);
	//debug_suspend();
	
	cd->code = icode.code;
	cd->code_sz = icode.code_sz;

	found = true;
	break;

    case '\'': 

	/* A single-quote introduces an expression to be "quoted" 
	 */
	
	form = form_create_as_quoted_expression();
	if (!form) goto FAIL;
	
	found = read_quote_expression(&cd->code, &cd->code_sz, form);
	if (found) {

	    if (!form_add_front(&lex->forms, form)) goto FAIL;
	}
	
	break;
	    
    case ';':

	/* A semicolon introduces characters to be ignored, such as comments. 
	 */
	
	found = read_comments(&cd->code, &cd->code_sz);
	break;

    case '\"':

        /* The double-quote is used to begin and end a string. 
	 */

	found = read_string(&cd->code, &cd->code_sz, NULL);
	break;

    case '`':

        /* The backquote introduces a template of a data structure to be built. 
	 */

	/* TODO: backquote syntax 
	 */

	found = read_template(&cd->code, &cd->code_sz, NULL);	
	
	break;


    case '#':

	/* Sharpsign is a non-terminating dispatching macro character. It uses 
	 * a character to select a function to run as a reader macro function.
	 * 1. Sharpsign Backslash
	 *    as a character object, syntax: #\<x>
	 *
	 * 2. TODO:
	 *    function abbreviation, syntax: #'<function name>
	 *    For example:
	 *        (apply #'+ l) ==  (apply (function +) l)
	 *        (apply #'list '(1 2))  =>  (1 2)
	 *        (apply #'+ '(1 2))  =>  3
	 *
	 *    more...
	 *
	 */

	character = char_get_name_as_code(cd->code, cd->code_sz);
	if (character) {

	    move_code(cd->code, cd->code_sz, strlen(character));
	}
	else {
		
	    next_code(cd->code, cd->code_sz);
	    character = read_character(&cd->code, &cd->code_sz);
	}
	found = !!character;

	if (character) {

	    form_s *form = form_create_as_character_obj(character);
	    if (!form) goto FAIL;
	    
	    if (!form_add_front(&lex->forms, form)) goto FAIL;
	}
	
	break;
    }

    
    func_e();
    return found;

  FAIL:
    out(fail, false);
}


static bool
identify_bool_constant(char **code, size_t *code_sz, form_s *form)
{     
    char *buf;
    int len;
    
    func_s();

    buf = *code;  
    while (*code_sz > 0) {

	debug("0x%02x %c \n", **code, **code);
	
	if (**code == SPACE || **code == LINEFEED) {

	    len = *code - buf;	    
	    next_code(*code, *code_sz);
	    
	    if (len == 3 && ml_util_strbufcmp("nil", buf, len)) {

		form->subtype = SELF_EVAL_FORM_BOOL;
		obj_set_nil(form->obj);     
		out(ok, true);
	    }
	    else if (len == 1 && (*buf == 't' || *buf == 'T')) {

		form->subtype = SELF_EVAL_FORM_BOOL;
		obj_set_t(form->obj);
		out(ok, true);
	    }
	    else {

		goto ERR;
	    }
	       
	}

	next_code(*code, *code_sz);
    }

  ERR:
    ml_err_signal(ML_ERR_ILLEGAL_CHAR);
    
    return false;
}



static bool
identify_empty_list(char **code, size_t *code_sz, form_s *form_head)
{
    if (**code != '(') return false;

    func_s();

    char *s = *code + 1;
    size_t len = *code_sz - 1;

    while (len > 0) {

	if (*s == ')') break;
	    
	if (*s != SPACE && *s != LINEFEED) goto FAIL;
	
	s++;
	len--;
	if (len == 0) goto FAIL;	
    }

    len = s - *code + 1;
    move_code(*code, *code_sz, len);
    
    form_s *f = form_create_nil();
    if (!f) goto FAIL;

    if (!form_add_front(form_head, f)) goto FAIL;

     
    out(ok, true);

  FAIL:
    out(fail, false);
}


static bool
read_self_eval_form(code_s *cd, lex_s *lex)
{
    bool found = false;
    form_s *form;
      
    if (!cd || !cd->code || !cd->code_sz) return false;

    char *code = cd->code;
    size_t code_sz = cd->code_sz;
    
    debug("0x%02x %c \n", *code, *code);

    
    if (like_num_token(code, code_sz)) {

	func_s();
	
	form = form_create_as_self_eval_form();
	if (!form) goto FAIL;
    
	found = identify_code_as_func(&code, &code_sz,
				      identify_number_token, form);
    }
    else if (*code == 'n' || *code == 'N' || *code == 't' || *code == 'T') {

	func_s();
	
	form = form_create_as_self_eval_form();
	if (!form) goto FAIL;
	
	found = identify_bool_constant(&code, &code_sz, form);
    }
    else {
	return false;
    }

    if (!found) goto FAIL;

    if (!form_add_front(&lex->forms, form)) goto FAIL;

    
    cd->code = code;
    cd->code_sz = code_sz;

    out(ok, true);

  FAIL:
    out(fail, false);
}



/**
 * Initialize lexer 
 */
lex_rt_t
ml_lex_init(void)
{
    func_s();


    debug("maximum value of SINGED INT32: %d \n", INT32_MAX);
    debug("minimum value of SINGED INT32: %d \n", INT32_MIN);
    debug("maximum value of SINGED INT64: %lld \n", INT64_MAX);
    debug("minimum value of SINGED INT64: %lld \n", INT64_MIN);

#if OS_64BIT_ENABLE
    debug("maximum value of fixnum: %lld \n", FIXNUM_MAX);
    debug("minimum value of fixnum: %lld \n", FIXNUM_MIN);
#else
    debug("maximum value of fixnum: %d \n", FIXNUM_MAX);
    debug("minimum value of fixnum: %d \n", FIXNUM_MIN);    
#endif
    
    

    func_ok();
    
    return LEX_OK;
}



/**
 * Lexical analyzing for the code
 */
lex_rt_t
ml_lex(lex_s *lex, code_s *cd)
{
    char *code;
    size_t code_sz;
    
    func_s();

    if (!lex || !cd || !cd->code) return LEX_ERR_NULL;
    
    if (cd->code_sz <= 0) return LEX_ERR_ARGU;

    ml_util_show_buf((char*)cd->code, cd->code_sz);
    

    code = cd->code;
    code_sz = cd->code_sz;
    
    
    /* Identify the tokens, see [Reader Algorithm] at:
     * http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm
     * Copyright 1996-2005, LispWorks Ltd. All Rights Reserved.
     */
    char x, y, z;
    while (1) {
    
	/* step 1
	 */
	if (code_sz == 0) break;

	x = *code; /* read one character */

	debug("0x%02x %c \n", x, x);
	
	/* step 2
	 */
	if (is_illegal_char(x)) {
	    
	    goto STEP_10;
	}

	
	/* step 3
	 */
	if (is_whitespace_char(x)) {

	    debug("whitespace char \n");
	    
	    next_code(code, code_sz);
	    continue;
	}

	
	/* step 4
	 */
	if (is_macro_char(x)) {

	    debug("macro char \n");

	    if (identify_empty_list(&code, &code_sz, &lex->forms)) continue;
	    
	    cd->code = code;
	    cd->code_sz = code_sz;	    

	    if (!read_macro(cd, lex)) return LEX_ERR;

	    form_show(&lex->forms);
	    
	    debug("remain code_sz:%d \n",cd->code_sz);
	    if (cd->code_sz == 0) break;

	    code = cd->code;
	    code_sz = cd->code_sz;   
	    continue;
	}


	/* step 5
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

	
	if (is_multiple_escape_char(x)) {

	    show("multiple escape char \n");
	    goto STEP_9;
	}


	if (is_constituent_char(x)) {

	    debug("constituent char \n");

	    cd->code = code;
	    cd->code_sz = code_sz;	    
        
	    if (!read_self_eval_form(cd, lex)) {

		form_s *f = form_create_symbol_form();
		if (!read_symbol(&code, &code_sz, f)) {
		    goto FAIL;
		}

	
		form_add_front(&lex->forms, f);
	
			    
		char *sym = obj_get_symbol(f->obj);
		if (var_match_binder(sym)) {
		}
		form_show(f);
		
	    }
	    else {
		form_show(&lex->forms);
	    
		debug("remain code_sz:%d \n",cd->code_sz);
		//if (cd->code_sz == 0) break;

		//ml_util_show_buf((char*)cd->code, cd->code_sz);
	    
		code = cd->code;
		code_sz = cd->code_sz;
	    }

	    
	    goto STEP_8;
	}

	goto STEP_10;
	
      STEP_8:

	if (code_sz == 0) break;
	continue;

      STEP_9:
	z = 0;
	
	next_code(code, code_sz);
	
    } /* end of while */

    
    form_show(&lex->forms);
    
    out(ok, LEX_OK);

  FAIL:
  STEP_10:
    ml_err_signal_x(ML_ERR_ILLEGAL_CHAR, __FUNCTION__, __LINE__);

    out(fail, LEX_ERR);
}
