
#ifndef ML_CHARS_H
#define ML_CHARS_H


/**
 * A graphic character is one that has a standard textual representation as a single glyph, 
 * such as A or * or =. Space, which effectively has a blank glyph, is defined to be a graphic.
 * Of the standard characters, newline is non-graphic and all others are graphic.
 *
 * Characters that are not graphic are called non-graphic.
 * #\Backspace, #\Tab, #\Rubout, #\Linefeed, #\Return, and #\Page, are non-graphic characters.
 */


/* '\b', #\backspace */
#define BACKSPACE 0x08

/* '\t', #\tab */
#define TAB 0x09

/* '\r'(unix), #\newline */
#define NEWLINE 0x0D

/* ' ', #\space */
#define SPACE 0x20

/* Carriage Return ("CR") character (0x0D, \r), #\return */
#define RETURN_CR 0x0D

/* Line Feed ("LF") character (0x0A, \n), #\linefeed */
#define LINEFEED 0x0A

/* TODO, #\page */
#define PAGE 0xFF

/* backward-delete-char (Rubout), #\rubout */
#define RUBOUT 0x7F

/* reverse solidus, or backslash */ 
#define BACKSLASH '\\'

/* vertical bar */
#define VERTICAL_BAR '|'



#define is_digit(x) ((x) >= '0' && (x) <= '9')

#define is_alpha(x) ( ((x) >= 'a' && (x) <= 'z') || ((x) >= 'A' && (x) <= 'Z') )

#define is_whitespace_char(x) ( \
        (x) == BACKSPACE ||    \
	(x) == TAB ||	       \
	(x) == NEWLINE ||      \
	(x) == LINEFEED ||     \
	(x) == RETURN_CR )

#define is_terminating_char(x) (       \
   (x) == '\"' ||		       \
   (x) == '\'' ||		       \
   (x) == '(' ||		       \
   (x) == ')' ||		       \
   (x) == ',' ||		       \
   (x) == ';' ||		       \
   (x) == '`' )

#define is_non_terminating_char(x) ((x) == '#')

#define is_macro_char(x) ( is_terminating_char(x) || is_non_terminating_char(x) )

#define is_escape_char(x) ((x) == BACKSLASH)

#define is_multiple_escape_char(x) ((x) == VERTICAL_BAR)

#define is_constituent_char(x) ( \
        (x) == BACKSPACE ||	 \
	(x) == RUBOUT ||	 \
        is_digit(x) ||		 \
	is_alpha(x) ||		 \
	(x) == '$' ||		 \
	(x) == '%' ||		 \
	(x) == '&' ||		 \
	(x) == '*' ||		 \
	(x) == '+' ||		 \
	(x) == '-' ||		 \
	(x) == '.' ||		 \
	(x) == '/' ||		 \
	(x) == ':' ||		 \
	(x) == '<' ||		 \
	(x) == '=' ||		 \
	(x) == '>' ||		 \
	(x) == '@' ||		 \
	(x) == '^' ||		 \
	(x) == '_' ||		 \
	(x) == '~' )

#define is_illegal_char(x) (   \
    !is_constituent_char(x) &&  \
    !is_whitespace_char(x) &&   \
    !is_macro_char(x) &&	       \
    !is_escape_char(x) &&       \
    !is_multiple_escape_char(x) )

#define check_char(i, who) ((i) == (who))

#define is_sign(x) (check_char(x, '-') || check_char(x, '+'))

#define is_exponent_maker(x)					\
(								\
 check_char(x, 'e') ||						\
 check_char(x, 's') ||						\
 check_char(x, 'f') ||						\
 check_char(x, 'd') ||						\
 check_char(x, 'l') ||						\
 check_char(x, 'E') ||						\
 check_char(x, 'S') ||						\
 check_char(x, 'F') ||						\
 check_char(x, 'D') ||						\
 check_char(x, 'L') )


#define is_ratio_marker(x) check_char(x, '/')


typedef enum
{
    CHAR_WHITESPACE = 0,
    CHAR_CONSTITUENT = 1,
    CHAR_SIGNEL_ESCAPE = 2,
    CHAR_MULTI_ESCAPE = 3,
    CHAR_MACRO = 4,    
    
} syntax_char_t;


typedef struct
{
    syntax_char_t type;
    
    char ch;
    
} char_type_s;


#endif /* ML_CHARS_H */

