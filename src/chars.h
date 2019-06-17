
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

/* The Carriage Return ("CR") character (0x0D, \r), #\return */
#define RETURN_CR 0x0D

/* The Line Feed ("LF") character (0x0A, \n), #\linefeed */
#define LINEFEED 0x0A

/* #\page */
#define PAGE 0xFF  /* TODO */


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

