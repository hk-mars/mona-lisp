
#ifndef ML_CHARS_H
#define ML_CHARS_H


#define backspace 0x08  /* '\b' */
#define tab 0x09  /* '\t' */
#define newline 0x0D  /* '\r' */
#define space 0x20  /* ' ' */
#define return_char 0xFF  /* TODO */
#define linefeed 0x0A  /* '\n' */
#define page 0xFF  /* TODO */


#define whitespace \
    backspace | \
    tab | \
    newline | \
    space | \
    return_char | \
    linefeed | \
    page







#endif /* ML_CHARS_H */

