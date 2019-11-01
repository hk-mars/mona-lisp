

#include "chars.h"

#include "debug.h"

#include "error.h"

#include "util.h"


const character_s char_name_tb[] =
{
    { "SPACE", SPACE },
    { "BACKSPACE", BACKSPACE },
    { "TAB", TAB },
    { "NEWLINE", NEWLINE},
    { "RETURN", RETURN_CR},
    { "LINEFEED", LINEFEED},
    { "PAGE", PAGE},
    { "RUBOUT", RUBOUT}, 
};


char
char_get(char *name)
{
    int len = ARR_LEN(char_name_tb);
    for (int i = 0; i < len; i++) {

	if (!strcasecmp(char_name_tb[i].name, name)) {

	    return char_name_tb[i].c;
	}
    }

    return CHAR_UNKNOWN;
}


char*
char_get_name(char c)
{
    int len = ARR_LEN(char_name_tb);
    for (int i = 0; i < len; i++) {

	if (char_name_tb[i].c == c) {

	    return (char*)char_name_tb[i].name;
	}
    }

    return NULL;
}



