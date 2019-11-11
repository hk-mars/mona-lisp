

#include "chars.h"

#include "debug.h"

#include "error.h"

#include "util.h"


const character_s char_name_tb[] =
{
    { "#\\space", SPACE },
    { "#\\BACKSPACE", BACKSPACE },
    { "#\\tab", TAB },
    { "#\\newline", NEWLINE},
    { "#\\RETURN", RETURN_CR},
    { "#\\linefeed", LINEFEED},
    { "#\\PAGE", PAGE},
    { "#\\RUBOUT", RUBOUT}, 
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


char*
char_get_name_as_code(char *code, size_t code_len)
{
    int cnt = ARR_LEN(char_name_tb);
    int len = 0;
    for (int i = 0; i < cnt; i++) {

	len = strlen(char_name_tb[i].name);
	if (len > code_len) continue;

	debug("%s \n", char_name_tb[i].name);
	
	if (ml_util_strbufcmp(char_name_tb[i].name, code, code_len)) {

	    return (char*)char_name_tb[i].name;
	}
    }

    return NULL;
}
    

