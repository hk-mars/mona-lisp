

#ifndef ML_SYNTAX_H
#define ML_SYNTAX_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


#include "config.h"

#include "form.h"

#include "tree.h"
#include "hsearch.h"


typedef enum
{
    SYNTAX_OK = 0,
    SYNTAX_ERR = 1,
    SYNTAX_ERR_FUNC = 2,
    SYNTAX_ERR_ARG1 = 3,
    SYNTAX_ERR_ARG2 = 4,
    SYNTAX_ERR_ARG3 = 5,

    
} syntax_rt_t;


syntax_rt_t syntax_init(void);

syntax_rt_t syntax_check(form_s *form);


int create_syntax_htab(int cnt);
hash_table_s* get_syntax_htab(void);
int push_syntax_htab(char *key, tr_node_s *root);
htab_entry_s* pop_syntax_htab(char *key);

#endif /* ML_SYNTAX_H */

