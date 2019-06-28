

#ifndef ML_READER_H
#define ML_READER_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lex.h"


typedef enum
{
    READER_OK = 0,
    READER_ERR = 1,
    READER_ERR_NULL = 2,
    READER_ERR_LEX = 3,
        
} reader_rt_t;


typedef struct
{
    const char *name;
    FILE *f;
    int f_sz;
    char *buf;
    size_t buf_sz;
    char *buf_e;
  
} file_info_s;



typedef struct
{
    file_info_s f;

    code_s cd;

    lex_s lex;
    
} reader_s;



reader_rt_t ml_reader_init(void);


reader_rt_t ml_reader_load_file(reader_s *reader, const char *fname);


#endif /* ML_READER_H */


