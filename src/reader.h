

#ifndef ML_READER_H
#define ML_READER_H


#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>



typedef enum
{
    READER_OK = 0,
    READER_ERR = 1,
    READER_ERR_NULL = 2,
    
    
} reader_rt_t;



reader_rt_t ml_reader_init(void);


reader_rt_t ml_reader_load_file(const char *fname);


#endif /* ML_READER_H */


