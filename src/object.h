

#ifndef ML_OBJECT_H
#define ML_OBJECT_H


#include <stdint.h>
#include <stdio.h>



typedef struct
{
    
} number_s;


typedef struct
{

} symbol_s;


typedef union
{
    number_s num;
    symbol_s sym;
    
} object_s;



#endif /* ML_OBJECT_H */


