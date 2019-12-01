

#include "test_cases.h"

#include "../debug.h"

#include "../reader.h"

#include "../gc.h"

#include "../util.h"

#include "../mem.h"



#define TEST_CHAR_ON true
#define TEST_PRINTER_ON true
#define TEST_LIST_ON true
#define TEST_EQ_ON true
#define TEST_EQ_IF true


const char* TEST_CASES[] =
{  
    "test_cases/test.lisp",

    #if TEST_CHAR_ON 
    "test_cases/test_char.lisp",
    #endif
    
    #if TEST_PRINTER_ON
    "test_cases/test_printer.lisp",
    #endif
    
    #if TEST_LIST_ON
    "test_cases/test_list.lisp",
    "test_cases/test_list_car.lisp",
    "test_cases/test_list_cdr.lisp",
    "test_cases/test_list_car_cdr.lisp",
    #endif

    #if TEST_EQ_ON
    "test_cases/test_eq.lisp",    
    #endif

    #if TEST_EQ_IF
    "test_cases/test_if.lisp",
    #endif

    
};


bool
test_case_run(void)
{
    reader_s reader;
    reader_rt_t reader_rt;
    gc_s gc;

    func_s();

    for (int i = 0; i < ARR_LEN(TEST_CASES); i++) {
	
	gc = gc_new();
	if (gc.id < 0) {

	    debug_err("create gc object failed \n");
	    goto FAIL;
	}
    
	reader_rt = ml_reader_load_file(&reader, TEST_CASES[i]);
	if (reader_rt != READER_OK) goto FAIL;

	gc_show();
	gc_free();
	mm_show();
    }

    out(ok, true);

  FAIL:
    out(fail, false);
}
