

#include "stack.h"

#include "debug.h"

#include "error.h"

#include "mem.h"


#define ALIGN_LEN sizeof(stack_t)


static ml_stack_t *m_stack = NULL;
static size_t m_stack_sz = 0;
static size_t *m_stack_pos = NULL;

static size_t m_stack_top = 0;

static stack_data_t m_db[MAX_STACK_ITEM_CNT];



stack_rt_t
stack_init(size_t sz)
{
    func_s();

    sz = (sz + ALIGN_LEN) / ALIGN_LEN * ALIGN_LEN;

    m_stack = (ml_stack_t*)ml_malloc(sz);
    if (!m_stack) return STACK_ERR_INIT;   

    m_stack_sz = sz;
    m_stack_pos = m_stack;
    
    debug("init stack with %d bytes \n", sz);

    func_ok();
    return STACK_OK;
}


stack_rt_t
stack_push(void *data, size_t data_cnt)
{
    size_t len = data_cnt*sizeof(stack_t);

    if (m_stack_top >= MAX_STACK_ITEM_CNT) {

	debug_err("err: stack db overflow \n");

	ml_err_signal(ML_ERR_STACK_OVERFLOW);
	return STACK_ERR_OVERFLOW;	
    }
    
    
    if (len > (m_stack_sz - (m_stack_pos-m_stack))) {

	debug_err("err: stack overflow \n");
	ml_err_signal(ML_ERR_STACK_OVERFLOW);
	return STACK_ERR_OVERFLOW;
    }

    
    memcpy(m_stack_pos, data, len);

    m_db[m_stack_top].data = m_stack_pos;
    m_db[m_stack_top].data_sz = len;
    
    m_stack_pos += len;
    m_stack_top++;

    func_ok();
    return STACK_OK;
}


stack_rt_t
stack_pop(stack_data_t *mydata)
{
    if (!mydata) return STACK_ERR_NULL;
    
    if (m_stack_top <= 0) {
	
	debug_err("err: nothing in the stack \n");
	return STACK_ERR;
    }

    
    mydata->data = m_db[m_stack_top].data;
    mydata->data_sz = m_db[m_stack_top].data_sz;
    
    m_stack_pos -= m_db[m_stack_top].data_sz;
    m_stack_top--;

    func_ok();
    return STACK_OK;
}





