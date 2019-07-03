
#include "stack.h"

#include "debug.h"

#include "error.h"

#include "mem.h"

#define ALIGN_LEN sizeof(stack_t)


static ml_stack_t *m_stack = NULL;
static size_t m_stack_sz = 0;
static ml_stack_t *m_stack_pos = NULL;

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
stack_push(void *data_in, size_t data_sz)
{
    size_t len = data_sz*sizeof(ml_stack_t);

    func_s();
    
    if (m_stack_top >= MAX_STACK_ITEM_CNT) {

	debug_err("err: stack db overflow \n");

	ml_err_signal(ML_ERR_STACK_OVERFLOW);
	return STACK_ERR_OVERFLOW;	
    }
  
    if (len > (m_stack_sz - (size_t)(m_stack_pos - m_stack))) {

	debug_err("err: stack overflow \n");
	ml_err_signal(ML_ERR_STACK_OVERFLOW);
	return STACK_ERR_OVERFLOW;
    }

    
    memcpy(m_stack_pos, data_in, len);

    m_db[m_stack_top].data = m_stack_pos;
    m_db[m_stack_top].data_sz = len;
    
    m_stack_pos += len;
    m_stack_top++;

    debug("in size: %d, m_stack_top: %d \n", len, m_stack_top);
    
    func_ok();
    return STACK_OK;
}


stack_rt_t
stack_pop(void *data_out)
{
    func_s();
    
    if (!data_out) return STACK_ERR_NULL;
    
    if (m_stack_top <= 0) {
	
	debug_err("err: nothing in the stack \n");
	return STACK_ERR;
    }

    size_t sz = m_db[m_stack_top-1].data_sz;
    memcpy(data_out, m_db[m_stack_top-1].data, sz);

    m_stack_pos -= sz;
    m_stack_top--;

    debug("out size: %d, m_stack_top: %d \n", sz, m_stack_top);
    
    func_ok();
    return STACK_OK;
}





