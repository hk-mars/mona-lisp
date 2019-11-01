

#include "mem.h"

#include "debug.h"

#include "error.h"

#include "gc.h"



#define MAX_MEM_CNT 200


typedef struct
{
    void *addr;
    size_t sz;
} mem_s;


typedef struct
{
    long block_cnt;
    long blocks_size;

    mem_s mem[MAX_MEM_CNT];
    
} mm_status_s;


static mm_status_s m_mm_st = { 0 };


void
mm_show(void)
{
    func_s();

    debug("mem block count: %ld \n", m_mm_st.block_cnt);
    debug("mem blocks size: %ld \n", m_mm_st.blocks_size);    
}


void*
ml_malloc(size_t sz)
{
    void *m;

    if (gc_is_valid()) {

	m = gc_malloc(sz);
    }
    else {
      
	//m = malloc(sz);
	m = mm_malloc(sz);
    }
  
    if (!m) {
      
	ml_err_signal(ML_ERR_MEM_FULL);

	return NULL;
    }

    //debug("sz: %d \n", sz);
    memset(m, 0, sz);
    
    return m;
}


void
ml_free(void *mem)
{   
    if (!mem) {

	debug_err("%s, @mem is NULL", __func__);
	return;
    }

    
    /* TODO:
     * search this memeory block in the gc, if found then marks it as free.
     */
    //if (gc_search(mem)) {
    if (0) {

	//gc_mark_free(mem);
    }

    if (!gc_is_valid()) {

	mm_free(mem);
    }
    else {
    }
}


void*
mm_malloc(size_t sz)
{
    void *m = malloc(sz);
  
    if (!m) {
      
	ml_err_signal(ML_ERR_MEM_FULL);

	return NULL;
    }

    //debug("sz: %d \n", sz);
    memset(m, 0, sz);

    m_mm_st.block_cnt++;
    m_mm_st.blocks_size += sz;

    for (long i = 0; i < MAX_MEM_CNT; i++) {

	if (m_mm_st.mem[i].addr == 0) {
	    m_mm_st.mem[i].addr = m;
	    m_mm_st.mem[i].sz = sz;
	    break;
	}
    }
    
    return m;
}


void
mm_free(void *mem)
{   
    if (!mem) {

	debug_err("%s, @mem is NULL", __func__);
	return;
    }
   
    free(mem);


    for (long i = 0; i < MAX_MEM_CNT; i++) {

	if (m_mm_st.mem[i].addr == mem) {
	    m_mm_st.mem[i].addr = 0;
	    m_mm_st.block_cnt--;
	    m_mm_st.blocks_size -= m_mm_st.mem[i].sz;
	    m_mm_st.mem[i].sz = 0;
	    break;
	}
    }    
}


