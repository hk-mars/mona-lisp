

/**
 * memory garbage collector.
 */
 

#include "gc.h"

#include "bin_tree.h"

#include "debug.h"

#include "mem.h"

#include "error.h"


#define GC_DEBUG_DISABLE true
#if GC_DEBUG_DISABLE					
#define debug(...) ;
#endif					



typedef struct
{
    void *addr;
    size_t size;
    
} gc_entry;



#define MAX_GC_TREE_CNT 50


/*
 * @gc_id => @gc_node(@id, @mm_tree) => @gc_tree,
 * @mm_tree => @mm_node(@mm_id, @mark).
 */

static long m_gc_id;
static s_bin_tree_node *m_gc_tree;
static long m_cur_gc_id;
static gc_s m_gc;


static gc_status_s m_gc_status[MAX_GC_TREE_CNT] = { 0 };


gc_s
gc_new(void)
{
    s_bin_tree_node *nd;
  
    fs();

    m_gc_id++;

    nd = binary_tree_isearch(INSERT, &m_gc_tree, &m_gc_id, sizeof(int), long_cmp);
    if (!nd) goto FAIL;
  
    debug("gc_id [%d].\n", m_gc_id);

    m_cur_gc_id = m_gc_id;

    m_gc_status[m_cur_gc_id-1].block_cnt = 0;
    m_gc_status[m_cur_gc_id-1].all_blocks_size = 0;
    
    m_gc.id = m_gc_id;
    memset(&m_gc.st, 0, sizeof(gc_status_s));
  
    out(ok, m_gc);

 FAIL:
    m_gc.id = -1;
    out(fail, m_gc);
}


void*
gc_malloc(size_t size)
{
    void *p;
    s_bin_tree_node *nd, *root;
    
    long mm_id;

    fs();

    debug("cur_gc_id [%d].\n", m_cur_gc_id);
    nd = binary_tree_isearch(SEARCH, &m_gc_tree, &m_cur_gc_id, 0, long_cmp);
    if (!nd) return NULL;

    gc_entry *e = malloc(sizeof(gc_entry));
    if (!e) goto FAIL;
    memset(e, 0, sizeof(gc_entry));
    
    p = malloc(size);
    if (!p) return NULL;
    memset(p, 0, size);

    e->addr = p;
    e->size = size;

    s_bin_tree_node **nd_addr = &nd->val;
    nd = binary_tree_isearch(INSERT, &nd->val, &e, sizeof(void*), long_cmp);
    if (!nd) goto FAIL;
    nd->val = e;

    debug("new gc entry 0x%x \n", e);
    debug("malloc 0x%x, size: %d \n", p, size);

    s_bin_tree_node *rt_nd;
    rt_nd = binary_tree_isearch(SEARCH, nd_addr, &e, sizeof(void*), long_cmp);
    if (!rt_nd) {

	debug_err("key 0x%x, val 0x%x\n", nd->key, nd->val);
	ml_err_signal(ML_ERR_NULL);
	goto FAIL;
    }
    

    m_gc_status[m_cur_gc_id-1].block_cnt++;
    m_gc_status[m_cur_gc_id-1].all_blocks_size += e->size;

    m_gc.st.block_cnt++;
    m_gc.st.all_blocks_size += e->size;    
  
    out(ok, p);

  FAIL:
    out(fail, NULL);
}


static void
free_mm_tree(s_bin_tree_node *root)
{
  
    if (!root) return;

    root->left ? free_mm_tree(root->left) : 0;
    root->right ? free_mm_tree(root->right) : 0;

    gc_entry *e = (gc_entry*)root->val;

    debug("free gc entry 0x%x \n", e);
    free(e);

    m_gc_status[m_cur_gc_id-1].block_cnt--;
    m_gc_status[m_cur_gc_id-1].all_blocks_size -= e->size;

    m_gc.st.block_cnt--;
    m_gc.st.all_blocks_size -= e->size;    
  
    binary_tree_delete(&root, root);

    return;


  FAIL:
    ml_err_signal(ML_ERR_NULL);
}


void
gc_free(void)
{
    s_bin_tree_node *nd;
  
    fs();

    nd = binary_tree_isearch(SEARCH, &m_gc_tree, &m_cur_gc_id, 0, long_cmp);
    if (!nd) return;

    free_mm_tree(nd->val);

    binary_tree_delete(&m_gc_tree, nd);

    gc_show();
    
    m_cur_gc_id = --m_gc_id;   
  
    fe();
}



void
gc_show(void)
{
    func_s();
    
    if (m_cur_gc_id <= 0) return;

    debug_err("gc block count: %d \n", m_gc_status[m_cur_gc_id-1].block_cnt);
    debug_err("gc all blocks size: %d \n", m_gc_status[m_cur_gc_id-1].all_blocks_size);
    
}


bool
gc_is_valid(void)
{
    return (m_gc_id > 0);
}


bool
gc_debug(void)
{
    fs();

    gc_s gc = gc_new();
    if (gc.id < 0) goto FAIL;

    for (int i = 0, j = 10; i < 30; i++, j += 10) {

	debug("i: %d \n", i);
	gc_malloc(j);
    }

    gc_show();
    
    gc_free();
    
    out(ok, true);

 FAIL:
    out(fail, false);
}



