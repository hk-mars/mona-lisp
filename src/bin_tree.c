

#include "bin_tree.h"

#include "debug.h"


#define BT_DEBUG_DISABLE true
#if BT_DEBUG_DISABLE
#undef debug
#define debug(...) ;
#endif


/*
 * [Binary Tree Insert Search Algorithm]
 * Devised by C.M.Berners-Lee[Comp.J.2(1959),5].
 * Published by P.F.Windley[Comp.J.3(1960),84-88],and
 * A.D.Booth and A.J.T.Colin[Information and Control 3(1960),327-334]
 * and Thomas N.Hibbard[JACM 9(1962),13-28]. 
 */


s_bin_tree_node* 
binary_tree_isearch
(
    int ins_flag,
    s_bin_tree_node **root, 
    void *key, 
    int key_sz, 
    cmp_func cmp
)
{
    s_bin_tree_node *p;
    s_bin_tree_node *q;
  
  
    if (!root  || !key || !cmp) return NULL;
    
    p = *root;
  
    if (!p) {
	if (!ins_flag) return NULL;
	goto INSERT;
    }

    fs();

    //ins_flag ? debug("insert new \n") : debug("search \n");
    
    while (1) { 
  
	if (cmp(key, p->key) < 0) {
    
	    if (!p->left) break;
	    p = p->left; continue;
	}
	else if (cmp(key, p->key) > 0) {
    
	    if (!p->right) break;
	    p = p->right; continue;
	}
	else {
    
	    debug("found. \n");
	    if (ins_flag) return NULL;
	    goto END;
	}
    }

    if (!ins_flag) return NULL;
       
  INSERT:
    debug("insert. \n");
  
    q = (s_bin_tree_node*)malloc(sizeof(s_bin_tree_node));
    if (!q) return NULL;
    memset(q, 0, sizeof(s_bin_tree_node));

    debug("malloc node: %x \n", q);
  
    q->key = malloc(key_sz);
    if (!q->key) {
	free(q);
	return NULL;
    }
    memcpy(q->key, key, key_sz);

    debug("malloc key %x \n", q->key);
    
  
    q->left = q->right = q->father = NULL;
  
    if (!p) {
	*root = q;
    }
    else {
	if (cmp(key, p->key) < 0) {
	    p->left = q;
	}
	else if (cmp(key, p->key) > 0) {
	    p->right = q;
	}
	else {
	}
    }
  
    q->father = p;
    p = q;
  
  END:
    func_ok();
    return p;
}


int 
binary_tree_delete
(
    s_bin_tree_node **root, 
    s_bin_tree_node *node
)
{
    s_bin_tree_node *t, *r, *s, *q;
  
    if (!root) return 0;

    fs();
  
    t = node;
    if (!t->right) {
  
	if (t->left) t->left->father = t->father;
	q = t->left;
    }
    else {
  
	r = t->right;
	if (!r->left) {
    
	    r->left = t->left;
	    if (t->left) t->left->father = r;
	    r->father = t->father;
	    q = r;
	}
	else {
    
	    s = r->left;
	    while (s->left) {
		r = s;
		s = r->left;
	    }
      
	    if (s->right) s->right->father = r;
	    if (s) s->father = t->father;
	    if (t->left) t->left->father = s;
	    if (t->right) t->right->father = s;
      
	    s->left = t->left;
	    r->left = s->right;
	    s->right = t->right;
      
	    q = s;
	}
    }
  
    if (t->father) {
	if (t->father->left == t) {
	    t->father->left = q;
	}
	else if (t->father->right == t) {
	    t->father->right = q;
	}
	else {
	}
    }
    else {
	*root = q;
    }
  
    if (t->key) {
	debug("free key: %x \n", t->key);
	free(t->key);
    }
    
    if (t->val) {
	//debug("free node: %x \n", t->val);
	//free(t->val);
    }
    
    if (t) {
	debug("free node: %x \n", t);
	free(t);
    }
  
    return 1;
}


int 
int_cmp(void* lv, void* rv)
{
    int tmp_lv, tmp_rv;
  
    tmp_lv = *(int*)lv;
    tmp_rv = *(int*)rv;
  
    return (tmp_lv - tmp_rv);
}


int 
long_cmp(void* lv, void* rv)
{
    long tmp_lv, tmp_rv;
  
    tmp_lv = *(long*)lv;
    tmp_rv = *(long*)rv;

    debug("%x, %x \n", tmp_lv, tmp_rv);
    
    return (tmp_lv - tmp_rv);
}



#ifdef BINARY_TREE_DEBUG

int
main(int argc, char* argv[])
{
    int i;
    int cnt;
    int int_items[16] = { 503, 87, 512, 61, 908, 170, 897, 275, 
			  653, 426, 154, 509, 612, 677, 765, 703
    };
    int key;
    s_bin_tree_node *root;
    s_bin_tree_node *ret;
    int rt;

    debug("[Debug binary tree search].. \r\n");
  
    cnt = sizeof(int_items)/sizeof(int_items[0]);
  
    debug("src database:\r\n----\r\n");
    for (i = 0; i < cnt; ++i) {
	debug("%d ", int_items[i]);
    }
    debug("\r\n");
  
    root = NULL;
    for (i = 0; i < cnt; ++i) {
	debug("\r\n----\r\n");
    
	key = int_items[i];
    
	debug("insert key %d into tree... \r\n", key);
	ret = binary_tree_isearch(INSERT, &root, &key, sizeof(key), int_cmp);
	if (!ret) {
	    debug("failed. \r\n");
	}
	else {
	    debug("done. \r\n");
	}
    }
  
    debug("\r\n----\r\n");
    key = int_items[0];
    debug("search key %d... \r\n", key);
    ret = binary_tree_isearch(SEARCH, &root, &key, sizeof(key), int_cmp);
    if (!ret) debug("not found.\r\n");
  
  
    debug("\r\n----\r\n");
    key = int_items[cnt-1];
    debug("search key %d... \r\n", key);
    ret = binary_tree_isearch(SEARCH, &root, &key, sizeof(key), int_cmp);
    if (!ret) debug("not found.\r\n");
  
  
    debug("\r\n----\r\n");
    key = int_items[cnt/2];
    debug("search key %d... \r\n", key);
    ret = binary_tree_isearch(SEARCH, &root, &key, sizeof(key), int_cmp);
    if (!ret) debug("not found.\r\n");
  
    debug("\r\n----\r\n");
    key = 12345;
    debug("search key %d... \r\n", key);
    ret = binary_tree_isearch(SEARCH, &root, &key, sizeof(key), int_cmp);
    if (!ret) debug("not found.\r\n");
  
    debug("\r\n");
  
    for (i = 0; i < cnt; ++i) {
	debug("\r\n----\r\n");
  
	key = int_items[i];
  
	debug("delete key %d from tree... \r\n", key);
	ret = binary_tree_isearch(INSERT, &root, &key, sizeof(key), int_cmp);
	if (!ret) {
	    debug("not found. \r\n");
	    continue;
	}
    
	rt = binary_tree_delete(&root, ret);
	if (!rt) debug("deletion failed. \r\n");
    }
  
    if (!root) debug("tree is empty now. \r\n");
  
    debug("[Debug binary tree search]..done. \r\n");
  
  END:
    return 1;
}

#endif /* BINARY_TREE_DEBUG */



