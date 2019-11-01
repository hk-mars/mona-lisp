
#ifndef __BIN_TREE_H__
#define __BIN_TREE_H__

#include <string.h>
#include <stdlib.h>


typedef
enum
{
    SEARCH = 0,
    INSERT = 1
} BIN_TREE_ACTION;

typedef
struct bin_tree_node
{
    struct bin_tree_node *left;
    struct bin_tree_node *right;
    struct bin_tree_node *father;
    void *key;
    void *val;
    int mark;
}
    s_bin_tree_node;

typedef int (*cmp_func) (void*, void*);

s_bin_tree_node* binary_tree_isearch(int ins_flag,
				     s_bin_tree_node **root, 
				     void *key, 
				     int key_sz, 
				     cmp_func cmp);

int binary_tree_delete(s_bin_tree_node **root, 
		       s_bin_tree_node *node);

int int_cmp(void* lv, void* rv);
int long_cmp(void* lv, void* rv);

#endif /* __BIN_TREE_H__ */

