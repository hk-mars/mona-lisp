
#include "tree.h"

#include "debug.h"

#include "mem.h"

#include "error.h"


static long m_node_cnt = 0;


static tr_node_s*  
get_father(tr_node_s *nd, char *key)
{
  tr_node_s *tmp;
  
  if (!nd) return 0;
  
  tmp = nd->father;
  if (tmp) tmp->next = nd;
  nd = tmp;
  while (nd) {
  
    if (strcmp(nd->key, key) == 0) {
    
      if (nd->sub) {
        if (nd->sub == nd->next) {
        
#if TREE_DBG_ENABLE
          debug("loop node: %s \n", nd->key);
#endif
          return nd;
        }
      }
    }
    
    tmp = nd->father;
    if (tmp) tmp->next = nd;
    nd = tmp;
  }
  
  return NULL;
}


tr_node_s* 
tree_insert_left(tr_node_s *root, char *key)
{
  tr_node_s *nd;
  
#if TREE_DBG_ENABLE
  
  fs();
  
  if (root) {
    if (strlen(root->key) < 100) {
      debug("father:\n%s\nleft:\n%s\n", root->key, key);
    }
    else {
      debug("father:\n%s\nleft:\n%s\n", "......", "......");
    }
  }
  else {
    debug("father:\n%s\nleft:%s\n", "NULL", key);
  }
#endif
  
  if (!key) return NULL;
  
  nd = (tr_node_s*)ml_malloc(sizeof(tr_node_s));
  if (!nd) return NULL;
  memset(nd, 0, sizeof(tr_node_s));
  nd->father = root;
  nd->key = key;	
  nd->loop = get_father(root, key);
  if (nd->loop) {
      set_inside_loop_node(nd);
  }
  
  if (root) root->left = nd;

  m_node_cnt++;
  
#if TREE_DBG_ENABLE  
  func_ok();
#endif
  
  return nd;
}


tr_node_s* 
tree_insert_right(tr_node_s *root, char *key)
{
  tr_node_s *nd;
  
#if TREE_DBG_ENABLE
  
  fs();
  
  if (root) {
    if (strlen(root->key) < 100) {
      debug("father:\n%s\nright:\n%s\n", root->key, key);
    }
    else {
      debug("father:\n%s\nright:\n%s\n", "......", "......");
    }
  }
  else {
    debug("father:\n%s\nright:%s\n", "NULL", key);
  }
#endif
  
  if (!key) return NULL;
  
  nd = (tr_node_s*)ml_malloc(sizeof(tr_node_s));
  if (!nd) return NULL;
  memset(nd, 0, sizeof(tr_node_s));
  nd->father = root;
  nd->key = key;	
  nd->loop = get_father(root, key);
  if (nd->loop) {
      set_inside_loop_node(nd);
  }
  
  if (root) root->right = nd;

  m_node_cnt++;
  
 #if TREE_DBG_ENABLE  
  func_ok();
 #endif
  
  return nd;
}


tr_node_s* 
tree_insert_sub(tr_node_s *root, char *key)
{
  tr_node_s *nd;
  
#if TREE_DBG_ENABLE
  
  fs();
  
  if (root) {
    if (strlen(root->key) < 100) {
      debug("father:\n%s\nsub:\n%s\n", root->key, key);
    }
    else {
      debug("father:\n%s\nsub:\n%s\n", "......", "......");
    }
  }
  else {
    debug("father:\n%s\nsub:%s\n", "NULL", key);
  }
#endif
  
  nd = (tr_node_s*)ml_malloc(sizeof(tr_node_s));
  if (!nd) return NULL;
  memset(nd, 0, sizeof(tr_node_s));
  nd->father = root;
  nd->key = key;	
  nd->loop = get_father(root, key);
  if (nd->loop) {
      set_inside_loop_node(nd);
  }
  
  if (root) root->sub = nd;

  m_node_cnt++;
  
#if TREE_DBG_ENABLE  
  func_ok();
#endif  
  return nd;
}


void
tree_show_node(tr_node_s *node)
{
    if (!node) return;

    debug("node: %s \n", node->key);
    if (node->left) debug("  left: %s \n", node->left->key);
    if (node->right) debug("  right: %s \n", node->right->key);
    if (node->sub) debug("  sub: %s \n", node->sub->key);


    if (node->back) {
	debug("  back-node: %s\n", node->back->key);
    }

    if (node->loop) {
	debug("  loop-node: %s\n", node->loop->key);
    }
}



void
tree_show(tr_node_s *root, int dep)
{
    if (!root) return;
    if (dep <= 0) return;
       
    debug("%*c%d%s  ", dep, ' ', dep, root->key);
    
    if (is_outside_loop_node(root)) {
	debug("{}%s  ", is_more_plus_node(root) ? "+" : "*");
    }
    
    if (root->loop) debug("loop ");
    
    debug("\n");

    if (root->left) {
	debug("L: ");
	tree_show(root->left, dep-1);
    }

    if (root->right) {
	debug("R: ");
	tree_show(root->right, dep-1);
    }

    if (root->sub) {
	debug("S: ");
	tree_show(root->sub, dep-1);
    }

    if (!root->left && !root->right && !root->sub) {
	debug("nil\r\n");
    }
}


void
tree_show_node_cnt(tr_node_s *root)
{
    debug("m_node_cnt: %ld \n", m_node_cnt);
}


void
tree_show_node_size(void)
{
  debug("node size: %ld \n", sizeof(tr_node_s));
}


void
tree_show_info(void)
{
    tree_show_node_cnt(NULL);
    tree_show_node_size();

    debug("tree size: %ld \n", sizeof(tr_node_s)*m_node_cnt);
}



