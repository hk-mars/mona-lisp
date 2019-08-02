


#include "ast_tree.h"

#include "debug.h"

#include "error.h"

#include "mem.h"

#include "chars.h"


static tr_node_s *lex_tr_root;
static hash_table_s *kw_htab;

void 
set_lex_tree(tr_node_s *root)
{
  lex_tr_root = root;
}


tr_node_s*
get_lex_tree(void)
{
  return lex_tr_root;
}

void 
set_kw_htab(hash_table_s *htab)
{
  kw_htab = htab;
}


static void
show_buf(char *buf, int size)
{
  debug("%d bytes:\n", size);
  buf += (size - 1);
  while(--size >= 0) debug("%c", *(buf - size));
  debug("\n");
}


static void
show_path(tr_node_s *s)
{
  fs();
  
  while(s) {
    debug("%s \n", s->key);
    s = s->next;
  }
  
  fe();
}


static int 
get_tk_key(char *s, char *e, char **ss, char **ee)
{
  while (s <= e)  {
  
    if (*s == ' ' || *s == '\r' || *s == '\n') {
      s++;
      continue;
    }
    if (s == e) return 0;
    
    *ee = memchr(s, ' ', e - s + 1);
    if (!*ee) {
      *ee = memchr(s, '\r', e - s + 1);
      if (!*ee) return 0;	
    }
    
    (*ee)--;
    *ss = s;
    return 1;
  }
  
  return 0;
}


static tr_node_s*
search_end(tr_node_s *root)
{
  tr_node_s *rtn;
  
  if (!root) return NULL;
  
  //debug("%s \n", root->key);
  
  if (strlen(root->key) == 1) {
  
    if (*root->key != '@') return NULL;
    if (root->right || root->left || root->sub) return NULL;
    
    //debug("@e. \n");
    return root;
  }
  
  if (root->sub) {
    //debug("sub %s \n", root->sub->key);
    rtn = search_end(root->sub);
    if (rtn) return rtn;
  }
  
  if (root->left) {
    //debug("left %s \n", root->left->key);
    rtn = search_end(root->left);
    if (rtn) return rtn;
  } 
  
  if (root->right) {
    //debug("right %s \n", root->right->key);
    rtn = search_end(root->right);
    if (rtn) return rtn;
  }
  
  return NULL;
}


int 
is_like_keyword(char *s)
{ 
  char *e;
  
  if (!s) return 0;
  
  e = s + strlen(s) - 1;
  if (s == e) return (*s == 'C');
  
  while (s <= e) {
    if (!is_latin_ch(*s) && *s != '_' && *s != '-') return 0;
    s++;
  }
  
  return 1;
}


static tr_node_s*
search_key(tr_node_s *root, char *key)
{
  tr_node_s *rtn;
  
  if (!root) return NULL;
  if (root->is_outside_loop_node) return NULL;
  if (strlen(root->key) == 1) return NULL;
  
  if (!root->left && !root->right && root->sub) {
  
    if (strcmp(root->key, key) == 0) {
      //debug("in token tree, key found: %s \n", key);
      return root;
    }
  }
  
  if (root->sub) {
  
    //debug("sub %s \n", root->sub->key);
    rtn = search_key(root->sub, key);
    if (rtn) return rtn;
  }
  
  if (root->left) {
  
    //debug("left %s \n", root->left->key);
    rtn = search_key(root->left, key);
    if (rtn) return rtn;
  } 
  
  if (root->right) {
  
    //debug("right %s \n", root->right->key);
    rtn = search_key(root->right, key);
    if (rtn) return rtn;
  }
  
  return NULL;
}


int
is_token(char *key)
{
  tr_node_s *root;
  ENTRY item, *rti;
  
  if (!lex_tr_root) return 0;
  if (!kw_htab) return 0;

  /*  
  if (is_like_keyword(key)) {
    memset(&item, 0, sizeof(item));
    item.key = key;
    rti = hsearch(kw_htab, item, FIND);
    if (rti) return 1;
  }
  */

  
  root = search_key(lex_tr_root, key);
  if (!root) return 0;
  
  
  return 1;
}


