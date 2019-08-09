


#include "ast_tree.h"

#include "debug.h"

#include "error.h"

#include "mem.h"

#include "chars.h"

#include "util.h"



static tr_node_s *lex_tr_root = NULL;
static hash_table_s *char_htab = NULL;
static hash_table_s *kw_htab = NULL;

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
set_char_htab(hash_table_s *htab)
{
    char_htab = htab;
}


hash_table_s* 
get_char_htab(void)
{
    return char_htab;
}


bool
ast_is_character(char *s, int len)
{
    htab_entry_s item;

    //func_s();

    memset(&item, 0, sizeof(htab_entry_s));
    
    item.key = ml_util_buf2str(s, len);
    if (!item.key) return false;

    ml_util_show_buf(item.key, len);
    
    debug("key: %s \n", item.key);
    
    if (hsearch(char_htab, item, FIND)) {

	ml_free(item.key);
	
	return true;
    }

    //func_fail();

    ml_free(item.key);
    return false;
}



void 
set_kw_htab(hash_table_s *htab)
{
    kw_htab = htab;
}


hash_table_s* 
get_kw_htab(void)
{
    return kw_htab;
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
 
    if (!char_htab) return 0;
    //if (!kw_htab) return 0;

    /*  
	if (is_like_keyword(key)) {
	memset(&item, 0, sizeof(item));
	item.key = key;

	rti = hsearch(kw_htab, item, FIND);
	if (rti) return 1;
	}
    */

    //func_s();

    //debug("%s \n", key);

    //memset(&item, 0, sizeof(item));
    //item.key = key;
    //rti = hsearch(char_htab, item, FIND);
    //if (rti) return 1;

    if (!lex_tr_root) return 0;
  
    root = search_key(lex_tr_root, key);
    if (!root) return 0;
  
    return 1;
}


static tr_node_s*
search_end(tr_node_s *root)
{
    tr_node_s *rtn;
  
    if (!root) return NULL;

    func_s();
  
    debug("%s \n", root->key);
  
    if (strlen(root->key) == 1) {
  
	if (*root->key != '@') return NULL;
	if (root->right || root->left || root->sub) return NULL;
    
	debug("@e. \n");
	return root;
    }
  
    if (root->sub) {
	debug("sub %s \n", root->sub->key);
	rtn = search_end(root->sub);
	if (rtn) return rtn;
    }
  
    if (root->left) {
	debug("left %s \n", root->left->key);
	rtn = search_end(root->left);
	if (rtn) return rtn;
    } 
  
    if (root->right) {
	debug("right %s \n", root->right->key);
	rtn = search_end(root->right);
	if (rtn) return rtn;
    }
  
    return NULL;
}


static tr_node_s*
search_graph(tr_node_s *root, char *s, char *e)
{
    tr_node_s *rtn;
  
    if (!root) return NULL;

    debug("%s, %s %c \n", __func__, root->key, *s);
    if (root->is_outside_loop_node) {

	debug("pattern: {} in %s \n", root->key);
    }
  
    if (strlen(root->key) == 1 && *root->key != '@') {
  
	if (*root->key != *s) return NULL;
    
	debug("found %c \n", *s);
    
	s++;
    
	/* the end char, check is the end node.
	 */
	if (s > e) {
    
	    debug("all chars found. \n");
      
	    if (root->sub || root->left || root->right) {
      
		rtn = search_end(root->sub);
		if (rtn) return root;
        
		rtn = search_end(root->left);
		if (rtn) return root;
        
		rtn = search_end(root->right);
		if (rtn) return root;
        
		return NULL;
	    }
	    debug("@e. \n");
	    return root;
	}
    
	debug("try find %c \n", *s);

    
	if (root->back) {
	    debug("go to back-node: %s %x \n", root->back->key, root);
	    rtn = search_graph(root->back, s, e);
	    if (rtn) return rtn;
      
	    debug("not found %c \n", *s);	    
	}
	else {

	    debug("%s %x has not back-node. \n", root->key, root);
	}

	
	if (root->sub) debug("root sub %s \n", root->sub->key);
	if (root->left) debug("root left %s \n", root->left->key);
	if (root->right) debug("root right %s \n", root->right->key);
    }

    if (root->sub) {
  
	debug("sub %s \n", root->sub->key);
	root->next = root->sub;
	rtn = search_graph(root->sub, s, e);
	if (rtn) return rtn;
    }
  
    if (root->left) {

      
	debug("left %s \n", root->left->key);

	if (!strcmp(root->key, root->left->key)) {

	    debug("The same key: %s, the end \n", root->key);
	    //return root;
	}
	

	if (root->left->left) {
	    debug("left-left %s \n", root->left->left->key);
	}
    
	root->next = root->left;
	rtn = search_graph(root->left, s, e);
	if (rtn) return rtn;
    }
  
    if (root->right) {
  
	debug("right %s \n", root->right->key);
	root->next = root->right;
	rtn = search_graph(root->right, s, e);
	if (rtn) return rtn;
    }
  
    root->next = NULL;
    return NULL;
}


int
ast_lex_debug(void)
{
    int rt;
    tr_node_s *nd;
    char *code;
    char *s, *e;

    char* cases[] = {

	"-12.",
	"-1234567890",
	"24680",
	"1234/56789",
	"-34/67",	
    };

    if (!lex_tr_root) return 0;
    
    func_s();


    for (int i = 0; i < ARR_LEN(cases); i++) {
	
	code = cases[i];
	s = code;
	e = s + strlen(s) - 1;
	nd = search_graph(lex_tr_root, s, e);
	if (!nd) {

	    debug("not found a solution for code: %s \n", code);
	    goto FAIL;
	}
	else {

	    debug("solution found for code: %s \n", code);
	}

    }
    
    func_ok();
    return 1;

  FAIL:
    func_fail();
    return 0;
}
