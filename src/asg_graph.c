

#include "asg_graph.h"

#include "debug.h"

#include "error.h"

#include "tree.h"

#include "util.h"

#include "mem.h"




static tr_node_s*
get_single_node(tr_node_s *root)
{
    
    if (root->left && !root->right && !root->sub) {

	return root->left;
    }

    if (!root->left && root->right && !root->sub) {

	return root->right;
    }

    if (!root->left && !root->right && root->sub) {
	
	return root->sub;
    }

    return NULL;
}


//static int m_count = 0;

void
asg_show_redundant_node(tr_node_s *root, int *count)
{
    tr_node_s *nd;
    
    if (!root) return;

    nd = NULL;

    //if (*count == 0) m_count = 0;

    
    if (root->loop) return;
    
    nd = get_single_node(root);
    if (nd) {

	unsigned long len = strlen(nd->key);
	if (len > strlen(" ::=")) {

	    len -= strlen(" ::=");
	    if (!strcmp(nd->key+len, " ::=")) {

		goto NEXT;
	    }
	}

	if (is_outside_loop_node(nd)) goto NEXT;
	if (nd->status != 0x0) goto NEXT;
	
	tr_node_s *nd_down = get_single_node(nd);
	if (nd_down) {
	    (*count)++;

	    debug("key: %s \n", nd->key);
	    
	    debug("count: %d \n", *count);
	}
    }

 
  NEXT:
    if (root->sub) {
	//m_count++;
	
	asg_show_redundant_node(root->sub, count);
    }
   
    if (root->left) {
	//m_count++;
	asg_show_redundant_node(root->left, count);
    }

    if (root->right) {
	//m_count++;
	asg_show_redundant_node(root->right, count);
    }    


    if (is_outside_loop_node(root)) return;    
}


void
asg_reduce_redundant_node(tr_node_s *root, int *count)
{
    tr_node_s *nd;
    
    if (!root) return;

    nd = NULL;

    if (root->loop) return;
    
    

    nd = get_single_node(root);
    if (nd) {
	
	unsigned long len = strlen(nd->key);
	if (len > strlen(" ::=")) {

	    len -= strlen(" ::=");
	    if (!strcmp(nd->key+len, " ::=")) {

		goto NEXT;
	    }
	}

	if (is_outside_loop_node(nd)) goto NEXT;
	if (nd->status != 0x0) goto NEXT;
	
	tr_node_s *nd_down = get_single_node(nd);
	if (nd_down) {

	    debug("redundant node: %s \n", nd->key);
	    
	    root->left = NULL;
	    root->right = NULL;
	    root->sub = NULL;
	    
	    if (nd->left) {
		root->left = nd->left;
		nd->left->father = root;

		(*count)--;
	    }
	    
	    if (nd->right) {
		root->right = nd->right;
		nd->right->father = root;

		(*count)--;
	    }
	    
	    if (nd->sub) {
		root->sub = nd->sub;
		nd->sub->father = root;
		(*count)--;
	    }

	    //TBD: free @nd node
	    
	    debug("all count: %d \n", *count);
	    
	}
    }

  NEXT:    
    if (root->sub) {

	asg_reduce_redundant_node(root->sub, count);
    }
   
    if (root->left) {
	asg_reduce_redundant_node(root->left, count);
    }

    if (root->right) {


	asg_reduce_redundant_node(root->right, count);
    }    


    if (is_outside_loop_node(root)) return;
}





