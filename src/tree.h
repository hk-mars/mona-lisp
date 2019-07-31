
#ifndef _TREE_H_
#define _TREE_H_


#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#define TREE_DBG_ENABLE 1

typedef struct tr_node 
{
  struct tr_node *father;
  struct tr_node *left;
  struct tr_node *right;
  struct tr_node *sub;
  struct tr_node *next;
  struct tr_node *back;
  struct tr_node *loop;
  
  char *key;
  unsigned char is_inside_loop_node;
  unsigned char is_outside_loop_node;
  unsigned char is_sub_bnf;
  unsigned char is_token;
  unsigned char is_in_syntax_tree;
} tr_node_s;


tr_node_s* tree_insert_left(tr_node_s *root, char *key);
tr_node_s* tree_insert_right(tr_node_s *root, char *key);
tr_node_s* tree_insert_sub(tr_node_s *root, char *key);


#endif /* _TREE_H_ */

