
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

  unsigned char status;

  bool mark;

} tr_node_s;


#define is_inside_loop_node(node)		\
  ((node->status) & 0x80)

#define set_inside_loop_node(node)		\
  (node->status) |= 0x80

#define is_outside_loop_node(node)		\
  ((node->status) & 0x40)

#define mark_outside_loop_node(node)		\
  (node->status) |= 0x40

#define is_token_node(node)			\
  ((node->status) & 0x20)

#define mark_token_node(node)			\
  (node->status) |= 0x20

#define is_in_syntax_tree(node)			\
  ((node->status) & 0x10)

#define mark_in_syntax_tree(node)		\
  (node->status) |= 0x10
  
#define is_more_plus_node(node)			\
  ((node->status) & 0x08)

#define mark_more_plus_node(node)		\
  (node->status) |= 0x08 

#define is_keyword_node(node)			\
  ((node->status) & 0x04)

#define mark_keyword_node(node)			\
  (node->status) |= 0x04

#define is_char_node(node)			\
  ((node->status) & 0x02)

#define mark_char_node(node)			\
  (node->status) |= 0x02 


tr_node_s* tree_insert_left(tr_node_s *root, char *key);
tr_node_s* tree_insert_right(tr_node_s *root, char *key);
tr_node_s* tree_insert_sub(tr_node_s *root, char *key);

void tree_show_node(tr_node_s *node);

void tree_show(tr_node_s *root, int dep);

void tree_show_node_cnt(tr_node_s *root);

void tree_show_node_size(void);

void tree_show_info(void);

#endif /* _TREE_H_ */

