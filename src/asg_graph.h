

#ifndef _ASG_GRAPH_H_
#define _ASG_GRAPH_H_

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "tree.h"



void asg_show_redundant_node(tr_node_s *root, int *count);

void asg_reduce_redundant_node(tr_node_s *root, int *count);



#endif /* _ASG_GRAPH_H_ */

