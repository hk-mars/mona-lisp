

/**
 * The rules of grammer and syntax.
 *
 * 1. A rule should be made by combining the elements of tokens and keywords.
 * 2. A rule defines the determined paths as solutions for a stentence of BNF syntax.
 * 3. A good rule system should include some core atom rules to be extendable from bottom-up.
 * 4. A good rule system should include less elements and some redefinable elements.
 * 5. I think the rules of common lisp includes above advantages.
 * 
 */



#ifndef ML_RULES_H
#define ML_RULES_H


#include "config.h"



/* Tips:
 * 1. The elements defined here MUST be consistent with the ones in monalisp1.0_syntax.txt
 * 2. Store the syntax objects in the codes(as consts) as to reduce the use of dynamic memory.
 *
 */


/* syntax elements
 */
#define UNDEFINED  "undefined"
#define SYMBOL_TOKEN  UNDEFINED
#define TOKEN  "token ::="



const char* rule_match_element(char *name, int len);

#endif /* ML_RULES_H */


