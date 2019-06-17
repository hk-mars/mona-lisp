

#include "reader.h"

#include "debug.h"

#include "lex.h"



/**
 * The purpose of the Lisp reader is to accept characters, interpret them as the printed 
 * representation of a Lisp object, and construct and return such an object.
 *
 * The reader cannot accept everything that the printer produces; for example, the printed 
 * representations of compiled code objects cannot be read in.
 * The reader is also parameterized in such a way that it can be used as a lexical analyzer for
 * a more general user-written parser.
 */


/**
 * The reader is organized as a recursive-descent parser. Broadly speaking, the reader operates 
 * by reading a character from the input stream and treating it in one of three ways:
 *
 * 1. Whitespace characters serve as separators but are otherwise ignored.
 * 2. Constituent and escape characters are accumulated to make a token, which is then 
 *    interpreted as a number or symbol.
 * 3. Macro characters trigger the invocation of functions (possibly user-supplied) that can 
 *    perform arbitrary parsing actions, including recursive invocation of the reader.
 */
