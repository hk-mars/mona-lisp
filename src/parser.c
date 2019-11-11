

#include "parser.h"

#include "debug.h"

#include "error.h"

#include "hsearch.h"
#include "tree.h"
#include "chars.h"

#include "ast_tree.h"
#include "asg_graph.h"

#include "syntax.h"

#include "util.h"

#include "mem.h"

#include "rules.h"


/* unlink
 */
#include <unistd.h>


const char *BNF_OBJ_STRING = "::=";
static tr_node_s *bnf_tree_root;


typedef 
struct s_file_info
{
    const char *f_name;
    FILE *f;
    long f_sz;
    char *f_buf;
    long buf_sz;
    char *buf_e;
} file_info;


static int make_bnf_tree(tr_node_s *root, char *bnf, int size,
			 hash_table_s *htab, int dep);


static void
show_buf(char *buf, int size, file_info *fi)
{
    if (fi) {
	if (buf + size - 1 > fi->buf_e) return;
    }
  
    debug("%d bytes:\n", size);
    buf += (size - 1);
    while(--size >= 0) debug("%c", *(buf - size));
    debug("\n");
}


#if 0
static char* 
dump_trail(char *s, char *e)
{
    while(s <= e) {
	if (*e != ' ') break;
	e--;
    }
  
    if (s > e) return NULL;
  
    *(e+1) = '\0';
  
    return e;
}
#endif


static long 
get_file_size(FILE *f) 
{
    long cur;
    long size; 
  
    cur = ftell(f);
    fseek(f, 0L, SEEK_END); 
    size = ftell(f); 
    fseek(f, cur, SEEK_SET);
  
    return size; 
}

#if 0
static char* 
filter_buf(char *s, long sz, long *o_sz)
{
    char *e, *ss, *buf;
    unsigned char flag;
  
    buf = (char*)malloc(sz);
    if (!buf) return NULL;
  
    e = s + sz -1;	
    
    /* dump the head
     */
    while(s <= e) {
	if (*s != ' ' && *s != '\r' && *s != '\n') break;
	s++;
    }
  
    ss = s;
    while (s <= e) {
	if (*s == '\r' || *s == '\n') *s = ' ';
	s++;
    }
  
    s = ss;
    ss = buf;
    flag = 0;
    while (s <= e) {
  
	if (*s == ' ' && flag) {
	    s++;
	}
	else if (*s == ' ' && !flag){
	    flag = 1;
	    *ss++ = *s++;
	}
	else {
	    flag = 0;
	    *ss++ = *s++;
	}
    }
  
    *o_sz = ss - buf;
  
    return buf;
}


static void
filter_bnf_buf(file_info *fi)
{
    char *buf;
  
    buf = filter_buf(fi->f_buf, fi->buf_sz, &fi->buf_sz);
    if (!buf) return;
  
    free(fi->f_buf);
    fi->f_buf = buf;
    fi->buf_e = fi->f_buf + fi->buf_sz - 1;
}
#endif


int 
load_syntax_file(file_info *fi)
{
    fs();
  
    fi->f = fopen(fi->f_name,"rb");
    if (!fi->f) return 0;
  
    fi->f_sz = get_file_size(fi->f);
    debug("file %s opened, size %lu bytes.\n", fi->f_name, fi->f_sz);
  
    fi->f_buf = (char*)ml_malloc(fi->f_sz);
    if (!fi->f_buf) return 0;
  
    fi->buf_sz = fread(fi->f_buf, 1, fi->f_sz, fi->f);
    if (fi->buf_sz < fi->f_sz) {
	//free(fi->f_buf);
	fi->f_buf = NULL;
	return 0;
    }
  
    fi->buf_e = fi->f_buf + fi->buf_sz - 1;
  
    //filter_bnf_buf(fi);
  
    fe();
    return 1;
}


static int 
find_bnf_unit_len(char *dt, file_info *fi)
{
    char *s, *e;

    func_s();
  
    if (!dt) return 0;
  
    s = dt;
    e = fi->buf_e;

    //ml_util_show_buf(s, e-s+1);
  
    while (s++ < e){
	if (memcmp(s, BNF_OBJ_STRING, strlen(BNF_OBJ_STRING)) == 0) break;
    }

  
    while (--s > dt) {
	if (*s == ' ' || *s == '\r' || *s == '\n') continue;

	//debug("found \n");
	//ml_util_show_buf(dt, s-dt+1);
    
	break;
    }


    while (--s > dt) {
      
	if (*s == '\n') {

	    while (--s > dt) {

		if (*s != '\n') break;
	    }
	  
	    debug("found \n");
	    ml_util_show_buf(dt, s-dt+1);
	  
	    return (s-dt+1);
	};
    }
  

    return 0;
}


static int 
make_hs_entry(ENTRY *item, char *key, int ksz, char *dt, int dsz)
{   
    if (!key || ksz <= 0) return 0;

    memset(item, 0, sizeof(ENTRY));

#if 1
    char *s = key;
    int sz = ksz;
    while (--sz >= 0) {

	if (*s == '\r' || *s == '\n') *s = ' ';
	s++;
    }
#endif

    
    ml_util_show_buf(key, ksz);
    

    item->key = (char*)rule_match_element(key, ksz);
    if (item->key) {

	ml_util_fwrite("found.txt", item->key);
	//debug_suspend();
    }
    else {
	
	#if 1
	item->key = (char*)ml_malloc(ksz + 1);
	if (!item->key) return 0;

	memset(item->key, '\0', ksz + 1);
	memcpy(item->key, key, ksz);


	

	ml_util_fwrite("keys.txt", item->key);
	#endif

	//debug_suspend();
	//item->key = "<x>";
    }

    item->data = dt;
    item->dt_sz = dsz;
  
    func_ok();
    return 1;
}


static int
push_htab(hash_table_s *htab, file_info *fi, char *key, int sz, char *dt)
{
    int rt;
    int dt_sz;
    ENTRY item;
    ENTRY *rti;

    func_s();


    char *s, *e;

    s = dt;
    e = fi->buf_e;
  
    /* dump the head
     */
    while(s <= e) {
	if (*s != ' ' && *s != '\r' && *s != '\n') break;
	s++;
    }

    dt = s;
  
    dt_sz = find_bnf_unit_len(dt, fi);
    debug("dt_sz: %d \n", dt_sz);
    rt = make_hs_entry(&item, key, sz, dt, dt_sz);
    if (!rt) return 0;
  
    rti = hsearch(htab, item, FIND);
    if (rti)  {
	//free(item.key);
	item.key = NULL;
	return 0;
    }
  
#if 0
    debug("%s, %d bytes \n", item.key, strlen(item.key));
#endif
  
    rti = hsearch(htab, item, ENTER);
    if (!rti) return 0;

    ml_util_fwrite("elements.txt", item.key);

    func_ok();
    return 1;
}


static int
htab_add(hash_table_s *htab, char *key, int ksz, char *dt, int dsz)
{
    int rt;
    ENTRY item;
    ENTRY *rti;

    func_s();


    rt = make_hs_entry(&item, key, ksz, dt, dsz);
    if (!rt) return 0;

    //ml_util_fwrite("tokens.txt", item.key);
    //item.key = key;
    //item.data = dt;
    //item.dt_sz = dsz;

    
    rti = hsearch(htab, item, FIND);
    if (rti)  {

	debug("%s already in hash table \n", item.key);

	return 0;
    }
  
#if 0
    debug("%s, %d bytes \n", item.key, strlen(item.key));
#endif
  
    rti = hsearch(htab, item, ENTER);
    if (!rti) return 0;

    ml_util_fwrite("keyword_char.txt", item.key);
    
    
    func_ok();
    return 1;
}


#if 0
static bool
htab_find(hash_table_s *htab, char *key, int ksz)
{
    ENTRY item;
    ENTRY *rti;

    func_s();

    item.key = ml_util_buf2str(key, ksz);
    rti = hsearch(htab, item, FIND);
    if (rti)  {
	ml_free(item.key);

	func_ok();
	return true;
    }

    func_fail();
    return false;
}
#endif


static char*
find_s_ch(char c, char *s, int sz)
{
    while (--sz >= 0) {
	if (*s == c) return s;
	if (*s != ' ' && *s != '\r' && *s != '\n') return NULL;
	s++;
    } 
  
    return NULL;
}


static char* 
find_e_ch(char lc, char rc, char *s, int sz)
{
    while (--sz >= 0) {
	if (*s== rc) return s;
	if (*s== lc) {
	    while (--sz >= 0) {
		if (*++s == rc) break;
	    }
	}
	s++;
    }
  
    return NULL;
}


static char*
create_syntax_obj_key(char *s, char *e)
{
    char buf[512];

    func_s();
    
    if (!s) return NULL;
    
    if (!e) {

	e = s + strlen(s);
    }

    memset(buf, 0, sizeof(buf));
    memcpy(buf, s, e-s+1);
    strcat(buf, " ::=");

    debug("x \n");
    
    char *key = NULL;
    //key = rule_match_element(buf, strlen(buf));
    if (!key) {

	key = ml_util_strdup(buf);
	ml_util_fwrite("obj_key.txt", key);
    }

    func_ok();
    
    return key;
}


static int 
push_token_into_htab(hash_table_s *htab, char *s, char *e)
{
    char *ss, *ee;
    unsigned char flag;
    char buf[512];
    int sz;
    ENTRY item;
    ENTRY *rti;

    func_s();
  
    //e = fi->buf_e;
  
    if (s > e) return 0;

    ml_util_show_buf(s, e-s+1);
  

    /* cut whitespace of the head
     */
    while(s <= e) {
	if (*s != ' ' && *s != '\r' && *s != '\n' && *s != '\t') break;
	s++;
    }
    if (s > e) return 0;


    /* cut whitespace of the trail
     */
    while (e > s) {
	if (*e != ' ' && *e != '\r' && *e != '\n' && *s != '\t') break;
	e--;
    }    

    ml_util_show_buf(s, e-s+1);
    
    
    flag = 0;
    ss = s;
    ee = e;
    while(ss <= ee) {
	
	//if (*ss == '|' || ss == ee || (flag && *ss == ' ')) {
	if (*ss == '|' || ss == ee) {
	    if (ss != ee) e = ss - 1;
	    
	    /* cut whitespace of the trail
	     */
	    while (e > s) {
		if (*e != ' ' && *e != '\r' && *e != '\n' && *e != '\t') break;
		e--;
	    }

	    ml_util_show_buf(s, e-s+1);

	    /* if it's lisp-character or keyword
	     */
	    item.key = ml_util_buf2str(s, e-s+1);
	    rti = hsearch(get_char_htab(), item, FIND);
	    if (!rti)  {

		rti = hsearch(get_kw_htab(), item, FIND);
		if (!rti)  {
		    
		    //free(item.key);
		    item.key = NULL;
		}
		else {
		    debug("key found in keyword hash table \n");
		}
	
	    }
	    else {
		debug("key found in character hash table \n");
	    }
	    

	    if (!rti) {
		
		ml_util_show_buf(s, e-s+1);
		debug("search hash table to check if it's a syntax object \n");
	    
		char key[512];
		memset(key, 0, sizeof(key));
		
		memcpy(key, s, e-s+1);
		strcat(key, " ::=");

		debug("key: %s \n", key);

		/* if it's a syntax object 
		 */
		item.key = key;
		rti = hsearch(htab, item, FIND);
		if (!rti)  {

		    debug("not found \n");

		    /* it can be a new character or a keyword or a combined object
		     */

		    
		    /* if it's a character, add it to char-hash-table
		     * character syntax: #\x
		     * TODO
		     */
		    if (0) {

			debug("add a new character \n");	     			
		    }
		    else {

			/* but it may be a {}* {}+ [] combined object or a keyword
			 * we need to add all keywords into hash table here.
			 * first, filtering { } }* }+ [ ], split the buffer into parts
			 * then check each part if it's a character or syntax object,
			 * then it must be a keyword.
			 */

			debug("check if it's a combined object or a keyword \n");
			
			sz = 0;
			while (s <= e) {

			    if (*s == '}') {

				if (s+1 <= e) {
				    if (*(s+1) == '*' || *(s+1) == '+') {

					s += 2;
					continue;

				    }
				}
			    }
			    else if (*s == '{' || *s == '[' || *s == ']') {
				
			    }			    
			    else if ( *s == '\r' || *s == '\n' || *s == '\t') {
			
			    }
			    else {
				buf[sz++] = *s;
			    }

			    s++;
			    continue;
			}

			ml_util_show_buf(buf, sz);

			
			char *sss;
			sss = s = buf;
			e = s + sz - 1;
			while (s <= e) {

			    if (*s == ' ') {

				if (s > sss) {

				    ml_util_show_buf(sss, s-sss);
			      
				    if (ast_is_character(sss, s-sss)) {

					debug("character \n");				
				    }
				    else {
				    
					/* if it is not a syntax object, then it must be a keyword
					 */
					item.key = create_syntax_obj_key(sss, s-1);
					if (!hsearch(htab, item, FIND)) {

					    debug("new keyword \n");
					    htab_add(get_kw_htab(), sss, s-sss, NULL, 0);
					    ml_util_fwrite_buf("keyword.txt", sss, s-sss);
					}
					else {

					    debug("syntax object \n");
					}
					if (item.key) ml_free(item.key);
				    }

				    sss = s+1;
				}
				else {

				    sss++;
				}
				
			    }
			    else if (s == e) {

				debug("end \n");
			
				ml_util_show_buf(sss, s-sss);
				if (ast_is_character(sss, s-sss)) {

				    debug("character \n");	
				}
				else {
				    
				    /* if it is not a syntax object, then it must be a keyword
				     */
				    item.key = create_syntax_obj_key(sss, s);
				    if (!hsearch(htab, item, FIND)) {

					debug("new keyword \n");
					htab_add(get_kw_htab(), sss, s-sss+1, NULL, 0);
					ml_util_fwrite_buf("keyword.txt", sss, s-sss+1);
					
				    }
				    if (item.key) ml_free(item.key);
				}
				    
			    }

			    s++;
			    
			    
			}
			
		    }

		}
		else {
		    debug("found syntax object \n");
		}

		//free(item.key);
		item.key = NULL;
	    }

	    if (ss == ee) break;
	    
	    ss++;
	    
	    /* cut whitespace of the head
	     */
	    while(ss <= ee) {
		if (*ss != ' ' && *ss != '\r' && *ss != '\n' && *ss != '\t') break;
		ss++;
	    }
	    if (ss > ee) return 0;

	    s = ss;
	    e = ee;

	    ml_util_show_buf(s, e-s+1);

	    continue;
	}
	else {

	    //flag = 1;
	}

	ss++;
    }
   
    
    func_ok();
    return 1;
}


static int 
parse_syntax_object(hash_table_s *htab, file_info *fi, bool parsing_sub_obj)
{
    char *dt, *s, *e, *ss;
    unsigned long sz;
    ENTRY item;
    ENTRY *rti;

    
    fs();
  
    memset(&item, 0, sizeof(item));
  
    dt = fi->f_buf;
    sz = fi->buf_sz;

    s = dt;
    while (sz >= strlen(BNF_OBJ_STRING)) {

	ss = s;
	
	//debug("search \"%s\" \n", BNF_OBJ_STRING);
	bool found = false;
	while (sz >= strlen(BNF_OBJ_STRING)) {
	
	    //debug("%x %c \n", *s, *s);
	    if (!memcmp(s, BNF_OBJ_STRING, strlen(BNF_OBJ_STRING))) {
	    
		//debug("\nfound \n");
		found = true;
		break;
	    }

	    s++;
	    sz--;
	}

	if (!found) break;

	
	//debug("move back to find the syntax object \n");
	found = false;
	e = s + strlen(BNF_OBJ_STRING);
	while (s != ss) {

	    //debug("%x %c \n", *s, *s);
	    if (eq(*s, '\n') || eq(*s, '\r')) {

		//debug("found \n");
		found = true;
		s++;
		sz--;
		break;
	    }

	    s--;
	    sz++;
	}

	if (!found) break;
	
	
	ml_util_show_buf(s, e-s);

	if (ast_is_character(s, e-s)) {

	    debug("character \n");
	}
	else {
	    
	    if (!push_htab(htab, fi, s, e-s, e+1)) {

		if (parsing_sub_obj) {
		    int dt_sz = find_bnf_unit_len(e+1, fi);
		    if (dt_sz > 0) {
			debug("element: \n");
			ml_util_show_buf(e+1, dt_sz);

			push_token_into_htab(htab, e+1, e+1+dt_sz-1);
		    }
		}
	    }
	}

	s = e;
	sz = fi->buf_sz - (s-dt);
    }

    
    if (parsing_sub_obj) {

	memset(&item, 0, sizeof(item));
	item.key = "@";
	rti = hsearch(htab, item, ENTER);
	if (!rti) return 0;	
    }

    
    debug("hash table %d entries, %d entries used, %d entries free. \n", 
	  htab->size, htab->filled, htab->size - htab->filled);

    
    fe();
    return 1;
}


static char*  
find_word(ENTRY *item, char *dt, unsigned long sz)
{
    int rt;
    char *s, *e, *ee;

    s = e = NULL;
    while (sz > 0) {
  
	if (*dt == SPACE || *dt == '\r' || *dt == '\n' || *dt == '\t') {
	    dt++;
	    sz--;
	    continue;
	}
    
	s = dt;

	e = (char*)memchr(dt+1, SPACE, sz-1);
	ee = (char*)memchr(dt+1, '\r', sz-1);
	if (!e && !ee) {

	    e = s + sz -1;
	}
	else {
	    if (!e) {
		e = ee;

		if (!e) e = s;
	    }
	    else {
		if (ee) e = (e > ee ? ee : e);
	    }

	    if (e > s) e--;
    
	    if (!e) return NULL;
	}
	
	
	ml_util_show_buf(s, e-s+1);


	if (e-s+1 == 1) {

	    if (!is_constituent_char(*s) &&
		!is_macro_char(*s) &&
		!is_escape_char(*s) &&
		!is_multiple_escape_char(*s)) {

		debug("illegal char: %x, %c \n", *s, *s);
		return NULL;
	    }
	}

	
	rt = make_hs_entry(item, s, e - s + 1, NULL, 0);
	return (rt ? e : NULL);
    }
  
    return NULL;
}


#if 0
static char* 
find_elipsis(char *s, unsigned long sz)
{
    while (sz >= strlen("...")) {
	if (memcmp(s, "...", strlen("...")) == 0) return s;
	if (*s != ' ' && *s != '\r' && *s != '\n') return NULL;
	s++;
	--sz;
    }
  
    return NULL;
}
#endif


static char* 
find_pattern_more(char *s, unsigned long sz)
{
    while (sz >= strlen("*")) {
	if (memcmp(s, "*", strlen("*")) == 0) return s;
	if (*s != ' ' && *s != '\r' && *s != '\n') return NULL;
	s++;
	--sz;
    }
  
    return NULL;
}


static char* 
find_pattern_more_plus(char *s, unsigned long sz)
{
    while (sz >= strlen("+")) {
	if (memcmp(s, "+", strlen("+")) == 0) return s;
	if (*s != ' ' && *s != '\r' && *s != '\n') return NULL;
	s++;
	--sz;
    }
  
    return NULL;
}




static char* 
find_bnf_obj_str(char *s, unsigned long sz)
{

    func_s();
    
    ml_util_show_buf(s, sz);
    
    while (sz >= strlen(BNF_OBJ_STRING)) {
	if (!memcmp(s, BNF_OBJ_STRING, strlen(BNF_OBJ_STRING))) {

	    debug("found ::= \n");
	    return s;
	}

	
	//if (*s != ' ' && *s != '\r' && *s != '\n') return NULL;
	s++;
	--sz;
    }
  
    return NULL;
}



static ENTRY*
find_insert_htab(ENTRY item, hash_table_s *htab)
{
    ENTRY *rti;

    func_s();

    rti = hsearch(htab, item, FIND);
    if (!rti) {
  
#if 0
	debug("insert hash entry: %s, %lu bytes\n", item.key, strlen(item.key));
#endif
    
	rti = hsearch(htab, item, ENTER);
	if (!rti) {
	    debug("hash entry insertion failed. %s \n", item.key);
	    goto END;
	}
	ml_util_fwrite("elements.txt", item.key);
    
	return rti;
    }
    else {

	debug("one found \n");
    }
  
  END:
    if (item.key) {
	//free(item.key);
	item.key = NULL;
    }
  
    return rti;
}


#if 0
static char* 
find_pair(char *s, char *e)
{
    char *ee;
  
    while (s <= e) {
  
	if (*s == '<') {
	    ee = find_e_ch('<', '>', s + 1, e - s + 1);
	    if (!ee) return NULL;
	}
	else if (*s == '[') {
	    ee = find_e_ch('[', ']', s + 1, e - s + 1);
	    if (!ee) return NULL;
	}
	else if (*s == '{') {
	    ee = find_e_ch('{', '}', s + 1, e - s + 1);
	    if (!ee) return NULL;
	}
	else {
	    s++;
	    ee = NULL;
	}
    
	if (ee) {
	    ee = find_pair(s + 1, ee - 1);
	    if (!ee) return NULL;
	    s = ee + 1;
	}
    }
  
    return e;
}
#endif


static void
drop_head_trail_whitespace(char **start, char **end)
{
    char *s, *e;

    s = *start;
    e = *end;
    
    /* cut whitespace of the head
     */
    while(s < e) {
	if (*s != ' ' && *s != '\r' && *s != '\n' && *s != '\t') break;
	s++;
    }

    /* cut whitespace of the trail
     */
    while (s < e) {
	if (*e != ' ' && *e != '\r' && *e != '\n' && *e != '\t') break;
	e--;
    }

    *start = s;
    *end = e;

    func_ok();
}



static int
make_or_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    int rt;
    char *s, *e, *ee;
    ENTRY si, ei;
    tr_node_s *lfn, *rin;
  
  
    /* find or
     */
    s= bnf;
    e = memchr(s, '|', size);
    if (!e) return 0;

    func_s();
  
    //ee = find_pair(s, e - 1);
    //if (!ee) return 0;

    ee = e;
    e--;
    
    drop_head_trail_whitespace(&s, &e);
    
  
    /* insert the object to the left.
     */
    rt = make_hs_entry(&si, s, e - s + 1, s, e - s + 1);
    if (!rt) return 2;
  
    lfn = tree_insert_left(root, "<tmp>");
    if (!lfn) goto END;

  
    make_bnf_tree(lfn, s, e - s + 1, htab, dep + 1);

    
    /* insert the remaining to the right.
     */
    s = ee;
    e = bnf + size - 1;
    rt = make_hs_entry(&ei, s, e - s + 1, s + 1, e - s);
    if (!rt) goto DONE;
  
    rin = tree_insert_right(root, "<tmp>");
    if (!rin) goto END;
  
    make_bnf_tree(rin, s + 1, e - s, htab, dep + 1);
  
  DONE:
    func_ok();
    return 1;

  END:

    func_e();
    return 0;
}


static int
make_sub_obj_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    int rt;
    char *s, *e;
    ENTRY *lfi;
    ENTRY si, ei;
    tr_node_s *lfn, *sub;
    
    bool flag;
  

    e = find_bnf_obj_str(bnf, size);
    flag = !!e;
    
    s = bnf;
    e = bnf + size - 1;

    drop_head_trail_whitespace(&s, &e);
    if (s > e) return 0;
    
    ml_util_show_buf(s, e-s+1);

    if (!flag) {
	char key2[512];
	memset(key2, 0, sizeof(key2));

	memcpy(key2, s, e-s+1);
	strcat(key2, " ::=");

	debug("key2: %s \n", key2);

	si.key = key2;
	lfi = hsearch(htab, si, FIND);
	if (!lfi) {
	    debug("not found \n");

	    //ml_free(key2);
	    return 0;
	}

	//ml_free(key2);
	debug("found \n");
    }
    else {
	
	rt = make_hs_entry(&si, s, e - s + 1, NULL, 0);
	if (!rt) return 2;
  
	lfi = find_insert_htab(si, htab);
	if (!lfi) goto END; 
    }

    func_s();
    
    debug("sub: \n");
    ml_util_show_buf(lfi->data, lfi->dt_sz);
	
    /* insert the object to the left.
     */    
    lfn = tree_insert_left(root, lfi->key);
    if (!lfn) goto END;
    
  
    /* if lex tree is there, check the left node if it's a token or in tree.
     */
    //if (get_lex_tree() && root)  lfn->is_token = is_token(lfn->key);
    if (root)  {
	if (is_token(lfn->key)) mark_token_node(lfn);
    }
    
    if (is_token_node(lfn)) {
	debug("in lex tree, found: %s \n", lfn->key);
	//debug_suspend();
    }
    
    /* if hash-table of sub syntax trees is there, check the left
     * node if it's a sub tree already in syntax-trees hash table.
     */   
    if (get_syntax_htab() && root) {

	if (pop_syntax_htab(lfn->key)) {
	    mark_in_syntax_tree(lfn);
	    debug("in syntax sub trees, found: %s \n", lfn->key);
	    //debug_suspend();
	}
    }
  
    /* remember the root node 
     */
    if (!root) bnf_tree_root = lfn;
  

    /* try to make a sub tree for the left node. 
     */
    if (!is_token_node(lfn) && !is_in_syntax_tree(lfn)) {

	debug("sub node, dt_sz: %d \n", lfi->dt_sz);
      
	/*insert the sub node of the left node
	 */
	if (!is_inside_loop_node(lfn) && lfi->dt_sz > 0) {
	    rt = make_hs_entry(&si, lfi->data, lfi->dt_sz, NULL, 0);
	    if (!rt) return 2;

	    
	    if (strlen(si.key) > 1) {
		//dump_trail(si.key, si.key + strlen(si.key) - 1);
	    }
	    
	    //ml_util_show_buf(lfi->data, lfi->dt_sz);
      
      
	    sub = tree_insert_sub(lfn, si.key);
	    if (!sub) goto END;

	    if (is_token(sub->key)) {
	      mark_token_node(sub);
	      //debug_suspend();
	    }
	    else {
	      make_bnf_tree(sub, lfi->data, lfi->dt_sz, htab, dep + 1);
	      if (!root) goto DONE;
	    }
	}
	else {

	    if (is_inside_loop_node(lfn)) {
		debug("is_inside_loop_node \n");
	    }
	}
    }

    
    rt = make_hs_entry(&ei, e + 1, size - (e - bnf + 1), 
		       e + 1, size - (e - bnf + 1));
    if (!rt) return 2;
  
    lfn = tree_insert_left(lfn, "<tmp>");
    if (!lfn) goto END;
  
    make_bnf_tree(lfn, e + 1, size - (e - bnf + 1), htab, dep + 1);

  DONE:
    func_ok();
    return 1;

  END:

    func_e();
    return 0;
}


static int
make_brackets_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    int rt;
    char *s, *e;
    ENTRY *lfi;
    ENTRY si, ei;
    tr_node_s *lfn, *sub, *rin;
    
  
    /* find [] 
     */
    s = find_s_ch('[', bnf, size);
    if (!s) return 0;

    e = find_e_ch('[', ']', s + 1, size - (s - bnf + 1));
    if (!e) return 0;

    func_s();
  
    /* insert the left node of the root node 
     */
    rt = make_hs_entry(&si, s, e - s + 1, s + 1, e - s - 1);
    if (!rt) return 2;
  
    lfi = find_insert_htab(si, htab);
    if (!lfi) goto END;
  
    lfn = tree_insert_left(root, lfi->key);
    if (!lfn) goto END;
  
  
    /* insert the sub node of the left node
     */
    if (lfi->data && lfi->dt_sz > 0) {
	rt = make_hs_entry(&si, lfi->data, lfi->dt_sz, NULL, 0);
	if (!rt) return 2;
    
	sub = tree_insert_sub(lfn, si.key);
	if (!sub) goto END;
    
	rin = tree_insert_right(sub, "@");
	if (!rin) goto END;	
    
	sub = tree_insert_left(sub, si.key);
	if (!sub) goto END;
    
	make_bnf_tree(sub, lfi->data, lfi->dt_sz, htab, dep + 1);
    }
  
 
    /* insert the remainning bnf as a node to 
     * the right node of the root
     */
    rt = make_hs_entry(&ei, e + 1, size - (e - bnf + 1), 
		       e + 1, size - (e - bnf + 1));
    if (!rt) return 2;
  
    rin = tree_insert_right(root, "<tmp>");
    if (!rin) goto END;
  
    make_bnf_tree(rin, e + 1, size - (e - bnf + 1), htab, dep);
  
    lfn = tree_insert_left(lfn, "<tmp>");
    if (!lfn) goto END;
  
    make_bnf_tree(lfn, e + 1, size - (e - bnf + 1), htab, dep + 1);


    func_ok();
    return 1;

  END:

    func_e();
    return 0;    
}


static int
make_braces_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    int rt;
    char *s, *e, *ss;
    ENTRY *lfi;
    ENTRY si, ei;
    tr_node_s *lfn, *sub;
  
  
    /* find {} 
     */
    s = find_s_ch('{', bnf, size);
    if (!s) return 0;
  
    e = find_e_ch('{', '}', s + 1, size - (s - bnf + 1));
    if (!e) return 0;

    func_s();
  
    /* insert the left node of the root node 
     */
    rt = make_hs_entry(&si, s, e - s + 1, s + 1, e - s - 1);
    if (!rt) return 2;
  
    lfi = find_insert_htab(si, htab);
    if (!lfi) goto END;
  
    lfn = tree_insert_left(root, lfi->key);
    if (!lfn) goto END;
  
  
    /* insert the sub node of the left node
     */
    if (lfi->data && lfi->dt_sz > 0) {
	rt = make_hs_entry(&si, lfi->data, lfi->dt_sz, NULL, 0);
	if (!rt) return 2;
  
	sub = tree_insert_sub(lfn, si.key);
	if (!sub) goto END;
  
	make_bnf_tree(sub, lfi->data, lfi->dt_sz, htab, dep + 1);
    }
  
  
    /* find {}*
     */
    ss = find_pattern_more(e + 1, size - (e - bnf + 1));
    if (ss) {
	mark_outside_loop_node(lfn);	
	e = ss + strlen("*") - 1;
	debug("pattern: {}* \n");
    }
    else {

	/* find {}+
	 */
	ss = find_pattern_more_plus(e + 1, size - (e - bnf + 1));
	if (ss) {
	    mark_outside_loop_node(lfn);
	    mark_more_plus_node(lfn);	    
	    e = ss + strlen("+") - 1;
	    debug("pattern: {}+ \n");
	}	
    }
  

    debug("remain: \n");
    ml_util_show_buf( e + 1, size - (e - bnf + 1));
    
    rt = make_hs_entry(&ei, e + 1, size - (e - bnf + 1), 
		       e + 1, size - (e - bnf + 1));
    if (!rt) return 2;
  
    lfn = tree_insert_left(lfn, "<tmp>");
    if (!lfn) goto END;

    
    make_bnf_tree(lfn, e + 1, size - (e - bnf + 1), htab, dep + 1);

    //tree_show(root, 5);
  
    func_ok();
    return 1;

  END:

    func_e();
    return 0;
}	



static int 
make_combined_obj_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    int rt;
    char *s, *e;
    ENTRY *lfi;
    ENTRY si, ei;
    tr_node_s *lfn, *sub;
    bool keyword_flag, char_flag;
  
  
    /* split the combined object
     */
    e = find_word(&si, bnf, size);
    if (!e) return 0;

    func_s();
    
    keyword_flag = false;
    char_flag = false;

    debug("%s \n", si.key);
    
    /* insert the keyword as a node to 
     * the left node of the root node.
     */
    lfi = hsearch(get_kw_htab(), si, FIND);
    if (!lfi) {
	
	/* check if it's a character
	 */
	lfi = hsearch(get_char_htab(), si, FIND);
	if (lfi) {

	    debug("character %s \n", si.key);
	    char_flag = true;
	    goto FOUND_LEAF;
	}
	
	
	s = create_syntax_obj_key(si.key, si.key + strlen(si.key) - 1);
	//free(si.key);
	si.key = s;
	lfi = hsearch(htab, si, FIND);
	if (!lfi) {

	    debug("unkown object: %s \n", si.key);
	    ml_free(si.key);
	    goto FAIL;
	}
	
	debug("found syntax object \n");

	ml_free(si.key);

	debug("root father: %s \n", root->father->key);
	
	lfn = tree_insert_left(root, lfi->key);
	if (!lfn) goto END;

	
	if (get_syntax_htab() && root) {

	    if (pop_syntax_htab(lfn->key)) {
		mark_in_syntax_tree(lfn);
		debug("in syntax sub trees, found: %s \n", lfn->key);
		//debug_suspend();
	    }
	}

	
	if (!is_in_syntax_tree(lfn)) {
	    /* insert the sub node of the left node
	     */
	    if (!is_inside_loop_node(lfn) && lfi->data && lfi->dt_sz > 0) {

		debug("its sub: \n");
		ml_util_show_buf(lfi->data, lfi->dt_sz);
	    
		rt = make_hs_entry(&si, lfi->data, lfi->dt_sz, NULL, 0);
		if (!rt) return 2;

		debug("si.key: %s, %lu bytes \n", si.key, strlen(si.key));
		
		sub = tree_insert_sub(lfn, "<sub>");
		if (!sub) goto END;

		if (is_token(sub->key)) {
		    mark_token_node(sub);
		    //debug_suspend();
		}
		else {
		  
		    make_bnf_tree(sub, lfi->data, lfi->dt_sz, htab, dep + 1);
		    if (!root) goto DONE;
		}
		
	    }

	}
	

	goto BUILD_REMAIN;
    	
    }
    else {

	debug("keyword \n");
	keyword_flag = true;
	
	//goto FOUND_LEAF;
    }

    
  FOUND_LEAF:
    
    //free(si.key);
  
    lfn = tree_insert_left(root, lfi->key);
    if (!lfn) goto END;

    mark_token_node(lfn);
    if (keyword_flag) {	
	mark_keyword_node(lfn);
    }
    
    if (char_flag) mark_char_node(lfn);

    
  BUILD_REMAIN:    

    if (size - (e - bnf + 1) <= 0) goto DONE;
    
    /* if lex tree is there, check the left node if it's a token.
     */
    //if (get_lex_tree())  lfn->is_token = is_token(lfn->key);
	
    ml_util_show_buf(e + 1, size - (e - bnf + 1));
    
    /* insert the remainning bnf to 
     * the left node of the root node
     */
    rt = make_hs_entry(&ei, e + 1, size - (e - bnf + 1), 
		       e + 1, size - (e - bnf + 1));
    if (!rt) return 2;
  
    lfn = tree_insert_left(lfn, "<tmp>");
    if (!lfn) goto END;
    
    make_bnf_tree(lfn, e + 1, size - (e - bnf + 1), htab, dep + 1);
  
  DONE:
    func_ok();
    return 1;

  END:

    func_e();
    return 0;

  FAIL:
    func_fail();
    return 0;
}	


static int 
make_bnf_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    char *s, *e;
  
    if (root && is_inside_loop_node(root)) return 0;
    if (!bnf || size <= 0 || !htab) return 0;

    
    s = bnf;
    e = bnf + size - 1;

    drop_head_trail_whitespace(&s, &e);
    
    size = e - s + 1;
    if (size <= 0) return 0;

    func_s();
    
    if (make_or_tree(root, s, size, htab, dep)) return 1;
    if (make_brackets_tree(root, s, size, htab, dep)) return 1;
    if (make_braces_tree(root, s, size, htab, dep)) return 1;
    if (make_sub_obj_tree(root, s, size, htab, dep)) return 1;
    if (make_combined_obj_tree(root, s, size, htab, dep)) return 1;
  
#if 1
    debug("dep:%d, %s: \n", dep, "unknown syntax");
    show_buf(bnf, size, NULL);
#endif
  
    return 0;
}



static tr_node_s*
make_graph(tr_node_s *root)
{
    tr_node_s *ses, *les, *res, *es;
    tr_node_s *nd;
  
    if (!root) return NULL;
  
    ses = les = res = es = NULL;
  
    if (!root->sub && !root->left && !root->right) {
  
	/* the end node.
	 */
	es = root;
	goto END;
    }
  
    if (root->sub) {
  
	/* sub tree, find all end nodes of sub tree.
	 */
	ses = make_graph(root->sub);
  
	if (!root->left && !root->right) {
  
	    /* the end node.
	     */
	    es = ses;
	    goto END;
	}
    }
  
    if (root->left) {
  
	/* left tree, find all end nodes of left tree.
	 */
	les = make_graph(root->left);
    }
  
    if (root->right) {
  
	/* right tree, find all end nodes of right tree.
	 */
	res = make_graph(root->right);
    }
  
    /* let all end nodes of sub tree of root node point to 
     * the left node of root node.
     */
    if (ses && root->left) {
  
	nd = ses;
	while(nd) {
	    nd->left = root->left;
	    nd = nd->next;
	}
    
	//root->left = NULL;
    }
  
    /* let all end nodes of sub tree of root node point to 
     * the right node of root node.
     */
    if (ses && root->right) {
  
	nd = ses;
	while(nd) {
	    nd->left = root->right;
	    nd = nd->next;
	}
  
	//root->right = NULL;
    }
  
    /* combine tow lists of the end nodes of left tree and right tree
     * into one list.
     */
    if (!les) {
	es = res;
    }
    else {
  
	/* find the last node of the left-end-nodes list.
	 */
	nd = les;
	while(nd) {
	    if (!nd->next) break;
	    nd = nd->next;
	}
    
	if(res) nd->next = res;
    
	es = les;
    }
  
  END:

    if (root->father) {

	if (is_outside_loop_node(root->father)) {

	    if (root == root->father->sub) {

		/* left loop node.
		 * let all the end nodes points to the loop node. 
		 * Note:
		 * For the bnf syntax tree of monalisp, the loop node can only be 
		 * the father of current node. 
		 */	
		nd = es;
		while (nd) {

#if 1
		    debug("back-node of %s is: %s \n", nd->key, root->key);
		    if (root->left) debug("left: %s \n", root->left->key);
		    if (root->right) debug("right: %s \n", root->right->key);
		    if (root->sub) debug("sub: %s \n", root->sub->key);

		    if (root->father) debug("father: %s \n", root->father->key);
		    if (root->father->father) debug("father-father: %s \n",
						    root->father->father->key);	    
#endif

	    
	    
		    nd->back = root;
		    nd = nd->next;
		}
	    }
	    
	}
    }
    
    
    if (!es) {
	debug("We can't believe that" 
	      "there are no end nodes for \"%s\" \n", root->key);
    }
  
#if 0
    debug("key %s \n", root->key);
    int cnt = show_nodes(es);    
#endif
  
    return es;
}
 


#if 0
static void
show_graph(tr_node_s *root)
{
    if (!root) return;
  
    debug("%s %dbytes, %x \n", root->key, strlen(root->key), root);
  
    if (is_inside_loop_node(root)) {
	debug("loop node \n");
    }
  
    if (!root->sub && !root->left && !root->right) {
	debug("@e. \n");	
    }
  
    if (root->back) {
	debug("go to back-node %s \n", root->back->key);
    }
  
    if (root->sub) {
	debug("@sub: ");
	show_graph(root->sub);
    }
  
    if (root->left) {
	debug("@left: ");
	show_graph(root->left);
    }
  
    if (root->right) {
	debug("@right: ");
	show_graph(root->right);
    } 
}


static void
show_bnf_tree(tr_node_s *root)
{
    if (!root) return;
  
    if (is_outside_loop_node(root))  {
	debug("external loop \n");
    }
  
    if (is_inside_loop_node(root))  {
	debug("self loop \n");
    }
  
    if (root->sub)	{
	debug("father:\n%s \n", root->key);
	debug("sub:\n%s\n", root->sub->key);
	show_bnf_tree(root->sub);
    }
  
    if (root->left)  {
	debug("father:\n%s \n", root->key);
	debug("left:\n%s\n", root->left->key);
	show_bnf_tree(root->left);
    } 
  
    if (root->right)	{
	debug("father:\n%s \n", root->key);
	debug("right:\n%s\n", root->right->key);
	show_bnf_tree(root->right);
    } 
  
}
#endif


static bool
search_alike_node(tr_node_s *root, tr_node_s *nd)
{
    if (!root) return false;

    if (root->mark && !strcmp(root->key, nd->key)) {

	debug("found marked node: %s \n", root->key);
	func_ok();
	return true;
    }
  
    if (root->sub) {
	
	if (is_outside_loop_node(root)) return false;
	
	if (search_alike_node(root->sub, nd)) return true;
	
    }
  
    if (root->left)  {
	
	if (search_alike_node(root->left, nd)) return true;
    } 
  
    if (root->right)	{
	
	if (search_alike_node(root->right, nd)) return true;
    } 


    return false;
}


static void
save_tree_node_name(tr_node_s *root, tr_node_s *cur)
{
    if (!root) return;
    if (!cur) return;


    //debug("key: %s \n", cur->key);

    if (!search_alike_node(root, cur)) {
	
	ml_util_fwrite("nodes_name.txt", cur->key);

	cur->mark = true;
    }
    
      
    if (is_outside_loop_node(cur)) {
	debug("external loop \n");
	//return;
    }
  
    if (is_inside_loop_node(cur)) {
	debug("self loop \n");
	//return;
    }
  
    if (cur->sub) {
	
	if (is_outside_loop_node(cur)) return;
	
	//debug("father:\n%s \n", root->key);
	debug("sub:\n%s\n", cur->sub->key);
	save_tree_node_name(root, cur->sub);
    }
  
    if (cur->left)  {
	//debug("father:\n%s \n", cur->key);
	debug("left:\n%s\n", cur->left->key);
	save_tree_node_name(root, cur->left);
    } 
  
    if (cur->right) {
	//debug("father:\n%s \n", cur->key);
	debug("right:\n%s\n", cur->right->key);
	save_tree_node_name(root, cur->right);
    } 
}



static hash_table_s htab, kw_htab, char_htab;


static parser_rt_t
construct_ast_tree(char *key)
{
    int rt;
    char *root_key;
    tr_node_s *es;

    func_s();
    
    root_key = create_syntax_obj_key(key, NULL);

    debug("root key: %s \n", root_key);
    
    debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
    rt = make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
    debug("\n[make_bnf_tree], done. \n\n");
  
    debug("hash table %d entries, %d entries used, %d entries free. \n", 
	  htab.size, htab.filled, htab.size - htab.filled);

    debug("show tree1: \n");
    tree_show(bnf_tree_root, 11);
    
    debug("\n\n[make_graph]... \n");	
    es = make_graph(bnf_tree_root);
    debug("\n[make_graph], done. \n\n");

    debug("show tree2: \n");
    tree_show(bnf_tree_root, 11);
    tree_show_node_cnt(bnf_tree_root);
    
    //show_graph(es);
    
    //show_nodes(es);

    //show_bnf_tree(bnf_tree_root);
  
    rt = push_syntax_htab(root_key, bnf_tree_root);
    if (!rt) return PARSER_ERR;

    func_ok();
    return PARSER_OK;
}


parser_rt_t
parser_init(void)
{
    int rt;
    file_info fi;
    tr_node_s *es;
    char *root_key;
    
    func_s();


    unlink("elements.txt");
    unlink("keyword_char.txt");
    unlink("keys.txt");
    unlink("obj_key.txt");
    unlink("nodes_name.txt");
    unlink("found.txt");
    unlink("chars.txt");
    unlink("keyword.txt");
    
    /* load the syntax file
     */
    memset(&fi, 0, sizeof(fi));
    fi.f_name = "monalisp1.0_syntax.txt";
  
    rt = load_syntax_file(&fi);
    if (!rt) return PARSER_ERR;


    /* create a hash table for syntax objects
     */
    memset(&htab, 0, sizeof(hash_table_s));
    rt = hcreate(&htab, 500);
    if(!rt) return PARSER_ERR;


    memset(&kw_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&kw_htab, 200);
    if(!rt) return PARSER_ERR;
    
    set_kw_htab(&kw_htab);


    memset(&char_htab, 0, sizeof(hash_table_s));
    rt = hcreate(&char_htab, 255);
    if(!rt) return PARSER_ERR;
    
    set_char_htab(&char_htab);    
    
    const char* lisp_chars[] = {
	"$" , "%" , "&" , "*" , "+" , "-" , "." , "/" ,
	"0" , "1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9" ,
	":" , "<" , "=" , ">" , "@" , 
	"A" , "B" , "C" , "D" , "E" , "F" , "G" , "H" , "I" , "J" , "K" , "L" , "M" , 
	"N" , "O" , "P" , "Q" , "R" , "S" , "T" , "U" , "V" , "W" , "X" , "Y" , "Z" ,
	"a" , "b" , "c" , "d" , "e" , "f" , "g" , "h" , "i" , "j" , "k" , "l" , "m" , 
	"n" , "o" , "p" , "q" , "r" , "s" , "t" , "u" , "v" , "w" , "x" , "y" , "z" ,	
	"^" , "_" , "~" , "#\\rubout",

	"\"", "'", "(", ")", ",", ";", "`",

	"#",

	"#\\backslash", "#\\vertical-bar",

	"#\\b", "#\\t", "#\\r", "#\\n", "#\\p", "#\\space"

    };

    for (int i = 0; i < ARR_LEN(lisp_chars); i++) {

	ml_util_fwrite("chars.txt", (char*)lisp_chars[i]);
	htab_add(&char_htab, (char*)lisp_chars[i], strlen(lisp_chars[i])+1, NULL, 0);
    }

    mm_show();
    //debug_suspend();
    
    /* parse syntax objects from file and push them into hash table 
     */
    rt = parse_syntax_object(&htab, &fi, false);
    if (!rt) return PARSER_ERR;
    rt = parse_syntax_object(&htab, &fi, true);
    if (!rt) return PARSER_ERR;

    
    debug("syntax object hash table %d entries, %d entries used, %d entries free. \n\n", 
	  htab.size, htab.filled, htab.size - htab.filled);
   
    debug("char hash table %d entries, %d entries used, %d entries free. \n\n", 
	  char_htab.size, char_htab.filled, char_htab.size - char_htab.filled);

    debug("keyword hash table %d entries, %d entries used, %d entries free. \n\n", 
	  kw_htab.size, kw_htab.filled, kw_htab.size - kw_htab.filled);


   
    rt = create_syntax_htab(500);
    if (!rt) return PARSER_ERR;    
    
    mm_show();
    //debug_suspend();
    
    /* create the lexical tree
     * TBD: reduce the node count of the tree
     */
    root_key = "token ::=";
 
    debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
    rt = make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
    debug("\n[make_bnf_tree], done. \n\n");
  
    debug("hash table %d entries, %d entries used, %d entries free. \n", 
	  htab.size, htab.filled, htab.size - htab.filled);

    //show_bnf_tree(bnf_tree_root);

    tree_show(bnf_tree_root, 15);
    
    int count = 0;
    asg_show_redundant_node(bnf_tree_root, &count);
    debug("redundant node count: %d \n\n", count);

    //asg_reduce_redundant_node(bnf_tree_root, &count);
    debug("redundant node count: %d \n\n", count);

    count = 0;
    asg_show_redundant_node(bnf_tree_root, &count);
    debug("redundant node count: %d \n\n", count);
    
    tree_show(bnf_tree_root, 15);
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    
    debug("\n\n[make_graph]... \n");	
    es = make_graph(bnf_tree_root);
    debug("\n[make_graph], done. \n\n");

    //show_graph(bnf_tree_root);
    
    //show_nodes(es);

    mm_show();
    tree_show_info();
    //debug_suspend();
    
    set_lex_tree(bnf_tree_root);

    rt = ast_lex_debug();
    if (!rt) return PARSER_ERR;


    tree_show(bnf_tree_root, 15);
    mm_show();
    

    //count = 0;
    //asg_show_redundant_node(bnf_tree_root, &count);
    tree_show_info();
    //save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    

    /* create the minimal AST tree
     * "minimal" means creating minimal tree nodes(use the minimal dynamic memory).
     */

    /* now, to identify the tree used more than one time.
     * maybe all the leaf nodes should be linked to a node "<x>", which contains an extendable
     * list for connecting to all its childs, in this way, it would save the memory.
     * 
     * to count the number for all types of node, as to identify how to save the memory.
     *
     */


    

    /* create the AST tree as the specified form
     */

    mm_show();
    //debug_suspend();


    //if (construct_ast_tree("lisp-char") != PARSER_OK) return PARSER_ERR;
    
    //save_tree_node_name(bnf_tree_root, bnf_tree_root);
    
    if (construct_ast_tree("object") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //tree_show(bnf_tree_root, 15);
    //debug_suspend();
     
    if (construct_ast_tree("list") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();

    
    if (construct_ast_tree("car") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("cdr") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    
    if (construct_ast_tree("num-add") != PARSER_OK) return PARSER_ERR;
    
    if (construct_ast_tree("num-less-than") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("num-less-or-equal-than") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("num-greater-than") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("num-greater-or-equal-than") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("num-euqal-than") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("num-not-equal-than") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();


    if (construct_ast_tree("eq") != PARSER_OK) return PARSER_ERR; 
    if (construct_ast_tree("eql") != PARSER_OK) return PARSER_ERR;
    if (construct_ast_tree("equal") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();

    if (construct_ast_tree("compound-form") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();

    
    if (construct_ast_tree("setq") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //tree_show(bnf_tree_root, 10);
    //debug_suspend();

    if (construct_ast_tree("defconstant") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    
    if (construct_ast_tree("if") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    
    if (construct_ast_tree("return") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    
    if (construct_ast_tree("loop") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
  
    if (construct_ast_tree("defun") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();

    if (construct_ast_tree("defstruct") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();

    if (construct_ast_tree("defmacro") != PARSER_OK) return PARSER_ERR;

    mm_show();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();


    if (construct_ast_tree("print") != PARSER_OK) return PARSER_ERR;

    mm_show();
    tree_show_info();
    save_tree_node_name(bnf_tree_root, bnf_tree_root);
    //debug_suspend();
    
#if 0
    count = 0;
    asg_show_redundant_node(bnf_tree_root, &count);
    
    asg_reduce_redundant_node(bnf_tree_root, &count);
    debug("redundant node count: %d \n\n", count);

    count = 0;
    asg_show_redundant_node(bnf_tree_root, &count);
    debug("redundant node count: %d \n\n", count);
#endif
    
    //debug_suspend();

      
    func_ok();

    return PARSER_OK;
}


