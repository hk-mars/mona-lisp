

#include "parser.h"

#include "debug.h"

#include "error.h"

#include "hsearch.h"
#include "tree.h"
#include "chars.h"

#include "ast_tree.h"
#include "asg_graph.h"

#include "util.h"

#include "mem.h"


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


int 
load_syntax_file(file_info *fi)
{
    fs();
  
    fi->f = fopen(fi->f_name,"rb");
    if (!fi->f) return 0;
  
    fi->f_sz = get_file_size(fi->f);
    debug("file %s opened, size %d bytes.\n", fi->f_name, fi->f_sz);
  
    fi->f_buf = (char*)malloc(fi->f_sz);
    if (!fi->f_buf) return 0;
  
    fi->buf_sz = fread(fi->f_buf, 1, fi->f_sz, fi->f);
    if (fi->buf_sz < fi->f_sz) {
	free(fi->f_buf);
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
  
    item->key = (char*)ml_malloc(ksz + 1);
    if (!item->key) return 0;
  
    memset(item->key, '\0', ksz + 1);
    memcpy(item->key, key, ksz);
  
    item->data = dt;
    item->dt_sz = dsz;
  
    //func_ok();
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
	free(item.key);
	item.key = NULL;
	return 0;
    }
  
#if 0
    debug("%s, %d bytes \n", item.key, strlen(item.key));
#endif
  
    rti = hsearch(htab, item, ENTER);
    if (!rti) return 0;

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

    char *s, *e;

    rt = make_hs_entry(&item, key, ksz, dt, dsz);
    if (!rt) return 0;
  
    rti = hsearch(htab, item, FIND);
    if (rti)  {

	debug("%s already in hash table \n", item.key);
	
	free(item.key);
	item.key = NULL;

	return 0;
    }
  
#if 0
    debug("%s, %d bytes \n", item.key, strlen(item.key));
#endif
  
    rti = hsearch(htab, item, ENTER);
    if (!rti) return 0;

    func_ok();
    return 1;
}


static bool
htab_find(hash_table_s *htab, char *key, int ksz)
{
    ENTRY item;
    ENTRY *rti;

    func_s();

    item.key = ml_util_buf2str(key, ksz);
    rti = hsearch(htab, item, FIND);
    if (rti)  {
	free(item.key);

	//func_ok();
	return true;
    }

    func_fail();
    return false;
}


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
    char *key = ml_malloc(e-s+1 + strlen(" ::=") + 1);
    if (!key) return NULL;

    memcpy(key, s, e-s+1);
    strcat(key, " ::=");

    return key;
}


static int 
push_token_into_htab(hash_table_s *htab, file_info *fi, char *s, char *e)
{
    char *ss, *ee;
    unsigned char flag;
    char buf[255];
    int sz;
    int rt;
    ENTRY item;
    ENTRY *rti;

    func_s();
  
    //e = fi->buf_e;
  
    if (s >= e) return 0;

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
		    
		    free(item.key);
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
	    
		char *key = ml_malloc(e-s+1 + strlen(" ::=") + 1);
		if (!key) return 0;

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
					}
					else {

					    debug("syntax object \n");
					}
				    }

				    sss = s+1;
				}
				else {

				    sss++;
				}
				
			    }
			    else if (s == e) {

				debug("end \n");
				ml_util_show_buf(sss, s-sss+1);
				if (ast_is_character(sss, s-sss+1)) {

				    debug("character \n");
				}
				else {
				    
				    /* if it is not a syntax object, then it must be a keyword
				     */
				    item.key = create_syntax_obj_key(sss, s);
				    if (!hsearch(htab, item, FIND)) {

					debug("new keyword \n");
					htab_add(get_kw_htab(), sss, s-sss+1, NULL, 0);
				    }
				}
				    
			    }

			    s++;
			    
			    
			}
			
		    }

		}
		else {
		    debug("found syntax object \n");
		}

		free(item.key);
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
    int sz;
    int rt;
    ENTRY item;
    ENTRY *rti;
    char buf[64];
  
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

			push_token_into_htab(htab, fi, e+1, e+1+dt_sz-1);
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
find_word(ENTRY *item, char *dt, int sz)
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


static char* 
find_elipsis(char *s, int sz)
{
    while (sz >= strlen("...")) {
	if (memcmp(s, "...", strlen("...")) == 0) return s;
	if (*s != ' ' && *s != '\r' && *s != '\n') return NULL;
	s++;
	--sz;
    }
  
    return NULL;
}


static char* 
find_pattern_more(char *s, int sz)
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
find_pattern_more_plus(char *s, int sz)
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
find_bnf_obj_str(char *s, int sz)
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
  
#if 1
	debug("insert hash entry: %s, %dB\n", item.key, strlen(item.key));
#endif
    
	rti = hsearch(htab, item, ENTER);
	if (!rti) {
	    debug("hash entry insertion failed. %s \n", item.key);
	    goto END;
	}
    
	return rti;
    }
    else {

	debug("one found \n");
    }
  
  END:
    if (item.key) {
	free(item.key);
	item.key = NULL;
    }
  
    return rti;
}


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
    ENTRY *lfi, *rii;
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
    char *s, *e, *ss;
    ENTRY *rti, *lfi, *rii;
    ENTRY si, ei;
    tr_node_s *lfn, *rin, *rtn, *sub;
    bool flag;
  

    e = find_bnf_obj_str(bnf, size);
    flag = !!e;
    
    s = bnf;
    e = bnf + size - 1;

    drop_head_trail_whitespace(&s, &e);
    if (s > e) return 0;
    
    ml_util_show_buf(s, e-s+1);

    if (!flag) {
	char *key2 = ml_malloc(e-s+1 + strlen(" ::=") + 1);
	if (!key2) return 0;

	memcpy(key2, s, e-s+1);
	strcat(key2, " ::=");

	debug("key2: %s \n", key2);

	si.key = key2;
	lfi = hsearch(htab, si, FIND);
	if (!lfi) {
	    debug("not found \n");

	    ml_free(key2);
	    return 0;
	}

	ml_free(key2);
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
    
    lfn->is_sub_bnf = 1;
  
    /* if lex tree is there, check the left node if it's a token or in tree.
     */
    //if (get_lex_tree() && root)  lfn->is_token = is_token(lfn->key);
    if (root)  lfn->is_token = is_token(lfn->key);
    
    if (lfn->is_token) debug("in lex tree, found: %s \n", lfn->key);
  
    /* if hash-table of sub syntax trees is there, check the left
     * node if it's a sub tree already in syntax-trees hash table.
     */   
    if (get_syntax_htab() && root) {
	if (pop_syntax_htab(lfn->key)) {
	    lfn->is_in_syntax_tree = 1;
	    debug("in syntax sub trees, found: %s \n", lfn->key);
	}
    }
  
    /* remember the root node 
     */
    if (!root) bnf_tree_root = lfn;
  

    /* try to make a sub tree for the left node. 
     */
    if (!lfn->is_token && !lfn->is_in_syntax_tree) {

	debug("sub node, dt_sz: %d \n", lfi->dt_sz);
      
	/*insert the sub node of the left node
	 */
	if (!lfn->is_inside_loop_node && lfi->dt_sz > 0) {
	    rt = make_hs_entry(&si, lfi->data, lfi->dt_sz, NULL, 0);
	    if (!rt) return 2;

	    dump_trail(si.key, si.key + strlen(si.key) - 1);

	    //ml_util_show_buf(lfi->data, lfi->dt_sz);
      
      
	    sub = tree_insert_sub(lfn, si.key);
	    if (!sub) goto END;
      
	    if (strlen(si.key) > 1) {
		make_bnf_tree(sub, lfi->data, lfi->dt_sz, htab, dep + 1);
		if (!root) goto DONE;
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
    char *s, *e, *ss;
    ENTRY *rti, *lfi, *rii;
    ENTRY si, ei;
    tr_node_s *lfn, *rin, *sub;
  
  
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


  DONE:
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
    ENTRY *rti, *lfi, *rii;
    ENTRY si, ei;
    tr_node_s *lfn, *rin, *sub, *rtn;
  
  
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
	lfn->is_outside_loop_node = 1;
	e = ss + strlen("*") - 1;
	debug("pattern: {}* \n");
    }
    else {

	/* find {}+
	 */
	ss = find_pattern_more_plus(e + 1, size - (e - bnf + 1));
	if (ss) {
	    lfn->is_outside_loop_node = 1;
	    lfn->is_more_plus = 1;
	    e = ss + strlen("+") - 1;
	    debug("pattern: {}+ \n");
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
make_combined_obj_tree(tr_node_s *root, char *bnf, int size, hash_table_s *htab, int dep)
{
    int rt;
    char *s, *e, *ss;
    ENTRY *rti, *lfi;
    ENTRY si, ei;
    tr_node_s *rtn, *lfn, *rin, *sub;
  
  
    /* split the combined object
     */
    e = find_word(&si, bnf, size);
    if (!e) return 0;

    func_s();
    
  
    /* insert the keyword as a node to 
     * the left node of the root node.
     */
    lfi = hsearch(get_kw_htab(), si, FIND);
    if (!lfi) {
	
	/* check if it's a character
	 */
	lfi = hsearch(get_char_htab(), si, FIND);
	if (lfi) {

	    debug("character \n");

	    lfn = tree_insert_left(root, lfi->key);
	    if (!lfn) goto END;
	    
  	    lfn->is_token = 1;

	    free(si.key);
	    
	    goto NEXT_STEP;
	}
	
	
	s = create_syntax_obj_key(si.key, si.key + strlen(si.key) - 1);
	free(si.key);
	si.key = s;
	lfi = hsearch(htab, si, FIND);
	if (!lfi) {

	    debug("unkown object: %s \n", si.key);
	    free(si.key);
	    goto FAIL;
	}
	
	debug("found syntax object \n");

	free(si.key);

	
	lfn = tree_insert_left(root, lfi->key);
	if (!lfn) goto END;

	
	/* insert the sub node of the left node
	 */
	if (lfi->data && lfi->dt_sz > 0) {

	    debug("its sub: \n");
	    ml_util_show_buf(lfi->data, lfi->dt_sz);
	    
	    rt = make_hs_entry(&si, lfi->data, lfi->dt_sz, NULL, 0);
	    if (!rt) return 2;

	    debug("si.key: %s, %dbytes \n", si.key, strlen(si.key));
	    //if (strlen(si.key) > 1) {

	    sub = tree_insert_sub(lfn, "<sub>");
	    if (!sub) goto END;
	    make_bnf_tree(sub, lfi->data, lfi->dt_sz, htab, dep + 1);
	    //if (!root) goto DONE;
	    //}

		
	    free(si.key);
	}

	

	goto NEXT_STEP;
    	
    }
    else {

	debug("keyword \n");
    }

    free(si.key);
  
    lfn = tree_insert_left(root, lfi->key);
    if (!lfn) goto END;

  NEXT_STEP:    

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
  
    if (root && root->is_inside_loop_node) return 0;
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


static int 
show_nodes(tr_node_s *sn)
{
#if 1
    long cnt;
  
    fs();
  
    if (!sn) return 0;
  
    cnt = 0;
    while(sn) {
	debug("%s ", sn->key);
	sn = sn->next;
	cnt++;
    }
  
    debug("\n");
    debug("end solutions cnt: %d \n", cnt);
  
    fe();

    return cnt;
#endif
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
    
	root->left = NULL;
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
  
	root->right = NULL;
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

	if (root->father->is_outside_loop_node) {

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
 

static void
show_graph(tr_node_s *root)
{
    if (!root) return;
  
    debug("%s %dbytes, %x \n", root->key, strlen(root->key), root);
  
    if (root->is_inside_loop_node) {
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
  
    if (root->is_outside_loop_node)  {
	debug("left loop \n");
    }
  
    if (root->is_inside_loop_node)  {
	debug("loop node \n");
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


static int 
push_kw_htab(hash_table_s *htab, tr_node_s *sn)
{
    ENTRY item, *rti;
  
    fs();
  
    if (!sn) return 0;
  
    memset(&item, 0, sizeof(item));
    while(sn) {
  
	if (!is_like_keyword(sn->key)) {
	    sn = sn->next;
	    continue;
	}
    
	item.key = sn->key;
	rti = hsearch(htab, item, FIND); 
	if (!rti) {
	    rti = hsearch(htab, item, ENTER);
	    if (!rti) return 0;
	}
    
	sn = sn->next;
    }
  
    fe();
  
    return 1;
}


static hash_table_s htab, kw_htab, char_htab;


parser_rt_t
parser_init(void)
{
    int rt;
    file_info fi;
    //char *sql_str;
    tr_node_s *es;
    //token_list tk_lst;
    ENTRY *rti;
    char *root_key;
    tr_node_s *root;
    
    func_s();

    
    /* load the syntax file
     */
    memset(&fi, 0, sizeof(fi));
    fi.f_name = "monalisp1.0_syntax.txt";
  
    rt = load_syntax_file(&fi);
    if (!rt) return PARSER_ERR;


    /* create a hash table for syntax objects
     */
    memset(&htab, 0, sizeof(hash_table_s));
    rt = hcreate(&htab, 1500);
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

	"|", "\\",

	"#\\b", "#\\t", "#\\r", "#\\n", "#\\p", "\#\\space"

    };

    for (int i = 0; i < ARR_LEN(lisp_chars); i++) {
    
	htab_add(&char_htab, lisp_chars[i], strlen(lisp_chars[i]), NULL, 0);
    }
    
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
    

#if 1    
    /* create lexical tree
     */
    root_key = "token ::=";
 
    debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
    rt = make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
    debug("\n[make_bnf_tree], done. \n\n");
  
    debug("hash table %d entries, %d entries used, %d entries free. \n", 
	  htab.size, htab.filled, htab.size - htab.filled);

    //show_bnf_tree(bnf_tree_root);
    
    debug("\n\n[make_graph]... \n");	
    es = make_graph(bnf_tree_root);
    debug("\n[make_graph], done. \n\n");

    //show_graph(bnf_tree_root);
    
    //show_nodes(es);

    set_lex_tree(bnf_tree_root);

    rt = ast_lex_debug();
    if (!rt) return PARSER_ERR;
    
#endif
    

#if 0    
    /* create AST and ASG for syntax: "list ::=" 
     */
    root_key = "list ::=";
    
    debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
    rt = make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
    debug("\n[make_bnf_tree], done. \n\n");
  
    debug("hash table %d entries, %d entries used, %d entries free. \n", 
	  htab.size, htab.filled, htab.size - htab.filled);
  
    debug("\n\n[make_graph]... \n");	
    es = make_graph(bnf_tree_root);
    debug("\n[make_graph], done. \n\n");

    //show_graph(es);
    
    //show_nodes(es);

    //show_bnf_tree(bnf_tree_root);
  
    rt = push_syntax_htab("list", bnf_tree_root);
    if (!rt) return PARSER_ERR;

    //rt = syntax_list_debug();
    
#endif    
    
  
    /*  
	root_key = "<token>";
  
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	set_lex_tree(bnf_tree_root);
  
	memset(&kw_htab, 0, sizeof(kw_htab));
	rt = hcreate(&kw_htab, 300);
	if(!rt) return PARSER_ERR;
  
	rt = push_kw_htab(&kw_htab, es);
	if(!rt) return PARSER_ERR;
  
	debug("kw_htab table %d entries, %d entries used, %d entries free. \n\n", 
	kw_htab.size, kw_htab.filled, kw_htab.size - kw_htab.filled);	
  
	set_kw_htab(&kw_htab);



	#if 0
	rt = dbg_lex();
	if (!rt) return PARSER_ERR;
	#endif
    
	rt = create_syntax_htab(16);
	if (!rt) return PARSER_ERR;
  
	rt = push_syntax_htab("<value expression>", NULL); 
	if (!rt) return PARSER_ERR;
  
	rt = push_syntax_htab("<predicate>", NULL); 
	if (!rt) return PARSER_ERR;
  
	rt = push_syntax_htab("<query expression>", NULL); 
	if (!rt) return PARSER_ERR;
  
	rt = push_syntax_htab("<query term>", NULL); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<value expression primary>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<value expression primary>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<value expression>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rti = pop_syntax_htab("<value expression>");
	if (!rti) return PARSER_ERR;
	rti->data = bnf_tree_root;
  
	root_key = "<predicate>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rti = pop_syntax_htab("<predicate>");
	if (!rti) return PARSER_ERR;
	rti->data = bnf_tree_root;
  
	root_key = "<table definition>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<table definition>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<drop table statement>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<drop table statement>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<alter table statement>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<alter table statement>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<SQL data change statement>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<SQL data change statement>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<table expression>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<table expression>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	root_key = "<query term>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rti = pop_syntax_htab("<query term>");
	if (!rti) return PARSER_ERR;
	rti->data = bnf_tree_root;
  
	root_key = "<query expression>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rti = pop_syntax_htab("<query expression>");
	if (!rti) return PARSER_ERR;
	rti->data = bnf_tree_root;
  
	root_key = "<direct select statement: multiple rows>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab(root_key, bnf_tree_root); 
	if (!rt) return PARSER_ERR;
  
	#if 0
	root_key = "<procedure>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<procedure>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
	#endif
  
	#if 0
	root_key = "<module>";
	debug("\n\n[make_bnf_tree]... root: %s \n", root_key);
	(void)make_bnf_tree(NULL, root_key, strlen(root_key), &htab, 0);
	debug("\n[make_bnf_tree], done. \n\n");
  
	debug("hash table %d entries, %d entries used, %d entries free. \n", 
	htab.size, htab.filled, htab.size - htab.filled);
  
	debug("\n\n[make_graph]... \n");	
	es = make_graph(bnf_tree_root);
	debug("\n[make_graph], done. \n\n");
  
	show_nodes(es);
  
	rt = push_syntax_htab("<module>", bnf_tree_root); 
	if (!rt) return PARSER_ERR;
	#endif
  
	debug("\nmake all lex and syntax trees done. \n\n");
  
	#if 0
	rt = dbg_syntax();
	if (!rt) return PARSER_ERR;
	#endif
  
	rt = init_stack();
	if (!rt) return PARSER_ERR;

    */  
  
    func_ok();
    return PARSER_OK;
}

/*
  s_object*
  parse(char *sql)
  {
  int rt;
  token_list lst;
  s_object *obj;
  ENTRY *rti;
  tr_node_s *root;
  char key[256];

  fs();
  
  memset(&lst, 0, sizeof(token_list));
  rt = check_lex(sql, &lst);
  if (!rt) goto ERR;

  strcpy(key, lst.next->tk.value);
  debug("key: %s \n", key);
  
  obj = pop_obj_htab(key);
  if (!obj) goto ERR;

  debug("object name: %s \n", obj->name);
  
  rti = pop_syntax_htab(obj->name);
  if (!rti) goto ERR;
  root = (tr_node_s*)rti->data;
  
  rt = check_syntax(root, &lst);
  if (!rt) goto ERR; 
  
  rt = push_stack(&lst, obj);
  if (!rt) goto ERR;
  
  fe();
  return obj;
  
  ERR:

  free_tokens(&lst);
  gc_free();
  return NULL;
  }


  int
  msql(void)
  {
  int rt;
  char sql[1024];
  s_object *obj;
  
  fs();

  rt = init_parser();
  if (!rt) return 0;
  
  while (1) {

  debug("\r\n\nMSQL\n$ ");

  fflush(stdout);
  fgets(sql, sizeof(sql) -1, stdin);

  if (strlen(sql) <= 1) continue;
  if (!strcmpi(sql, "q\n") || !strcmpi(sql, "quit\n") || 
  !strcmpi(sql, "exit\n")) break;

  sql[strlen(sql) - 1] = ' ';

  set_gc(gc());
    
  obj = parse(sql);
  if (!obj) continue;

  if (obj->execute) obj->execute(obj->stack);

  gc_free();
  }
  
  fe();
  return 1;
  }
*/



