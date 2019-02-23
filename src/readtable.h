#ifndef __READTABLE_HEADER__
#define __READTABLE_HEADER__

#include "typedef.h"

/*
 *  readtable                  : smallsize
 *    READTABLE_ARRAY          : array2(80)
 *    READTABLE_TABLE          : hashtable: (character, codetype)
 *    READTABLE_DISPATCH       : hashtable: (character2, function)
 *  body
 *    case                     : enum ReadTable_Case
 */
enum ReadTable_Case {
	ReadTable_upcase,
	ReadTable_downcase,
	ReadTable_preserve,
	ReadTable_invert,
	ReadTable_SIZE
};

enum ReadTable_float {
	ReadTable_short,
	ReadTable_single,
	ReadTable_double,
	ReadTable_long,
	ReadTable_size
};

enum READTABLE_INDEX {
	READTABLE_ARRAY = 0,
	READTABLE_TABLE,
	READTABLE_DISPATCH,
	READTABLE_SIZE
};

/* interface */
void copy_readtable(addr from, addr to);
void copy_readtable_heap(addr copy, addr *ret);
void copy_default_readtable(addr pos);
void readtable_heap(addr *ret);
void make_dispatch_macro_character(addr pos, addr character, int nonterm);
int read_delimited_list(Execute ptr, addr stream, unicode limit, int recursive);
void get_default_dispatch_macro(addr code1, addr code2, addr *ret);
void get_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr *ret);
void rem_dispatch_macro_character(addr pos, unicode u1, unicode u2);
void set_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr call);
void get_default_macro_character(unicode u, addr *ret, int *nonterm);
void get_macro_character(addr pos, unicode u, addr *ret, int *nonterm);
void set_macro_character(addr pos, unicode u, int nonterm, addr call);
void set_syntax_from_default(unicode u1, unicode u2, addr to);
void set_syntax_from_char(unicode u1, unicode u2, addr to, addr from);
enum ReadTable_Case getcase_readtable(addr pos);
void setcase_readtable(addr pos, enum ReadTable_Case mode);
void getreadtable(Execute ptr, addr *ret);
int read_stream(Execute ptr, addr stream, int *result, addr *ret);
int read_preserving(Execute ptr, addr stream, int *result, addr *ret);
int read_recursive(Execute ptr, addr stream, int *result, addr *ret);
int read_from_string(int *result, addr *ret, addr pos);
int readstring(addr *ret, const char *code);
addr readr(const char *code);

void init_readtable(void);
void build_readtable(void);
void in_package_lisp_package(void);

#endif

