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
_g void copy_readtable(addr from, addr to);
_g void copy_readtable_heap(addr copy, addr *ret);
_g void copy_default_readtable(addr pos);
_g void readtable_heap(addr *ret);
_g void make_dispatch_macro_character(addr pos, addr character, int nonterm);
_g int read_delimited_list(Execute ptr, addr stream, unicode limit, int recursive);
_g void get_default_dispatch_macro(addr code1, addr code2, addr *ret);
_g void get_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr *ret);
_g void rem_dispatch_macro_character(addr pos, unicode u1, unicode u2);
_g void set_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr call);
_g void get_default_macro_character(unicode u, addr *ret, int *nonterm);
_g void get_macro_character(addr pos, unicode u, addr *ret, int *nonterm);
_g void set_macro_character(addr pos, unicode u, int nonterm, addr call);
_g void set_syntax_from_default(unicode u1, unicode u2, addr to);
_g void set_syntax_from_char(unicode u1, unicode u2, addr to, addr from);
enum ReadTable_float float_readtable(Execute ptr);
_g enum ReadTable_Case readcase_readtable(Execute ptr);
_g enum ReadTable_Case getcase_readtable(addr pos);
_g void setcase_readtable(addr pos, enum ReadTable_Case mode);
_g void getreadtable(Execute ptr, addr *ret);
_g int read_stream(Execute ptr, addr stream, int *result, addr *ret);
_g int read_preserving(Execute ptr, addr stream, int *result, addr *ret);
_g int read_recursive(Execute ptr, addr stream, int *result, addr *ret);
_g int read_from_string(int *result, addr *ret, addr pos);
_g int readstring(addr *ret, const char *code);
_g addr readr(const char *code);

_g void init_readtable(void);
_g void build_readtable(void);
_g void in_package_lisp_package(void);

#endif

