#ifndef __READER_TABLE_HEADER__
#define __READER_TABLE_HEADER__

#include "execute.h"
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

#define GetArrayReadtable_Low(p,v)		GetArraySS((p), READTABLE_ARRAY, (v))
#define SetArrayReadtable_Low(p,v)		SetArraySS((p), READTABLE_ARRAY, (v))
#define GetTableReadtable_Low(p,v)		GetArraySS((p), READTABLE_TABLE, (v))
#define SetTableReadtable_Low(p,v)		SetArraySS((p), READTABLE_TABLE, (v))
#define GetDispatchReadtable_Low(p,v)	GetArraySS((p), READTABLE_DISPATCH, (v))
#define SetDispatchReadtable_Low(p,v)	SetArraySS((p), READTABLE_DISPATCH, (v))
#define PtrReadtable_Low(p)				PtrBodySSa((p), READTABLE_SIZE)
#define PtrCaseReadtable_Low(p)			((enum ReadTable_Case *)PtrReadtable(p))

#ifdef LISP_DEBUG
#define GetArrayReadtable				getarray_readtable
#define SetArrayReadtable				setarray_readtable
#define GetTableReadtable				gettable_readtable
#define SetTableReadtable				settable_readtable
#define GetDispatchReadtable			getdispatch_readtable
#define SetDispatchReadtable			setdispatch_readtable
#define PtrReadtable					ptr_readtable
#define PtrCaseReadtable				ptrcase_readtable
#else
#define GetArrayReadtable				GetArrayReadtable_Low
#define SetArrayReadtable				SetArrayReadtable_Low
#define GetTableReadtable				GetTableReadtable_Low
#define SetTableReadtable				SetTableReadtable_Low
#define GetDispatchReadtable			GetDispatchReadtable_Low
#define SetDispatchReadtable			SetDispatchReadtable_Low
#define PtrReadtable					PtrReadtable_Low
#define PtrCaseReadtable				PtrCaseReadtable_Low
#endif

_g void getarray_readtable(addr pos, addr *ret);
_g void setarray_readtable(addr pos, addr value);
_g void gettable_readtable(addr pos, addr *ret);
_g void settable_readtable(addr pos, addr value);
_g void getdispatch_readtable(addr pos, addr *ret);
_g void setdispatch_readtable(addr pos, addr value);
_g void *ptr_readtable(addr pos);
_g enum ReadTable_Case *ptrcase_readtable(addr pos);

_g void readtable_heap(addr *ret);
_g void copy_readtable(addr from, addr to);
_g void copy_readtable_heap(addr copy, addr *ret);
_g void copy_default_readtable(addr pos);
_g void make_dispatch_macro_character(addr pos, addr character, int nonterm);
_g void get_default_dispatch_macro(addr code1, addr code2, addr *ret);
_g int macro_character_execute(Execute ptr, int *result, addr *ret,
		unicode c, addr stream, addr table);
_g void get_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr *ret);
_g void rem_dispatch_macro_character(addr pos, unicode u1, unicode u2);
_g void set_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr call);
_g void get_default_macro_character(unicode u, addr *ret, int *nonterm);
_g void readtype_readtable(addr pos, unicode c, addr *ret);
_g void get_macro_character(addr pos, unicode u, addr *ret, int *nonterm);
_g void set_macro_character(addr pos, unicode u, int nonterm, addr call);
_g void set_syntax_from_default(unicode u1, unicode u2, addr to);
_g void set_syntax_from_char(unicode u1, unicode u2, addr to, addr from);

enum ReadTable_float float_readtable(Execute ptr);
_g enum ReadTable_Case readcase_readtable(Execute ptr);
_g enum ReadTable_Case getcase_readtable(addr pos);
_g void setcase_readtable(addr pos, enum ReadTable_Case mode);
_g void getreadtable(Execute ptr, addr *ret);

#endif

