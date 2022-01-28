#ifndef __READER_TABLE_HEADER__
#define __READER_TABLE_HEADER__

#include "execute.h"
#include "typedef.h"

#define getarray_readtable _n(getarray_readtable)
#define setarray_readtable _n(setarray_readtable)
#define gettable_readtable _n(gettable_readtable)
#define settable_readtable _n(settable_readtable)
#define getdispatch_readtable _n(getdispatch_readtable)
#define setdispatch_readtable _n(setdispatch_readtable)
#define ptr_readtable _n(ptr_readtable)
#define ptrcase_readtable _n(ptrcase_readtable)
#define readtable_heap_ _n(readtable_heap_)
#define copy_readtable_ _n(copy_readtable_)
#define copy_readtable_heap_ _n(copy_readtable_heap_)
#define copy_default_readtable_ _n(copy_default_readtable_)
#define make_dispatch_macro_character_ _n(make_dispatch_macro_character_)
#define get_default_dispatch_macro_ _n(get_default_dispatch_macro_)
#define macro_character_execute_ _n(macro_character_execute_)
#define get_dispatch_macro_character_ _n(get_dispatch_macro_character_)
#define rem_dispatch_macro_character_ _n(rem_dispatch_macro_character_)
#define set_dispatch_macro_character_ _n(set_dispatch_macro_character_)
#define get_default_macro_character _n(get_default_macro_character)
#define readtype_readtable_ _n(readtype_readtable_)
#define get_macro_character_ _n(get_macro_character_)
#define set_macro_character_ _n(set_macro_character_)
#define set_syntax_from_default_ _n(set_syntax_from_default_)
#define set_syntax_from_char_ _n(set_syntax_from_char_)
#define float_readtable_ _n(float_readtable_)
#define readcase_readtable_ _n(readcase_readtable_)
#define getcase_readtable _n(getcase_readtable)
#define setcase_readtable _n(setcase_readtable)
#define getreadtable_ _n(getreadtable_)

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

void getarray_readtable(addr pos, addr *ret);
void setarray_readtable(addr pos, addr value);
void gettable_readtable(addr pos, addr *ret);
void settable_readtable(addr pos, addr value);
void getdispatch_readtable(addr pos, addr *ret);
void setdispatch_readtable(addr pos, addr value);
void *ptr_readtable(addr pos);
enum ReadTable_Case *ptrcase_readtable(addr pos);

int readtable_heap_(addr *ret);
int copy_readtable_(addr from, addr to);
int copy_readtable_heap_(addr from, addr *ret);
int copy_default_readtable_(addr pos);
int make_dispatch_macro_character_(addr pos, addr character, int nonterm);
int get_default_dispatch_macro_(addr code1, addr code2, addr *ret);
int macro_character_execute_(Execute ptr, int *result, addr *ret,
		unicode c, addr stream, addr table);
int get_dispatch_macro_character_(addr pos, unicode u1, unicode u2, addr *ret);
int rem_dispatch_macro_character_(addr pos, unicode u1, unicode u2);
int set_dispatch_macro_character_(addr pos, unicode u1, unicode u2, addr call);
void get_default_macro_character(unicode u, addr *ret, int *nonterm);
int readtype_readtable_(addr pos, unicode c, addr *ret);
int get_macro_character_(addr pos, unicode u, addr *ret, int *nonterm);
int set_macro_character_(addr pos, unicode u, int nonterm, addr call);
int set_syntax_from_default_(unicode u1, unicode u2, addr to);
int set_syntax_from_char_(unicode u1, unicode u2, addr to, addr from);

int float_readtable_(Execute ptr, enum ReadTable_float *ret);
int readcase_readtable_(Execute ptr, enum ReadTable_Case *ret);
enum ReadTable_Case getcase_readtable(addr pos);
void setcase_readtable(addr pos, enum ReadTable_Case mode);
int getreadtable_(Execute ptr, addr *ret);

#endif

