#ifndef __READER_TYPE_HEADER__
#define __READER_TYPE_HEADER__

#include "typedef.h"

enum ReadTable_Type {
	ReadTable_Type_illegal,
	ReadTable_Type_whitespace,
	ReadTable_Type_constituent,
	ReadTable_Type_macro_term,
	ReadTable_Type_macro_nonterm,
	ReadTable_Type_escape_single,
	ReadTable_Type_escape_multiple,
	ReadTable_Type_SIZE
};

struct readtype_struct {
	unsigned dispatch : 1;
	enum ReadTable_Type type : 5;
	unicode code;
};

#define PtrReadType_Low(p)		PtrBodySSa((p), 1)
#define ReadTypeStruct_Low(p)	((struct readtype_struct *)PtrReadType_Low(p))
#define GetReadType_Low(p,v)	GetArraySS((p), 0, (v))
#define SetReadType_Low(p,v)	SetArraySS((p), 0, (v))

#ifdef LISP_DEBUG
#define PtrReadType				ptr_readtype
#define ReadTypeStruct			struct_readtype
#define GetReadType				get_readtype
#define SetReadType				set_readtype
#else
#define PtrReadType				PtrReadType_Low
#define ReadTypeStruct			ReadTypeStruct_Low
#define GetReadType				GetReadType_Low
#define SetReadType				SetReadType_Low
#endif

_g void *ptr_readtype(addr pos);
_g struct readtype_struct *struct_readtype(addr pos);
_g void get_readtype(addr pos, addr *ret);
_g void set_readtype(addr pos, addr value);

_g int dispatch_readtype(addr pos);
_g void make_readtype(addr *ret,
		enum ReadTable_Type type, unicode code, unsigned dispatch);
_g void copy_readtype(addr *ret, addr copy);
_g void default_array_readtype(addr array);
_g int default_dispatch_readtype_(addr pos, unicode u);
_g void array_readtype_heap(addr *ret);
_g void dispatch_readtype_heap(addr *ret);
_g void make_array_readtype(addr *ret);
_g void make_table_readtype(addr *ret);
_g int make_dispatch_readtype_(addr *ret);

_g int readtype_whitespace(unicode u);
_g int readtype_constituent(unicode u);
_g int readtype_termmacro(unicode u, addr *ret);
_g int readtype_sharpmacro(unicode u, addr *ret);
_g int delete_readtype_(addr pos, unicode c);

#endif

