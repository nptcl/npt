#ifndef __READER_TYPE_HEADER__
#define __READER_TYPE_HEADER__

#include "typedef.h"

#define ptr_readtype _n(ptr_readtype)
#define struct_readtype _n(struct_readtype)
#define get_readtype _n(get_readtype)
#define set_readtype _n(set_readtype)
#define dispatch_readtype _n(dispatch_readtype)
#define make_readtype _n(make_readtype)
#define copy_readtype _n(copy_readtype)
#define default_array_readtype _n(default_array_readtype)
#define default_dispatch_readtype_ _n(default_dispatch_readtype_)
#define array_readtype_heap _n(array_readtype_heap)
#define dispatch_readtype_heap _n(dispatch_readtype_heap)
#define make_array_readtype _n(make_array_readtype)
#define make_table_readtype _n(make_table_readtype)
#define make_dispatch_readtype_ _n(make_dispatch_readtype_)
#define readtype_whitespace _n(readtype_whitespace)
#define readtype_constituent _n(readtype_constituent)
#define readtype_termmacro _n(readtype_termmacro)
#define readtype_sharpmacro _n(readtype_sharpmacro)
#define delete_readtype_ _n(delete_readtype_)

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

void *ptr_readtype(addr pos);
struct readtype_struct *struct_readtype(addr pos);
void get_readtype(addr pos, addr *ret);
void set_readtype(addr pos, addr value);

int dispatch_readtype(addr pos);
void make_readtype(addr *ret,
		enum ReadTable_Type type, unicode code, unsigned dispatch);
void copy_readtype(addr *ret, addr copy);
void default_array_readtype(addr array);
int default_dispatch_readtype_(addr pos, unicode u);
void array_readtype_heap(addr *ret);
void dispatch_readtype_heap(addr *ret);
void make_array_readtype(addr *ret);
void make_table_readtype(addr *ret);
int make_dispatch_readtype_(addr *ret);

int readtype_whitespace(unicode u);
int readtype_constituent(unicode u);
int readtype_termmacro(unicode u, addr *ret);
int readtype_sharpmacro(unicode u, addr *ret);
int delete_readtype_(addr pos, unicode c);

#endif

