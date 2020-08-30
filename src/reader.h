#ifndef __READER_HEADER__
#define __READER_HEADER__

#include "execute.h"
#include "reader_type.h"
#include "typedef.h"

enum ReadTable_Result {
	ReadTable_Result_normal,
	ReadTable_Result_macro,
	ReadTable_Result_eof
};

#define readtable_typetable_ _n(readtable_typetable_)
#define readtable_result_ _n(readtable_result_)
#define readtable_novalue _n(readtable_novalue)
#define read_call _n(read_call)
#define read_stream _n(read_stream)
#define read_preserving _n(read_preserving)
#define read_recursive _n(read_recursive)
#define read_from_string _n(read_from_string)
#define readstring_debug _n(readstring_debug)
#define readr_debug _n(readr_debug)
#define init_reader _n(init_reader)
#define build_reader _n(build_reader)

_g int readtable_typetable_(addr pos, unicode c, enum ReadTable_Type *ret);
_g int readtable_result_(Execute ptr,
		addr *token, addr stream, addr table, enum ReadTable_Result *ret);
_g int readtable_novalue(Execute ptr, int *result, addr *ret, addr stream, addr table);
_g int read_call(Execute ptr, addr stream, int *result, addr *ret);
_g int read_stream(Execute ptr, addr stream, int *result, addr *ret);
_g int read_preserving(Execute ptr, addr stream, int *result, addr *ret);
_g int read_recursive(Execute ptr, addr stream, int *result, addr *ret);
_g int read_from_string(Execute ptr, int *result, addr *ret, addr pos);
_g int readstring_debug(addr *ret, const char *code);
_g addr readr_debug(const char *code);

_g void init_reader(void);
_g void build_reader(void);

#endif

