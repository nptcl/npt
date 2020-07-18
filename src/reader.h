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

_g enum ReadTable_Type readtable_typetable(addr pos, unicode c);
_g int readtable_result_(Execute ptr,
		addr *token, addr stream, addr table, enum ReadTable_Result *ret);
_g int readtable_novalue(Execute ptr, int *result, addr *ret, addr stream, addr table);
_g int read_call(Execute ptr, addr stream, int *result, addr *ret);
_g int read_stream(Execute ptr, addr stream, int *result, addr *ret);
_g int read_preserving(Execute ptr, addr stream, int *result, addr *ret);
_g int read_recursive(Execute ptr, addr stream, int *result, addr *ret);
_g int read_from_string(Execute ptr, int *result, addr *ret, addr pos);
_g int readstring(addr *ret, const char *code);
_g addr readr(const char *code);

_g void init_reader(void);
_g void build_reader(void);

#endif

