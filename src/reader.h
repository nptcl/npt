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

int readtable_typetable_(addr pos, unicode c, enum ReadTable_Type *ret);
int readtable_result_(Execute ptr,
		addr *token, addr stream, addr table, enum ReadTable_Result *ret);
int readtable_novalue(Execute ptr, int *result, addr *ret, addr stream, addr table);
int read_call(Execute ptr, addr stream, int *result, addr *ret);
int read_stream(Execute ptr, addr stream, int *result, addr *ret);
int read_preserving(Execute ptr, addr stream, int *result, addr *ret);
int read_recursive(Execute ptr, addr stream, int *result, addr *ret);
int read_from_string(Execute ptr, int *result, addr *ret, addr pos);
int readstring_debug(addr *ret, const char *code);
addr readr_debug(const char *code);

void init_reader(void);
void build_reader(void);

#endif

