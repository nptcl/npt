#include <string.h>
#include "build.h"
#include "cons.h"
#include "constant.h"
#include "hashtable.h"
#include "memory.h"
#include "object.h"
#include "package_symbol.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  build_constant
 */
_g void build_constant(void)
{
	addr array;
	size_t i;

	heap_array4(&array, LISPSYSTEM_CONSTANT, CONSTANT_SIZE);
	for (i = 0; i < CONSTANT_SIZE; i++)
		SetArrayA4(array, i, Unbound);
	SetLispRoot(CONST, array);
}


/*
 *  interface
 */
_g int specialconstant_(constindex index, const char *package, const char *name)
{
	addr symbol;

	Return(internchar_(package, name, &symbol, NULL));
	setspecial_symbol(symbol);
	SetConstant(index, symbol);

	return 0;
}

_g void gensymconstant(constindex index, const char *name)
{
	addr string, symbol;

	strvect_char_heap(&string, name);
	symbol_heap(&symbol);
	SetNameSymbol(symbol, string);
	SetConstant(index, symbol);
}

_g int keywordconstant_(constindex index, const char *name)
{
	addr symbol;

	Return(internchar_keyword_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}

_g int commonconstant_(constindex index, const char *name)
{
	addr symbol;

	Return(interncommon_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}


/*
 *  symbol.c
 */
#define SYMBOLCHAR_SIZE	256

static void copy_symbolchar(char *dst, const char *src, size_t size)
{
	int c;
	size_t i;

	for (i = 0; ; i++) {
		if ((SYMBOLCHAR_SIZE - 1) <= i) {
			Abort("buffer overflow.");
		}
		c = src[i];
		if (c == 0) break;
		dst[i] = (c == '_')? '-': c;
	}
	dst[i] = 0;
}

_g int symbolchar_common_(constindex index, const char *name)
{
	char buffer[SYMBOLCHAR_SIZE];
	addr symbol;

	copy_symbolchar(buffer, name, SYMBOLCHAR_SIZE);
	Return(interncommon_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}

_g int symbolchar_keyword_(constindex index, const char *name)
{
	char buffer[SYMBOLCHAR_SIZE];
	addr symbol;

	copy_symbolchar(buffer, name, SYMBOLCHAR_SIZE);
	Return(internchar_keyword_(name, &symbol, NULL));
	SetConstant(index, symbol);

	return 0;
}

_g void quotelist_heap(addr *ret, addr name)
{
	addr quote;
	GetConst(COMMON_QUOTE, &quote);
	list_heap(ret, quote, name, NULL);
}

_g void pushconst_heap(addr *ret, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	cons_heap(ret, pos, *ret);
}

