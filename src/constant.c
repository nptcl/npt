#include <string.h>
#include "build.h"
#include "constant.h"
#include "hashtable.h"
#include "memory.h"
#include "object.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"

/*
 *  build_constant
 */
void build_constant(void)
{
	addr array;
	size_t i;

	heap_array4(&array, LISPTYPE_SYSTEM, CONSTANT_SIZE);
	for (i = 0; i < CONSTANT_SIZE; i++)
		SetArrayA4(array, i, Unbound);
	SetRoot(LISPINDEX_CONST, array);
}


/*
 *  interface
 */
void specialconstant(enum CONSTANT_INDEX index, const char *package, const char *name)
{
	addr symbol;

	internchar(package, name, &symbol);
	setspecial_symbol(symbol);
	SetConstant(index, symbol);
}

void gensymconstant(enum CONSTANT_INDEX index, const char *name)
{
	addr string, symbol;

	strvect_char_heap(&string, name);
	symbol_heap(&symbol);
	SetNameSymbol(symbol, string);
	SetConstant(index, symbol);
}

void keywordconstant(enum CONSTANT_INDEX index, const char *name)
{
	addr symbol;

	internchar_keyword(name, &symbol);
	SetConstant(index, symbol);
}

void commonconstant(enum CONSTANT_INDEX index, const char *name)
{
	addr symbol;

	interncommon(name, &symbol);
	SetConstant(index, symbol);
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

void symbolchar_common(enum CONSTANT_INDEX index, const char *name)
{
	char buffer[SYMBOLCHAR_SIZE];
	addr symbol;

	copy_symbolchar(buffer, name, SYMBOLCHAR_SIZE);
	interncommon(name, &symbol);
	SetConstant(index, symbol);
}

void symbolchar_keyword(enum CONSTANT_INDEX index, const char *name)
{
	char buffer[SYMBOLCHAR_SIZE];
	addr symbol;

	copy_symbolchar(buffer, name, SYMBOLCHAR_SIZE);
	internchar_keyword(name, &symbol);
	SetConstant(index, symbol);
}

