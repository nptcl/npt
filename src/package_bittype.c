#include "heap.h"
#include "hashtable.h"
#include "package_bittype.h"
#include "symbol.h"
#include "typedef.h"

static void alloc_bitpackage(addr *ret, addr symbol, enum PACKAGE_TYPE type)
{
	addr bit;
	struct bittype_struct *str;

	heap_smallsize(&bit, LISPSYSTEM_BITTYPE, 1, sizeof(struct bittype_struct));
	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	clearpoint(str);
	str->intern = type;
	*ret = bit;
}

static void make_bitpackage(addr *ret, addr name, addr package)
{
	addr bit, symbol;

	symbol_heap(&symbol);
	setname_symbol(symbol, name);
	setpackage_symbol(symbol, package);
	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

_g void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package)
{
	make_bitpackage(ret, name, package);
	GetBitTypeSymbol(*ret, symbol);
}

_g void internbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

_g void importbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeImport(bit, 1);
	*ret = bit;
}

_g void inheritedbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INHERITED);
	SetBitTypeInherit(bit, 1);
	*ret = bit;
}

_g void shadowintern_bitpackage(addr bit, addr name, addr package)
{
	addr symbol;
	struct bittype_struct *str;

	symbol_heap(&symbol);
	setname_symbol(symbol, name);
	setpackage_symbol(symbol, package);
	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	str->intern = PACKAGE_TYPE_INTERNAL;
	str->base = 1;
	str->inherit = 0;
}

_g void shadowimport_bitpackage(addr bit, addr symbol)
{
	struct bittype_struct *str;

	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	str->intern = PACKAGE_TYPE_INTERNAL;
	str->base = 0;
	str->import = 1;
	str->inherit = 0;
}

_g int intern_bitpackage(addr package, addr name, addr *ret)
{
	addr table, cons, bit;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	intern_hashheap(table, name, &cons);
	GetCdr(cons, &bit);
	if (bit == Nil) {
		make_bitpackage(&bit, name, package);
		SetCdr(cons, bit);
		*ret = bit;
		return 1; /* new */
	}
	*ret = bit;
	return 0; /* exist */
}

_g void find_bitpackage(addr package, addr name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	findvalue_hashtable(package, name, ret);
}

_g void find_char_bitpackage(addr package, const char *name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	findvalue_char_hashtable(package, name, ret);
}

