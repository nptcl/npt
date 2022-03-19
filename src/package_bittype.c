#include "condition_define.h"
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
	SetNameSymbol(symbol, name);
	SetPackageSymbol(symbol, package);
	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package)
{
	make_bitpackage(ret, name, package);
	GetBitTypeSymbol(*ret, symbol);
}

void internbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

void importbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeImport(bit, 1);
	*ret = bit;
}

void inheritedbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INHERITED);
	SetBitTypeInherit(bit, 1);
	*ret = bit;
}

void shadowintern_bitpackage(addr bit, addr name, addr package)
{
	addr symbol;
	struct bittype_struct *str;

	symbol_heap(&symbol);
	SetNameSymbol(symbol, name);
	SetPackageSymbol(symbol, package);
	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	str->intern = PACKAGE_TYPE_INTERNAL;
	str->base = 1;
	str->inherit = 0;
}

void shadowimport_bitpackage(addr bit, addr symbol)
{
	struct bittype_struct *str;

	SetBitTypeSymbol(bit, symbol);
	str = StructBitType(bit);
	str->intern = PACKAGE_TYPE_INTERNAL;
	str->base = 0;
	str->import = 1;
	str->inherit = 0;
}

static int intern_read_bitpackage_(addr package, addr name, addr *value, int *ret)
{
	addr table, cons;

	Check(! get_readonly_package(package), "readonly error.");
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(findcons_hashtable_(table, name, &cons));
	if (cons == Nil) {
		*value = Nil;
		*ret = 0;
		return call_simple_package_error_va_(NULL,
				"Cannot intern the symbol ~S because ~S is a readonly package.",
				name, package, NULL);
	}
	GetCdr(cons, value);
	Check(*value == Nil, "bitpackage error.");
	return Result(ret, 0); /* exist */
}

static int intern_write_bitpackage_(addr package, addr name, addr *value, int *ret)
{
	addr table, cons, bit;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(intern_hashheap_(table, name, &cons));
	GetCdr(cons, &bit);
	if (bit == Nil) {
		make_bitpackage(&bit, name, package);
		SetCdr(cons, bit);
		*value = bit;
		return Result(ret, 1); /* new */
	}
	*value = bit;
	return Result(ret, 0); /* exist */
}

int intern_bitpackage_(addr package, addr name, addr *value, int *ret)
{
	if (get_readonly_package(package))
		return intern_read_bitpackage_(package, name, value, ret);
	else
		return intern_write_bitpackage_(package, name, value, ret);
}

int find_bitpackage_(addr package, addr name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	return findnil_hashtable_(package, name, ret);
}

int find_char_bitpackage_(addr package, const char *name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	return findnil_char_hashtable_(package, name, ret);
}

