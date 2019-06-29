#include "constant.h"
#include "condition.h"
#include "hashtable.h"
#include "package.h"
#include "sxhash.h"
#include "symbol.h"

enum symbol_package {
	COMMON,
	KEYWORD,
	SYSTEM,
	CODE,
	CLOS,
	RT,
	DEFAULT
};

struct symbol_header {
	enum CONSTANT_INDEX index;
	enum symbol_package package;
	const char *symbol;
	size_t length;
	fixnum sxhash;
	int specialp;
	int findp;
};

static struct symbol_header SymbolHeader[] = {
#ifdef LISP_ARCH_64BIT
#include "intern_symbol_64.h"
#else
#include "intern_symbol_32.h"
#endif
	{ CONSTANT_EMPTY, DEFAULT, NULL, 0, 0, 0, 0 }
};

static void intern_symbol_package(addr package, struct symbol_header *str, addr *ret)
{
	addr name, value, car, cdr;
	size_t size, index;

	/* bitpackage */
	Check(! packagep(package), "type error");
	name = strvect_size1_allocr(NULL, str->symbol, str->length);
	if (str->findp) {
		intern_package(package, name, ret);
		return;
	}
	Check(find_symbol_package(package, name, &cdr) != PACKAGE_TYPE_NIL, "find error");
	make_bitpackage_symbol(&value, ret, name, package);
	Check(sxhash_char_equal(str->symbol) != str->sxhash, "sxhash error");

	/* intern */
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	getsize_hashtable(package, &size);
	inccount_hashtable(package, 1);
	GetTableHash(package, &package);
	/* array[index] -> ((key . nil) . next)
	 * ret -> (key . nil) */
	index = str->sxhash % size;
	GetArrayHash(package, index, &cdr);
	cons_heap(&car, name, value);
	cons_heap(&cdr, car, cdr);
	SetArrayHash(package, index, cdr);
}

_g void intern_symbol_header(void)
{
	addr symbol, p_common, p_keyword, p_system, p_code, p_clos, p_rt;
	struct symbol_header *table;
	size_t i;

	GetConst(PACKAGE_COMMON_LISP, &p_common);
	GetConst(PACKAGE_KEYWORD, &p_keyword);
	GetConst(PACKAGE_SYSTEM, &p_system);
	GetConst(PACKAGE_CODE, &p_code);
	GetConst(PACKAGE_CLOS, &p_clos);
	GetConst(PACKAGE_RT, &p_rt);

	for (i = 0; ; i++) {
		table = &SymbolHeader[i];
		if (table->index == CONSTANT_EMPTY) break;
		switch (table->package) {
			case COMMON:
				intern_symbol_package(p_common, table, &symbol);
				export_package(p_common, symbol);
				break;

			case KEYWORD:
				intern_symbol_package(p_keyword, table, &symbol);
				setkeyword_package(symbol);
				break;

			case SYSTEM:
				intern_symbol_package(p_system, table, &symbol);
				break;

			case CODE:
				intern_symbol_package(p_code, table, &symbol);
				break;

			case CLOS:
				intern_symbol_package(p_clos, table, &symbol);
				break;

			case RT:
				intern_symbol_package(p_rt, table, &symbol);
				break;

			default:
				fmte("package error.", NULL);
				break;
		}
		if (table->specialp)
			setspecial_symbol(symbol);
		SetConstant(table->index, symbol);
	}
}

