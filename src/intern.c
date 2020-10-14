#include "constant.h"
#include "condition.h"
#include "define.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_export.h"
#include "package_object.h"
#include "package_symbol.h"
#include "strvect.h"
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
	constindex index;
	enum symbol_package package;
	const char *symbol;
	size_t length;
	fixnum sxhash;
	int specialp;
	int exportp;
	int findp;
};

static struct symbol_header SymbolHeader[] = {
#ifdef LISP_64BIT
#include "intern_symbol_64.h"
#else
#include "intern_symbol_32.h"
#endif
	{ CONSTANT_EMPTY, DEFAULT, NULL, 0, 0, 0, 0, 0 }
};

static int intern_symbol_package_(addr package, struct symbol_header *str, addr *ret)
{
	addr name, value, car, cdr;
	size_t size, index;

	/* bitpackage */
	Check(! packagep(package), "type error");
	Return(strvect_size1_heap_(&name, str->symbol, str->length));
	if (str->findp)
		return intern_package_(package, name, ret, NULL);

#ifdef LISP_DEBUG
	{
		enum PACKAGE_TYPE debug;
		addr ignore;
		Error(find_symbol_package_(package, name, &ignore, &debug));
		Check(debug != PACKAGE_TYPE_NIL, "find error");
	}
#endif
	make_bitpackage_symbol(&value, ret, name, package);
#ifdef LISP_DEBUG
	{
		fixnum debug;
		Error(sxhash_char_equal_(str->symbol, &debug));
		Check(debug != str->sxhash, "sxhash error");
	}
#endif

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

	return 0;
}

_g int intern_symbol_header_(void)
{
	addr symbol, p_common, p_keyword, p_system, p_code, p_clos, p_rt, package;
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
		if (table->index == CONSTANT_EMPTY)
			break;
		switch (table->package) {
			case COMMON:
				package = p_common;
				Return(intern_symbol_package_(package, table, &symbol));
				Return(export_package_(package, symbol));
				break;

			case KEYWORD:
				package = p_keyword;
				Return(intern_symbol_package_(package, table, &symbol));
				Return(setkeyword_package_(symbol));
				break;

			case SYSTEM:
				package = p_system;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			case CODE:
				package = p_code;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			case CLOS:
				package = p_clos;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			case RT:
				package = p_rt;
				Return(intern_symbol_package_(package, table, &symbol));
				break;

			default:
				return fmte_("package error.", NULL);
		}
		if (table->specialp)
			setspecial_symbol(symbol);
		if (table->exportp) {
			Return(export_package_(package, symbol));
		}
		SetConstant(table->index, symbol);
	}

	return 0;
}

