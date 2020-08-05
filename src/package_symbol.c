#include "condition.h"
#include "constant.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_object.h"
#include "package_symbol.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  intern
 */
_g int intern_package_table_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	int check;
	enum PACKAGE_TYPE result;
	addr keyword, bit;

	/* intern */
	Return(intern_bitpackage_(package, name, &bit, &check));
	result = check? PACKAGE_TYPE_NIL: StructBitType(bit)->intern;
	GetBitTypeSymbol(bit, value);

	/* keyword */
	GetConst(PACKAGE_KEYWORD, &keyword);
	if (keyword == package) {
		Return(setkeyword_package_(*value));
	}

	if (ret)
		return Result(ret, result);
	else
		return 0;
}

_g int intern_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(! stringp(name), "type error");

	Return(package_designer_(package, &package));
	return intern_package_table_(package, name, value, ret);
}

_g int intern_char_package_(addr package, const char *name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr symbol;

	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(GetType(package) != LISPTYPE_PACKAGE, "type error");

	/* find symbol */
	Return(find_char_bitpackage_(package, name, &symbol));
	if (symbol != Nil) {
		GetBitTypeSymbol(symbol, value);
		if (ret)
			return Result(ret, StructBitType(symbol)->intern);
		else
			return 0;
	}

	/* intern */
	strvect_char_heap(&symbol, name);
	return intern_package_(package, symbol, value, ret);
}


/*
 *  unintern
 */
static int check_export_unintern_(addr pos, addr name, int *ret)
{
	int check;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	Return(find_hashtable_(pos, name, &pos));
	check = (pos != Unbound && StructBitType(pos)->expt);

	return Result(ret, check);
}

static int check_shadowing_unintern_(addr package, addr name, int *ret)
{
	int loop, check;
	addr left, right;

	GetPackage(package, PACKAGE_INDEX_USE, &right);
	loop = 0;
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(check_export_unintern_(left, name, &check));
		if (check) {
			if (loop)
				return Result(ret, 1);
			loop = 1;
		}
	}

	return Result(ret, 0);
}

/*
 * If package have no symbols, return 1.
 * If package don't have the symbol, return 1.
 * If there are symbols in shadowing-symbols, check conflict in use-package.
 */
static int unintern_check_package_(addr package, addr symbol, int *ret)
{
	int check;
	addr bit, name, pos;

	/* If the package have no symbols, return 1. */
	GetPackage(package, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(pos, name, &bit));
	if (bit == Nil)
		return Result(ret, 1);

	/* If package don't have the symbol, return 1. */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return Result(ret, 1);

	/* If symbols in shadowing-symbols, check conflict in use-package. */
	if (StructBitType(bit)->shadow) {
		Return(check_shadowing_unintern_(package, name, &check));
		if (check) {
			*ret = 0;
			return fmte_("Shadowing symbol ~S occer conflict.", symbol, NULL);
		}
	}

	return Result(ret, 0);
}

static int remove_unintern_package_(addr package, addr name, int *ret)
{
	int check;
	addr bit, symbol;

	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	Return(findnil_hashtable_(package, name, &bit));
	if (StructBitType(bit)->base) {
		GetBitTypeSymbol(bit, &symbol);
		SetPackageSymbol(symbol, Nil);
	}
	Return(delete_hashtable_(package, name, &check));

	return Result(ret, StructBitType(bit)->shadow);
}

static int intern_inherited_unintern_package_(addr package, addr name)
{
	addr left, right, bit, table, cons;

	GetPackage(package, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(findnil_hashtable_(left, name, &left));
		if (left != Nil && StructBitType(left)->expt) {
			/* symbol */
			GetBitTypeSymbol(left, &left);

			/* intern inherited */
			inheritedbitpackage(&bit, left);
			GetPackage(package, PACKAGE_INDEX_TABLE, &table);
			Return(intern_hashheap_(table, name, &cons));
			SetCdr(cons, bit);
			return 0;
		}
	}
	/* Count of export symbols in use-package should be a one.
	 * (checked by check_shadowing_unintern function.)
	 */
	return fmte_("remove use error.", NULL);
}

static int remove_shadowing_symbols_package_(addr package, addr symbol)
{
	if (remove_check_package(package, PACKAGE_INDEX_SHADOW, symbol))
		return fmte_("remove_shadowing_symbols_package error.", NULL);

	return 0;
}

/*
 *  remove package table.
 *  if symbol in shadowing-symbols,
 *    intern inherited-symbol if symbol in shadowing-symbols.
 *    remove shadowing-symbols.
 */
static int unintern_symbol_package_(addr package, addr symbol)
{
	int check;
	addr name;

	/* remove package table. */
	GetNameSymbol(symbol, &name);
	Return(remove_unintern_package_(package, name, &check));
	if (check) {
		/* intern inherited-symbol if symbol in shadowing-symbols. */
		Return(intern_inherited_unintern_package_(package, name));
		/* remove shadowing-symbols. */
		Return(remove_shadowing_symbols_package_(package, symbol));
	}

	return 0;
}

_g int unintern_package_(addr package, addr symbol, int *ret)
{
	int check;

	Check(! symbolp(symbol), "type error");
	Return(package_designer_(package, &package));
	Return(unintern_check_package_(package, symbol, &check));
	if (check)
		return Result(ret, 1);
	Return(unintern_symbol_package_(package, symbol));

	return Result(ret, 0);
}


/*
 *  import
 */
static int import_already_exist_package(addr symbol, addr bit, addr *ret)
{
	enum PACKAGE_TYPE type;
	struct bittype_struct *str;
	addr check;

	GetBitTypeIntern(bit, &type);
	if (type == PACKAGE_TYPE_INHERITED) {
		/* inherited -> import */
		str = StructBitType(bit);
		str->intern = PACKAGE_TYPE_INTERNAL;
		str->import = 1;
		str->inherit = 0;
		*ret = bit;
		return 0;
	}
	else {
		/* conflict or shadowing-symbol */
		*ret = bit;
		GetBitTypeSymbol(bit, &check);
		return check != symbol;
	}
}

static int import_bitpackage_(addr package, addr symbol, addr *value, int *ret)
{
	addr table, name, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);

	/* intern check */
	Return(findnil_hashtable_(table, name, &bit));
	if (bit != Nil) {
		*ret = import_already_exist_package(symbol, bit, value);
		return 0;
	}

	/* intern hashtable */
	GetPackageSymbol(symbol, &check);
	if (check == Nil) {
		/* intern */
		Return(intern_hashheap_(table, name, &check));
		internbitpackage(&bit, symbol);
		SetCdr(check, bit);
		/* set package */
		SetPackageSymbol(symbol, package);
	}
	else {
		/* import */
		Return(intern_hashheap_(table, name, &check));
		importbitpackage(&bit, symbol);
		SetCdr(check, bit);
	}

	*value = bit;
	return Result(ret, 0);
}

static int import_symbol_package_(addr package, addr pos)
{
	int check;

	Check(! symbolp(pos), "type error");
	Return(import_bitpackage_(package, pos, &package, &check));
	if (check)
		return fmte_("Import symbol ~S occer conflict.", pos, NULL);

	return 0;
}

static int import_list_package_(addr package, addr pos)
{
	addr left, right, table, check;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			return fmte_("Import ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		GetNameSymbol(left, &check);

		/* intern check */
		Return(findnil_hashtable_(table, check, &check));
		if (check != Nil) {
			GetBitTypeSymbol(check, &check);
			if (left != check)
				return fmte_("Import symbol ~S occer conflict.", left, NULL);
		}
	}

	/* import */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(import_symbol_package_(package, left));
	}

	return 0;
}

_g int import_package_(addr package, addr pos)
{
	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return import_symbol_package_(package, pos);

		case LISPTYPE_CONS:
			return import_list_package_(package, pos);

		default:
			return fmte_("import ~S must be a symbol or list.", pos, NULL);
	}
}


/*
 *  shadow
 */
static int shadow_symbol_package_(addr package, addr pos)
{
	int check;
	addr bit;
	struct bittype_struct *str;

	Return(string_designer_heap_(&pos, pos, NULL));
	Return(intern_bitpackage_(package, pos, &bit, &check));
	str = StructBitType(bit);
	if (str->inherit) {
		/* change type to intern from inherit. */
		shadowintern_bitpackage(bit, pos, package);
	}
	if (! str->shadow) {
		GetBitTypeSymbol(bit, &pos);
		pushlist_package(package, PACKAGE_INDEX_SHADOW, pos);
		SetBitTypeShadow(bit, 1);
	}

	return 0;
}

_g int shadow_list_package_(addr package, addr pos)
{
	enum LISPTYPE type;
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if ((! IsValueSymbol(type)) && (! stringp(left)))
			return fmte_("shadow ~S must be a string-desinger.", left, NULL);
	}

	/* shadow */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(shadow_symbol_package_(package, left));
	}

	return 0;
}

_g int shadow_package_(addr package, addr pos)
{
	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			return shadow_symbol_package_(package, pos);

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos))
				goto error;
			return shadow_symbol_package_(package, pos);

		case LISPTYPE_CONS:
			return shadow_list_package_(package, pos);

		default:
			goto error;
	}

error:
	return fmte_("shadow ~S must be a string-designer or list.", pos, NULL);
}


/*
 *  shadowing-import
 */
_g int shadowing_import_symbol_package_(addr package, addr symbol)
{
	int check;
	addr bit, value;
	struct bittype_struct *str;

	Return(import_bitpackage_(package, symbol, &bit, &check));
	if (check) {
		str = StructBitType(bit);
		/* conflict, change type to intern from import. */
		if (str->shadow) {
			GetBitTypeSymbol(bit, &value);
			Return(remove_shadowing_symbols_package_(package, value));
			str->shadow = 0;
		}
		shadowimport_bitpackage(bit, symbol);
	}

	if (StructBitType(bit)->shadow == 0) {
		GetBitTypeSymbol(bit, &symbol);
		pushlist_package(package, PACKAGE_INDEX_SHADOW, symbol);
		SetBitTypeShadow(bit, 1);
	}

	return 0;
}

static int shadowing_import_list_package_(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			return fmte_("shadowing-symbol ~S must be a symbol.", left, NULL);
	}

	/* shadowing-import */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(shadowing_import_symbol_package_(package, left));
	}

	return 0;
}

_g int shadowing_import_package_(addr package, addr pos)
{
	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return shadowing_import_symbol_package_(package, pos);

		case LISPTYPE_CONS:
			return shadowing_import_list_package_(package, pos);

		default:
			return fmte_("shadowing-import ~S must be a symbol or list.", pos, NULL);
	}
}


/*
 *  export
 */
static int check_exportsymbol_package_(addr package, addr symbol)
{
	addr name, left, right;

	GetNameSymbol(symbol, &name);

	/* export check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil)
		return fmte_("Package don't have a symbol ~S.", symbol, NULL);

	/* Invalid package */
	GetBitTypeSymbol(right, &left);
	if (left != symbol)
		return fmte_("The symbol ~S is not accesble in this package.", symbol, NULL);

	/* already exported */
	if (StructBitType(right)->expt)
		return 0;

	/* use-list */
	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(find_bitpackage_(left, name, &left));
		if (left != Nil && ! StructBitType(left)->shadow) {
			/* symbol exists */
			return fmte_("export ~S occer conflict.", symbol, NULL);
		}
	}

	return 0;
}

static int intern_export_symbol_package_(addr package, addr symbol, addr name)
{
	addr left, right, bit;

	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(intern_hashheap_(left, name, &left));
		GetCdr(left, &bit);
		if (bit == Nil) {
			inheritedbitpackage(&bit, symbol);
			SetCdr(left, bit);
		}
		/* If left != Nil, the symbol may be a shadowing symbol. */
	}

	return 0;
}

static int exportsymbol_nocheck_package_(addr package, addr symbol)
{
	addr table, bit, name;
	struct bittype_struct *str;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(table, name, &bit));
	str = StructBitType(bit);
	if (str->expt) {
		/* symbol is already exported. */
		return 0;
	}
	if (str->inherit) {
		/* If the symbol type is inherited, the type change to import.  */
		str->inherit = 0;
		str->import = 1;
		str->expt = 1;
		str->intern = PACKAGE_TYPE_EXTERNAL;
	}
	else {
		/* export symbol */
		str->expt = 1;
		str->intern = PACKAGE_TYPE_EXTERNAL;
	}
	Return(intern_export_symbol_package_(package, symbol, name));
	pushlist_package(package, PACKAGE_INDEX_EXPORT, name);

	return 0;
}

static int export_symbol_package_(addr package, addr symbol)
{
	if (! symbolp(symbol))
		return fmte_("export ~S must be a symbol.", symbol, NULL);
	Return(check_exportsymbol_package_(package, symbol));
	return exportsymbol_nocheck_package_(package, symbol);
}

_g int export_list_package_(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			return fmte_("export ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(check_exportsymbol_package_(package, left));
	}

	/* export */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(exportsymbol_nocheck_package_(package, left));
	}

	return 0;
}

_g int export_package_(addr package, addr pos)
{
	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return export_symbol_package_(package, pos);

		case LISPTYPE_CONS:
			return export_list_package_(package, pos);

		default:
			return fmte_("export ~S must be a symbol or list.", pos, NULL);
	}
}


/*
 *  unexport
 */
static int delete_stringlist_package_(addr root, addr right, addr *value, int *ret)
{
	int check;
	addr left, right1, right2, right3;

	right1 = NULL;
	right2 = root;
	while (right2 != Nil) {
		GetCons(right2, &left, &right3);
		Return(string_equal_(left, right, &check));
		if (check) {
			/* delete */
			if (right1 == NULL) {
				*value = right3;
			}
			else {
				SetCdr(right1, right3);
				*value = root;
			}
			return Result(ret, 0);
		}
		right1 = right2;
		right2 = right3;
	}

	/* not found */
	return Result(ret, 1);
}

static int remove_export_list_package_(addr package, addr name)
{
	int check;
	addr right;

	GetPackage(package, PACKAGE_INDEX_EXPORT, &right);
	Return(delete_stringlist_package_(right, name, &right, &check));
	if (check)
		return fmte_("There is no ~S in export list.", name, NULL);
	SetPackage(package, PACKAGE_INDEX_EXPORT, right);

	return 0;
}

static int check_unexportsymbol_package_(addr package, addr symbol)
{
	addr table, name;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(table, name, &name));
	if (name == Nil)
		return fmte_("Symbol ~S is not exist in package ~S.", symbol, package, NULL);
	GetBitTypeSymbol(name, &name);
	if (symbol != name)
		return fmte_("Package of Symbol ~S don't access.", symbol, NULL);

	return 0;
}

static int unexport_usedbylist_package_(addr package, addr symbol)
{
	int check;
	addr left, right, table;

	GetNameSymbol(symbol, &symbol);
	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &table);
		Return(findnil_hashtable_(table, symbol, &left));
		if (left != Nil) {
			if (StructBitType(left)->inherit) {
				Return(delete_hashtable_(table, symbol, &check));
			}
		}
	}

	return 0;
}

static int unexport_symboltype_package_(addr package, addr symbol)
{
	addr table, name, bit;
	struct bittype_struct *str;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(table, name, &bit));
	str = StructBitType(bit);
	if (str->expt) {
		str->intern = PACKAGE_TYPE_INTERNAL;
		str->expt = 0;
		Return(remove_export_list_package_(package, name));
	}

	return 0;
}

static int unexport_symbol_package_(addr package, addr symbol)
{
	Return(check_unexportsymbol_package_(package, symbol));
	Return(unexport_usedbylist_package_(package, symbol));
	Return(unexport_symboltype_package_(package, symbol));

	return 0;
}

static int unexport_list_package_(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			return fmte_("unexport ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(check_unexportsymbol_package_(package, left));
	}

	/* unexport */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(unexport_symbol_package_(package, left));
	}

	return 0;
}

_g int unexport_package_(addr package, addr pos)
{
	addr check;

	Return(package_designer_(package, &package));
	GetConst(PACKAGE_KEYWORD, &check);
	if (check == package)
		return fmte_("KEYWORD package can't unexport.", NULL);

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return unexport_symbol_package_(package, pos);

		case LISPTYPE_CONS:
			return unexport_list_package_(package, pos);

		default:
			return fmte_("unexport ~S must be a symbol or list.", pos, NULL);
	}
}


/*
 *  use_package
 */
static int check_alreadyuse_package(addr package, addr pos)
{
	addr left;

	if (package == pos)
		return 1;
	GetPackage(package, PACKAGE_INDEX_USE, &package);
	while (package != Nil) {
		GetCons(package, &left, &package);
		if (left == pos)
			return 1;
	}

	return 0;
}

static int check_useconflict_package_(addr package, addr pos)
{
	enum PACKAGE_TYPE ignore;
	addr list, table, left, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &left, &list);
		Return(findnil_hashtable_(table, left, &bit));
		if (bit != Nil && ! StructBitType(bit)->shadow) {
			GetBitTypeSymbol(bit, &bit);
			Return(find_symbol_package_(pos, left, &check, &ignore));
			if (bit != check)
				return fmte_("Symbol ~S conflict occered.", left, NULL);
		}
	}

	return 0;
}

static int check_usepackage_package_(addr package, addr pos)
{
	if (check_alreadyuse_package(package, pos))
		return 0;
	return check_useconflict_package_(package, pos);
}

static int use_package_operator_package_(addr package, addr pos)
{
	addr table, loop, left, right, symbol, cons, bit;

	Return(package_designer_(pos, &pos));
	if (check_alreadyuse_package(package, pos))
		return 0;
	Return(check_useconflict_package_(package, pos));

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &loop);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(intern_hashheap_(table, left, &cons));
		GetCdr(cons, &bit);
		if (bit == Nil) {
			Return(findnil_hashtable_(loop, left, &symbol));
			Check(symbol == Nil, "use-package error");
			GetBitTypeSymbol(symbol, &symbol);
			inheritedbitpackage(&bit, symbol);
			SetCdr(cons, bit);
		}
	}
	pushlist_package(package, PACKAGE_INDEX_USE, pos);
	pushlist_package(pos, PACKAGE_INDEX_USED, package);

	return 0;
}

static int check_user_package_list_package_(addr package, addr right)
{
	addr base, left, table, loop, check, expt, name, find;

	GetPackage(package, PACKAGE_INDEX_TABLE, &base);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(package_designer_(left, &left));
		GetPackage(left, PACKAGE_INDEX_TABLE, &table);
		for (loop = right; loop != Nil; ) {
			GetCons(loop, &check, &loop);
			Return(package_designer_(check, &check));
			if (check == left)
				continue;
			if (check == package)
				continue;

			GetPackage(check, PACKAGE_INDEX_EXPORT, &expt);
			while (expt != Nil) {
				GetCons(expt, &name, &expt);
				Return(findnil_hashtable_(table, name, &find));
				if (find != Nil) {
					if (! StructBitType(find)->expt)
						continue;
					Return(findnil_hashtable_(base, name, &find));
					if (find == Nil || ! StructBitType(find)->shadow)
						return fmte_("Pakcage conflict ~S.", name, NULL);
				}
			}
		}
	}

	return 0;
}

_g int use_package_list_package_(addr package, addr pos)
{
	enum LISPTYPE type;
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if (type != LISPTYPE_PACKAGE && (! IsValueSymbol(type)) && (! stringp(left)))
			return fmte_("use-package ~S must be a package-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(package_designer_(left, &left));
		Return(check_usepackage_package_(package, left));
	}
	Return(check_user_package_list_package_(package, pos));

	/* use-package */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(use_package_operator_package_(package, left));
	}

	return 0;
}

_g int use_package_(addr package, addr pos)
{
	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			return use_package_operator_package_(package, pos);

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos))
				goto error;
			return use_package_operator_package_(package, pos);

		case LISPTYPE_CONS:
			return use_package_list_package_(package, pos);

		default:
			goto error;
	}

error:
	return fmte_("use-package ~S must be a package-designer or list.", pos, NULL);
}


/*
 *  unuse_package
 */
static int check_uselist_package(addr package, addr pos)
{
	addr left, right;

	GetPackage(package, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == pos)
			return 1;
	}

	return 0;
}

static int unuse_package_operator_package_(addr package, addr pos)
{
	int check;
	addr table, right, left, bit;

	Return(package_designer_(pos, &pos));
	if (check_uselist_package(package, pos) == 0)
		return 0;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(findnil_hashtable_(table, left, &bit));
		Check(bit == Nil, "unuse-package error");
		if (StructBitType(bit)->inherit) {
			Return(delete_hashtable_(table, left, &check));
		}
	}

	if (remove_check_package(package, PACKAGE_INDEX_USE, pos))
		Abort("PACKAGE_INDEX_USE error");
	if (remove_check_package(pos, PACKAGE_INDEX_USED, package))
		Abort("PACKAGE_INDEX_USED error");

	return 0;
}

static int unuse_package_list_package_(addr package, addr pos)
{
	enum LISPTYPE type;
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if (type != LISPTYPE_PACKAGE && (! IsValueSymbol(type)) && (! stringp(left)))
			return fmte_("unuse-package ~S must be a package-desinger.", left, NULL);
	}

	/* unuse-package */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(unuse_package_operator_package_(package, left));
	}

	return 0;
}

_g int unuse_package_(addr package, addr pos)
{
	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			return unuse_package_operator_package_(package, pos);

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos))
				goto error;
			return unuse_package_operator_package_(package, pos);

		case LISPTYPE_CONS:
			return unuse_package_list_package_(package, pos);

		default:
			goto error;
	}

error:
	return fmte_("unuse-package ~S must be a package-designer or list.", pos, NULL);
}


/*
 *  keyword
 */
_g int setkeyword_package_(addr pos)
{
	addr check, package;

	GetConst(PACKAGE_KEYWORD, &package);
	Return(export_package_(package, pos));
	GetValueSymbol(pos, &check);
	if (check == Unbound) {
		SetValueSymbol(pos, pos);
		SetStatusReadOnly(pos);
	}

	return 0;
}


/*
 *  clang
 */
_g int intern_default_package_(Execute ptr, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package;
	Return(getpackage_(ptr, &package));
	return intern_package_(package, name, value, ret);
}

_g int internchar_(const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package, name;

	Check(pname == NULL, "argument package error");
	Check(sname == NULL, "argument name error");

	/* find package */
	Return(find_char_package_(pname, &package));
	if (package == Nil) {
		strvect_char_heap(&name, pname);
		return fmte_("No such a package ~S.", name, NULL);
	}

	return intern_char_package_(package, sname, value, ret);
}

_g int internchar_default_(Execute ptr, const char *name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package;
	Return(getpackage_(ptr, &package));
	return intern_char_package_(package, name, value, ret);
}

_g int internchar_null_(Execute ptr, const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret)
{
	if (pname)
		return internchar_(pname, sname, value, ret);
	else
		return internchar_default_(ptr, sname, value, ret);
}

_g int internchar_keyword_(const char *name, addr *value, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	GetConst(PACKAGE_KEYWORD, &pos);
	Return(intern_char_package_(pos, name, &pos, &type));
	Return(setkeyword_package_(pos));
	*value = pos;

	if (ret)
		return Result(ret, type);
	else
		return 0;
}

_g int interncommon_(const char *name, addr *value, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	addr package, pos;

	GetConst(PACKAGE_COMMON_LISP, &package);
	Return(intern_char_package_(package, name, &pos, &type));
	Return(export_package_(package, pos));
	*value = pos;

	if (ret)
		return Result(ret, type);
	else
		return 0;
}


/*
 *  debug
 */
_g void internchar_debug(const char *pname, const char *sname, addr *value)
{
	Error(internchar_(pname, sname, value, NULL));
}

_g void internchar_keyword_debug(const char *name, addr *value)
{
	Error(internchar_keyword_(name, value, NULL));
}

_g void interncommon_debug(const char *name, addr *value)
{
	Error(interncommon_(name, value, NULL));
}

_g addr interncharr_debug(const char *pname, const char *sname)
{
	addr pos;
	pos = NULL;
	Error(internchar_(pname, sname, &pos, NULL));
	return pos;
}

_g addr interncharr_null_debug(Execute ptr, const char *pname, const char *sname)
{
	addr pos;
	pos = NULL;
	Error(internchar_null_(ptr, pname, sname, &pos, NULL));
	return pos;
}

_g addr interncommonr_debug(const char *name)
{
	addr pos;
	pos = NULL;
	Error(interncommon_(name, &pos, NULL));
	return pos;
}

