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
_g enum PACKAGE_TYPE intern_package_table(addr package, addr name, addr *ret)
{
	enum PACKAGE_TYPE result;
	addr keyword, bit;

	/* intern */
	result = intern_bitpackage(package, name, &bit)?
		PACKAGE_TYPE_NIL:
		StructBitType(bit)->intern;
	GetBitTypeSymbol(bit, ret);

	/* keyword */
	GetConst(PACKAGE_KEYWORD, &keyword);
	if (keyword == package)
		setkeyword_package(*ret);

	return result;
}

_g enum PACKAGE_TYPE intern_package(addr package, addr name, addr *ret)
{
	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(! stringp(name), "type error");

	package_designer(package, &package);
	return intern_package_table(package, name, ret);
}

_g enum PACKAGE_TYPE intern_char_package(addr package, const char *name, addr *ret)
{
	addr symbol;

	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(GetType(package) != LISPTYPE_PACKAGE, "type error");

	/* find symbol */
	find_char_bitpackage(package, name, &symbol);
	if (symbol != Nil) {
		GetBitTypeSymbol(symbol, ret);
		return StructBitType(symbol)->intern;
	}

	/* intern */
	strvect_char_heap(&symbol, name);
	return intern_package(package, symbol, ret);
}


/*
 *  unintern
 */
static int check_export_unintern(addr pos, addr name)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	findvalue_hashtable(pos, name, &pos);

	return pos != Nil && StructBitType(pos)->expt;
}

static int check_shadowing_unintern(addr package, addr name)
{
	int check;
	addr left, right;

	GetPackage(package, PACKAGE_INDEX_USE, &right);
	check = 0;
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (check_export_unintern(left, name)) {
			if (check)
				return 1;
			check = 1;
		}
	}

	return 0;
}

/*
 * If package have no symbols, return 1.
 * If package don't have the symbol, return 1.
 * If there are symbols in shadowing-symbols, check conflict in use-package.
 */
static int unintern_check_package(addr package, addr symbol)
{
	addr bit, name, pos;

	/* If the package have no symbols, return 1. */
	GetPackage(package, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(pos, name, &bit);
	if (bit == Nil)
		return 1;

	/* If package don't have the symbol, return 1. */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return 1;

	/* If symbols in shadowing-symbols, check conflict in use-package. */
	if (StructBitType(bit)->shadow) {
		if (check_shadowing_unintern(package, name)) {
			fmte("Shadowing symbol ~S occer conflict.", symbol, NULL);
			return 1;
		}
	}

	return 0;
}

static int remove_unintern_package(addr package, addr name)
{
	addr bit, symbol;

	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	findvalue_hashtable(package, name, &bit);
	if (StructBitType(bit)->base) {
		GetBitTypeSymbol(bit, &symbol);
		SetPackageSymbol(symbol, Nil);
	}
	delete_hashtable(package, name);

	return StructBitType(bit)->shadow;
}

static void intern_inherited_unintern_package(addr package, addr name)
{
	addr left, right, bit, table, cons;

	GetPackage(package, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		findvalue_hashtable(left, name, &left);
		if (left != Nil && StructBitType(left)->expt) {
			/* symbol */
			GetBitTypeSymbol(left, &left);

			/* intern inherited */
			inheritedbitpackage(&bit, left);
			GetPackage(package, PACKAGE_INDEX_TABLE, &table);
			intern_hashheap(table, name, &cons);
			SetCdr(cons, bit);
			return;
		}
	}
	/* Count of export symbols in use-package should be a one.
	 * (checked by check_shadowing_unintern function.)
	 */
	Abort("remove use error.");
}

static void remove_shadowing_symbols_package(addr package, addr symbol)
{
	if (remove_check_package(package, PACKAGE_INDEX_SHADOW, symbol))
		Abort("remove_shadowing_symbols_package error.");
}

/*
 *  remove package table.
 *  if symbol in shadowing-symbols,
 *    intern inherited-symbol if symbol in shadowing-symbols.
 *    remove shadowing-symbols.
 */
static void unintern_symbol_package(addr package, addr symbol)
{
	addr name;

	/* remove package table. */
	GetNameSymbol(symbol, &name);
	if (remove_unintern_package(package, name)) {
		/* intern inherited-symbol if symbol in shadowing-symbols. */
		intern_inherited_unintern_package(package, name);
		/* remove shadowing-symbols. */
		remove_shadowing_symbols_package(package, symbol);
	}
}

_g int unintern_package(addr package, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	package_designer(package, &package);
	if (unintern_check_package(package, symbol))
		return 1;
	unintern_symbol_package(package, symbol);

	return 0;
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

static int import_bitpackage(addr package, addr symbol, addr *ret)
{
	addr table, name, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);

	/* intern check */
	findvalue_hashtable(table, name, &bit);
	if (bit != Nil)
		return import_already_exist_package(symbol, bit, ret);

	/* intern hashtable */
	GetPackageSymbol(symbol, &check);
	if (check == Nil) {
		/* intern */
		intern_hashheap(table, name, &check);
		internbitpackage(&bit, symbol);
		SetCdr(check, bit);
		/* set package */
		SetPackageSymbol(symbol, package);
	}
	else {
		/* import */
		intern_hashheap(table, name, &check);
		importbitpackage(&bit, symbol);
		SetCdr(check, bit);
	}
	*ret = bit;

	return 0;
}

static void import_symbol_package(addr package, addr pos)
{
	Check(! symbolp(pos), "type error");
	if (import_bitpackage(package, pos, &package))
		fmte("Import symbol ~S occer conflict.", pos, NULL);
}

static void import_list_package(addr package, addr pos)
{
	addr left, right, table, check;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			fmte("Import ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		GetNameSymbol(left, &check);

		/* intern check */
		findvalue_hashtable(table, check, &check);
		if (check != Nil) {
			GetBitTypeSymbol(check, &check);
			if (left != check)
				fmte("Import symbol ~S occer conflict.", left, NULL);
		}
	}

	/* import */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		import_symbol_package(package, left);
	}
}

_g void import_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			import_symbol_package(package, pos);
			break;

		case LISPTYPE_CONS:
			import_list_package(package, pos);
			break;

		default:
			fmte("import ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  shadow
 */
static void shadow_symbol_package(addr package, addr pos)
{
	addr bit;
	struct bittype_struct *str;

	string_designer_heap(&pos, pos);
	intern_bitpackage(package, pos, &bit);
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
}

_g void shadow_list_package(addr package, addr pos)
{
	enum LISPTYPE type;
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if ((! IsValueSymbol(type)) && (! stringp(left)))
			fmte("shadow ~S must be a string-desinger.", left, NULL);
	}

	/* shadow */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		shadow_symbol_package(package, left);
	}
}

_g void shadow_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			shadow_symbol_package(package, pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			shadow_symbol_package(package, pos);
			break;

		case LISPTYPE_CONS:
			shadow_list_package(package, pos);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("shadow ~S must be a string-designer or list.", pos, NULL);
}


/*
 *  shadowing-import
 */
_g void shadowing_import_symbol_package(addr package, addr symbol)
{
	addr bit, check;
	struct bittype_struct *str;

	if (import_bitpackage(package, symbol, &bit)) {
		str = StructBitType(bit);
		/* conflict, change type to intern from import. */
		if (str->shadow) {
			GetBitTypeSymbol(bit, &check);
			remove_shadowing_symbols_package(package, check);
			str->shadow = 0;
		}
		shadowimport_bitpackage(bit, symbol);
	}

	if (StructBitType(bit)->shadow == 0) {
		GetBitTypeSymbol(bit, &symbol);
		pushlist_package(package, PACKAGE_INDEX_SHADOW, symbol);
		SetBitTypeShadow(bit, 1);
	}
}

static void shadowing_import_list_package(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			fmte("shadowing-symbol ~S must be a symbol.", left, NULL);
	}

	/* shadowing-import */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		shadowing_import_symbol_package(package, left);
	}
}

_g void shadowing_import_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			shadowing_import_symbol_package(package, pos);
			break;

		case LISPTYPE_CONS:
			shadowing_import_list_package(package, pos);
			break;

		default:
			fmte("shadowing-import ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  export
 */
static void check_exportsymbol_package(addr package, addr symbol)
{
	addr name, left, right;

	GetNameSymbol(symbol, &name);

	/* export check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	findvalue_hashtable(right, name, &right);
	if (right == Nil) {
		fmte("Package don't have a symbol ~S.", symbol, NULL);
		return;
	}

	/* Invalid package */
	GetBitTypeSymbol(right, &left);
	if (left != symbol) {
		fmte("The symbol ~S is not accesble in this package.", symbol, NULL);
		return;
	}

	/* already exported */
	if (StructBitType(right)->expt)
		return;

	/* use-list */
	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		find_bitpackage(left, name, &left);
		if (left != Nil && ! StructBitType(left)->shadow) {
			/* symbol exists */
			fmte("export ~S occer conflict.", symbol, NULL);
			return;
		}
	}
}

static void intern_export_symbol_package(addr package, addr symbol, addr name)
{
	addr left, right, bit;

	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		intern_hashheap(left, name, &left);
		GetCdr(left, &bit);
		if (bit == Nil) {
			inheritedbitpackage(&bit, symbol);
			SetCdr(left, bit);
		}
		/* If left != Nil, the symbol may be a shadowing symbol. */
	}
}

static void exportsymbol_nocheck_package(addr package, addr symbol)
{
	addr table, bit, name;
	struct bittype_struct *str;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(table, name, &bit);
	str = StructBitType(bit);
	if (str->expt) {
		/* symbol is already exported. */
		return;
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
	intern_export_symbol_package(package, symbol, name);
	pushlist_package(package, PACKAGE_INDEX_EXPORT, name);
}

static void export_symbol_package(addr package, addr symbol)
{
	if (! symbolp(symbol))
		fmte("export ~S must be a symbol.", symbol, NULL);
	check_exportsymbol_package(package, symbol);
	exportsymbol_nocheck_package(package, symbol);
}

_g void export_list_package(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			fmte("export ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		check_exportsymbol_package(package, left);
	}

	/* export */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		exportsymbol_nocheck_package(package, left);
	}
}

_g void export_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			export_symbol_package(package, pos);
			break;

		case LISPTYPE_CONS:
			export_list_package(package, pos);
			break;

		default:
			fmte("export ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  unexport
 */
static int delete_stringlist_package(addr root, addr check, addr *ret)
{
	addr left, right1, right2, right3;

	right1 = NULL;
	right2 = root;
	while (right2 != Nil) {
		GetCons(right2, &left, &right3);
		if (string_equal(left, check)) {
			/* delete */
			if (right1 == NULL) {
				*ret = right3;
			}
			else {
				SetCdr(right1, right3);
				*ret = root;
			}
			return 0;
		}
		right1 = right2;
		right2 = right3;
	}

	/* not found */
	return 1;
}

static void remove_export_list_package(addr package, addr name)
{
	addr right;

	GetPackage(package, PACKAGE_INDEX_EXPORT, &right);
	if (delete_stringlist_package(right, name, &right))
		fmte("There is no ~S in export list.", name, NULL);
	SetPackage(package, PACKAGE_INDEX_EXPORT, right);
}

static void check_unexportsymbol_package(addr package, addr symbol)
{
	addr table, name;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(table, name, &name);
	if (name == Nil)
		fmte("Symbol ~S is not exist in package ~S.", symbol, package, NULL);
	GetBitTypeSymbol(name, &name);
	if (symbol != name)
		fmte("Package of Symbol ~S don't access.", symbol, NULL);
}

static void unexport_usedbylist_package(addr package, addr symbol)
{
	addr left, right, table;

	GetNameSymbol(symbol, &symbol);
	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &table);
		findvalue_hashtable(table, symbol, &left);
		if (left != Nil) {
			if (StructBitType(left)->inherit)
				delete_hashtable(table, symbol);
		}
	}
}

static void unexport_symboltype_package(addr package, addr symbol)
{
	addr table, name, bit;
	struct bittype_struct *str;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(table, name, &bit);
	str = StructBitType(bit);
	if (str->expt) {
		str->intern = PACKAGE_TYPE_INTERNAL;
		str->expt = 0;
		remove_export_list_package(package, name);
	}
}

static void unexport_symbol_package(addr package, addr symbol)
{
	check_unexportsymbol_package(package, symbol);
	unexport_usedbylist_package(package, symbol);
	unexport_symboltype_package(package, symbol);
}

static void unexport_list_package(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! symbolp(left))
			fmte("unexport ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		check_unexportsymbol_package(package, left);
	}

	/* unexport */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		unexport_symbol_package(package, left);
	}
}

_g void unexport_package(addr package, addr pos)
{
	addr check;

	package_designer(package, &package);
	GetConst(PACKAGE_KEYWORD, &check);
	if (check == package)
		fmte("KEYWORD package can't unexport.", NULL);

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			unexport_symbol_package(package, pos);
			break;

		case LISPTYPE_CONS:
			unexport_list_package(package, pos);
			break;

		default:
			fmte("unexport ~S must be a symbol or list.", pos, NULL);
			break;
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

static void check_useconflict_package(addr package, addr pos)
{
	addr list, table, left, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &left, &list);
		findvalue_hashtable(table, left, &bit);
		if (bit != Nil && ! StructBitType(bit)->shadow) {
			GetBitTypeSymbol(bit, &bit);
			find_symbol_package(pos, left, &check);
			if (bit != check)
				fmte("Symbol ~S conflict occered.", left, NULL);
		}
	}
}

static void check_usepackage_package(addr package, addr pos)
{
	if (check_alreadyuse_package(package, pos))
		return;
	check_useconflict_package(package, pos);
}

static void use_package_operator_package(addr package, addr pos)
{
	addr table, loop, left, right, symbol, cons, bit;

	package_designer(pos, &pos);
	if (check_alreadyuse_package(package, pos))
		return;
	check_useconflict_package(package, pos);

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &loop);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		intern_hashheap(table, left, &cons);
		GetCdr(cons, &bit);
		if (bit == Nil) {
			findvalue_hashtable(loop, left, &symbol);
			Check(symbol == Nil, "use-package error");
			GetBitTypeSymbol(symbol, &symbol);
			inheritedbitpackage(&bit, symbol);
			SetCdr(cons, bit);
		}
	}
	pushlist_package(package, PACKAGE_INDEX_USE, pos);
	pushlist_package(pos, PACKAGE_INDEX_USED, package);
}

static void check_user_package_list_package(addr package, addr right)
{
	addr base, left, table, loop, check, expt, name, find;

	GetPackage(package, PACKAGE_INDEX_TABLE, &base);
	while (right != Nil) {
		GetCons(right, &left, &right);
		package_designer(left, &left);
		GetPackage(left, PACKAGE_INDEX_TABLE, &table);
		for (loop = right; loop != Nil; ) {
			GetCons(loop, &check, &loop);
			package_designer(check, &check);
			if (check == left)
				continue;
			if (check == package)
				continue;

			GetPackage(check, PACKAGE_INDEX_EXPORT, &expt);
			while (expt != Nil) {
				GetCons(expt, &name, &expt);
				findvalue_hashtable(table, name, &find);
				if (find != Nil) {
					if (! StructBitType(find)->expt)
						continue;
					findvalue_hashtable(base, name, &find);
					if (find == Nil || ! StructBitType(find)->shadow)
						fmte("Pakcage conflict ~S.", name, NULL);
				}
			}
		}
	}
}

_g void use_package_list_package(addr package, addr pos)
{
	enum LISPTYPE type;
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if (type != LISPTYPE_PACKAGE && (! IsValueSymbol(type)) && (! stringp(left)))
			fmte("use-package ~S must be a package-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		package_designer(left, &left);
		check_usepackage_package(package, left);
	}
	check_user_package_list_package(package, pos);

	/* use-package */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		use_package_operator_package(package, left);
	}
}

_g void use_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			use_package_operator_package(package, pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			use_package_operator_package(package, pos);
			break;

		case LISPTYPE_CONS:
			use_package_list_package(package, pos);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("use-package ~S must be a package-designer or list.", pos, NULL);
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

static void unuse_package_operator_package(addr package, addr pos)
{
	addr table, right, left, bit;

	package_designer(pos, &pos);
	if (check_uselist_package(package, pos) == 0)
		return;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		findvalue_hashtable(table, left, &bit);
		Check(bit == Nil, "unuse-package error");
		if (StructBitType(bit)->inherit)
			delete_hashtable(table, left);
	}

	if (remove_check_package(package, PACKAGE_INDEX_USE, pos))
		Abort("PACKAGE_INDEX_USE error");
	if (remove_check_package(pos, PACKAGE_INDEX_USED, package))
		Abort("PACKAGE_INDEX_USED error");
}

static void unuse_package_list_package(addr package, addr pos)
{
	enum LISPTYPE type;
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if (type != LISPTYPE_PACKAGE && (! IsValueSymbol(type)) && (! stringp(left)))
			fmte("unuse-package ~S must be a package-desinger.", left, NULL);
	}

	/* unuse-package */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		unuse_package_operator_package(package, left);
	}
}

_g void unuse_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			unuse_package_operator_package(package, pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			unuse_package_operator_package(package, pos);
			break;

		case LISPTYPE_CONS:
			unuse_package_list_package(package, pos);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("unuse-package ~S must be a package-designer or list.", pos, NULL);
}


/*
 *  clang
 */
_g enum PACKAGE_TYPE intern_default_package(Execute ptr, addr name, addr *ret)
{
	addr package;
	getpackage(ptr, &package);
	return intern_package(package, name, ret);
}

_g enum PACKAGE_TYPE internchar(const char *pname, const char *sname, addr *ret)
{
	addr package, name;

	Check(pname == NULL, "argument package error");
	Check(sname == NULL, "argument name error");

	/* find package */
	find_char_package(pname, &package);
	if (package == Nil) {
		strvect_char_heap(&name, pname);
		fmte("No such a package ~S.", name, NULL);
	}

	return intern_char_package(package, sname, ret);
}

_g addr interncharr(const char *pname, const char *sname)
{
	addr pos;
	internchar(pname, sname, &pos);
	return pos;
}

_g enum PACKAGE_TYPE internchar_default(Execute ptr, const char *name, addr *ret)
{
	addr package;
	getpackage(ptr, &package);
	return intern_char_package(package, name, ret);
}

_g enum PACKAGE_TYPE internchar_check(Execute ptr,
		const char *pname, const char *sname, addr *ret)
{
	if (pname)
		return internchar(pname, sname, ret);
	else
		return internchar_default(ptr, sname, ret);
}

_g addr internchar_checkr(Execute ptr, const char *pname, const char *sname)
{
	addr pos;
	internchar_check(ptr, pname, sname, &pos);
	return pos;
}

_g void setkeyword_package(addr pos)
{
	addr check, package;

	GetConst(PACKAGE_KEYWORD, &package);
	export_package(package, pos);
	GetValueSymbol(pos, &check);
	if (check == Unbound) {
		SetValueSymbol(pos, pos);
		SetStatusReadOnly(pos);
	}
}

_g enum PACKAGE_TYPE internchar_keyword(const char *name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	GetConst(PACKAGE_KEYWORD, &pos);
	type = intern_char_package(pos, name, &pos);
	setkeyword_package(pos);
	*ret = pos;

	return type;
}

_g enum PACKAGE_TYPE interncommon(const char *name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr package, pos;

	GetConst(PACKAGE_COMMON_LISP, &package);
	type = intern_char_package(package, name, &pos);
	export_package(package, pos);
	*ret = pos;

	return type;
}

_g addr interncommonr(const char *name)
{
	addr ret;
	(void)interncommon(name, &ret);
	return ret;
}

