#include "condition.h"
#include "cons.h"
#include "local.h"
#include "intern_count.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_common.h"
#include "package_object.h"
#include "package_symbol.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

#define LISP_PACKAGE_HASHSIZE        16

/*
 *  package object
 */
static int find_package_direct_(addr pos, addr *ret)
{
	addr table;
	PackageTable(&table);
	return findnil_hashtable_(table, pos, ret);
}

static int find_package_local_(addr pos, addr *ret)
{
	addr table, name;
	LocalRoot local;
	LocalStack stack;
	unicode c;

	PackageTable(&table);
	GetCharacter(pos, &c);

	/* findvalue */
	local = Local_Thread;
	push_local(local, &stack);
	strvect_local(local, &name, 1);
	strvect_setc(name, 0, c);
	Return(findnil_hashtable_(table, name, ret));
	rollback_local(local, stack);

	return 0;
}

_g int find_package_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
			return Result(ret, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			GetNameSymbol(pos, &pos);
			return find_package_direct_(pos, ret);

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos))
				goto error;
			return find_package_direct_(pos, ret);

		case LISPTYPE_STRING:
			return find_package_direct_(pos, ret);

		case LISPTYPE_CHARACTER:
			return find_package_local_(pos, ret);

		default:
			goto error;
	}

error:
	*ret = NULL;
	return fmte_("Argument ~S must be a string, symbol or package.", pos, NULL);
}

_g int find_char_package_(const char *name, addr *ret)
{
	addr pos;
	PackageTable(&pos);
	return findnil_char_hashtable_(pos, name, ret);
}

_g int package_designer_(addr pos, addr *ret)
{
	addr package;

	Return(find_package_(pos, &package));
	if (package == Nil)
		return fmte_("No such a package ~S.", pos, NULL);

	return Result(ret, package);
}

static int append_root_package_(addr name, addr package)
{
	addr table;

	PackageTable(&table);
	Return(intern_hashheap_(table, name, &name));
	SetCdr(name, package);  /* (name . package) */

	return 0;
}

_g int package_size_heap_(addr *ret, addr name, size_t size)
{
	addr pos, table;

	/* name check */
	string_designer_heap(&name, name);
	Return(find_package_direct_(name, &pos));
	if (pos != Nil)
		return fmte_("Package name ~S already used.", name, NULL);

	/* make package */
	heap_array2(&pos, LISPTYPE_PACKAGE, PACKAGE_INDEX_SIZE);
	if (size)
		hashtable_size_heap(&table, size);
	else
		hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	SetPackage(pos, PACKAGE_INDEX_TABLE, table);
	SetPackage(pos, PACKAGE_INDEX_NAME, name);

	/* append root */
	Return(append_root_package_(name, pos));

	return Result(ret, pos);
}

_g int package_heap_(addr *ret, addr name)
{
	return package_size_heap_(ret, name, 0);
}

static int package_char_heap_(addr *ret, const char *name)
{
	addr key;
	strvect_char_heap(&key, name);
	return package_heap_(ret, key);
}

static void package_root_heap(addr *ret)
{
	addr pos;

	hashtable_size_heap(&pos, LISP_PACKAGE_HASHSIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

static int intern_common_constant_(addr package, const char *str, addr symbol)
{
	int check;
	addr pos, name;

	/* set table */
	strvect_char_heap(&name, str);
	Return(intern_bitpackage_(package, name, &pos, &check));
	SetBitTypeSymbol(pos, symbol);

	/* set symbol */
	SetStatusValue(symbol, LISPSTATUS_READONLY, 0);
	SetPackageSymbol(symbol, package);
	SetStatusValue(symbol, LISPSTATUS_READONLY, 1);
	return export_package_(package, symbol);
}

static int intern_common_default_(void)
{
	addr common;

	Return(find_char_package_(LISP_COMMON, &common));
	Return(intern_common_constant_(common, "NIL", Nil));
	Return(intern_common_constant_(common, "T", T));
	SetConstant(CONSTANT_COMMON_NIL, Nil);
	SetConstant(CONSTANT_COMMON_T, T);

	return 0;
}

static int intern_package_symbol_(void)
{
	addr symbol;

	/* common-lisp::*package* */
	Return(internchar_(LISP_COMMON, "*PACKAGE*", &symbol, NULL));
	setspecial_symbol(symbol);
	SetConstant(CONSTANT_SPECIAL_PACKAGE, symbol);

	/* common-lisp-user package */
	Return(find_char_package_(LISP_COMMON_USER, &symbol));
	Check(symbol == Nil, LISP_COMMON_USER " package is not found.");
	SetConstant(CONSTANT_PACKAGE_COMMON_LISP_USER, symbol);

	return 0;
}

static void set_default_package(addr package)
{
	addr symbol;

	/* setq *package* */
	GetConst(SPECIAL_PACKAGE, &symbol);
	/* not special-stack */
	SetValueSymbol(symbol, package);
}

static int build_package_settings_(void)
{
	addr package, common, cons, name;

	/* COMMON-LISP */
	Return(find_char_package_(LISP_COMMON, &common));
	strvect_char_heap(&name, "CL");
	list_heap(&cons, name, NULL);
	Return(append_nicknames_package_(common, cons));

	/* COMMON-LISP-USER */
	Return(find_char_package_(LISP_COMMON_USER, &package));
	strvect_char_heap(&name, "CL-USER");
	list_heap(&cons, name, NULL);
	Return(append_nicknames_package_(package, cons));
	Return(use_package_(package, common));

	/* LISP-PACKAGE */
	Return(find_char_package_(LISP_PACKAGE, &package));
	Return(use_package_(package, common));

	return 0;
}

static void set_gentemp_package(void)
{
	addr pos;
	fixnum_heap(&pos, 1);
	SetConst(PACKAGE_GENTEMP, pos);
}

static void build_default_use_package(void)
{
	addr name, list;

	strvect_char_heap(&name, "COMMON-LISP");
	list_heap(&list, name, NULL);
	SetConstant(CONSTANT_PACKAGE_DEFAULT_USE, list);
}

static int import_exit_and_quit_package_(addr package)
{
	addr symbol;

	/* (import 'lisp-system::exit 'common-lisp-user) */
	GetConst(SYSTEM_EXIT, &symbol);
	Return(import_package_(package, symbol));
	/* (import 'lisp-system::quit 'common-lisp-user) */
	GetConst(SYSTEM_QUIT, &symbol);
	Return(import_package_(package, symbol));

	return 0;
}

static int system_package_(const char *name, size_t size, constindex index)
{
	addr pos;

	strvect_char_heap(&pos, name);
	Return(package_size_heap_(&pos, pos, size + 1));
	SetConstant(index, pos);

	return 0;
}

#define SystemPackage(x,y,z) { \
	Return(system_package_(LISP_##x, LISP_PACKAGE_COUNT_##y, CONSTANT_PACKAGE_##z)); \
}
static int build_package_value_(void)
{
	addr root, package, user;

	/* package root */
	package_root_heap(&root);
	SetLispRoot(PACKAGE, root);

	/* make package */
	SystemPackage(COMMON, COMMON, COMMON_LISP);
	SystemPackage(KEYWORD, KEYWORD, KEYWORD);
	SystemPackage(SYSTEM, SYSTEM, SYSTEM);
	SystemPackage(CODE, CODE, CODE);
	SystemPackage(CLOS, CLOS, CLOS);
	SystemPackage(RT, RT, RT);
	Return(package_char_heap_(&user, LISP_COMMON_USER));
	Return(package_char_heap_(&package, LISP_PACKAGE));
	Return(package_char_heap_(&package, LISP_USER));

	/* symbol setting */
	Return(intern_common_default_());
	Return(intern_symbol_header_());
	Return(intern_package_symbol_());
	Return(build_package_settings_());
	set_default_package(user);
	set_gentemp_package();
	build_default_use_package();
	Return(import_exit_and_quit_package_(user));

	return 0;
}

_g void build_package(void)
{
	Error(build_package_value_());
}

_g int getpackage_(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(SPECIAL_PACKAGE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (GetType(pos) != LISPTYPE_PACKAGE)
		return fmte_("symbol *package* is not a package type.", NULL);

	return Result(ret, pos);
}


/*
 *  make_package
 */
_g void pushlist_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr check, right;

	GetPackage(package, index, &check);
	if (check == Nil) {
		/* first element */
		consnil_heap(&check);
		SetCar(check, pos);
		SetPackage(package, index, check);
	}
	else {
		/* push */
		consnil_heap(&right);
		SetCons(right, pos, check);
		SetPackage(package, index, right);
	}
}

static void pushnewlist_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr left, right;

	GetPackage(package, index, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == pos)
			return; /* don't push */
	}
	pushlist_package(package, index, pos);
}

static int check_nicknames_package_(addr name, addr right)
{
	addr left, check, table;

	/* check name */
	PackageTable(&table);
	Return(findcons_hashtable_(table, name, &check));
	if (check != Nil)
		return fmte_("Package ~S already exists.", name, NULL);

	/* check names */
	while (right != Nil) {
		GetCons(right, &left, &right);
		string_designer_heap(&left, left);
		Return(findcons_hashtable_(table, left, &check));
		if (check != Nil)
			return fmte_("Nickname ~S already exists.", left, NULL);
	}

	return 0;
}

static int check_listconflict_package_(addr pos1, addr pos2)
{
	addr one1, one2, loop;

	GetPackage(pos1, PACKAGE_INDEX_EXPORT, &pos1);
	GetPackage(pos2, PACKAGE_INDEX_EXPORT, &pos2);
	while (pos1 != Nil) {
		GetCons(pos1, &one1, &pos1);
		for (loop = pos2; loop != Nil; ) {
			GetCons(loop, &one2, &loop);
			if (string_equal(one1, one2))
				return fmte_("Conflict occured name ~S.", one1, NULL);
		}
	}

	return 0;
}

static int check_first_usepackage_(addr right)
{
	addr left, one, next;

	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(package_designer_(left, &left));
		for (next = right; next != Nil; ) {
			GetCons(next, &one, &next);
			Return(package_designer_(one, &one));
			Return(check_listconflict_package_(left, one));
		}
	}

	return 0;
}

_g int append_nicknames_package_(addr pos, addr right)
{
	addr table, left, cons, check;

	if (right != Nil) {
		PackageTable(&table);
		while (right != Nil) {
			/* intern nickname */
			GetCons(right, &left, &right);
			string_designer_heap(&left, left);
			Return(intern_hashheap_(table, left, &cons));
			GetCdr(cons, &check);
			/* if name duplicates, check has value. */
			if (check == Nil) {
				SetCdr(cons, pos);
				/* push nickname */
				pushlist_package(pos, PACKAGE_INDEX_NICKNAME, left);
			}
		}
	}

	return 0;
}

static int append_exportname_package_(addr pos, addr left, addr name)
{
	addr bit, cons, one;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &one);
	Return(intern_hashheap_(one, name, &cons));

	/* duplicate check (if same package in use list.) */
	GetCdr(cons, &one);
	if (one == Nil) {
		/* make bitpackage */
		Return(findnil_hashtable_(left, name, &one));
		Check(one == Nil, "export nil error");
		Check(StructBitType(one)->expt == 0, "export error");
		GetBitTypeSymbol(one, &one);
		inheritedbitpackage(&bit, one);
		/* push package */
		SetCdr(cons, bit);
	}

	return 0;
}

static int append_usepackage_package_(addr pos, addr right)
{
	addr left, table, cons, name;

	while (right != Nil) {
		/* intern export */
		GetCons(right, &left, &right);
		Return(package_designer_(left, &left));
		GetPackage(left, PACKAGE_INDEX_EXPORT, &cons);
		GetPackage(left, PACKAGE_INDEX_TABLE, &table);
		while (cons != Nil) {
			GetCons(cons, &name, &cons);
			Return(append_exportname_package_(pos, table, name));
		}

		/* push use-list, used-by-list */
		pushnewlist_package(pos, PACKAGE_INDEX_USE, left);
		pushnewlist_package(left, PACKAGE_INDEX_USED, pos);
	}

	return 0;
}

_g int make_package_(addr name, addr names, addr use, addr *ret)
{
	addr pos;

	/* check */
	string_designer_heap(&name, name);
	Return(check_nicknames_package_(name, names));
	Return(check_first_usepackage_(use));

	/* make package */
	Return(package_heap_(&pos, name));
	Return(append_nicknames_package_(pos, names));
	Return(append_usepackage_package_(pos, use));

	return Result(ret, pos);
}


/*
 *  delete_package
 */
static int delete_eqlist_package(addr root, addr check, addr *ret)
{
	addr left, right1, right2, right3;

	right1 = NULL;
	right2 = root;
	while (right2 != Nil) {
		GetCons(right2, &left, &right3);
		if (left == check) {
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

_g int remove_check_package(addr package, enum PACKAGE_INDEX index, addr symbol)
{
	addr right;

	GetPackage(package, index, &right);
	if (delete_eqlist_package(right, symbol, &right)) {
		/* Cannot delete, abort. */
		return 1;
	}
	SetPackage(package, index, right);

	return 0;
}

static int allunintern_inherited_package_(addr pos, addr right)
{
	int ignore;
	addr left, table, value;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(findnil_hashtable_(table, left, &value));
		Check(value == Nil, "symbol error");
		if (StructBitType(value)->inherit) {
			Return(delete_hashtable_(table, left, &ignore));
		}
	}

	return 0;
}

static int allunintern_uselist_package_(addr pos)
{
	addr left, right, root;

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &root);
	GetPackage(pos, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		/* unintern symbols */
		Return(allunintern_inherited_package_(left, root));
		/* remove use-list */
		if (remove_check_package(left, PACKAGE_INDEX_USED, pos))
			Abort("remove-eqpackage error");
	}

	return 0;
}

static void all_unintern_package(addr pos)
{
	addr table, array, left, right;
	size_t i, size;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);

	/* all unintern */
	GetTableHash(table, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCdr(left, &left);

			/* package nil */
			if (StructBitType(left)->base) {
				GetBitTypeSymbol(left, &left);
				SetPackageSymbol(left, Nil);
			}
		}
	}
	clear_hashtable(table);

	SetPackage(pos, PACKAGE_INDEX_USE, Nil);
	SetPackage(pos, PACKAGE_INDEX_SHADOW, Nil);
	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);
}

/*
 *  return 0:  delete package.
 *  return 1:  package name is nil.
 */
_g int delete_package_(addr pos, int *ret)
{
	int check;
	addr name, right, table;

	Return(package_designer_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	if (name == Nil) {
		/* package object already deleted. */
		return Result(ret, 1);
	}

	/* used-by-list */
	GetPackage(pos, PACKAGE_INDEX_USED, &right);
	if (right != Nil) {
		*ret = 1;
		return fmte_("Package ~S is used by ~S.", name, right, NULL);
	}

	/* all symbon unintern in use-list. */
	Return(allunintern_uselist_package_(pos));

	/* all symbol unintern in my package. */
	all_unintern_package(pos);

	/* delete name and nickname */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	Return(delete_hashtable_(table, name, &check));
	while (right != Nil) {
		GetCons(right, &name, &right);
		Return(delete_hashtable_(table, name, &check));
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);

	return Result(ret, 0);
}


/*
 *  rename_package
 */
static int check_renameone_package_(
		addr table, addr name, addr root, addr right, int *ret)
{
	addr cons;

	string_designer_heap(&name, name);
	Return(findcons_hashtable_(table, name, &cons));
	if (cons == Nil)
		return Result(ret, 0);

	/* If the argument name already registed in the table,
	 *    check unregisted a name and nicknames in package.
	 */
	string_designer_heap(&root, root);
	/* The name may unregist in table. */
	if (string_equal(name, root))
		return Result(ret, 0);
	while (right != Nil) {
		GetCons(right, &root, &right);
		string_designer_heap(&root, root);
		/* The nickname may unregist in table. */
		if (string_equal(name, root))
			return Result(ret, 0);
	}

	/* conflict */
	return Result(ret, 1);
}

static int check_rename_package_(addr pos, addr name, addr right)
{
	int check;
	addr table, root, roots, left;

	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &root);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &roots);

	Return(check_renameone_package_(table, name, root, roots, &check));
	if (check)
		return fmte_("Package rename ~S is conflict.", name, NULL);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(check_renameone_package_(table, left, root, roots, &check));
		if (check)
			return fmte_("Package rename nickname ~S is conflict.", left, NULL);
	}

	return 0;
}

_g int delete_renameone_package_(addr table, addr name)
{
	int check;
	string_designer_heap(&name, name);
	return delete_hashtable_(table, name, &check);
}

static int delete_allnames_package_(addr pos)
{
	addr table, name, left, right;

	/* name */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	Return(delete_renameone_package_(table, name));

	/* nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(delete_renameone_package_(table, left));
	}

	/* index */
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);

	return 0;
}

static int intern_renameone_package_(addr pos, addr table, addr name, int nickname)
{
	addr cons, check;

	string_designer_heap(&name, name);
	Return(intern_hashheap_(table, name, &cons));
	GetCdr(cons, &check);
	if (check == Nil) {
		SetCdr(cons, pos);

		/* nickname */
		if (nickname == 0) {
			SetPackage(pos, PACKAGE_INDEX_NAME, name);
		}
		else {
			pushlist_package(pos, PACKAGE_INDEX_NICKNAME, name);
		}
	}

	return 0;
}

static int intern_allnames_package_(addr pos, addr name, addr right)
{
	addr table, left;

	PackageTable(&table);
	Return(intern_renameone_package_(pos, table, name, 0));
	while (right != Nil) {
		GetCons(right, &left, &right);
		Return(intern_renameone_package_(pos, table, left, 1));
	}

	return 0;
}

_g int rename_package_(addr pos, addr name, addr right, addr *ret)
{
	Return(package_designer_(pos, &pos));
	/* check conflict */
	Return(check_rename_package_(pos, name, right));
	/* delete name and nicknames */
	Return(delete_allnames_package_(pos));
	/* intern name and nicknames */
	Return(intern_allnames_package_(pos, name, right));
	/* result */
	return Result(ret, pos);
}


/*
 *  find-symbol
 */
_g int find_symbol_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	Check(! stringp(name), "type error");
	Return(package_designer_(package, &package));
	Return(find_bitpackage_(package, name, &name));
	if (name == Nil) {
		*value = Nil;
		return Result(ret, PACKAGE_TYPE_NIL);
	}
	GetBitTypeSymbol(name, value);

	return Result(ret, StructBitType(name)->intern);
}


/*
 *  find_allsymbols
 */
static int push_basesymbol_package_(addr key, addr left, addr name, addr *cons)
{
	addr check;

	GetPackage(left, PACKAGE_INDEX_NAME, &check);
	if (string_equal(key, check)) {
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(findnil_hashtable_(left, name, &left));
		if (left != Nil && StructBitType(left)->base) {
			GetBitTypeSymbol(left, &left);
			cons_heap(cons, left, *cons);
		}
	}

	return 0;
}

_g int find_allsymbols_package_(addr name, addr *ret)
{
	addr array, left, right, key, cons;
	size_t i, size;

	cons = Nil;
	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			Return(push_basesymbol_package_(key, left, name, &cons));
		}
	}

	return Result(ret, cons);
}


/*
 *  list_all_packages
 */
static void pushbase_package(addr key, addr package, addr *cons)
{
	addr check;

	GetPackage(package, PACKAGE_INDEX_NAME, &check);
	if (string_equal(key, check))
		cons_heap(cons, package, *cons);
}

_g void list_all_packages(addr *ret)
{
	addr array, left, right, key, cons;
	size_t i, size;

	cons = Nil;
	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			pushbase_package(key, left, &cons);
		}
	}
	*ret = cons;
}


/*
 *  in-package
 */
_g int in_package_(Execute ptr, addr package, addr *ret)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	Return(package_designer_(package, &package));
	setspecial_local(ptr, symbol, package);
	if (ret)
		*ret = package;

	return 0;
}

_g int in_package_lisp_package_(void)
{
	addr package, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	Return(find_char_package_(LISP_PACKAGE, &package));
	GetConst(SPECIAL_PACKAGE, &symbol);
	setspecial_local(ptr, symbol, package);

	return 0;
}


/*
 *  for C language
 */
_g int externalp_package_(addr symbol, addr package, int *ret)
{
	addr name, left, right;

	Check(! symbolp(symbol), "type error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* export check */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil)
		return Result(ret, 1);

	/* table */
	GetBitTypeSymbol(right, &left);
	return Result(ret, left != symbol);
}

_g int exportp_package_(addr symbol, addr package, int *ret)
{
	addr name, left, right;

	Check(! symbolp(symbol), "type error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* export check */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil)
		return Result(ret, 0);

	/* table */
	GetBitTypeSymbol(right, &left);
	return Result(ret, (left == symbol) && (int)StructBitType(right)->expt);
}

_g int exportp_name_package_(addr package, addr name, addr *value, int *ret)
{
	addr right;

	Check(! stringp(name), "type error");
	Return(package_designer_(package, &package));

	/* export check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	Return(findnil_hashtable_(right, name, &right));
	if (right == Nil) {
		*value = Unbound;
		return Result(ret, 0);
	}

	/* table */
	GetBitTypeSymbol(right, value);
	return Result(ret, (int)StructBitType(right)->expt);
}

_g int checksymbol_package_(addr symbol, addr package, int *ret)
{
	enum PACKAGE_TYPE type;
	addr check, name;

	GetNameSymbol(symbol, &name);
	Return(find_symbol_package_(package, name, &check, &type));

	return Result(ret, type != PACKAGE_TYPE_NIL && check == symbol);
}

_g void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret)
{
	switch (type) {
		case PACKAGE_TYPE_INTERNAL:
			GetConst(KEYWORD_INTERNAL, ret);
			break;

		case PACKAGE_TYPE_EXTERNAL:
			GetConst(KEYWORD_EXTERNAL, ret);
			break;

		case PACKAGE_TYPE_INHERITED:
			GetConst(KEYWORD_INHERITED, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}


/*
 *  initialize
 */
_g void init_package(void)
{
	init_package_common();
}

