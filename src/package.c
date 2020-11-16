#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "local.h"
#include "intern_count.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_delete.h"
#include "package_designer.h"
#include "package_export.h"
#include "package_import.h"
#include "package_intern.h"
#include "package_make.h"
#include "package_object.h"
#include "package_use.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"
#include "type_table.h"

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
	Return(strvect_setc_(name, 0, c));
	Return(findnil_hashtable_(table, name, ret));
	rollback_local(local, stack);

	return 0;
}

int find_package_(addr pos, addr *ret)
{
	addr type;

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
	*ret = Nil;
	GetTypeTable(&type, PackageDesigner);
	return call_type_error_(NULL, pos, type);
}

int find_char_package_(const char *name, addr *ret)
{
	addr pos;
	PackageTable(&pos);
	return findnil_char_hashtable_(pos, name, ret);
}

static int append_root_package_(addr name, addr package)
{
	addr table;

	PackageTable(&table);
	Return(intern_hashheap_(table, name, &name));
	SetCdr(name, package);  /* (name . package) */

	return 0;
}

int package_size_heap_(addr *ret, addr name, size_t size)
{
	addr pos, table;

	/* name check */
	Return(string_designer_heap_(&name, name, NULL));
	Return(find_package_direct_(name, &pos));
	if (pos != Nil) {
		return call_simple_package_error_va_(NULL,
				"The package name ~S is already used.", name, NULL);
	}

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

int package_heap_(addr *ret, addr name)
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

static int set_default_package_(addr package)
{
	addr symbol;

	/* setq *package* */
	GetConst(SPECIAL_PACKAGE, &symbol);
	/* not special-stack */
	return setvalue_symbol_(symbol, package);
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
	addr root, user;

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

	/* symbol setting */
	Return(intern_common_default_());
	Return(intern_symbol_header_());
	Return(intern_package_symbol_());
	Return(build_package_settings_());
	Return(set_default_package_(user));
	set_gentemp_package();
	build_default_use_package();
	Return(import_exit_and_quit_package_(user));

	return 0;
}

void build_package(void)
{
	Error(build_package_value_());
}

int getpackage_(Execute ptr, addr *ret)
{
	addr pos, type;

	GetConst(SPECIAL_PACKAGE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (GetType(pos) != LISPTYPE_PACKAGE) {
		GetTypeTable(&type, Package);
		return call_type_error_(NULL, pos, type);
	}

	return Result(ret, pos);
}


/*
 *  make_package
 */
int append_nicknames_package_(addr pos, addr right)
{
	addr table, left, cons, check;

	if (right != Nil) {
		PackageTable(&table);
		while (right != Nil) {
			/* intern nickname */
			GetCons(right, &left, &right);
			Return(string_designer_heap_(&left, left, NULL));
			Return(intern_hashheap_(table, left, &cons));
			GetCdr(cons, &check);
			/* if name duplicates, check has value. */
			if (check == Nil) {
				SetCdr(cons, pos);
				/* push nickname */
				push_list_nicknames_package(pos, left);
			}
		}
	}

	return 0;
}


/*
 *  rename_package
 */
static int check_renameone_package_(
		addr table, addr name, addr root, addr right, int *ret)
{
	int check;
	addr cons;

	Return(string_designer_heap_(&name, name, NULL));
	Return(findcons_hashtable_(table, name, &cons));
	if (cons == Nil)
		return Result(ret, 0);

	/* If the argument name already registed in the table,
	 *    check unregisted a name and nicknames in package.
	 */
	Return(string_designer_heap_(&root, root, NULL));
	/* The name may unregist in table. */
	Return(string_equal_(name, root, &check));
	if (check)
		return Result(ret, 0);
	while (right != Nil) {
		GetCons(right, &root, &right);
		Return(string_designer_heap_(&root, root, NULL));
		/* The nickname may unregist in table. */
		Return(string_equal_(name, root, &check));
		if (check)
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

int delete_renameone_package_(addr table, addr name)
{
	int check;
	Return(string_designer_heap_(&name, name, NULL));
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

	Return(string_designer_heap_(&name, name, NULL));
	Return(intern_hashheap_(table, name, &cons));
	GetCdr(cons, &check);
	if (check == Nil) {
		SetCdr(cons, pos);

		/* nickname */
		if (nickname == 0) {
			SetPackage(pos, PACKAGE_INDEX_NAME, name);
		}
		else {
			push_list_nicknames_package(pos, name);
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

int rename_package_(addr pos, addr name, addr right, addr *ret)
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
int find_symbol_package_(addr package, addr name,
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
	int check;
	addr value;

	GetPackage(left, PACKAGE_INDEX_NAME, &value);
	Return(string_equal_(key, value, &check));
	if (check) {
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(findnil_hashtable_(left, name, &left));
		if (left != Nil && StructBitType(left)->base) {
			GetBitTypeSymbol(left, &left);
			cons_heap(cons, left, *cons);
		}
	}

	return 0;
}

int find_allsymbols_package_(addr name, addr *ret)
{
	addr array, left, right, key, cons;
	size_t i, size;

	Return(string_designer_heap_(&name, name, NULL));
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
static int pushbase_package_(addr key, addr package, addr *cons)
{
	int check;
	addr value;

	GetPackage(package, PACKAGE_INDEX_NAME, &value);
	Return(string_equal_(key, value, &check));
	if (check)
		cons_heap(cons, package, *cons);

	return 0;
}

int list_all_packages_(addr *ret)
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
			Return(pushbase_package_(key, left, &cons));
		}
	}

	return Result(ret, cons);
}


/*
 *  in-package
 */
int in_package_(Execute ptr, addr package, addr *ret)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	Return(package_designer_(package, &package));
	setspecial_local(ptr, symbol, package);
	if (ret)
		*ret = package;

	return 0;
}


/*
 *  for C language
 */
int externalp_package_(addr symbol, addr package, int *ret)
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

int exportp_package_(addr symbol, addr package, int *ret)
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

int exportp_name_package_(addr package, addr name, addr *value, int *ret)
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

int checksymbol_package_(addr symbol, addr package, int *ret)
{
	enum PACKAGE_TYPE type;
	addr check, name;

	GetNameSymbol(symbol, &name);
	Return(find_symbol_package_(package, name, &check, &type));

	return Result(ret, type != PACKAGE_TYPE_NIL && check == symbol);
}

void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret)
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
void init_package(void)
{
	init_package_designer();
	init_package_intern();
	init_package_make();
}

