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
static void find_package_direct(addr pos, addr *ret)
{
	addr table;
	PackageTable(&table);
	findvalue_hashtable(table, pos, ret);
}

static void find_package_local(addr pos, addr *ret)
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
	findvalue_hashtable(table, name, ret);
	rollback_local(local, stack);
}

_g void find_package(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
			*ret = pos;
			break;

		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			GetNameSymbol(pos, &pos);
			find_package_direct(pos, ret);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos))
				goto error;
			find_package_direct(pos, ret);
			break;

		case LISPTYPE_STRING:
			find_package_direct(pos, ret);
			break;

		case LISPTYPE_CHARACTER:
			find_package_local(pos, ret);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("Argument ~S must be a string, symbol or package.", pos, NULL);
	*ret = NULL;
}

_g void find_char_package(const char *name, addr *ret)
{
	addr pos;
	PackageTable(&pos);
	findvalue_char_hashtable(pos, name, ret);
}

_g void package_designer(addr pos, addr *ret)
{
	addr package;

	find_package(pos, &package);
	if (package == Nil)
		fmte("No such a package ~S.", pos, NULL);
	*ret = package;
}

static void append_root_package(addr name, addr package)
{
	addr table;

	PackageTable(&table);
	intern_hashheap(table, name, &name);
	SetCdr(name, package);  /* (name . package) */
}

_g void package_size_heap(addr *ret, addr name, size_t size)
{
	addr pos, table;

	/* name check */
	string_designer_heap(&name, name);
	find_package_direct(name, &pos);
	if (pos != Nil)
		fmte("Package name ~S already used.", name, NULL);

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
	append_root_package(name, pos);

	*ret = pos;
}

_g void package_heap(addr *ret, addr name)
{
	package_size_heap(ret, name, 0);
}

static void package_char_heap(addr *ret, const char *name)
{
	addr key;
	strvect_char_heap(&key, name);
	package_heap(ret, key);
}

static void package_root_heap(addr *ret)
{
	addr pos;

	hashtable_size_heap(&pos, LISP_PACKAGE_HASHSIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

static void intern_common_constant(addr package, const char *str, addr symbol)
{
	addr pos, name;

	/* set table */
	strvect_char_heap(&name, str);
	intern_bitpackage(package, name, &pos);
	SetBitTypeSymbol(pos, symbol);

	/* set symbol */
	SetStatusValue(symbol, LISPSTATUS_READONLY, 0);
	SetPackageSymbol(symbol, package);
	SetStatusValue(symbol, LISPSTATUS_READONLY, 1);
	export_package(package, symbol);
}

static void intern_common_default(void)
{
	addr common;

	find_char_package(LISP_COMMON, &common);
	intern_common_constant(common, "NIL", Nil);
	intern_common_constant(common, "T", T);
	SetConstant(CONSTANT_COMMON_NIL, Nil);
	SetConstant(CONSTANT_COMMON_T, T);
}

static void intern_package_symbol(void)
{
	addr symbol;

	/* common-lisp::*package* */
	internchar(LISP_COMMON, "*PACKAGE*", &symbol);
	setspecial_symbol(symbol);
	SetConstant(CONSTANT_SPECIAL_PACKAGE, symbol);

	/* common-lisp-user package */
	find_char_package(LISP_COMMON_USER, &symbol);
	Check(symbol == Nil, LISP_COMMON_USER " package is not found.");
	SetConstant(CONSTANT_PACKAGE_COMMON_LISP_USER, symbol);
}

static void set_default_package(addr package)
{
	addr symbol;

	/* setq *package* */
	GetConst(SPECIAL_PACKAGE, &symbol);
	/* not special-stack */
	SetValueSymbol(symbol, package);
}

_g void build_package_settings(void)
{
	addr package, common, cons, name;

	/* COMMON-LISP */
	find_char_package(LISP_COMMON, &common);
	strvect_char_heap(&name, "CL");
	list_heap(&cons, name, NULL);
	append_nicknames_package(common, cons);

	/* COMMON-LISP-USER */
	find_char_package(LISP_COMMON_USER, &package);
	strvect_char_heap(&name, "CL-USER");
	list_heap(&cons, name, NULL);
	append_nicknames_package(package, cons);
	use_package(package, common);

	/* LISP-PACKAGE */
	find_char_package(LISP_PACKAGE, &package);
	use_package(package, common);
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

static void import_exit_and_quit_package(addr package)
{
	addr symbol;

	/* (import 'lisp-system::exit 'common-lisp-user) */
	GetConst(SYSTEM_EXIT, &symbol);
	import_package(package, symbol);
	/* (import 'lisp-system::quit 'common-lisp-user) */
	GetConst(SYSTEM_QUIT, &symbol);
	import_package(package, symbol);
}

static void system_package(const char *name, size_t size, constindex index)
{
	addr pos;

	strvect_char_heap(&pos, name);
	package_size_heap(&pos, pos, size + 1);
	SetConstant(index, pos);
}

#define SystemPackage(x,y,z) { \
	system_package(LISP_##x, LISP_PACKAGE_COUNT_##y, CONSTANT_PACKAGE_##z); \
}
_g void build_package(void)
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
	package_char_heap(&user, LISP_COMMON_USER);
	package_char_heap(&package, LISP_PACKAGE);
	package_char_heap(&package, LISP_USER);

	/* symbol setting */
	intern_common_default();
	intern_symbol_header();
	intern_package_symbol();
	build_package_settings();
	set_default_package(user);
	set_gentemp_package();
	build_default_use_package();
	import_exit_and_quit_package(user);
}

_g void getpackage(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(SPECIAL_PACKAGE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (GetType(pos) != LISPTYPE_PACKAGE)
		fmte("symbol *package* is not a package type.", NULL);
	*ret = pos;
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

static void check_nicknames_package(addr name, addr right)
{
	addr left, check, table;

	/* check name */
	PackageTable(&table);
	findcons_hashtable(table, name, &check);
	if (check != Nil)
		fmte("Package ~S already exists.", name, NULL);

	/* check names */
	while (right != Nil) {
		GetCons(right, &left, &right);
		string_designer_heap(&left, left);
		findcons_hashtable(table, left, &check);
		if (check != Nil)
			fmte("Nickname ~S already exists.", left, NULL);
	}
}

static void check_listconflict_package(addr pos1, addr pos2)
{
	addr one1, one2, loop;

	GetPackage(pos1, PACKAGE_INDEX_EXPORT, &pos1);
	GetPackage(pos2, PACKAGE_INDEX_EXPORT, &pos2);
	while (pos1 != Nil) {
		GetCons(pos1, &one1, &pos1);
		for (loop = pos2; loop != Nil; ) {
			GetCons(loop, &one2, &loop);
			if (string_equal(one1, one2))
				fmte("Conflict occured name ~S.", one1, NULL);
		}
	}
}

static void check_first_usepackage(addr right)
{
	addr left, one, next;

	while (right != Nil) {
		GetCons(right, &left, &right);
		package_designer(left, &left);
		for (next = right; next != Nil; ) {
			GetCons(next, &one, &next);
			package_designer(one, &one);
			check_listconflict_package(left, one);
		}
	}
}

_g void append_nicknames_package(addr pos, addr right)
{
	addr table, left, cons, check;

	if (right != Nil) {
		PackageTable(&table);
		while (right != Nil) {
			/* intern nickname */
			GetCons(right, &left, &right);
			string_designer_heap(&left, left);
			intern_hashheap(table, left, &cons);
			GetCdr(cons, &check);
			/* if name duplicates, check has value. */
			if (check == Nil) {
				SetCdr(cons, pos);
				/* push nickname */
				pushlist_package(pos, PACKAGE_INDEX_NICKNAME, left);
			}
		}
	}
}

static void append_exportname_package(addr pos, addr left, addr name)
{
	addr bit, cons, one;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &one);
	intern_hashheap(one, name, &cons);

	/* duplicate check (if same package in use list.) */
	GetCdr(cons, &one);
	if (one == Nil) {
		/* make bitpackage */
		findvalue_hashtable(left, name, &one);
		Check(one == Nil, "export nil error");
		Check(StructBitType(one)->expt == 0, "export error");
		GetBitTypeSymbol(one, &one);
		inheritedbitpackage(&bit, one);
		/* push package */
		SetCdr(cons, bit);
	}
}

static void append_usepackage_package(addr pos, addr right)
{
	addr left, table, cons, name;

	while (right != Nil) {
		/* intern export */
		GetCons(right, &left, &right);
		package_designer(left, &left);
		GetPackage(left, PACKAGE_INDEX_EXPORT, &cons);
		GetPackage(left, PACKAGE_INDEX_TABLE, &table);
		while (cons != Nil) {
			GetCons(cons, &name, &cons);
			append_exportname_package(pos, table, name);
		}

		/* push use-list, used-by-list */
		pushnewlist_package(pos, PACKAGE_INDEX_USE, left);
		pushnewlist_package(left, PACKAGE_INDEX_USED, pos);
	}
}

_g void make_package(addr name, addr names, addr use, addr *ret)
{
	addr pos;

	/* check */
	string_designer_heap(&name, name);
	check_nicknames_package(name, names);
	check_first_usepackage(use);

	/* make package */
	package_heap(&pos, name);
	append_nicknames_package(pos, names);
	append_usepackage_package(pos, use);
	*ret = pos;
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

static void allunintern_inherited_package(addr pos, addr right)
{
	addr left, table, check;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);
	while (right != Nil) {
		GetCons(right, &left, &right);
		findvalue_hashtable(table, left, &check);
		Check(check == Nil, "symbol error");
		if (StructBitType(check)->inherit)
			delete_hashtable(table, left);
	}
}

static void allunintern_uselist_package(addr pos)
{
	addr left, right, root;

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &root);
	GetPackage(pos, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		/* unintern symbols */
		allunintern_inherited_package(left, root);
		/* remove use-list */
		if (remove_check_package(left, PACKAGE_INDEX_USED, pos))
			Abort("remove-eqpackage error");
	}
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
_g int delete_package(addr pos)
{
	addr name, right, table;

	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	if (name == Nil) {
		/* package object already deleted. */
		return 1;
	}

	/* used-by-list */
	GetPackage(pos, PACKAGE_INDEX_USED, &right);
	if (right != Nil) {
		fmte("Package ~S is used by ~S.", name, right, NULL);
		return 1;
	}

	/* all symbon unintern in use-list. */
	allunintern_uselist_package(pos);

	/* all symbol unintern in my package. */
	all_unintern_package(pos);

	/* delete name and nickname */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	delete_hashtable(table, name);
	while (right != Nil) {
		GetCons(right, &name, &right);
		delete_hashtable(table, name);
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);

	return 0;
}


/*
 *  rename_package
 */
static int check_renameone_package(addr table, addr name, addr root, addr right)
{
	addr cons;

	string_designer_heap(&name, name);
	findcons_hashtable(table, name, &cons);
	if (cons == Nil)
		return 0;

	/* If the argument name already registed in the table,
	 *    check unregisted a name and nicknames in package.
	 */
	string_designer_heap(&root, root);
	/* The name may unregist in table. */
	if (string_equal(name, root))
		return 0;
	while (right != Nil) {
		GetCons(right, &root, &right);
		string_designer_heap(&root, root);
		/* The nickname may unregist in table. */
		if (string_equal(name, root))
			return 0;
	}

	/* conflict */
	return 1;
}

static void check_rename_package(addr pos, addr name, addr right)
{
	addr table, root, roots, left;

	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &root);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &roots);

	if (check_renameone_package(table, name, root, roots))
		fmte("Package rename ~S is conflict.", name, NULL);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (check_renameone_package(table, left, root, roots))
			fmte("Package rename nickname ~S is conflict.", left, NULL);
	}
}

_g void delete_renameone_package(addr table, addr name)
{
	string_designer_heap(&name, name);
	delete_hashtable(table, name);
}

static void delete_allnames_package(addr pos)
{
	addr table, name, left, right;

	/* name */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	delete_renameone_package(table, name);

	/* nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		delete_renameone_package(table, left);
	}

	/* index */
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);
}

static void intern_renameone_package(addr pos, addr table, addr name, int nickname)
{
	addr cons, check;

	string_designer_heap(&name, name);
	intern_hashheap(table, name, &cons);
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
}

static void intern_allnames_package(addr pos, addr name, addr right)
{
	addr table, left;

	PackageTable(&table);
	intern_renameone_package(pos, table, name, 0);
	while (right != Nil) {
		GetCons(right, &left, &right);
		intern_renameone_package(pos, table, left, 1);
	}
}

_g void rename_package(addr pos, addr name, addr right, addr *ret)
{
	package_designer(pos, &pos);
	/* check conflict */
	check_rename_package(pos, name, right);
	/* delete name and nicknames */
	delete_allnames_package(pos);
	/* intern name and nicknames */
	intern_allnames_package(pos, name, right);
	/* result */
	*ret = pos;
}


/*
 *  find-symbol
 */
_g enum PACKAGE_TYPE find_symbol_package(addr package, addr name, addr *ret)
{
	Check(! stringp(name), "type error");
	package_designer(package, &package);
	find_bitpackage(package, name, &name);
	if (name == Nil) {
		*ret = Nil;
		return PACKAGE_TYPE_NIL;
	}
	GetBitTypeSymbol(name, ret);

	return StructBitType(name)->intern;
}


/*
 *  find_allsymbols
 */
static void push_basesymbol_package(addr key, addr left, addr name, addr *cons)
{
	addr check;

	GetPackage(left, PACKAGE_INDEX_NAME, &check);
	if (string_equal(key, check)) {
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		findvalue_hashtable(left, name, &left);
		if (left != Nil && StructBitType(left)->base) {
			GetBitTypeSymbol(left, &left);
			cons_heap(cons, left, *cons);
		}
	}
}

_g void find_allsymbols_package(addr name, addr *ret)
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
			push_basesymbol_package(key, left, name, &cons);
		}
	}
	*ret = cons;
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
_g void in_package(Execute ptr, addr package, addr *ret)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	package_designer(package, &package);
	setspecial_local(ptr, symbol, package);
	if (ret)
		*ret = package;
}

_g void in_package_lisp_package(void)
{
	addr package, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	find_char_package(LISP_PACKAGE, &package);
	GetConst(SPECIAL_PACKAGE, &symbol);
	setspecial_local(ptr, symbol, package);
}


/*
 *  for C language
 */
_g int externalp_package(addr symbol, addr package)
{
	addr name, left, right;

	Check(! symbolp(symbol), "type error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* export check */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	findvalue_hashtable(right, name, &right);
	if (right == Nil)
		return 1;

	/* table */
	GetBitTypeSymbol(right, &left);
	return left != symbol;
}

_g int exportp_package(addr symbol, addr package)
{
	addr name, left, right;

	Check(! symbolp(symbol), "type error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* export check */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	findvalue_hashtable(right, name, &right);
	if (right == Nil)
		return 0;

	/* table */
	GetBitTypeSymbol(right, &left);
	return (left == symbol) && (int)StructBitType(right)->expt;
}

_g int exportp_name_package(addr package, addr name, addr *ret)
{
	addr right;

	Check(! stringp(name), "type error");
	package_designer(package, &package);

	/* export check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &right);
	findvalue_hashtable(right, name, &right);
	if (right == Nil) {
		*ret = Unbound;
		return 0;
	}

	/* table */
	GetBitTypeSymbol(right, ret);
	return (int)StructBitType(right)->expt;
}

_g int checksymbol_package(addr symbol, addr package)
{
	enum PACKAGE_TYPE type;
	addr check, name;

	GetNameSymbol(symbol, &name);
	type = find_symbol_package(package, name, &check);

	return type != PACKAGE_TYPE_NIL && check == symbol;
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

