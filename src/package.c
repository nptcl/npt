#include "bignum.h"
#include "build.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "intern_count.h"
#include "memory.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "strtype.h"
#include "symbol.h"

#define PackageTable(x) (*(x) = Root(LISPINDEX_PACKAGE))

struct bittype_struct {
	unsigned base : 1;
	unsigned expt : 1;
	unsigned import : 1;
	unsigned inherit : 1;
	unsigned shadow : 1;
	enum PACKAGE_TYPE intern;
};
#define PtrBitTypeBody(x) PtrBodySSa(x, 1)
#define StructBitType(x) ((struct bittype_struct *)PtrBitTypeBody(x))
#define GetBitTypeSymbol(x,y) GetArraySS((x), 0, (y))
#define SetBitTypeSymbol(x,y) SetArraySS((x), 0, (y))

#define GetBitTypeIntern(x,y) (*(y) = StructBitType(x)->intern)
#define GetBitTypeBase(x,y) (*(y) = StructBitType(x)->base)
#define GetBitTypeExport(x,y) (*(y) = StructBitType(x)->expt)
#define GetBitTypeImport(x,y) (*(y) = StructBitType(x)->import)
#define GetBitTypeInherit(x,y) (*(y) = StructBitType(x)->inherit)
#define GetBitTypeShadow(x,y) (*(y) = StructBitType(x)->shadow)

#define SetBitTypeIntern(x,y) (StructBitType(x)->intern = (y))
#define SetBitTypeBase(x,y) (StructBitType(x)->base = (y))
#define SetBitTypeExport(x,y) (StructBitType(x)->expt = (y))
#define SetBitTypeImport(x,y) (StructBitType(x)->import = (y))
#define SetBitTypeInherit(x,y) (StructBitType(x)->inherit = (y))
#define SetBitTypeShadow(x,y) (StructBitType(x)->shadow = (y))

static int intern_bitpackage(addr package, addr name, addr *ret);


/*
 *  bitpackage
 */
static void alloc_bitpackage(addr *ret, addr symbol, enum PACKAGE_TYPE type)
{
	addr bit;
	struct bittype_struct *ptr;

	heap_smallsize(&bit, LISPSYSTEM_BITTYPE, 1, sizeof(struct bittype_struct));
	SetBitTypeSymbol(bit, symbol);
	ptr = StructBitType(bit);
	clearpoint(ptr);
	ptr->intern = type;
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

void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package)
{
	make_bitpackage(ret, name, package);
	GetBitTypeSymbol(*ret, symbol);
}

static void internbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeBase(bit, 1);
	*ret = bit;
}

static void importbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INTERNAL);
	SetBitTypeImport(bit, 1);
	*ret = bit;
}

static void inheritedbitpackage(addr *ret, addr symbol)
{
	addr bit;

	alloc_bitpackage(&bit, symbol, PACKAGE_TYPE_INHERITED);
	SetBitTypeInherit(bit, 1);
	*ret = bit;
}

static void shadowintern_bitpackage(addr bit, addr name, addr package)
{
	addr symbol;
	struct bittype_struct *ptr;

	symbol_heap(&symbol);
	setname_symbol(symbol, name);
	setpackage_symbol(symbol, package);
	SetBitTypeSymbol(bit, symbol);
	ptr = StructBitType(bit);
	ptr->intern = PACKAGE_TYPE_INTERNAL;
	ptr->base = 1;
	ptr->inherit = 0;
}

static void shadowimport_bitpackage(addr bit, addr symbol)
{
	struct bittype_struct *ptr;

	SetBitTypeSymbol(bit, symbol);
	ptr = StructBitType(bit);
	ptr->intern = PACKAGE_TYPE_INTERNAL;
	ptr->base = 0;
	ptr->import = 1;
	ptr->inherit = 0;
}


/*
 *  package object
 */
static void string_designer(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			*ret = pos;
			break;

		case LISPTYPE_STRING:
			*ret = pos;
			break;

		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			GetNameSymbol(pos, ret);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("Package name ~S must be a symbol or string.", pos, NULL);
}

static addr findstringlocal(addr pos)
{
	addr table;

	PackageTable(&table);
	findvalue_hashtable(table, pos, &pos);

	return pos;
}

static addr findcharacterlocal(addr pos)
{
	addr table, name;
	LocalRoot local;
	LocalStack stack;
	unicode u;

	PackageTable(&table);
	GetCharacter(pos, &u);

	/* findvalue */
	local = Local_Thread;
	push_local(local, &stack);
	strvect_local(local, &name, 1);
	strvect_setc(name, 0, u);
	findvalue_hashtable(table, name, &pos);
	rollback_local(local, stack);

	return pos;
}

void find_package(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
			*ret = pos;
			break;

		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			GetNameSymbol(pos, &pos);
			*ret = findstringlocal(pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			*ret = findstringlocal(pos);
			break;

		case LISPTYPE_STRING:
			*ret = findstringlocal(pos);
			break;

		case LISPTYPE_CHARACTER:
			*ret = findcharacterlocal(pos);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("Argument ~S must be a string, symbol or package.", pos, NULL);
	*ret = NULL;
}

addr findr_char_package(const char *name)
{
	addr pos;

	PackageTable(&pos);
	findvalue_char_hashtable(pos, name, &pos);

	return pos;
}

void find_char_package(const char *name, addr *ret)
{
	*ret = findr_char_package(name);
}

static void package_designer(addr pos, addr *ret)
{
	addr package;

	find_package(pos, &package);
	if (package == Nil)
		fmte("No such a package ~S.", pos, NULL);
	*ret = package;
}

static void appendrootpackage(addr name, addr package)
{
	addr table;

	PackageTable(&table);
	intern_hashheap(table, name, &name);
	SetCdr(name, package);  /* (name . package) */
}

static void package_size_heap(addr *ret, addr name, size_t size)
{
	addr pos, table;

	/* name check */
	string_designer(name, &name);
	if (findstringlocal(name) != Nil)
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
	appendrootpackage(name, pos);

	*ret = pos;
}

static void package_heap(addr *ret, addr name)
{
	package_size_heap(ret, name, 0);
}

static void package_char_heap(addr *ret, const char *name)
{
	addr key;
	strvect_char_heap(&key, name);
	package_heap(ret, key);
}

static void packageroot_heap(addr *ret)
{
	addr pos;

	hashtable_size_heap(&pos, PACKAGE_HASHSIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

static void constantsymbol(addr package, const char *str, addr symbol)
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
	constantsymbol(common, "NIL", Nil);
	constantsymbol(common, "T", T);
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

static void append_nicknames(addr pos, addr right);
void build_package_settings(void)
{
	addr package, common, cons;

	/* COMMON-LISP */
	find_char_package(LISP_COMMON, &common);
	list_heap(&cons, strvect_char_heapr("CL"), NULL);
	append_nicknames(common, cons);

	/* COMMON-LISP-USER */
	find_char_package(LISP_COMMON_USER, &package);
	list_heap(&cons, strvect_char_heapr("CL-USER"), NULL);
	append_nicknames(package, cons);
	use_package(package, common);

	/* LISP-PACKAGE */
	find_char_package(LISP_PACKAGE, &package);
	use_package(package, common);
}

static void set_gentemp_counter(void)
{
	addr pos;
	fixnum_heap(&pos, 1);
	SetConst(PACKAGE_GENTEMP, pos);
}

static void build_package_default_use(void)
{
	addr name, list;

	strvect_char_heap(&name, "COMMON-LISP");
	list_heap(&list, name, NULL);
	SetConstant(CONSTANT_PACKAGE_DEFAULT_USE, list);
}

static void import_exit_and_quit(addr package)
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
void build_package(void)
{
	addr root, package, user;

	/* package root */
	packageroot_heap(&root);
	SetRoot(LISPINDEX_PACKAGE, root);

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
	set_gentemp_counter();
	build_package_default_use();
	import_exit_and_quit(user);
}

void getpackage(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(SPECIAL_PACKAGE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (GetType(pos) != LISPTYPE_PACKAGE)
		fmte("symbol *package* is not a package type.", NULL);
	*ret = pos;
}

int packagep(addr pos)
{
	return GetType(pos) == LISPTYPE_PACKAGE;
}

int package_designer_p(addr pos)
{
	return packagep(pos) || string_designer_p(pos);
}

int package_designer_equal(addr left, addr right)
{
	if (packagep(left))
		GetPackage(left, PACKAGE_INDEX_NAME, &left);
	if (packagep(right))
		GetPackage(right, PACKAGE_INDEX_NAME, &right);

	return string_designer_equal(left, right);
}

void getdocument_package(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_DOCUMENT, ret);
}

void setdocument_package(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	SetPackage(pos, PACKAGE_INDEX_DOCUMENT, value);
}


/*
 *  make_package
 */
static void pushlist(addr package, enum PACKAGE_INDEX index, addr pos)
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

static void pushnewlist(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr left, right;

	GetPackage(package, index, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == pos)
			return; /* don't push */
	}
	pushlist(package, index, pos);
}

static void check_nicknames(addr name, addr right)
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
		string_designer(left, &left);
		findcons_hashtable(table, left, &check);
		if (check != Nil)
			fmte("Nickname ~S already exists.", left, NULL);
	}
}

static void check_listconflict(addr pos1, addr pos2)
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
			check_listconflict(left, one);
		}
	}
}

static void append_nicknames(addr pos, addr right)
{
	addr table, left, cons, check;

	if (right != Nil) {
		PackageTable(&table);
		while (right != Nil) {
			/* intern nickname */
			GetCons(right, &left, &right);
			string_designer(left, &left);
			intern_hashheap(table, left, &cons);
			GetCdr(cons, &check);
			/* if name duplicates, check has value. */
			if (check == Nil) {
				SetCdr(cons, pos);
				/* push nickname */
				pushlist(pos, PACKAGE_INDEX_NICKNAME, left);
			}
		}
	}
}

static void append_exportname(addr pos, addr left, addr name)
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

static void append_usepackage(addr pos, addr right)
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
			append_exportname(pos, table, name);
		}

		/* push use-list, used-by-list */
		pushnewlist(pos, PACKAGE_INDEX_USE, left);
		pushnewlist(left, PACKAGE_INDEX_USED, pos);
	}
}

void make_package(addr name, addr names, addr use, addr *ret)
{
	addr pos;

	/* check */
	string_designer(name, &name);
	check_nicknames(name, names);
	check_first_usepackage(use);

	/* make package */
	package_heap(&pos, name);
	append_nicknames(pos, names);
	append_usepackage(pos, use);
	*ret = pos;
}


/*
 *  delete_package
 */
static int delete_eqlist(addr root, addr check, addr *ret)
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

static int remove_eqpackage(addr package, enum PACKAGE_INDEX index, addr symbol)
{
	addr right;

	GetPackage(package, index, &right);
	if (delete_eqlist(right, symbol, &right)) {
		/* Cannot delete, abort. */
		return 1;
	}
	SetPackage(package, index, right);

	return 0;
}

static void allunintern_inherited(addr pos, addr right)
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

static void allunintern_uselist(addr pos)
{
	addr left, right, root;

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &root);
	GetPackage(pos, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		/* unintern symbols */
		allunintern_inherited(left, root);
		/* remove use-list */
		if (remove_eqpackage(left, PACKAGE_INDEX_USED, pos))
			Abort("remove-eqpackage error");
	}
}

static void allunintern(addr pos)
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
int delete_package(addr pos)
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
	allunintern_uselist(pos);

	/* all symbol unintern in my package. */
	allunintern(pos);

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
static int check_renameone(addr table, addr name, addr root, addr right)
{
	addr cons;

	string_designer(name, &name);
	findcons_hashtable(table, name, &cons);
	if (cons == Nil) return 0;

	/* If the argument name already registed in the table,
	 *    check unregisted a name and nicknames in package.
	 */
	string_designer(root, &root);
	/* The name may unregist in table. */
	if (string_equal(name, root)) return 0;
	while (right != Nil) {
		GetCons(right, &root, &right);
		string_designer(root, &root);
		/* The nickname may unregist in table. */
		if (string_equal(name, root)) return 0;
	}

	/* conflict */
	return 1;
}

static void check_rename(addr pos, addr name, addr right)
{
	addr table, root, roots, left;

	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &root);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &roots);

	if (check_renameone(table, name, root, roots))
		fmte("Package rename ~S is conflict.", name, NULL);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (check_renameone(table, left, root, roots))
			fmte("Package rename nickname ~S is conflict.", left, NULL);
	}
}

static void delete_renameone(addr table, addr name)
{
	string_designer(name, &name);
	delete_hashtable(table, name);
}

static void delete_allnames(addr pos)
{
	addr table, name, left, right;

	/* name */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	delete_renameone(table, name);

	/* nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		delete_renameone(table, left);
	}

	/* index */
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);
}

static void intern_renameone(addr pos, addr table, addr name, int nickname)
{
	addr cons, check;

	string_designer(name, &name);
	intern_hashheap(table, name, &cons);
	GetCdr(cons, &check);
	if (check == Nil) {
		SetCdr(cons, pos);

		/* nickname */
		if (nickname == 0) {
			SetPackage(pos, PACKAGE_INDEX_NAME, name);
		}
		else {
			pushlist(pos, PACKAGE_INDEX_NICKNAME, name);
		}
	}
}

static void intern_allnames(addr pos, addr name, addr right)
{
	addr table, left;

	PackageTable(&table);
	intern_renameone(pos, table, name, 0);
	while (right != Nil) {
		GetCons(right, &left, &right);
		intern_renameone(pos, table, left, 1);
	}
}

void rename_package(addr pos, addr name, addr right, addr *ret)
{
	package_designer(pos, &pos);
	/* check conflict */
	check_rename(pos, name, right);
	/* delete name and nicknames */
	delete_allnames(pos);
	/* intern name and nicknames */
	intern_allnames(pos, name, right);
	/* result */
	*ret = pos;
}


/*
 *  package function
 */
void getname_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_NAME, ret);
}

void getnickname_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, ret);
}

void getuselist_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_USE, ret);
}

void getusedbylist_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_USED, ret);
}

void getshadow_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, ret);
}


/*
 *  find-symbol
 */
static void find_bitpackage(addr package, addr name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	findvalue_hashtable(package, name, ret);
}

static void find_char_bitpackage(addr package, const char *name, addr *ret)
{
	GetPackage(package, PACKAGE_INDEX_TABLE, &package);
	findvalue_char_hashtable(package, name, ret);
}

enum PACKAGE_TYPE find_symbol_package(addr package, addr name, addr *ret)
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
static void push_basesymbol(addr key, addr left, addr name, addr *cons)
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

void find_allsymbols_package(addr name, addr *ret)
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
			push_basesymbol(key, left, name, &cons);
		}
	}
	*ret = cons;
}


/*
 *  list_all_packages
 */
static void push_basepackage(addr key, addr package, addr *cons)
{
	addr check;

	GetPackage(package, PACKAGE_INDEX_NAME, &check);
	if (string_equal(key, check))
		cons_heap(cons, package, *cons);
}

void list_all_packages(addr *ret)
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
			push_basepackage(key, left, &cons);
		}
	}
	*ret = cons;
}


/*
 *  intern
 */
static int intern_bitpackage(addr package, addr name, addr *ret)
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

static enum PACKAGE_TYPE intern_package_table(addr package, addr name, addr *ret)
{
	if (intern_bitpackage(package, name, &name)) {
		GetBitTypeSymbol(name, ret);
		return PACKAGE_TYPE_NIL;
	}
	GetBitTypeSymbol(name, ret);

	return StructBitType(name)->intern;
}

enum PACKAGE_TYPE intern_package(addr package, addr name, addr *ret)
{
	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(! stringp(name), "type error");

	package_designer(package, &package);
	return intern_package_table(package, name, ret);
}

enum PACKAGE_TYPE intern_char_package(addr package, const char *name, addr *ret)
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
			if (check) return 1;
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
static int uninterncheck(addr package, addr symbol)
{
	addr bit, name, pos;

	/* If the package have no symbols, return 1. */
	GetPackage(package, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(pos, name, &bit);
	if (bit == Nil) return 1;

	/* If package don't have the symbol, return 1. */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol) return 1;

	/* If symbols in shadowing-symbols, check conflict in use-package. */
	if (StructBitType(bit)->shadow) {
		if (check_shadowing_unintern(package, name)) {
			fmte("Shadowing symbol ~S occer conflict.", symbol, NULL);
			return 1;
		}
	}

	return 0;
}

static int remove_package_unintern(addr package, addr name)
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

static void intern_inherited_unintern(addr package, addr name)
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

static void remove_shadowing_symbols(addr package, addr symbol)
{
	if (remove_eqpackage(package, PACKAGE_INDEX_SHADOW, symbol))
		Abort("remove_shadowing_symbols error.");
}

/*
 *  remove package table.
 *  if symbol in shadowing-symbols,
 *    intern inherited-symbol if symbol in shadowing-symbols.
 *    remove shadowing-symbols.
 */
static void uninternsymbol(addr package, addr symbol)
{
	addr name;

	/* remove package table. */
	GetNameSymbol(symbol, &name);
	if (remove_package_unintern(package, name)) {
		/* intern inherited-symbol if symbol in shadowing-symbols. */
		intern_inherited_unintern(package, name);
		/* remove shadowing-symbols. */
		remove_shadowing_symbols(package, symbol);
	}
}

int unintern_package(addr package, addr symbol)
{
	Check(! IsSymbol(symbol), "type error");
	package_designer(package, &package);
	if (uninterncheck(package, symbol)) return 1;
	uninternsymbol(package, symbol);

	return 0;
}


/*
 *  import
 */
static int import_already_exist(addr symbol, addr bit, addr *ret)
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
		return import_already_exist(symbol, bit, ret);

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

static void importsymbol(addr package, addr pos)
{
	Check(! IsSymbol(pos), "type error");
	if (import_bitpackage(package, pos, &package))
		fmte("Import symbol ~S occer conflict.", pos, NULL);
}

static void importlist(addr package, addr pos)
{
	addr left, right, table, check;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! IsSymbol(left))
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
		importsymbol(package, left);
	}
}

void import_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			importsymbol(package, pos);
			break;

		case LISPTYPE_CONS:
			importlist(package, pos);
			break;

		default:
			fmte("import ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  shadow
 */
static void shadowsymbol(addr package, addr pos)
{
	addr bit;
	struct bittype_struct *ptr;

	string_designer(pos, &pos);
	intern_bitpackage(package, pos, &bit);
	ptr = StructBitType(bit);
	if (ptr->inherit) {
		/* change type to intern from inherit. */
		shadowintern_bitpackage(bit, pos, package);
	}
	if (! ptr->shadow) {
		GetBitTypeSymbol(bit, &pos);
		pushlist(package, PACKAGE_INDEX_SHADOW, pos);
		SetBitTypeShadow(bit, 1);
	}
}

static void shadowlist(addr package, addr pos)
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
		shadowsymbol(package, left);
	}
}

void shadow_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			shadowsymbol(package, pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			shadowsymbol(package, pos);
			break;

		case LISPTYPE_CONS:
			shadowlist(package, pos);
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
static void shadowimportsymbol(addr package, addr symbol)
{
	addr bit, check;
	struct bittype_struct *ptr;

	if (import_bitpackage(package, symbol, &bit)) {
		ptr = StructBitType(bit);
		/* conflict, change type to intern from import. */
		if (ptr->shadow) {
			GetBitTypeSymbol(bit, &check);
			remove_shadowing_symbols(package, check);
			ptr->shadow = 0;
		}
		shadowimport_bitpackage(bit, symbol);
	}

	if (StructBitType(bit)->shadow == 0) {
		GetBitTypeSymbol(bit, &symbol);
		pushlist(package, PACKAGE_INDEX_SHADOW, symbol);
		SetBitTypeShadow(bit, 1);
	}
}

static void shadowimportlist(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! IsSymbol(left))
			fmte("shadowing-symbol ~S must be a symbol.", left, NULL);
	}

	/* shadowing-import */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		shadowimportsymbol(package, left);
	}
}

void shadowing_import_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			shadowimportsymbol(package, pos);
			break;

		case LISPTYPE_CONS:
			shadowimportlist(package, pos);
			break;

		default:
			fmte("shadowing-import ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  export
 */
static void check_exportsymbol(addr package, addr symbol)
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
	if (StructBitType(right)->expt) return;

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

static void intern_export_symbol(addr package, addr symbol, addr name)
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

static void exportsymbol_nocheck(addr package, addr symbol)
{
	addr table, bit, name;
	struct bittype_struct *ptr;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(table, name, &bit);
	ptr = StructBitType(bit);
	if (ptr->expt) {
		/* symbol is already exported. */
		return;
	}
	if (ptr->inherit) {
		/* If the symbol type is inherited, the type change to import.  */
		ptr->inherit = 0;
		ptr->import = 1;
		ptr->expt = 1;
		ptr->intern = PACKAGE_TYPE_EXTERNAL;
	}
	else {
		/* export symbol */
		ptr->expt = 1;
		ptr->intern = PACKAGE_TYPE_EXTERNAL;
	}
	intern_export_symbol(package, symbol, name);
	pushlist(package, PACKAGE_INDEX_EXPORT, name);
}

static void exportsymbol(addr package, addr symbol)
{
	if (! IsSymbol(symbol))
		fmte("export ~S must be a symbol.", symbol, NULL);
	check_exportsymbol(package, symbol);
	exportsymbol_nocheck(package, symbol);
}

static void exportlist(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! IsSymbol(left))
			fmte("export ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		check_exportsymbol(package, left);
	}

	/* export */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		exportsymbol_nocheck(package, left);
	}
}

void export_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			exportsymbol(package, pos);
			break;

		case LISPTYPE_CONS:
			exportlist(package, pos);
			break;

		default:
			fmte("export ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  unexport
 */
static int delete_stringlist(addr root, addr check, addr *ret)
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

static void remove_export_list(addr package, addr name)
{
	addr right;

	GetPackage(package, PACKAGE_INDEX_EXPORT, &right);
	if (delete_stringlist(right, name, &right))
		fmte("There is no ~S in export list.", name, NULL);
	SetPackage(package, PACKAGE_INDEX_EXPORT, right);
}

static void check_unexportsymbol(addr package, addr symbol)
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

static void unexport_usedbylist(addr package, addr symbol)
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

static void unexport_symboltype(addr package, addr symbol)
{
	addr table, name, bit;
	struct bittype_struct *ptr;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);
	findvalue_hashtable(table, name, &bit);
	ptr = StructBitType(bit);
	if (ptr->expt) {
		ptr->intern = PACKAGE_TYPE_INTERNAL;
		ptr->expt = 0;
		remove_export_list(package, name);
	}
}

static void unexportsymbol(addr package, addr symbol)
{
	check_unexportsymbol(package, symbol);
	unexport_usedbylist(package, symbol);
	unexport_symboltype(package, symbol);
}

static void unexportlist(addr package, addr pos)
{
	addr left, right;

	/* type check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		if (! IsSymbol(left))
			fmte("unexport ~S must be a string-desinger.", left, NULL);
	}

	/* conflict check */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		check_unexportsymbol(package, left);
	}

	/* unexport */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		unexportsymbol(package, left);
	}
}

void unexport_package(addr package, addr pos)
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
			unexportsymbol(package, pos);
			break;

		case LISPTYPE_CONS:
			unexportlist(package, pos);
			break;

		default:
			fmte("unexport ~S must be a symbol or list.", pos, NULL);
			break;
	}
}


/*
 *  use_package
 */
static int check_alreadyuse(addr package, addr pos)
{
	addr left;

	if (package == pos) return 1;
	GetPackage(package, PACKAGE_INDEX_USE, &package);
	while (package != Nil) {
		GetCons(package, &left, &package);
		if (left == pos) return 1;
	}

	return 0;
}

static void check_useconflict(addr package, addr pos)
{
	addr list, table, left, bit;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &left, &list);
		findvalue_hashtable(table, left, &bit);
		if (bit != Nil && ! StructBitType(bit)->shadow) {
			GetBitTypeSymbol(bit, &bit);
			find_symbol_package(pos, left, &pos);
			if (bit != pos)
				fmte("Symbol ~S conflict occered.", left, NULL);
		}
	}
}

static void check_usepackage(addr package, addr pos)
{
	if (check_alreadyuse(package, pos)) return;
	check_useconflict(package, pos);
}

static void usepackageoperator(addr package, addr pos)
{
	addr table, loop, left, right, symbol, cons, bit;

	package_designer(pos, &pos);
	if (check_alreadyuse(package, pos)) return;
	check_useconflict(package, pos);

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
	pushlist(package, PACKAGE_INDEX_USE, pos);
	pushlist(pos, PACKAGE_INDEX_USED, package);
}

static void check_usepackagelist(addr package, addr right)
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
			if (check == left) continue;
			if (check == package) continue;

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

static void usepackagelist(addr package, addr pos)
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
		check_usepackage(package, left);
	}
	check_usepackagelist(package, pos);

	/* use-package */
	for (right = pos; right != Nil; ) {
		GetCons(right, &left, &right);
		usepackageoperator(package, left);
	}
}

void use_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			usepackageoperator(package, pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			usepackageoperator(package, pos);
			break;

		case LISPTYPE_CONS:
			usepackagelist(package, pos);
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
static int check_uselist(addr package, addr pos)
{
	addr left, right;

	GetPackage(package, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == pos) return 1;
	}

	return 0;
}

static void unusepackageoperator(addr package, addr pos)
{
	addr table, right, left, bit;

	package_designer(pos, &pos);
	if (check_uselist(package, pos) == 0) return;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		findvalue_hashtable(table, left, &bit);
		Check(bit == Nil, "unuse-package error");
		if (StructBitType(bit)->inherit)
			delete_hashtable(table, left);
	}

	if (remove_eqpackage(package, PACKAGE_INDEX_USE, pos))
		Abort("PACKAGE_INDEX_USE error");
	if (remove_eqpackage(pos, PACKAGE_INDEX_USED, package))
		Abort("PACKAGE_INDEX_USED error");
}

static void unusepackagelist(addr package, addr pos)
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
		unusepackageoperator(package, left);
	}
}

void unuse_package(addr package, addr pos)
{
	package_designer(package, &package);
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
			unusepackageoperator(package, pos);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(pos)) goto error;
			unusepackageoperator(package, pos);
			break;

		case LISPTYPE_CONS:
			unusepackagelist(package, pos);
			break;

		default:
			goto error;
	}
	return;

error:
	fmte("unuse-package ~S must be a package-designer or list.", pos, NULL);
}


/*
 *  in-package
 */
void in_package(Execute ptr, addr package, addr *ret)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	package_designer(package, &package);
	setspecial_local(ptr, symbol, package);
	if (ret) *ret = package;
}


/*
 *  for C language
 */
enum PACKAGE_TYPE intern_default_package(Execute ptr, addr name, addr *ret)
{
	addr package;

	getpackage(ptr, &package);
	return intern_package(package, name, ret);
}

enum PACKAGE_TYPE internchar(const char *pname, const char *sname, addr *ret)
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

addr interncharr(const char *pname, const char *sname)
{
	addr pos;
	internchar(pname, sname, &pos);
	return pos;
}

enum PACKAGE_TYPE internchar_default(Execute ptr, const char *name, addr *ret)
{
	addr package;
	getpackage(ptr, &package);
	return intern_char_package(package, name, ret);
}

enum PACKAGE_TYPE internchar_check(Execute ptr,
		const char *pname, const char *sname, addr *ret)
{
	if (pname)
		return internchar(pname, sname, ret);
	else
		return internchar_default(ptr, sname, ret);
}

addr internchar_checkr(Execute ptr, const char *pname, const char *sname)
{
	addr pos;
	internchar_check(ptr, pname, sname, &pos);
	return pos;
}

void setkeyword_package(addr pos)
{
	addr check;

	GetValueSymbol(pos, &check);
	if (check == Unbound) {
		SetValueSymbol(pos, pos);
		SetStatusReadOnly(pos);
	}
}

enum PACKAGE_TYPE internchar_keyword(const char *name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	GetConst(PACKAGE_KEYWORD, &pos);
	type = intern_char_package(pos, name, &pos);
	setkeyword_package(pos);
	*ret = pos;

	return type;
}

enum PACKAGE_TYPE interncommon(const char *name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr package, pos;

	GetConst(PACKAGE_COMMON_LISP, &package);
	type = intern_char_package(package, name, &pos);
	export_package(package, pos);
	*ret = pos;

	return type;
}

addr interncommonr(const char *name)
{
	addr ret;
	(void)interncommon(name, &ret);
	return ret;
}

int checksymbol_package(addr symbol, addr package)
{
	enum PACKAGE_TYPE type;
	addr check, name;

	GetNameSymbol(symbol, &name);
	type = find_symbol_package(package, name, &check);

	return type != PACKAGE_TYPE_NIL && check == symbol;
}

int checksymbol_default_package(Execute ptr, addr symbol)
{
	addr package;
	getpackage(ptr, &package);
	return checksymbol_package(symbol, package);
}

void make_gentemp(Execute ptr, addr prefix, addr package, addr *ret)
{
	enum PACKAGE_TYPE type;
	int keyword;
	addr value, queue, name, gentemp;
	LocalRoot local;
	LocalStack stack;

	/* package check */
	if (package == NULL)
		getpackage(ptr, &package);
	else
		package_designer(package, &package);
	GetConst(PACKAGE_KEYWORD, &value);
	keyword = (value == package);

	/* symbol-name */
	GetConst(PACKAGE_GENTEMP, &value);
	Check(! integerp(value), "type error");

	local = ptr->local;
	for (;;) {
		/* make symbol-name */
		push_local(local, &stack);
		charqueue_local(local, &queue, 1 + 16);
		if (prefix == NULL)
			pushchar_charqueue_local(local, queue, "T");
		else
			pushstring_charqueue_local(local, queue, prefix);
		decimal_charqueue_integer_local(local, value, queue);
		make_charqueue_local(local, queue, &name);
		type = find_symbol_package(package, name, &gentemp);
		if (type == PACKAGE_TYPE_NIL)
			make_charqueue_heap(queue, &name);
		rollback_local(local, stack);

		/* (1+ *gentemp-counter*) */
		oneplus_integer_common(local, value, &value);
		SetConst(PACKAGE_GENTEMP, value);

		/* check intern */
		if (type == PACKAGE_TYPE_NIL) break;
	}

	/* gentemp */
	intern_package(package, name, &gentemp);
	if (keyword)
		export_package(package, gentemp);
	*ret = gentemp;
}

/*
 *  iterator
 */
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

struct StructPackageIterator {
	unsigned internal : 1;
	unsigned external : 1;
	unsigned inherited : 1;
	unsigned finish : 1;
};

enum PackageIterator {
	PackageIterator_List,
	PackageIterator_Table,
	PackageIterator_Package,
	PackageIterator_Size
};

#define PtrPackageIterator(x) PtrBodySSa(x, PackageIterator_Size)
#define PtrStructPackageIterator(x) \
	((struct StructPackageIterator *)PtrPackageIterator(x))
#define GetIndexPackageIterator(x) (PtrStructPackagetable(x)->index)
#define RetPackageIterator RetArraySS
#define GetPackageIterator GetArraySS
#define SetPackageIterator SetArraySS

void package_iterator_alloc(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	addr pos, package, table;
	struct StructPackageIterator *ptr;

	/* object */
	alloc_smallsize(local, &pos,
			LISPSYSTEM_PACKAGEITERATOR,
			PackageIterator_Size,
			sizeoft(struct StructPackageIterator));
	ptr = PtrStructPackageIterator(pos);
	clearpoint(ptr);
	ptr->internal = (internal != 0);
	ptr->external = (external != 0);
	ptr->inherited = (inherited != 0);
	ptr->finish = 0;

	/* no-package */
	if (list == Nil) {
		ptr->finish = 1;
		*ret = pos;
		return;
	}

	/* package or list */
	if (listp(list)) {
		getcons(list, &package, &list);
	}
	else {
		package = list;
		list = Nil;
	}

	/* package -> hash-iterator */
	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	hash_iterator_alloc(local, &table, table);

	/* result */
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Table, table);
	SetPackageIterator(pos, PackageIterator_Package, package);
	*ret = pos;
}

void package_iterator_local(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	Check(local == NULL, "local error");
	package_iterator_alloc(local, ret, list, internal, external, inherited);
}

void package_iterator_heap(addr *ret,
		addr list, int internal, int external, int inherited)
{
	package_iterator_alloc(NULL, ret, list, internal, external, inherited);
}

static enum PACKAGE_TYPE hash_package_iterator(addr pos, addr *rets, addr *retp)
{
	enum PACKAGE_TYPE type;
	addr table, key, value;
	struct StructPackageIterator *ptr;
	struct bittype_struct *bit;

	ptr = PtrStructPackageIterator(pos);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	while (next_hash_iterator(table, &key, &value)) {
		bit = StructBitType(value);
		type = bit->intern;
		if ((ptr->internal && type == PACKAGE_TYPE_INTERNAL) ||
				(ptr->external && type == PACKAGE_TYPE_EXTERNAL) ||
				(ptr->inherited && type == PACKAGE_TYPE_INHERITED)) {
			GetBitTypeSymbol(value, rets);
			GetPackageIterator(pos, PackageIterator_Package, retp);
			return type;
		}
	}

	return PACKAGE_TYPE_NIL;
}

static void forward_package_iterator(addr pos)
{
	addr list, raw, table, package;

	GetPackageIterator(pos, PackageIterator_List, &list);
	if (list == Nil) {
		PtrStructPackageIterator(pos)->finish = 1;
		return;
	}
	getcons(list, &package, &list);
	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &raw);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	set_hash_iterator(table, raw);
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Package, package);
}

enum PACKAGE_TYPE next_package_iterator(addr pos, addr *rets, addr *retp)
{
	enum PACKAGE_TYPE type;
	struct StructPackageIterator *ptr;

	CheckType(pos, LISPSYSTEM_PACKAGEITERATOR);
	ptr = PtrStructPackageIterator(pos);
	while (! ptr->finish) {
		type = hash_package_iterator(pos, rets, retp);
		if (type != PACKAGE_TYPE_NIL) return type;
		forward_package_iterator(pos);
	}

	return PACKAGE_TYPE_NIL;
}


/*
 *  syscall
 */
static void defpackage_findcons(addr table, addr key, addr *ret)
{
	if (stringp(key)) {
		findcons_hashtable(table, key, ret);
		return;
	}
	if (symbolp(key)) {
		GetNameSymbol(key, &key);
		findcons_hashtable(table, key, ret);
		return;
	}
	if (characterp(key)) {
		findcons_unicode_hashtable(table, RefCharacter(key), ret);
		return;
	}
	*ret = Nil;
}

static void defpackage_make_nicknames(LocalRoot local, addr *ret, addr list)
{
	addr root, child, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &pos, &child);
			cons_local(local, &root, pos, root);
		}
	}
	nreverse_list_unsafe(ret, root);
}

static void defpackage_check_nicknames(addr pos, addr names)
{
	addr table, list, name, check;

	PackageTable(&table);
	for (list = names; list != Nil; ) {
		GetCons(list, &name, &list);
		defpackage_findcons(table, name, &check);
		if (check != Nil) {
			GetCdr(check, &check);
			if (pos != check)
				fmte("nickname ~A is already exists.", name, NULL);
		}
	}
}

static void defpackage_update_nicknames(addr pos, addr names)
{
	addr table, list, name;

	PackageTable(&table);

	/* delete nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		delete_renameone(table, name);
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);

	/* append nicknames */
	append_nicknames(pos, names);
}

static void defpackage_update_shadow(LocalRoot local, addr pos, addr list)
{
	defpackage_make_nicknames(local, &list, list);
	shadowlist(pos, list);
}

static void defpackage_update_shadowing(addr pos, addr list)
{
	addr child, package, key;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		package_designer(package, &package);
		while (child != Nil) {
			GetCons(child, &key, &child);
			string_designer_heap(&key, key);
			intern_package(package, key, &key);
			shadowimportsymbol(pos, key);
		}
	}
}

static void defpackage_update_use(LocalRoot local, addr pos, addr list)
{
	defpackage_make_nicknames(local, &list, list);
	usepackagelist(pos, list);
}

static void defpackage_update_import(LocalRoot local, addr pos, addr list)
{
	addr child, package, args, symbol;
	LocalStack stack;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		package_designer(package, &package);
		push_local(local, &stack);
		for (args = Nil; child != Nil; ) {
			GetCons(child, &symbol, &child);
			string_designer_heap(&symbol, symbol);
			intern_package(package, symbol, &symbol);
			cons_local(local, &args, symbol, args);
		}
		nreverse_list_unsafe(&args, args);
		import_package(pos, args);
		rollback_local(local, stack);
	}
}

static void defpackage_update_intern(addr pos, addr list)
{
	addr child, name;

	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &name, &child);
			string_designer_heap(&name, name);
			intern_package_table(pos, name, &name);
		}
	}
}

static void defpackage_update_export(LocalRoot local, addr pos, addr list)
{
	addr root, child, name;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &name, &child);
			string_designer_heap(&name, name);
			intern_package_table(pos, name, &name);
			cons_local(local, &root, name, root);
		}
	}
	nreverse_list_unsafe(&root, root);
	exportlist(pos, root);
}

static void defpackage_update(Execute ptr, addr pos, addr rest)
{
	addr nicknames, use, shadow, shadowing, import, expt, intern;
	LocalRoot local;

	local = ptr->local;
	List_bind(rest, &nicknames, &use, &shadow, &shadowing,
			&import, &expt, &intern, NULL);
	/* nicknames */
	defpackage_make_nicknames(local, &nicknames, nicknames);
	defpackage_check_nicknames(pos, nicknames);
	defpackage_update_nicknames(pos, nicknames);
	/* shadow, shadowing-symbols */
	defpackage_update_shadow(local, pos, shadow);
	defpackage_update_shadowing(pos, shadowing);
	/* use */
	defpackage_update_use(local, pos, use);
	/* import-from, intern */
	defpackage_update_import(local, pos, import);
	defpackage_update_intern(pos, intern);
	/* export */
	defpackage_update_export(local, pos, expt);
}

static void defpackage_make_delete(Execute ptr, addr condition)
{
	addr pos;

	/* delete */
	getdata_control(ptr, &pos);
	delete_package(pos);
	/* throw */
	error_function(condition);
}

static int defpackage_make(Execute ptr, addr pos, addr rest)
{
	addr control, symbol, call;

	/* push */
	push_close_control(ptr, &control);
	/* handler-case */
	GetConst(COMMON_ERROR, &symbol);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, defpackage_make_delete);
	SetDataFunction(call, pos);
	pushhandler_control(ptr, symbol, call, 0);
	/* code */
	defpackage_update(ptr, pos, rest);
	/* free */
	return free_control(ptr, control);
}

static void resize_pacakge(addr pos, size_t size)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	force_resize_hashtable(pos, size);
}

static int defpackage_execute(Execute ptr, addr rest)
{
	int sizep;
	addr name, pos, size, doc;
	size_t value;

	GetCons(rest, &name, &rest);
	GetCons(rest, &size, &rest);
	sizep = (size != Nil);
	if (sizep) {
		if (getindex_integer(size, &value))
			fmte(":size ~S is too large.", size, NULL);
	}
	GetCons(rest, &doc, &rest);
	find_package(name, &pos);
	if (pos == Nil) {
		if (sizep)
			package_size_heap(&pos, name, value);
		else
			package_heap(&pos, name);
		if (defpackage_make(ptr, pos, rest)) return 1;
	}
	else {
		if (sizep)
			resize_pacakge(pos, value);
		defpackage_update(ptr, pos, rest);
	}
	setresult_control(ptr, pos);

	return 0;
}

void syscall_defpackage(Execute ptr, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	if (defpackage_execute(ptr, rest)) return;
	rollback_local(local, stack);
}

static int syscall_do_symbols_check(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			GetBitTypeSymbol(bit, &bit);
			if (callclang_funcall(ptr, &bit, call, bit, NULL))
				return 1;
		}
	}
	setvalues_nil_control(ptr);

	return 0;
}

void syscall_do_symbols(Execute ptr, addr call, addr package)
{
	(void)syscall_do_symbols_check(ptr, call, package);
}

void syscall_do_external_symbols(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			if (StructBitType(bit)->intern == PACKAGE_TYPE_EXTERNAL) {
				GetBitTypeSymbol(bit, &bit);
				if (callclang_funcall(ptr, &bit, call, bit, NULL))
					return;
			}
		}
	}
	setvalues_nil_control(ptr);
}

void syscall_do_all_symbols(Execute ptr, addr call)
{
	addr array, left, right, key, check;
	size_t i, size;

	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			GetPackage(left, PACKAGE_INDEX_NAME, &check);
			if (string_equal(key, check)) {
				if (syscall_do_symbols_check(ptr, call, left))
					return;
			}
		}
	}
	setvalues_nil_control(ptr);
}

