#include "package.c"

#if 0
#include <stdarg.h>
#include "clos.h"
#include "common.h"
#include "degrade.h"
#include "print.h"
#include "reader.h"
#include "stream.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_packagetable(void)
{
	addr pos;

	PackageTable(&pos);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "packagetable1");

	RETURN;
}

static int test_alloc_bitpackage(void)
{
	enum PACKAGE_TYPE type;
	unsigned v;
	addr pos, check;
	struct bittype_struct *ptr;

	alloc_bitpackage(&pos, T, PACKAGE_TYPE_INHERITED);
	test(GetType(pos) == LISPSYSTEM_BITTYPE, "alloc_bitpackage1");
	GetBitTypeSymbol(pos, &check);
	test(check == T, "alloc_bitpackage2");

	ptr = StructBitType(pos);
	type = PACKAGE_TYPE_NIL;
	GetBitTypeIntern(pos, &type);
	test(type == PACKAGE_TYPE_INHERITED, "alloc_bitpackage3");
	SetBitTypeIntern(pos, PACKAGE_TYPE_EXTERNAL);
	test(ptr->intern == PACKAGE_TYPE_EXTERNAL, "alloc_bitpackage4");

	test(ptr->base == 0, "alloc_bitpackage5");
	SetBitTypeBase(pos, 1);
	GetBitTypeBase(pos, &v);
	test(v, "alloc_bitpackage6");
	test(ptr->base, "alloc_bitpackage7");
	SetBitTypeBase(pos, 0);

	test(ptr->expt == 0, "alloc_bitpackage8");
	SetBitTypeExport(pos, 1);
	GetBitTypeExport(pos, &v);
	test(v, "alloc_bitpackage9");
	test(ptr->expt, "alloc_bitpackage10");
	SetBitTypeExport(pos, 0);

	test(ptr->import == 0, "alloc_bitpackage11");
	SetBitTypeImport(pos, 1);
	GetBitTypeImport(pos, &v);
	test(v, "alloc_bitpackage12");
	test(ptr->import, "alloc_bitpackage13");
	SetBitTypeImport(pos, 0);

	test(ptr->inherit == 0, "alloc_bitpackage14");
	SetBitTypeInherit(pos, 1);
	GetBitTypeInherit(pos, &v);
	test(v, "alloc_bitpackage15");
	test(ptr->inherit, "alloc_bitpackage16");
	SetBitTypeInherit(pos, 0);

	test(ptr->shadow == 0, "alloc_bitpackage17");
	SetBitTypeShadow(pos, 1);
	GetBitTypeShadow(pos, &v);
	test(v, "alloc_bitpackage18");
	test(ptr->shadow, "alloc_bitpackage19");
	SetBitTypeShadow(pos, 0);

	RETURN;
}

static void internpackage(addr *ret, const char *name)
{
	addr pos, check;

	strvect_char_heap(&pos, name);
	find_package(pos, &check);
	if (check == Nil) {
		package_heap(&pos, pos);
		if (ret) *ret = pos;
		return;
	}
	if (ret) *ret = check;
}

static int test_make_bitpackage(void)
{
	addr package, pos, check;
	struct bittype_struct *ptr;

	internpackage(&package, "BITPACKAGE");
	strvect_char_heap(&pos, "HELLO");
	make_bitpackage(&pos, pos, package);
	ptr = StructBitType(pos);
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "make_bitpackage1");
	test(ptr->base, "make_bitpackage2");
	GetBitTypeSymbol(pos, &pos);
	GetNameSymbol(pos, &check);
	test(string_equal_char(check, "HELLO"), "make_bitpackage3");
	GetPackageSymbol(pos, &check);
	test(package == check, "make_bitpackage4");

	RETURN;
}

static int test_importbitpackage(void)
{
	addr pos;

	importbitpackage(&pos, T);
	test(StructBitType(pos)->intern == PACKAGE_TYPE_INTERNAL, "importbitpackage1");
	test(StructBitType(pos)->import, "importbitpackage2");
	GetBitTypeSymbol(pos, &pos);
	test(pos == T, "importbitpackage3");

	RETURN;
}

static int test_inheritbitpackage(void)
{
	addr pos;

	inheritedbitpackage(&pos, T);
	test(StructBitType(pos)->intern == PACKAGE_TYPE_INHERITED, "inheritbitpackage1");
	test(StructBitType(pos)->inherit, "inheritbitpackage2");
	GetBitTypeSymbol(pos, &pos);
	test(pos == T, "inheritbitpackage3");

	RETURN;
}

static int test_shadowintern_bitpackage(void)
{
	addr pos, package, name;
	struct bittype_struct *ptr;

	alloc_bitpackage(&pos, T, PACKAGE_TYPE_INHERITED);
	ptr = StructBitType(pos);
	ptr->base = 0;
	ptr->inherit = 1;

	strvect_char_heap(&name, "QQQQ");
	internpackage(&package, "BITPACKAGE");
	shadowintern_bitpackage(pos, name, package);
	test(ptr->base, "shadowintern_bitpackage1");
	test(ptr->inherit == 0, "shadowintern_bitpackage2");
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "shadowintern_bitpackage3");
	GetBitTypeSymbol(pos, &pos);
	test(pos != T, "shadowintern_bitpackage4");
	GetNameSymbol(pos, &name);
	test(string_equal_char(name, "QQQQ"), "shadowintern_bitpackage5");
	GetPackageSymbol(pos, &pos);
	test(pos == package, "shadowintern_bitpackage6");

	RETURN;
}

static int test_shadowimport_bitpackage(void)
{
	addr pos;
	struct bittype_struct *ptr;

	alloc_bitpackage(&pos, Nil, PACKAGE_TYPE_INHERITED);
	ptr = StructBitType(pos);
	ptr->base = 1;
	ptr->import = 0;
	ptr->inherit = 1;

	shadowimport_bitpackage(pos, T);
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "shadowimport_bitpackage1");
	test(ptr->base == 0, "shadowimport_bitpackage2");
	test(ptr->import == 1, "shadowimport_bitpackage3");
	test(ptr->inherit == 0, "shadowimport_bitpackage4");
	GetBitTypeSymbol(pos, &pos);
	test(pos == T, "shadowimport_bitpackage5");

	RETURN;
}

static int test_string_designer(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	string_designer(pos, &pos);
	test(string_equal_char(pos, "Hello"), "string_designer1");

	strvect_char_heap(&pos, "AAABBB");
	string_designer(pos, &pos);
	test(string_equal_char(pos, "AAABBB"), "string_designer2");

	internpackage(NULL, "BITPACKAGE");
	internchar("BITPACKAGE", "symbol", &pos);
	string_designer(pos, &pos);
	test(string_equal_char(pos, "symbol"), "string_designer3");

	RETURN;
}

static int test_findstringlocal(void)
{
	addr pos, check;

	internpackage(&pos, "BITPACKAGE");
	strvect_char_heap(&check, "BITPACKAGE");
	check = findstringlocal(check);
	test(check == pos, "findstringlocal1");

	strvect_char_heap(&check, "___NO_SUCH_PACKAGE__");
	check = findstringlocal(check);
	test(check == Nil, "findstringlocal2");

	RETURN;
}

static int test_find_package(void)
{
	addr pos, check;

	internpackage(&pos, "BITPACKAGE");
	find_package(pos, &check);
	test(pos == check, "find_package1");

	internchar(LISP_SYSTEM, "BITPACKAGE", &check);
	find_package(check, &check);
	test(pos == check, "find_package2");

	strvect_char_heap(&check, "BITPACKAGE");
	find_package(check, &check);
	test(pos == check, "find_package3");

	strvect_char_heap(&check, "BITPACKAGE");
	find_package(check, &check);
	test(pos == check, "find_package4");

	strvect_char_heap(&check, "__NO_SUCH_PACKAGE__");
	find_package(check, &check);
	test(check == Nil, "find_package5");

	RETURN;
}

static int test_package_designer(void)
{
	addr pos, check;

	internpackage(&pos, "BITPACKAGE");
	strvect_char_heap(&check, "BITPACKAGE");
	package_designer(check, &check);
	test(pos == check, "package_designer1");

	RETURN;
}

static int test_package_heap(void)
{
	addr name, pos, table, left;
	enum HASHTABLE_TEST test;

	strvect_char_heap(&name, "__TEST_PACKAGE_HEAP");
	package_heap(&pos, name);

	/* table check */
	PackageTable(&table);
	findvalue_hashtable(table, name, &table);
	test(table == pos, "package_heap1");

	/* package object */
	GetPackage(pos, PACKAGE_INDEX_NAME, &left);
	test(left == name, "package_heap2");
	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);
	test(GetType(table) == LISPTYPE_HASHTABLE, "package_heap3");
	gettest_hashtable(table, &test);
	test(test == HASHTABLE_TEST_EQUAL, "package_heap4");

	RETURN;
}

static int test_package_char_heap(void)
{
	addr pos, name, check;

	package_char_heap(&pos, "__TEST_PACKAGE_CHAR_HEAP");
	strvect_char_heap(&name, "__TEST_PACKAGE_CHAR_HEAP");
	find_package(name, &check);
	test(pos == check, "package_char_heap1");

	RETURN;
}

#if 0
static int test_specialsymbol(void)
{
	addr pos, package, check;
	struct bittype_struct *ptr;

	specialsymbol(Execute_Thread, "HELLO", &pos);
	GetPackageSymbol(pos, &package);
	getname_package(package, &check);
	test(string_equal_char(check, "COMMON-LISP"), "specialsymbol1");
	test(specialp_symbol(pos), "specialsymbol2");

	GetPackage(package, PACKAGE_INDEX_TABLE, &pos);
	strvect_char_heap(&check, "HELLO");
	findvalue_hashtable(pos, check, &pos);
	test(pos != Nil, "specialsymbol3");
	ptr = StructBitType(pos);
	test(ptr->intern == PACKAGE_TYPE_EXTERNAL, "specialsymbol4");
	test(ptr->base, "specialsymbol5");
	test(ptr->expt, "specialsymbol6");

	RETURN;
}
#endif

static int test_interndefault(void)
{
	addr pos, check;

	internchar("COMMON-LISP", "NIL", &pos);
	test(GetStatusReadOnly(pos), "interndefault1");
	internchar("COMMON-LISP", "T", &pos);
	test(GetStatusReadOnly(pos), "interndefault2");

	internpackage(&pos, LISPNAME);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);

	strvect_char_heap(&check, "NIL");
	findvalue_hashtable(pos, check, &check);
	test(check != Nil, "interndefault3");
	test(StructBitType(check)->inherit, "interndefault4");

	strvect_char_heap(&check, "T");
	findvalue_hashtable(pos, check, &check);
	test(check != Nil, "interndefault5");
	test(StructBitType(check)->inherit, "interndefault6");

	RETURN;
}

static int test_constantintern(void)
{
	addr left, right;

	internchar(LISP_COMMON, "*PACKAGE*", &left);
	GetConstant(CONSTANT_SPECIAL_PACKAGE, &right);
	test(left == right, "constantintern1");
	test(specialp_symbol(left), "constantintern2");

	internpackage(&left, LISP_COMMON);
	GetConstant(CONSTANT_PACKAGE_COMMON_LISP, &right);
	test(left == right, "constantintern3");

	internpackage(&left, LISP_KEYWORD);
	GetConstant(CONSTANT_PACKAGE_KEYWORD, &right);
	test(left == right, "constantintern4");

	internpackage(&left, LISP_KEYWORD);
	GetConstant(CONSTANT_PACKAGE_KEYWORD, &right);
	test(left == right, "constantintern5");

	internpackage(&left, LISP_CODE);
	GetConstant(CONSTANT_PACKAGE_CODE, &right);
	test(left == right, "constantintern6");

	RETURN;
}

static int test_set_default_package(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	find_char_package_(LISP_COMMON, &pos);
	set_default_package(pos);
	GetConstant(CONSTANT_SPECIAL_PACKAGE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_COMMON), "set_default_package1");

	find_char_package_(LISP_PACKAGE, &pos);
	set_default_package(pos);
	GetConstant(CONSTANT_SPECIAL_PACKAGE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_PACKAGE), "set_default_package2");

	RETURN;
}

static int test_initpackage(void)
{
	addr pos;

	find_char_package_(LISP_KEYWORD, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_KEYWORD), "initpackage1");

	find_char_package_(LISP_COMMON, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_COMMON), "initpackage2");

	find_char_package_(LISP_COMMON_USER, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_COMMON_USER), "initpackage3");

	find_char_package_(LISP_PACKAGE, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_PACKAGE), "initpackage4");

	find_char_package_(LISP_SYSTEM, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_SYSTEM), "initpackage5");

	find_char_package_(LISP_CODE, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_CODE), "initpackage6");

	RETURN;
}

static int test_getpackage(void)
{
	addr pos;

	getpackage(Execute_Thread, &pos);
	getname_package(pos, &pos);
	test(string_equal_char(pos, LISP_PACKAGE), "getpackage1");

	RETURN;
}


/*
 *  make_package
 */
static int test_pushlist_package(void)
{
	addr pos, check, left, right;

	internpackage(&pos, "BITPACKAGE");
	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &check);
	test(check == Nil, "pushlist_package1");
	strvect_char_heap(&check, "AAA");
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, check);

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	test(GetType(right) == LISPTYPE_CONS, "pushlist_package2");
	GetCons(right, &left, &right);
	test(string_equal_char(left, "AAA"), "pushlist_package3");
	test(right == Nil, "pushlist_package4");

	strvect_char_heap(&check, "BBB");
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, check);

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &right);
	test(GetType(right) == LISPTYPE_CONS, "pushlist_package5");
	GetCons(right, &left, &right);
	test(string_equal_char(left, "BBB"), "pushlist_package6");
	GetCons(right, &left, &right);
	test(string_equal_char(left, "AAA"), "pushlist_package7");
	test(right == Nil, "pushlist_package8");

	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);

	RETURN;
}

static int listlength(addr cons)
{
	int i;

	for (i = 0; cons != Nil; i++) {
		GetCdr(cons, &cons);
	}

	return i;
}

static int test_pushnewlist(void)
{
	addr pos, pos1, pos2, pos3, pos4, cons;

	internpackage(&pos, "BITPACKAGE");
	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);
	strvect_char_heap(&pos1, "STR1");
	strvect_char_heap(&pos2, "STR2");
	strvect_char_heap(&pos3, "STR3");
	strvect_char_heap(&pos4, "STR4");
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, pos1);
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, pos2);
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, pos3);
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, pos1);

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 4, "pushnewlist1");
	pushnewlist(pos, PACKAGE_INDEX_EXPORT, pos1);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 4, "pushnewlist2");
	pushnewlist(pos, PACKAGE_INDEX_EXPORT, pos2);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 4, "pushnewlist3");
	pushnewlist(pos, PACKAGE_INDEX_EXPORT, pos3);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 4, "pushnewlist4");
	pushnewlist(pos, PACKAGE_INDEX_EXPORT, pos4);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 5, "pushnewlist5");

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	GetCar(cons, &pos1);
	test(pos1 == pos4, "pushnewlist6");

	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);

	RETURN;
}

static void makelist(addr *pos, ...)
{
	addr root, tail, left, right;
	va_list args;

	root = tail = Nil;
	va_start(args, pos);
	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL) break;
		consnil_heap(&right);
		SetCar(right, left);
		if (root == Nil) {
			root = tail = right;
		}
		else {
			SetCdr(tail, right);
			tail = right;
		}
	}
	va_end(args);

	*pos = root;
}

static void makestringlist(addr *pos, ...)
{
	addr root, tail, left, right;
	const char *ptr;
	va_list args;

	root = tail = Nil;
	va_start(args, pos);
	for (;;) {
		ptr = va_arg(args, const char *);
		if (ptr == NULL) break;
		consnil_heap(&right);
		strvect_char_heap(&left, ptr);
		SetCar(right, left);
		if (root == Nil) {
			root = tail = right;
		}
		else {
			SetCdr(tail, right);
			tail = right;
		}
	}
	va_end(args);

	*pos = root;
}

static int test_check_nicknames(void)
{
	addr pos, cons;

	strvect_char_heap(&pos, "Hello");
	testnormal(check_nicknames(pos, Nil), "check_nicknames1");

	makestringlist(&cons, "testaaa", "testbbb", "testccc", NULL);
	testnormal(check_nicknames(pos, cons), "check_nicknames2");

	internpackage(NULL, "BITPACKAGE");
	strvect_char_heap(&pos, "BITPACKAGE");
	testerror(check_nicknames(pos, Nil), "check_nicknames3");

	internpackage(NULL, "BITPACKAGE");
	strvect_char_heap(&pos, "Hello");
	makestringlist(&cons, "testaaa", "BITPACKAGE", "testccc", NULL);
	testerror(check_nicknames(pos, cons), "check_nicknames4");

	RETURN;
}

static int test_check_listconflict(void)
{
	addr pos1, pos2, cons1, cons2;

	internpackage(&pos1, "BITPACKAGE");
	internpackage(&pos2, "BITPACKAGE2");
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, Nil);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, Nil);
	testnormal(check_listconflict(pos1, pos2), "check_listconflict1");

	makestringlist(&cons1, "AAA", "BBB", "CCC", NULL);
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, cons1);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, Nil);
	testnormal(check_listconflict(pos1, pos2), "check_listconflict2");
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, Nil);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, cons1);
	testnormal(check_listconflict(pos1, pos2), "check_listconflict3");

	makestringlist(&cons2, "DDD", "EE", NULL);
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, cons1);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, cons2);
	testnormal(check_listconflict(pos1, pos2), "check_listconflict4");
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, cons2);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, cons1);
	testnormal(check_listconflict(pos1, pos2), "check_listconflict5");

	makestringlist(&cons2, "DDD", "BBB", NULL);
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, cons2);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, cons1);
	testerror(check_listconflict(pos1, pos2), "check_listconflict6");
	SetPackage(pos1, PACKAGE_INDEX_EXPORT, cons1);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, cons2);
	testerror(check_listconflict(pos1, pos2), "check_listconflict7");

	SetPackage(pos1, PACKAGE_INDEX_EXPORT, Nil);
	SetPackage(pos2, PACKAGE_INDEX_EXPORT, Nil);

	RETURN;
}

static int test_check_first_usepackage(void)
{
	addr pack1, pack2, pack3, pack;
	addr cons1, cons2, cons3;

	internpackage(&pack1, "BITPACKAGE1");
	internpackage(&pack2, "BITPACKAGE2");
	internpackage(&pack3, "BITPACKAGE3");
	makelist(&pack, pack1, pack2, pack3, NULL);
	testnormal(check_first_usepackage(pack), "check_first_usepackage1");

	makestringlist(&cons1, "AAA", "BBB", "CCC", "DDD", NULL);
	makestringlist(&cons2, "EEE", "FF", NULL);
	makestringlist(&cons3, "GG", "HHH", "III", NULL);
	SetPackage(pack1, PACKAGE_INDEX_EXPORT, cons1);
	SetPackage(pack2, PACKAGE_INDEX_EXPORT, cons2);
	SetPackage(pack3, PACKAGE_INDEX_EXPORT, cons3);
	testnormal(check_first_usepackage(pack), "check_first_usepackage2");

	makestringlist(&cons2, "EEE", "BBB", NULL);
	SetPackage(pack2, PACKAGE_INDEX_EXPORT, cons2);
	testerror(check_first_usepackage(pack), "check_first_usepackage3");

	makestringlist(&cons1, "AAA", "III", "CCC", "DDD", NULL);
	makestringlist(&cons2, "EEE", "FF", NULL);
	SetPackage(pack1, PACKAGE_INDEX_EXPORT, cons1);
	SetPackage(pack2, PACKAGE_INDEX_EXPORT, cons2);
	testerror(check_first_usepackage(pack), "check_first_usepackage4");

	SetPackage(pack1, PACKAGE_INDEX_EXPORT, Nil);
	SetPackage(pack2, PACKAGE_INDEX_EXPORT, Nil);
	SetPackage(pack3, PACKAGE_INDEX_EXPORT, Nil);

	RETURN;
}

static void force_delete_package(const char *str)
{
	addr table, name;

	PackageTable(&table);
	strvect_char_heap(&name, str);
	delete_hashtable(table, name);
}

static addr findcharlocal(const char *str)
{
	addr pos;

	strvect_char_heap(&pos, str);
	return findstringlocal(pos);
}

static int test_append_nicknames(void)
{
	addr pos, package;

	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	force_delete_package("BITPACKAGE3");
	test(findcharlocal("BITPACKAGE1") == Nil, "append_nicknames1");
	test(findcharlocal("BITPACKAGE2") == Nil, "append_nicknames2");
	test(findcharlocal("BITPACKAGE3") == Nil, "append_nicknames3");

	internpackage(&package, "BITPACKAGE");
	makestringlist(&pos, "BITPACKAGE1", "BITPACKAGE2", "BITPACKAGE3", NULL);
	append_nicknames(package, pos);
	test(findcharlocal("BITPACKAGE1") == package, "append_nicknames4");
	test(findcharlocal("BITPACKAGE2") == package, "append_nicknames5");
	test(findcharlocal("BITPACKAGE3") == package, "append_nicknames6");

	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	force_delete_package("BITPACKAGE3");
	test(findcharlocal("BITPACKAGE1") == Nil, "append_nicknames7");
	test(findcharlocal("BITPACKAGE2") == Nil, "append_nicknames8");
	test(findcharlocal("BITPACKAGE3") == Nil, "append_nicknames9");

	internpackage(&package, "BITPACKAGE");
	makestringlist(&pos,
			"BITPACKAGE1", "BITPACKAGE2", "BITPACKAGE2",
			"BITPACKAGE3", "BITPACKAGE1", "BITPACKAGE1", NULL);
	append_nicknames(package, pos);
	test(findcharlocal("BITPACKAGE1") == package, "append_nicknames10");
	test(findcharlocal("BITPACKAGE2") == package, "append_nicknames11");
	test(findcharlocal("BITPACKAGE3") == package, "append_nicknames12");

	RETURN;
}

static void exportchar(const char *package, const char *name)
{
	addr pos, key, bit;
	struct bittype_struct *ptr;

	internchar(package, name, &bit);
	internpackage(&pos, package);
	strvect_char_heap(&key, name);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &bit);
	findvalue_hashtable(bit, key, &bit);
	ptr = StructBitType(bit);
	ptr->intern = PACKAGE_TYPE_EXTERNAL;
	ptr->expt = 1;
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, key);
}

static int test_append_exportname(void)
{
	addr pos, left, name, symbol, one;
	struct bittype_struct *ptr;

	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	internpackage(&pos, "BITPACKAGE1");
	internpackage(&left, "BITPACKAGE2");
	exportchar("BITPACKAGE2", "HELLO");
	internchar("BITPACKAGE2", "HELLO", &symbol);

	strvect_char_heap(&name, "HELLO");
	GetPackage(left, PACKAGE_INDEX_TABLE, &left);
	append_exportname(pos, left, name);

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &one);
	test(one == Nil, "append_exportname1");

	GetPackage(pos, PACKAGE_INDEX_TABLE, &one);
	findvalue_hashtable(one, name, &one);
	ptr = StructBitType(one);
	test(ptr->intern == PACKAGE_TYPE_INHERITED, "append_exportname2");
	test(ptr->base == 0, "append_exportname3");
	test(ptr->inherit, "append_exportname4");
	GetBitTypeSymbol(one, &one);
	test(one == symbol, "append_exportname5");

	/* two times */
	testnormal(append_exportname(pos, left, name), "append_exportname6");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &one);
	test(one == Nil, "append_exportname7");

	RETURN;
}

static int findstringchar(addr right, const char *name)
{
	enum LISPTYPE type;
	addr left;

	while (right != Nil) {
		GetCons(right, &left, &right);
		type = GetType(left);
		if (type == LISPTYPE_STRING) {
			if (string_equal_char(left, name)) return 1;
		}
	}

	return 0;
}

static int check_inherited(addr pos, const char *str)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	findvalue_char_hashtable(pos, str, &pos);
	if (pos == Nil) return 0;
	return StructBitType(pos)->inherit;
}

static int test_append_usepackage(void)
{
	addr pos, p1, p2, p3, use, check, one;

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	force_delete_package("BITPACKAGE3");
	internpackage(&pos, "BITPACKAGE");
	internpackage(&p1, "BITPACKAGE1");
	internpackage(&p2, "BITPACKAGE2");
	internpackage(&p3, "BITPACKAGE3");
	exportchar("BITPACKAGE2", "AAA");
	exportchar("BITPACKAGE2", "BBB");
	exportchar("BITPACKAGE3", "CCC");
	internchar("BITPACKAGE2", "HELLO", &check);
	makelist(&use, p1, p1, p2, p2, p3, NULL);
	append_usepackage(pos, use);

	test(check_inherited(pos, "AAA"), "append_usepackage1");
	test(check_inherited(pos, "BBB"), "append_usepackage2");
	test(check_inherited(pos, "CCC"), "append_usepackage3");
	test(! check_inherited(pos, "HELLO"), "append_usepackage4");

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	force_delete_package("BITPACKAGE3");
	internpackage(&pos, "BITPACKAGE");
	internpackage(NULL, "BITPACKAGE1");
	internpackage(NULL, "BITPACKAGE2");
	internpackage(NULL, "BITPACKAGE3");
	exportchar("BITPACKAGE2", "AAA");
	exportchar("BITPACKAGE2", "BBB");
	exportchar("BITPACKAGE3", "CCC");
	internchar("BITPACKAGE2", "HELLO", &check);
	makestringlist(&use,
			"BITPACKAGE1", "BITPACKAGE2", "BITPACKAGE2",
			"BITPACKAGE3", "BITPACKAGE3", "BITPACKAGE3", NULL);
	append_usepackage(pos, use);

	test(check_inherited(pos, "AAA"), "append_usepackage5");
	test(check_inherited(pos, "BBB"), "append_usepackage6");
	test(check_inherited(pos, "CCC"), "append_usepackage7");
	test(! check_inherited(pos, "HELLO"), "append_usepackage8");

	GetPackage(pos, PACKAGE_INDEX_USE, &check);
	test(listlength(check) == 3, "append_usepackage9");

	GetCons(check, &one, &check);
	GetPackage(one, PACKAGE_INDEX_USED, &one);
	test(listlength(one) == 1, "append_usepackage10");
	GetCar(one, &one);
	test(pos == one, "append_usepackage11");

	GetCons(check, &one, &check);
	GetPackage(one, PACKAGE_INDEX_USED, &one);
	test(listlength(one) == 1, "append_usepackage12");
	GetCar(one, &one);
	test(pos == one, "append_usepackage13");

	GetCons(check, &one, &check);
	GetPackage(one, PACKAGE_INDEX_USED, &one);
	test(listlength(one) == 1, "append_usepackage14");
	GetCar(one, &one);
	test(pos == one, "append_usepackage15");

	RETURN;
}

static int test_make_package(void)
{
	addr pos, name, nicknames, use, cons;

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	internpackage(&use, "BITPACKAGE1");
	exportchar("BITPACKAGE1", "NAME1");
	exportchar("BITPACKAGE1", "NAME2");
	strvect_char_heap(&name, "BITPACKAGE");
	makestringlist(&nicknames, "NAME1", "NAME2", NULL);
	makestringlist(&use, "BITPACKAGE1", NULL);
	make_package(name, nicknames, use, &pos);

	test(check_inherited(pos, "NAME1"), "amake_package1");
	test(check_inherited(pos, "NAME2"), "amake_package2");
	test(! check_inherited(pos, "HELLO"), "amake_package3");

	GetPackage(pos, PACKAGE_INDEX_USE, &cons);
	test(listlength(cons) == 1, "make_package4");
	GetCar(cons, &cons);
	getname_package(cons, &cons);
	test(string_equal_char(cons, "BITPACKAGE1"), "make_package5");

	internpackage(&use, "BITPACKAGE1");
	GetPackage(use, PACKAGE_INDEX_USED, &use);
	test(listlength(use) == 1, "make_package6");
	GetCar(use, &use);
	test(pos == use, "make_package7");

	RETURN;
}


/*
 *  delete_package
 */
static int test_delete_eqlist(void)
{
	addr pos, check, left, right, p1, p2, p3;

	pos = Nil;
	strvect_char_heap(&check, "AAA");
	test(delete_eqlist(pos, check, &pos), "delete_eqlist1");
	makestringlist(&pos, "AAA", NULL);
	test(delete_eqlist(pos, check, &check), "delete_eqlist2");
	strvect_char_heap(&p1, "AAA");
	strvect_char_heap(&p2, "BBB");
	strvect_char_heap(&p3, "CCC");
	makelist(&pos, p1, NULL);
	test(delete_eqlist(pos, p1, &pos) == 0, "delete_eqlist3");
	test(pos == Nil, "delete_eqlist4");

	makelist(&pos, p1, p2, p3, NULL);
	test(delete_eqlist(pos, p1, &pos) == 0, "delete_eqlist4");
	test(listlength(pos) == 2, "delete_eqlist5");
	GetCons(pos, &left, &right);
	test(left == p2, "delete_eqlist6");
	GetCons(right, &left, &right);
	test(left == p3, "delete_eqlist7");
	test(right == Nil, "delete_eqlist8");

	makelist(&pos, p1, p2, p3, NULL);
	test(delete_eqlist(pos, p2, &pos) == 0, "delete_eqlist9");
	test(listlength(pos) == 2, "delete_eqlist10");
	GetCons(pos, &left, &right);
	test(left == p1, "delete_eqlist11");
	GetCons(right, &left, &right);
	test(left == p3, "delete_eqlist12");
	test(right == Nil, "delete_eqlist13");

	makelist(&pos, p1, p2, p3, NULL);
	test(delete_eqlist(pos, p3, &pos) == 0, "delete_eqlist14");
	test(listlength(pos) == 2, "delete_eqlist15");
	GetCons(pos, &left, &right);
	test(left == p1, "delete_eqlist16");
	GetCons(right, &left, &right);
	test(left == p2, "delete_eqlist17");
	test(right == Nil, "delete_eqlist18");

	RETURN;
}

static int test_remove_eqpackage(void)
{
	addr pos, p1, p2, p3, cons;

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	strvect_char_heap(&p1, "AAA");
	strvect_char_heap(&p2, "BBB");
	strvect_char_heap(&p3, "CCC");
	makelist(&cons, p1, p2, p3, NULL);
	SetPackage(pos, PACKAGE_INDEX_EXPORT, cons);
	test(remove_eqpackage(pos, PACKAGE_INDEX_EXPORT, T), "remove_eqpackage1");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 3, "remove_eqpackage2");
	test(remove_eqpackage(pos, PACKAGE_INDEX_EXPORT, p1) == 0, "remove_eqpackage3");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 2, "remove_eqpackage4");

	RETURN;
}

static addr findbit(addr pos, const char *name)
{
	addr key;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	strvect_char_heap(&key, name);
	findvalue_hashtable(pos, key, &pos);

	return pos;
}

static void inheritedchar(const char *pname, const char *sname, addr *ret)
{
	addr package, pos, bit;

	internchar(pname, sname, &pos);
	internpackage(&package, pname);
	bit = findbit(package, sname);
	StructBitType(bit)->base = 0;
	StructBitType(bit)->inherit = 1;
}

static int test_allunintern_inherited(void)
{
	addr pos, cons, check;

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	makestringlist(&cons, "AAA", "BBB", NULL);
	internchar("BITPACKAGE", "AAA", &check);
	inheritedchar("BITPACKAGE", "BBB", &check);
	allunintern_inherited(pos, cons);
	test(findbit(pos, "AAA") != Nil, "allunintern_inherited1");
	test(findbit(pos, "BBB") == Nil, "allunintern_inherited1");

	RETURN;
}

static int findeq(addr right, addr check)
{
	addr left;

	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == check) return 1;
	}

	return 0;
}

static int test_allunintern_uselist(void)
{
	addr pos, p1, p2, cons;

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	internpackage(&pos, "BITPACKAGE");
	internpackage(&p1, "BITPACKAGE1");
	internpackage(&p2, "BITPACKAGE2");
	pushlist_package(p1, PACKAGE_INDEX_USED, pos);
	pushlist_package(p2, PACKAGE_INDEX_USED, p1);
	pushlist_package(p2, PACKAGE_INDEX_USED, pos);
	inheritedchar("BITPACKAGE1", "AAA", &cons);
	inheritedchar("BITPACKAGE2", "AAA", &cons);
	pushlist_package(pos, PACKAGE_INDEX_USE, p1);
	pushlist_package(pos, PACKAGE_INDEX_USE, p2);
	strvect_char_heap(&cons, "AAA");
	pushlist_package(pos, PACKAGE_INDEX_EXPORT, cons);
	makelist(&cons, p1, p2, NULL);
	allunintern_uselist(pos);
	test(findbit(p1, "AAA") == Nil, "allunintern_uselist1");
	test(findbit(p2, "AAA") == Nil, "allunintern_uselist2");
	GetPackage(p1, PACKAGE_INDEX_USED, &cons);
	test(findeq(cons, pos) == 0, "allunintern_uselist3");
	GetPackage(p2, PACKAGE_INDEX_USED, &cons);
	test(findeq(cons, pos) == 0, "allunintern_uselist4");

	RETURN;
}

static int test_allunintern(void)
{
	addr pos, table, check1, check2, check3, check;
	size_t size;

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	SetPackage(pos, PACKAGE_INDEX_USE, T);
	SetPackage(pos, PACKAGE_INDEX_EXPORT, T);
	SetPackage(pos, PACKAGE_INDEX_SHADOW, T);
	internchar("BITPACKAGE", "AAA", &check1);
	internchar("BITPACKAGE", "BBB", &check2);
	internchar("BITPACKAGE", "CCC", &check3);
	check = findbit(pos, "CCC");
	StructBitType(check)->base = 0;
	StructBitType(check)->inherit = 1;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);
	getcount_hashtable(table, &size);
	test(size == 3, "allunintern1");

	allunintern(pos);
	getcount_hashtable(table, &size);
	test(size == 0, "allunintern2");
	test(findbit(pos, "AAA") == Nil, "allunintern3");
	test(findbit(pos, "BBB") == Nil, "allunintern4");
	test(findbit(pos, "CCC") == Nil, "allunintern5");

	GetPackageSymbol(check1, &check);
	test(check == Nil, "allunintern6");
	GetNameSymbol(check1, &check);
	test(string_equal_char(check, "AAA"), "allunintern7");

	GetPackageSymbol(check2, &check);
	test(check == Nil, "allunintern8");
	GetNameSymbol(check2, &check);
	test(string_equal_char(check, "BBB"), "allunintern9");

	GetPackageSymbol(check3, &check);
	test(check != Nil, "allunintern10");
	GetNameSymbol(check3, &check);
	test(string_equal_char(check, "CCC"), "allunintern11");

	GetPackage(pos, PACKAGE_INDEX_USE, &check);
	test(check == Nil, "allunintern12");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &check);
	test(check == Nil, "allunintern13");
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &check);
	test(check == Nil, "allunintern14");

	RETURN;
}

static int test_delete_package(void)
{
	addr pos, name, check;

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);
	test(delete_package(pos), "delete_package1");

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	pushlist_package(pos, PACKAGE_INDEX_USED, T);
	testerror(delete_package(pos), "delete_package2");
	SetPackage(pos, PACKAGE_INDEX_USED, Nil);

	test(delete_package(pos) == 0, "delete_package3");
	GetPackage(pos, PACKAGE_INDEX_NAME, &check);
	test(check == Nil, "delete_package4");

	force_delete_package("BITPACKAGE");
	makestringlist(&check, "AAA", "BBB", NULL);
	strvect_char_heap(&name, "BITPACKAGE");

	make_package(name, check, Nil, &pos);
	test(findcharlocal("BITPACKAGE") != Nil, "delete_package5");
	test(findcharlocal("AAA") != Nil, "delete_package6");
	test(findcharlocal("BBB") != Nil, "delete_package7");
	GetPackage(pos, PACKAGE_INDEX_NAME, &check);
	internchar("AAA", "HELLO", &name);
	test(delete_package(pos) == 0, "delete_package8");

	test(findcharlocal("BITPACKAGE") == Nil, "delete_package9");
	test(findcharlocal("AAA") == Nil, "delete_package10");
	test(findcharlocal("BBB") == Nil, "delete_package11");
	GetPackageSymbol(name, &name);
	test(name == Nil, "delete_pacakge12");

	RETURN;
}


/*
 *  rename_package
 */
static int test_check_renameone(void)
{
	addr table, name, root, cons;

	internpackage(NULL, "BITPACKAGE");
	internpackage(NULL, "BITPACKAGE1");
	internpackage(NULL, "BITPACKAGE2");
	PackageTable(&table);
	strvect_char_heap(&name, "Hello");
	test(check_renameone(table, name, Nil, Nil) == 0, "check_renameone1");
	strvect_char_heap(&name, "BITPACKAGE2");
	test(check_renameone(table, name, Nil, Nil), "check_renameone2");
	test(check_renameone(table, name, name, Nil) == 0, "check_renameone3");
	strvect_char_heap(&root, "AAAA");
	test(check_renameone(table, name, root, Nil), "check_renameone4");
	makestringlist(&cons, "AAA", "BBB", NULL);
	test(check_renameone(table, name, root, cons), "check_renameone5");
	test(check_renameone(table, name, name, cons) == 0, "check_renameone6");

	strvect_char_heap(&name, "BITPACKAGE2");
	strvect_char_heap(&root, "AAAA");
	makestringlist(&cons, "AAA", "BITPACKAGE2", "CCC", NULL);
	test(check_renameone(table, name, root, cons) == 0, "check_renameone7");

	RETURN;
}

static int test_check_rename(void)
{
	addr pos, name, cons;

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	internpackage(&pos, "BITPACKAGE");
	internpackage(NULL, "BITPACKAGE1");
	internpackage(NULL, "BITPACKAGE2");

	strvect_char_heap(&name, "AAA");
	testnormal(check_rename(pos, name, Nil), "check_rename1");
	strvect_char_heap(&name, "BITPACKAGE");
	testnormal(check_rename(pos, name, Nil), "check_rename2");
	strvect_char_heap(&name, "BITPACKAGE2");
	testerror(check_rename(pos, name, Nil), "check_rename3");
	strvect_char_heap(&name, "AAA");
	makestringlist(&cons, "BBB", "CCC", NULL);
	testnormal(check_rename(pos, name, cons), "check_rename4");
	makestringlist(&cons, "BBB", "BITPACKAGE2", NULL);
	testerror(check_rename(pos, name, cons), "check_rename5");

	SetPackage(pos, PACKAGE_INDEX_NICKNAME, cons);
	strvect_char_heap(&name, "BITPACKAGE2");
	testnormal(check_rename(pos, name, Nil), "check_rename6");
	strvect_char_heap(&name, "BITPACKAGE1");
	testerror(check_rename(pos, name, Nil), "check_rename7");

	RETURN;
}

static int test_delete_renameone(void)
{
	addr pos, name, cons;

	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	strvect_char_heap(&name, "HELLO");
	intern_hashheap(pos, name, &cons);
	SetCdr(cons, T);

	force_delete_package("BITPACKAGE");
	internpackage(NULL, "BITPACKAGE");
	internchar("BITPACKAGE", "HELLO", &cons);
	delete_renameone(pos, cons);
	findvalue_hashtable(pos, name, &cons);
	test(cons == Nil, "delete_renameone1");

	RETURN;
}

static int test_delete_allnames(void)
{
	addr pos, cons;

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	test(findcharlocal("BITPACKAGE") != Nil, "delete_allnames1");
	delete_allnames(pos);
	test(findcharlocal("BITPACKAGE") == Nil, "delete_allnames2");

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	internpackage(&pos, "BITPACKAGE");
	internpackage(NULL, "BITPACKAGE1");
	internpackage(NULL, "BITPACKAGE2");
	makestringlist(&cons, "BITPACKAGE1", "BITPACKAGE2", NULL);
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, cons);
	delete_allnames(pos);
	test(findcharlocal("BITPACKAGE") == Nil, "delete_allnames3");
	test(findcharlocal("BITPACKAGE1") == Nil, "delete_allnames4");
	test(findcharlocal("BITPACKAGE2") == Nil, "delete_allnames5");

	GetPackage(pos, PACKAGE_INDEX_NAME, &cons);
	test(cons == Nil, "delete_allnames6");
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &cons);
	test(cons == Nil, "delete_allnames7");

	RETURN;
}

static int test_intern_renameone(void)
{
	addr table, pos, name, check;

	PackageTable(&table);
	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	strvect_char_heap(&name, "BITPACKAGE");
	delete_package(name);
	getname_package(pos, &check);
	test(check == Nil, "intern_renameone1");

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);
	strvect_char_heap(&name, "HELLO");
	intern_renameone(pos, table, name, 0);
	getname_package(pos, &check);
	test(string_equal_char(check, "HELLO"), "intern_renameone2");

	testnormal(intern_renameone(pos, table, name, 1), "intern_renameone3");
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &check);
	test(check == Nil, "intern_renameone4");

	strvect_char_heap(&name, "AAA");
	intern_renameone(pos, table, name, 1);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &check);
	GetCons(check, &name, &check);
	test(string_equal_char(name, "AAA"), "intern_renameone5");
	test(check == Nil, "intern_renameone6");

	RETURN;
}

static int test_intern_allnames(void)
{
	addr pos, name, cons;

	force_delete_package("BITPACKAGE");
	internpackage(&pos, "BITPACKAGE");
	strvect_char_heap(&name, "BITPACKAGE");
	delete_package(name);

	strvect_char_heap(&name, "HELLO");
	makestringlist(&cons, "AAA", "BBB", "CCC", NULL);
	intern_allnames(pos, name, cons);

	getname_package(pos, &name);
	test(string_equal_char(name, "HELLO"), "intern_allnames1");
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &cons);
	test(listlength(cons) == 3, "intern_allnames2");
	test(findstringchar(cons, "AAA"), "intern_allnames3");
	test(findstringchar(cons, "BBB"), "intern_allnames4");
	test(findstringchar(cons, "CCC"), "intern_allnames5");
	test(findcharlocal("HELLO") != Nil, "intern_allnames6");
	test(findcharlocal("AAA") != Nil, "intern_allnames7");
	test(findcharlocal("BBB") != Nil, "intern_allnames8");
	test(findcharlocal("CCC") != Nil, "intern_allnames9");

	RETURN;
}

static int test_rename_package(void)
{
	addr pos, name;

	force_delete_package("BITPACKAGE");
	force_delete_package("BITPACKAGE1");
	force_delete_package("BITPACKAGE2");
	force_delete_package("BITPACKAGE3");
	force_delete_package("HELLO");
	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos, "BITPACKAGE");

	strvect_char_heap(&name, "AAA");
	rename_package(pos, name, Nil, &pos);
	test(findcharlocal("BITPACKAGE") == Nil, "rename_package1");
	test(findcharlocal("AAA") != Nil, "rename_package2");
	getname_package(pos, &name);
	test(string_equal_char(name, "AAA"), "rename_pacakge3");

	RETURN;
}


/*
 *  package function
 */
static int test_getname_package(void)
{
	addr pos;

	internpackage(&pos, "AAA");
	getname_package(pos, &pos);
	test(string_equal_char(pos, "AAA"), "getname_package1");
	force_delete_package("AAA");

	RETURN;
}

static int test_getnickname_package(void)
{
	addr pos;

	internpackage(&pos, "AAA");
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, T);
	getnickname_package(pos, &pos);
	test(pos == T, "getnickname_package1");
	force_delete_package("AAA");

	RETURN;
}

static int test_getuselist_package(void)
{
	addr pos;

	internpackage(&pos, "AAA");
	SetPackage(pos, PACKAGE_INDEX_USE, T);
	getuselist_package(pos, &pos);
	test(pos == T, "getuselist_package1");
	force_delete_package("AAA");

	RETURN;
}

static int test_getusedbylist_package(void)
{
	addr pos;

	internpackage(&pos, "AAA");
	SetPackage(pos, PACKAGE_INDEX_USED, T);
	getusedbylist_package(pos, &pos);
	test(pos == T, "getusedbylist_package1");
	force_delete_package("AAA");

	RETURN;
}

static int test_getshadow_package(void)
{
	addr pos;

	internpackage(&pos, "AAA");
	SetPackage(pos, PACKAGE_INDEX_SHADOW, T);
	getshadow_package(pos, &pos);
	test(pos == T, "getshadow_package1");
	force_delete_package("AAA");

	RETURN;
}


/*
 *  find_symbol_package
 */
static int test_find_bitpackage(void)
{
	addr pos, name, check;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &check);
	strvect_char_heap(&name, "HELLO");
	find_bitpackage(pos, name, &check);
	test(check != Nil, "find_bitpacakge1");
	strvect_char_heap(&name, "ZZZZ");
	find_bitpackage(pos, name, &check);
	test(check == Nil, "find_bitpacakge2");
	force_delete_package("AAA");

	RETURN;
}

static int test_find_symbol_package(void)
{
	enum PACKAGE_TYPE type;
	addr pos, name, check;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &check);
	strvect_char_heap(&name, "HELLO");
	type = find_symbol_package(pos, name, &check);
	test(check != Nil, "find_symbol_package1");
	test(type == PACKAGE_TYPE_INTERNAL, "find_symbol_package2");
	strvect_char_heap(&name, "ZZZZ");
	type = find_symbol_package(pos, name, &check);
	test(check == Nil, "find_symbol_package3");
	test(type == PACKAGE_TYPE_NIL, "find_symbol_package4");
	force_delete_package("AAA");

	RETURN;
}


/*
 *  find_allsymbols_package
 */
static int test_push_basesymbol(void)
{
	addr pos, key, symbol, name, cons, left, right;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &symbol);
	strvect_char_heap(&key, "AAA");
	strvect_char_heap(&name, "HELLO");

	cons = Nil;
	push_basesymbol(key, pos, name, &cons);
	test(cons != Nil, "push_basesymbol1");
	GetCons(cons, &left, &right);
	GetNameSymbol(left, &left);
	test(string_equal_char(left, "HELLO"), "push_basesymbol2");
	test(right == Nil, "push_basesymbol3");

	cons = Nil;
	StructBitType(findbit(pos, "HELLO"))->base = 0;
	push_basesymbol(key, pos, name, &cons);
	test(cons == Nil, "push_basesymbol4");
	StructBitType(findbit(pos, "HELLO"))->base = 1;

	cons = Nil;
	strvect_char_heap(&name, "HELLOAAA");
	push_basesymbol(key, pos, name, &cons);
	test(cons == Nil, "push_basesymbol5");

	cons = Nil;
	strvect_char_heap(&key, "AAAAA");
	strvect_char_heap(&name, "HELLO");
	push_basesymbol(key, pos, name, &cons);
	test(cons == Nil, "push_basesymbol6");

	RETURN;
}

static void clear_allpackage(void)
{
	addr pos;

	PackageTable(&pos);
	clear_hashtable(pos);
}

static int test_find_allsymbols_package(void)
{
	addr pos, name, names, left, right;

	clear_allpackage();
	strvect_char_heap(&name, "AAA");
	makestringlist(&names, "NAMEA", "NAMEB", NULL);
	make_package(name, names, Nil, &pos);
	internpackage(NULL, "BBB");
	internpackage(NULL, "CCC");
	internpackage(NULL, "DDD");
	internchar("AAA", "HELLO", &pos);
	internchar("AAA", "PB", &pos);
	internchar("BBB", "PA", &pos);
	internchar("CCC", "HELLO", &pos);
	internchar("CCC", "PA", &pos);
	internchar("CCC", "PD", &pos);
	internchar("DDD", "9HELLO", &pos);
	internchar("DDD", "HELLO1", &pos);

	strvect_char_heap(&name, "HELLO");
	find_allsymbols_package(name, &pos);
	test(listlength(pos) == 2, "find_allsymbols_package1");

	GetCons(pos, &left, &right);
	GetNameSymbol(left, &name);
	test(string_equal_char(name, "HELLO"), "find_allsymbols_package2");
	GetPackageSymbol(left, &left);
	getname_package(left, &left);
	test(string_equal_char(left, "AAA") || string_equal_char(left, "CCC"),
			"find_allsymbols_package3");
	test(right != Nil, "find_allsymbols_package4");

	GetCons(right, &left, &right);
	GetNameSymbol(left, &name);
	test(string_equal_char(name, "HELLO"), "find_allsymbols_package5");
	GetPackageSymbol(left, &left);
	getname_package(left, &left);
	test(string_equal_char(left, "AAA") || string_equal_char(left, "CCC"),
			"find_allsymbols_package6");
	test(right == Nil, "find_allsymbols_package7");

	RETURN;
}


/*
 *  list_all_packages
 */
static int test_push_basepackage(void)
{
	addr pos, cons, key, left, right;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	strvect_char_heap(&key, "HELLO");
	cons = Nil;
	push_basepackage(key, pos, &cons);
	test(cons == Nil, "push_basepackage1");

	strvect_char_heap(&key, "AAA");
	cons = Nil;
	push_basepackage(key, pos, &cons);
	test(cons != Nil, "push_basepackage2");
	GetCons(cons, &left, &right);
	test(left == pos, "push_basepackage3");
	test(right == Nil, "push_basepackage4");

	RETURN;
}

static int test_list_all_packages(void)
{
	addr name, names, pos, p1, p2, left;

	clear_allpackage();
	strvect_char_heap(&name, "AAA");
	makestringlist(&names, "NAMEA", "NAMEB", NULL);
	make_package(name, names, Nil, &p1);
	internpackage(&p2, "BBB");

	list_all_packages(&pos);
	test(listlength(pos) == 2, "list_all_packages1");
	GetCons(pos, &left, &pos);
	test(left == p1 || left == p2, "list_all_package2");
	GetCons(pos, &left, &pos);
	test(left == p1 || left == p2, "list_all_package3");
	test(pos == Nil, "list_all_package4");

	RETURN;
}


/*
 *  intern
 */
static int test_intern_bitpackage(void)
{
	addr pos, name, check;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	strvect_char_heap(&name, "HELLO");
	test(intern_bitpackage(pos, name, &check), "intern_bitpackage1");
	test(StructBitType(check)->base, "intern_bitpackage2");

	check = Nil;
	test(intern_bitpackage(pos, name, &check) == 0, "intern_bitpackage3");
	test(StructBitType(check)->base, "intern_bitpackage4");

	StructBitType(check)->base = 0;
	check = Nil;
	test(intern_bitpackage(pos, name, &check) == 0, "intern_bitpackage5");
	test(StructBitType(check)->base == 0, "intern_bitpackage6");

	RETURN;
}

static int test_intern_package_table(void)
{
	enum PACKAGE_TYPE type;
	addr pos, name, check, symbol;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	strvect_char_heap(&name, "HELLO");
	type = intern_package_table(pos, name, &check);
	symbol = check;
	test(type == PACKAGE_TYPE_NIL, "intern_package_table1");
	GetNameSymbol(check, &name);
	test(string_equal_char(name, "HELLO"), "intern_package_table2");
	GetPackageSymbol(check, &check);
	test(check == pos, "intern_package_table3");

	strvect_char_heap(&name, "HELLO");
	type = intern_package_table(pos, name, &check);
	test(type == PACKAGE_TYPE_INTERNAL, "intern_package_table4");
	test(check == symbol, "intern_package_table5");

	RETURN;
}

static int test_intern_package(void)
{
	enum PACKAGE_TYPE type;
	addr pos, design, name, check, symbol;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	strvect_char_heap(&design, "AAA");

	strvect_char_heap(&name, "HELLO");
	type = intern_package(design, name, &check);
	symbol = check;
	test(type == PACKAGE_TYPE_NIL, "intern_package1");
	GetNameSymbol(check, &name);
	test(string_equal_char(name, "HELLO"), "intern_package2");
	GetPackageSymbol(check, &check);
	test(check == pos, "intern_package3");

	strvect_char_heap(&name, "HELLO");
	type = intern_package(design, name, &check);
	test(type == PACKAGE_TYPE_INTERNAL, "intern_package4");
	test(check == symbol, "intern_package5");

	RETURN;
}


/*
 *  unintern
 */
static int test_check_export_unintern(void)
{
	addr pos, name, symbol;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &symbol);

	strvect_char_heap(&name, "ZZZZ");
	test(check_export_unintern(pos, name) == 0, "check_export_unintern1");

	strvect_char_heap(&name, "HELLO");
	test(check_export_unintern(pos, name) == 0, "check_export_unintern2");

	StructBitType(findbit(pos, "HELLO"))->expt = 1;
	strvect_char_heap(&name, "HELLO");
	test(check_export_unintern(pos, name), "check_export_unintern3");

	RETURN;
}

static int test_check_shadowing_unintern(void)
{
	addr pos, pos1, pos2, name, check;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos,  "AAA");
	internpackage(&pos1, "BBB");
	internpackage(&pos2, "CCC");

	internchar("AAA", "HELLO", &check);
	internchar("BBB", "HELLO", &check);
	internchar("CCC", "HELLO", &check);

	strvect_char_heap(&name, "HELLO");
	test(check_shadowing_unintern(pos, name) == 0, "check_shadowing_unintern1");

	makelist(&check, pos1, pos2, NULL);
	SetPackage(pos, PACKAGE_INDEX_USE, check);
	test(check_shadowing_unintern(pos, name) == 0, "check_shadowing_unintern2");

	StructBitType(findbit(pos1, "HELLO"))->expt = 1;
	test(check_shadowing_unintern(pos, name) == 0, "check_shadowing_unintern3");

	StructBitType(findbit(pos2, "HELLO"))->expt = 1;
	test(check_shadowing_unintern(pos, name), "check_shadowing_unintern4");

	RETURN;
}

static int test_uninterncheck(void)
{
	addr pos, p1, p2, symbol;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos, "AAA");
	internpackage(&p1, "BBB");
	internpackage(&p2, "CCC");
	internchar("CCC", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	test(uninterncheck(pos, symbol), "uninterncheck1");

	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	test(uninterncheck(pos, symbol), "uninterncheck2");

	internchar("BBB", "HELLO", &symbol);
	internchar("AAA", "HELLO", &symbol);
	test(uninterncheck(pos, symbol) == 0, "uninterncheck3");

	pushlist_package(pos, PACKAGE_INDEX_USE, p1);
	pushlist_package(pos, PACKAGE_INDEX_USE, p2);
	StructBitType(findbit(pos, "HELLO"))->shadow = 1;
	StructBitType(findbit(p1, "HELLO"))->expt = 1;
	StructBitType(findbit(p2, "HELLO"))->expt = 1;
	testerror(uninterncheck(pos, symbol), "uninterncheck4");

	RETURN;
}

static int test_remove_package_unintern(void)
{
	addr pos, symbol, name;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &symbol);
	strvect_char_heap(&name, "HELLO");
	test(remove_package_unintern(pos, name) == 0, "remove_package_unintern1");
	test(findbit(pos, "HELLO") == Nil, "remove_package_unintern2");
	GetPackageSymbol(symbol, &symbol);
	test(symbol == Nil, "remove_package_unintern3");

	internchar("AAA", "HELLO", &symbol);
	StructBitType(findbit(pos, "HELLO"))->shadow = 1;
	test(remove_package_unintern(pos, name), "remove_package_unintern4");
	test(findbit(pos, "HELLO") == Nil, "remove_package_unintern5");
	GetPackageSymbol(symbol, &symbol);
	test(symbol == Nil, "remove_package_unintern6");

	RETURN;
}

static int test_intern_inherited_unintern(void)
{
	addr pos, pos1, pos2, symbol, name, bit;
	struct bittype_struct *ptr;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos, "AAA");
	internpackage(&pos1, "BBB");
	internpackage(&pos2, "CCC");
	internchar("BBB", "HELLO", &symbol);
	internchar("CCC", "HELLO", &name);
	StructBitType(findbit(pos1, "HELLO"))->expt = 1;
	strvect_char_heap(&name, "HELLO");
	pushlist_package(pos, PACKAGE_INDEX_USE, pos1);
	pushlist_package(pos, PACKAGE_INDEX_USE, pos2);
	intern_inherited_unintern(pos, name);
	bit = findbit(pos, "HELLO");
	ptr = StructBitType(bit);
	GetBitTypeSymbol(bit, &name);
	test(ptr->inherit, "intern_inherited_unintern1");
	test(symbol == name, "intern_inherited_unintern2");

	RETURN;
}

static int test_remove_shadowing_symbols(void)
{
	addr pos, symbol, cons;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "ZZZZ", &symbol);
	pushlist_package(pos, PACKAGE_INDEX_SHADOW, symbol);
	internchar("AAA", "QQQQ", &symbol);
	pushlist_package(pos, PACKAGE_INDEX_SHADOW, symbol);
	internchar("AAA", "HELLO", &symbol);
	pushlist_package(pos, PACKAGE_INDEX_SHADOW, symbol);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &cons);
	test(listlength(cons) == 3, "remove_shadowing_symbol1");
	test(findeq(cons, symbol), "remove_shadowing_symbol2");
	remove_shadowing_symbols(pos, symbol);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &cons);
	test(listlength(cons) == 2, "remove_shadowing_symbol3");
	test(findeq(cons, symbol) == 0, "remove_shadowing_symbol4");

	RETURN;
}

static int test_uninternsymbol(void)
{
	addr pos, pos1, arg, symbol, bit;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(&pos1, "BBB");
	internchar("AAA", "HELLO", &arg);
	internchar("BBB", "HELLO", &symbol);
	pushlist_package(pos, PACKAGE_INDEX_USE, pos1);
	pushlist_package(pos, PACKAGE_INDEX_SHADOW, arg);
	StructBitType(findbit(pos, "HELLO"))->shadow = 1;
	StructBitType(findbit(pos1, "HELLO"))->expt = 1;
	uninternsymbol(pos, arg);
	bit = findbit(pos, "HELLO");
	test(StructBitType(bit)->inherit, "uninternsymbol1");
	GetBitTypeSymbol(bit, &bit);
	test(bit = symbol, "uninternsymbol2");

	RETURN;
}

static int test_unintern(void)
{
	addr pos, pos1, symbol;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(&pos1, "BBB");

	internchar("BBB", "HELLO", &symbol);
	test(unintern_package(pos, symbol), "unintern1");

	internchar("AAA", "HELLO", &symbol);
	test(unintern_package(pos, symbol) == 0, "unintern2");
	test(findbit(pos, "HELLO") == Nil, "unintern3");
	test(unintern_package(pos, symbol), "unintern4");

	RETURN;
}


/*
 *  import
 */
static int test_import_bitpackage(void)
{
	addr pos, pos1, symbol, bit;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(&pos1, "BBB");
	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	test(import_bitpackage(pos, symbol, &bit), "import_bitpackage1");
	test(import_bitpackage(pos1, symbol, &bit) == 0, "import_bitpackage2");
	test(StructBitType(findbit(pos1, "HELLO"))->import == 0, "import_bitpackage3");

	internchar("AAA", "HELLO", &symbol);
	unintern_package(pos, symbol);
	internchar("BBB", "HELLO", &symbol);
	test(import_bitpackage(pos, symbol, &bit) == 0, "import_bitpackage4");
	test(bit == findbit(pos, "HELLO"), "import_bitpackage5");
	test(StructBitType(bit)->import == 1, "import_bitpackage6");
	GetBitTypeSymbol(bit, &bit);
	test(bit == symbol, "import_bitpackage7");

	internchar("AAA", "HELLO", &symbol);
	unintern_package(pos, symbol);
	internchar("BBB", "HELLO", &symbol);
	unintern_package(pos1, symbol);
	GetPackageSymbol(symbol, &bit);
	test(bit == Nil, "import_bitpackage8");
	test(import_bitpackage(pos, symbol, &bit) == 0, "import_bitpackage9");
	test(bit == findbit(pos, "HELLO"), "import_bitpackage10");
	test(StructBitType(bit)->import == 0, "import_bitpackage11");
	test(StructBitType(bit)->base == 1, "import_bitpackage12");
	GetBitTypeSymbol(bit, &bit);
	test(bit == symbol, "import_bitpackage13");
	GetPackageSymbol(symbol, &bit);
	test(bit == pos, "import_bitpackage14");

	RETURN;
}

static int test_importsymbol(void)
{
	addr pos1, pos2, sym1, sym2;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internchar("AAA", "HELLO", &sym1);
	internchar("BBB", "HELLO", &sym2);
	testerror(importsymbol(pos1, sym2), "importsymbol1");
	testnormal(importsymbol(pos1, sym1), "importsymbol2");
	unintern_package(pos1, sym1);
	testnormal(importsymbol(pos1, sym2), "importsymbol3");

	RETURN;
}

static int test_importlist(void)
{
	addr pos1, pos2, pos3, name, cons, sym1, sym2, sym3;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	strvect_char_heap(&name, "Hello");
	makelist(&cons, name, NULL);
	testerror(importlist(pos1, cons), "importlist1");

	internchar("AAA", "HELLO", &sym1);
	internchar("BBB", "HELLO", &sym2);
	internchar("BBB", "ABC", &sym3);
	makelist(&cons, sym2, NULL);
	testerror(importlist(pos1, cons), "importlist2");
	unintern_package(pos1, sym1);
	makelist(&cons, sym2, sym3, NULL);
	testnormal(importlist(pos1, cons), "importlist3");
	test(StructBitType(findbit(pos1, "HELLO"))->import, "importlist4");
	test(StructBitType(findbit(pos1, "ABC"))->import, "importlist5");

	RETURN;
}

static int test_import_package(void)
{
	addr pos1, pos2, sym, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internchar("BBB", "HELLO", &sym);
	import_package(pos1, sym);
	test(StructBitType(findbit(pos1, "HELLO"))->import, "import_package1");

	internchar("BBB", "ABC", &sym);
	makelist(&cons, sym, NULL);
	import_package(pos1, cons);
	test(StructBitType(findbit(pos1, "ABC"))->import, "import_package2");

	RETURN;
}


/*
 *  shadow
 */
static int test_shadowsymbol(void)
{
	addr pos, name, bit, symbol, check;
	struct bittype_struct *ptr;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	strvect_char_heap(&name, "HELLO");

	shadowsymbol(pos, name);
	bit = findbit(pos, "HELLO");
	ptr = StructBitType(bit);
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "shadowsymbol1");
	test(ptr->base, "shadowsymbol2");
	test(ptr->shadow, "shadowsymbol3");
	GetBitTypeSymbol(bit, &symbol);
	GetPackageSymbol(symbol, &check);
	test(check == pos, "shadowsymbol4");
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &check);
	test(listlength(check) == 1, "shadowsymbol5");
	GetCar(check, &check);
	test(check == symbol, "shadowsymbol6");

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(&pos, "BBB");
	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	bit = findbit(pos, "HELLO");
	ptr = StructBitType(bit);
	SetBitTypeSymbol(bit, symbol);
	ptr->intern = PACKAGE_TYPE_INHERITED;
	ptr->base = 0;
	ptr->inherit = 1;

	shadowsymbol(pos, name);
	bit = findbit(pos, "HELLO");
	ptr = StructBitType(bit);
	GetBitTypeSymbol(bit, &check);
	test(check != symbol, "shadowsymbol7");
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "shadowsymbol8");
	test(ptr->base, "shadowsymbol9");
	test(ptr->inherit == 0, "shadowsymbol10");
	GetPackageSymbol(check, &check);
	getname_package(check, &check);
	test(string_equal_char(check, "BBB"), "shadowsymbol11");

	RETURN;
}

static int test_shadowlist(void)
{
	addr pos, cons, symbol;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	fixnum_heap(&cons, 100);
	makelist(&cons, cons, NULL);
	testerror(shadowlist(pos, cons), "shadowlist1");

	makestringlist(&cons, "HELLO", "ZZZ", NULL);
	testnormal(shadowlist(pos, cons), "shadowlist2");

	test(StructBitType(findbit(pos, "HELLO"))->shadow, "shadowlist3");
	test(StructBitType(findbit(pos, "ZZZ"))->shadow, "shadowlist4");
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &cons);
	test(listlength(cons) == 2, "shadowlist5");
	internchar("AAA", "HELLO", &symbol);
	test(findeq(cons, symbol), "shadowlist6");
	internchar("AAA", "ZZZ", &symbol);
	test(findeq(cons, symbol), "shadowlist7");

	RETURN;
}

static int test_shadow_package(void)
{
	addr pos, cons;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	strvect_char_heap(&cons, "HELLO");
	testnormal(shadow_package(pos, cons), "shadow_package1");
	testnormal(shadow_package(pos, cons), "shadow_package2");
	makestringlist(&cons, "AAA", "BBB", "CCC", NULL);
	testnormal(shadow_package(pos, cons), "shadow_package3");
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &cons);
	test(listlength(cons) == 4, "shadow_package4");

	RETURN;
}


/*
 *  shadowing-import
 */
static int test_shadowimportsymbol(void)
{
	addr pos1, pos2, symbol, symbol2, bit, check;
	struct bittype_struct *ptr;

	/* own symbol import */
	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internchar("AAA", "HELLO", &symbol);

	shadowimportsymbol(pos1, symbol);
	ptr = StructBitType(findbit(pos1, "HELLO"));
	test(ptr->base, "shadowimportsymbol1");
	test(ptr->import == 0, "shadowimportsymbol2");
	test(ptr->shadow, "shadowimportsymbol3");
	GetPackage(pos1, PACKAGE_INDEX_SHADOW, &check);
	test(listlength(check) == 1, "shadowimportsymbol4");
	GetCar(check, &check);
	test(check == symbol, "shadowimportsymbol5");

	/* conflict */
	force_delete_package("AAA");
	internpackage(&pos1, "AAA");
	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	shadowimportsymbol(pos1, symbol);
	bit = findbit(pos1, "HELLO");
	ptr = StructBitType(bit);
	test(ptr->base == 0, "shadowimportsymbol6");
	test(ptr->import, "shadowimportsymbol7");
	test(ptr->shadow, "shadowimportsymbol8");
	GetBitTypeSymbol(bit, &check);
	test(check == symbol, "shadowimportsymbol9");

	GetPackage(pos1, PACKAGE_INDEX_SHADOW, &check);
	test(listlength(check) == 1, "shadowimportsymbol10");
	GetCar(check, &check);
	test(check == symbol, "shadowimportsymbol11");
	shadowimportsymbol(pos1, symbol);
	GetPackage(pos1, PACKAGE_INDEX_SHADOW, &check);
	test(listlength(check) == 1, "shadowimportsymbol12");

	/* shadowing-symbols conflict */
	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	force_delete_package("DDD");
	internpackage(&pos1, "AAA");
	internpackage(NULL, "BBB");
	internpackage(NULL, "CCC");
	internpackage(NULL, "DDD");
	internchar("BBB", "HELLO", &symbol);
	import_package(pos1, symbol);
	internchar("CCC", "HELLO", &symbol);
	shadowimportsymbol(pos1, symbol);
	GetPackage(pos1, PACKAGE_INDEX_SHADOW, &check);
	test(listlength(check) == 1, "shadowimportsymbol13");
	GetCar(check, &check);
	test(check == symbol, "shadowimportsymbol14");

	internchar("DDD", "HELLO", &symbol2);
	shadowimportsymbol(pos1, symbol2);
	GetPackage(pos1, PACKAGE_INDEX_SHADOW, &check);
	test(listlength(check) == 1, "shadowimportsymbol15");
	GetCar(check, &check);
	test(check == symbol2, "shadowimportsymbol16");

	RETURN;
}

static int test_shadowimportlist(void)
{
	addr pos, cons, symbol;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	makestringlist(&cons, "HELLO", NULL);
	testerror(shadowimportlist(pos, cons), "shadowimportlist1");
	internchar("AAA", "ZZZ", &symbol);
	makelist(&cons, symbol, NULL);
	testnormal(shadowimportlist(pos, cons), "shadowimportlist2");
	test(StructBitType(findbit(pos, "ZZZ"))->shadow, "shadowimportlist3");

	RETURN;
}

static int test_shadowing_import_package(void)
{
	addr pos, cons, sym1, sym2;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos, "AAA");
	internpackage(NULL, "BBB");
	internpackage(NULL, "CCC");

	internchar("AAA", "HELLO", &cons);
	shadowing_import_package(pos, cons);
	internchar("AAA", "QQQ", &sym1);
	internchar("AAA", "ZZZ", &sym2);
	makelist(&cons, sym1, sym2, sym1, cons, NULL);
	shadowing_import_package(pos, cons);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, &cons);
	test(listlength(cons) == 3, "shadwing_import_package1");

	RETURN;
}


/*
 *  export
 */
static int test_check_exportsymbol(void)
{
	addr pos, pos1, pos2, symbol;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(NULL, "BBB");
	internchar("BBB", "HELLO", &symbol);
	testerror(check_exportsymbol(pos, symbol), "check_exportsymbol1");

	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	testerror(check_exportsymbol(pos, symbol), "check_exportsymbol2");

	internchar("AAA", "HELLO", &symbol);
	StructBitType(findbit(pos, "HELLO"))->expt = 1;
	testnormal(check_exportsymbol(pos, symbol), "check_exportsymbol3");

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos, "AAA");
	internpackage(&pos1, "BBB");
	internpackage(&pos2, "CCC");
	internchar("AAA", "HELLO", &symbol);
	pushlist_package(pos, PACKAGE_INDEX_USED, pos1);
	pushlist_package(pos, PACKAGE_INDEX_USED, pos2);
	testnormal(check_exportsymbol(pos, symbol), "check_exportsymbol3");

	internchar("CCC", "HELLO", &symbol);
	SetBitTypeSymbol(findbit(pos, "HELLO"), symbol);
	testerror(check_exportsymbol(pos, symbol), "check_exportsymbol4");

	StructBitType(findbit(pos2, "HELLO"))->shadow = 1;
	testnormal(check_exportsymbol(pos, symbol), "check_exportsymbol5");

	RETURN;
}

static int test_exportsymbol_nocheck(void)
{
	addr pos, symbol, check;
	struct bittype_struct *ptr;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &symbol);
	ptr = StructBitType(findbit(pos, "HELLO"));
	ptr->expt = 1;
	testnormal(exportsymbol_nocheck(pos, symbol), "exportsymbol_nocheck1");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &check);
	test(check == Nil, "exportsymbol_nocheck2");

	ptr->expt = 0;
	testnormal(exportsymbol_nocheck(pos, symbol), "exportsymbol_nocheck3");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &check);
	test(check != Nil, "exportsymbol_nocheck4");
	test(ptr->expt, "exportsymbol_nocheck5");
	test(ptr->intern == PACKAGE_TYPE_EXTERNAL, "exportsymbol_nocheck6");

	ptr->import = 0;
	ptr->expt = 0;
	ptr->inherit = 1;
	ptr->intern = PACKAGE_TYPE_INHERITED;
	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);
	testnormal(exportsymbol_nocheck(pos, symbol), "exportsymbol_nocheck7");
	test(ptr->import, "exportsymbol_nocheck8");
	test(ptr->expt, "exportsymbol_nocheck9");
	test(ptr->inherit == 0, "exportsymbol_nocheck10");
	test(ptr->intern == PACKAGE_TYPE_EXTERNAL, "exportsymbol_nocheck11");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &check);
	test(check != Nil, "exportsymbol_nocheck12");

	RETURN;
}

static int test_exportsymbol(void)
{
	addr pos, name;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	strvect_char_heap(&name, "HELLO");
	testerror(exportsymbol(pos, name), "exportsymbol1");
	internchar("AAA", "HELLO", &name);
	testnormal(exportsymbol(pos, name), "exportsymbol2");
	test(StructBitType(findbit(pos, "HELLO"))->expt, "exportsymbol3");

	RETURN;
}

static int test_exportlist(void)
{
	addr pos, pos1, cons, symbol, symbol1;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(&pos1, "BBB");
	makestringlist(&cons, "HELLO", NULL);
	testerror(exportlist(pos, cons), "exportlist1");

	internchar("BBB", "HELLO", &symbol);
	makelist(&cons, symbol, NULL);
	testerror(exportlist(pos, cons), "exportlist2");

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(cons == Nil, "exportlist3");

	internchar("AAA", "HELLO", &symbol);
	internchar("AAA", "ZZZZ", &symbol1);
	makelist(&cons, symbol, symbol1, NULL);
	testnormal(exportlist(pos, cons), "exportlist4");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 2, "exportlist5");

	RETURN;
}

static int test_export_package(void)
{
	addr pos, cons, symbol, sym1, sym2;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	internchar("AAA", "HELLO", &symbol);
	testnormal(export_package(pos, symbol), "export_package1");

	internchar("AAA", "BBB", &symbol);
	makelist(&cons, symbol, NULL);
	testnormal(export_package(pos, cons), "export_package2");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 2, "export_package3");

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "XXX", &symbol);
	internchar("AAA", "YYY", &sym1);
	internchar("AAA", "ZZZ", &sym2);
	makelist(&cons, symbol, sym1, sym2, sym1, sym1, sym1, NULL);
	testnormal(export_package(pos, cons), "export_package4");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 3, "export_package5");

	RETURN;
}


/*
 *  unexport
 */
static int test_delete_stringlist(void)
{
	addr pos, cons, key;

	pos = Nil;
	makestringlist(&cons, "AAA", "BBB", "CCC", NULL);
	strvect_char_heap(&key, "HELLO");
	test(delete_stringlist(cons, key, &pos), "delete_stringlist1");

	strvect_char_heap(&key, "AAA");
	test(delete_stringlist(cons, key, &pos) == 0, "delete_stringlist2");
	test(listlength(pos) == 2, "delete_stringlist3");
	test(findstringchar(pos, "BBB"), "delete_stringlist4");
	test(findstringchar(pos, "CCC"), "delete_stringlist5");

	makestringlist(&cons, "AAA", "BBB", "CCC", NULL);
	strvect_char_heap(&key, "BBB");
	test(delete_stringlist(cons, key, &pos) == 0, "delete_stringlist6");
	test(listlength(pos) == 2, "delete_stringlist7");
	test(findstringchar(pos, "AAA"), "delete_stringlist8");
	test(findstringchar(pos, "CCC"), "delete_stringlist9");

	makestringlist(&cons, "AAA", "BBB", "CCC", NULL);
	strvect_char_heap(&key, "CCC");
	test(delete_stringlist(cons, key, &pos) == 0, "delete_stringlist10");
	test(listlength(pos) == 2, "delete_stringlist11");
	test(findstringchar(pos, "AAA"), "delete_stringlist12");
	test(findstringchar(pos, "BBB"), "delete_stringlist13");

	RETURN;
}

static int test_remove_export_list(void)
{
	addr pos, cons, key;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	makestringlist(&cons, "AAA", "BBB", NULL);
	SetPackage(pos, PACKAGE_INDEX_EXPORT, cons);

	strvect_char_heap(&key, "HELLO");
	testerror(remove_export_list(pos, key), "remove_export_list1");

	strvect_char_heap(&key, "BBB");
	testnormal(remove_export_list(pos, key), "remove_export_list2");

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 1, "remove_export_list3");
	GetCar(cons, &cons);
	test(string_equal_char(cons, "AAA"), "remove_export_list4");

	RETURN;
}

static int test_check_unexportsymbol(void)
{
	addr pos, symbol;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(NULL, "BBB");

	internchar("BBB", "HELLO", &symbol);
	testerror(check_unexportsymbol(pos, symbol), "check_unexportsymbol1");

	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	testerror(check_unexportsymbol(pos, symbol), "check_unexportsymbol2");

	internchar("AAA", "HELLO", &symbol);
	testnormal(check_unexportsymbol(pos, symbol), "check_unexportsymbol3");

	RETURN;
}

static int test_unexport_usedbylist(void)
{
	addr pos1, pos2, pos3, symbol, name, cons, bit;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");
	pushlist_package(pos1, PACKAGE_INDEX_USED, pos2);
	pushlist_package(pos1, PACKAGE_INDEX_USED, pos3);

	internchar("AAA", "HELLO", &symbol);
	strvect_char_heap(&name, "HELLO");

	/* package BBB */
	GetPackage(pos2, PACKAGE_INDEX_TABLE, &cons);
	intern_hashheap(cons, name, &cons);
	inheritedbitpackage(&bit, symbol);
	SetCdr(cons, bit);

	/* package CCC */
	GetPackage(pos3, PACKAGE_INDEX_TABLE, &cons);
	intern_hashheap(cons, name, &cons);
	importbitpackage(&bit, symbol);
	SetCdr(cons, bit);

	/* unexport */
	unexport_usedbylist(pos1, symbol);
	GetPackage(pos2, PACKAGE_INDEX_TABLE, &cons);
	findvalue_hashtable(cons, name, &cons);
	test(cons == Nil, "unexport_usedbylist1");
	GetPackage(pos3, PACKAGE_INDEX_TABLE, &cons);
	findvalue_hashtable(cons, name, &cons);
	test(cons != Nil, "unexport_usedbylist2");

	RETURN;
}

static int test_unexport_symboltype(void)
{
	addr pos, symbol, bit, cons;
	struct bittype_struct *ptr;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &symbol);
	testnormal(unexport_symboltype(pos, symbol), "unexport_symboltype1");

	export_package(pos, symbol);
	bit = findbit(pos, "HELLO");
	ptr = StructBitType(bit);
	test(ptr->expt, "unexport_symboltype2");
	test(ptr->base, "unexport_symboltype3");
	test(ptr->intern == PACKAGE_TYPE_EXTERNAL, "unexport_symboltype4");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 1, "unexport_symboltype4a");

	testnormal(unexport_symboltype(pos, symbol), "unexport_symboltype5");
	test(ptr->expt == 0, "unexport_symboltype6");
	test(ptr->base, "unexport_symboltype7");
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "unexport_symboltype8");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 0, "unexport_symboltype8a");

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(&pos, "BBB");
	internchar("BBB", "HELLO", &symbol);
	internpackage(&pos, "AAA");
	import_package(pos, symbol);

	export_package(pos, symbol);
	bit = findbit(pos, "HELLO");
	ptr = StructBitType(bit);
	test(ptr->expt, "unexport_symboltype9");
	test(ptr->base == 0, "unexport_symboltype10");
	test(ptr->import, "unexport_symboltype11");
	test(ptr->intern == PACKAGE_TYPE_EXTERNAL, "unexport_symboltype12");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 1, "unexport_symboltype12a");
	testnormal(unexport_symboltype(pos, symbol), "unexport_symboltype13");
	test(ptr->expt == 0, "unexport_symboltype14");
	test(ptr->base == 0, "unexport_symboltype15");
	test(ptr->import, "unexport_symboltype16");
	test(ptr->intern == PACKAGE_TYPE_INTERNAL, "unexport_symboltype17");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 0, "unexport_symboltype17a");

	RETURN;
}

static int test_unexportsymbol(void)
{
	addr pos, symbol, cons;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");
	internchar("AAA", "HELLO", &symbol);

	export_package(pos, symbol);
	test(StructBitType(findbit(pos, "HELLO"))->expt, "unexportsymbol1");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 1, "unexportsymbol2");
	unexportsymbol(pos, symbol);
	test(StructBitType(findbit(pos, "HELLO"))->expt == 0, "unexportsymbol3");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 0, "unexportsymbol4");

	RETURN;
}

static int test_unexportlist(void)
{
	addr pos, symbol, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos, "AAA");
	internpackage(NULL, "BBB");

	makestringlist(&cons, "HELLO", "AAA", NULL);
	testerror(unexportlist(pos, cons), "unexportlist1");

	internchar("BBB", "HELLO", &symbol);
	makelist(&cons, symbol, NULL);
	testerror(unexportlist(pos, cons), "unexportlist2");

	internchar("AAA", "HELLO", &symbol);
	export_package(pos, symbol);
	test(StructBitType(findbit(pos, "HELLO"))->expt, "unexportlist3");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 1, "unexportlist4");

	makelist(&cons, symbol, NULL);
	unexportlist(pos, cons);
	test(StructBitType(findbit(pos, "HELLO"))->expt == 0, "unexportlist5");
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &cons);
	test(listlength(cons) == 0, "unexportlist6");

	RETURN;
}

static int test_unexport_package(void)
{
	addr pos, symbol;

	force_delete_package("AAA");
	internpackage(&pos, "AAA");

	internchar("AAA", "HELLO", &symbol);
	export_package(pos, symbol);
	testnormal(unexport_package(pos, symbol), "unexport_package1");
	test(StructBitType(findbit(pos, "HELLO"))->expt == 0, "unexport_package2");

	testnormal(unexport_package(pos, symbol), "unexport_package3");

	RETURN;
}


/*
 *  use_package
 */
static int test_check_alreadyuse(void)
{
	addr pos1, pos2, pos3;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");
	test(check_alreadyuse(pos1, pos2) == 0, "check_alreadyuse1");
	pushlist_package(pos1, PACKAGE_INDEX_USE, pos2);
	test(check_alreadyuse(pos1, pos2), "check_alreadyuse2");
	test(check_alreadyuse(pos1, pos3) == 0, "check_alreadyuse3");
	pushlist_package(pos1, PACKAGE_INDEX_USE, pos3);
	test(check_alreadyuse(pos1, pos2), "check_alreadyuse4");
	test(check_alreadyuse(pos1, pos3), "check_alreadyuse5");
	test(check_alreadyuse(pos2, pos1) == 0, "check_alreadyuse6");
	test(check_alreadyuse(pos3, pos1) == 0, "check_alreadyuse7");

	RETURN;
}

static int test_check_useconflict(void)
{
	addr pos1, pos2, symbol;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internchar("AAA", "ZZZ", &symbol);
	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	testnormal(check_useconflict(pos1, pos2), "check_useconflict1");

	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	testerror(check_useconflict(pos1, pos2), "check_useconflict2");

	internchar("AAA", "HELLO", &symbol);
	shadow_package(pos1, symbol);
	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	testnormal(check_useconflict(pos1, pos2), "check_useconflict3");

	RETURN;
}

static int test_check_usepackage(void)
{
	addr pos1, pos2, symbol;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	testerror(check_usepackage(pos1, pos2), "check_usepackage1");

	pushlist_package(pos1, PACKAGE_INDEX_USE, pos2);
	testnormal(check_usepackage(pos1, pos2), "check_usepackage2");

	RETURN;
}

static int test_usepackageoperator(void)
{
	addr pos1, pos2, symbol, check;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");

	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	usepackageoperator(pos1, pos2);
	test(findbit(pos1, "HELLO") != Nil, "usepackageoperator1a");
	test(StructBitType(findbit(pos1, "HELLO"))->inherit, "usepackageoperator1b");

	GetPackage(pos1, PACKAGE_INDEX_USE, &check);
	test(listlength(check) == 1, "usepackageoperator2");
	GetCar(check, &check);
	test(check == pos2, "usepackageoperator3");

	GetPackage(pos2, PACKAGE_INDEX_USED, &check);
	test(listlength(check) == 1, "usepackageoperator4");
	GetCar(check, &check);
	test(check == pos1, "usepackageoperator5");

	RETURN;
}

static int test_check_usepackagelist(void)
{
	addr pos1, pos2, pos3, symbol, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	internchar("BBB", "HELLO", &symbol);
	internchar("CCC", "HELLO", &symbol);
	export_package(pos3, symbol);

	makelist(&cons, pos2, pos3, NULL);
	testnormal(check_usepackagelist(pos1, cons), "check_usepackagelist1");

	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	testerror(check_usepackagelist(pos1, cons), "check_usepackagelist2");

	internchar("AAA", "HELLO", &symbol);
	testerror(check_usepackagelist(pos1, cons), "check_usepackagelist3");

	internchar("AAA", "HELLO", &symbol);
	shadow_package(pos1, symbol);
	testnormal(check_usepackagelist(pos1, cons), "check_usepackagelist4");

	RETURN;
}

static int test_usepackagelist(void)
{
	addr pos1, pos2, pos3, key, symbol, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	fixnum_heap(&key, 100);
	makelist(&cons, pos2, key, pos3, NULL);
	testerror(usepackagelist(pos1, cons), "usepackagelist1");

	internchar("AAA", "HELLO", &symbol);
	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	makelist(&cons, pos2, pos3, NULL);
	testerror(usepackagelist(pos1, cons), "usepackagelist2");

	internchar("AAA", "HELLO", &symbol);
	unintern_package(pos1, symbol);
	internchar("CCC", "HELLO", &symbol);
	export_package(pos3, symbol);
	testerror(usepackagelist(pos1, cons), "usepackagelist4");

	internchar("AAA", "HELLO", &symbol);
	shadow_package(pos1, symbol);
	testnormal(usepackagelist(pos1, cons), "usepackagelist5");

	RETURN;
}

static int test_use_package(void)
{
	addr pos1, pos2, pos3, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	use_package(pos1, pos2);
	makestringlist(&cons, "CCC", NULL);
	use_package(pos1, cons);
	GetPackage(pos1, PACKAGE_INDEX_USE, &cons);
	test(listlength(cons) == 2, "use_package1");
	test(findeq(cons, pos2), "use_package2");
	test(findeq(cons, pos3), "use_package3");

	RETURN;
}


/*
 *  unuse_package
 */
static int test_check_uselist(void)
{
	addr pos1, pos2;

	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");

	test(check_uselist(pos1, pos2) == 0, "check_uselist1");
	use_package(pos1, pos2);
	test(check_uselist(pos1, pos2), "check_uselist2");
	test(check_uselist(pos2, pos1) == 0, "check_uselist3");

	RETURN;
}

static int test_unusepackageoperator(void)
{
	addr pos1, pos2, pos3, symbol, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	internchar("BBB", "HELLO", &symbol);
	export_package(pos2, symbol);
	makelist(&cons, pos2, pos3, NULL);
	use_package(pos1, cons);
	GetPackage(pos1, PACKAGE_INDEX_USE, &cons);
	test(listlength(cons) == 2, "unusepackageoperator1");
	test(findeq(cons, pos2), "unusepackageoperator2");
	test(findeq(cons, pos3), "unusepackageoperator3");
	test(findbit(pos1, "HELLO") != Nil, "unusepackageoperator4a");
	test(StructBitType(findbit(pos1, "HELLO"))->inherit, "unusepackageoperator4b");
	GetPackage(pos2, PACKAGE_INDEX_USED, &cons);
	test(listlength(cons) == 1, "unusepackageoperator5");
	test(findeq(cons, pos1), "unusepackageoperator6");

	unusepackageoperator(pos1, pos2);
	GetPackage(pos1, PACKAGE_INDEX_USE, &cons);
	test(listlength(cons) == 1, "unusepackageoperator7");
	test(findeq(cons, pos3), "unusepackageoperator8");
	test(findbit(pos1, "HELLO") == Nil, "unusepackageoperator9");
	GetPackage(pos2, PACKAGE_INDEX_USED, &cons);
	test(cons == Nil, "unusepackageoperator10");
	GetPackage(pos3, PACKAGE_INDEX_USED, &cons);
	test(cons != Nil, "unusepackageoperator11");

	RETURN;
}

static int test_unusepackagelist(void)
{
	addr pos1, pos2, pos3, symbol, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	fixnum_heap(&symbol, 100);
	makelist(&cons, symbol, NULL);
	testerror(unusepackagelist(pos1, cons), "unusepackagelist1");

	makelist(&cons, pos1, pos2, pos3, NULL);
	testnormal(unusepackagelist(pos1, cons), "unusepackagelist2");

	makestringlist(&cons, "AAA", "BBB", "CCC", NULL);
	use_package(pos1, cons);
	testnormal(unusepackagelist(pos1, cons), "unusepackagelist3");

	RETURN;
}

static int test_unuse_package(void)
{
	addr pos1, pos2, pos3, cons;

	force_delete_package("AAA");
	force_delete_package("BBB");
	force_delete_package("CCC");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	internpackage(&pos3, "CCC");

	use_package(pos1, pos2);
	unuse_package(pos1, pos2);

	makelist(&cons, pos1, pos2, pos3, NULL);
	use_package(pos1, cons);

	makelist(&cons, pos1, pos3, NULL);
	unuse_package(pos1, cons);
	GetPackage(pos1, PACKAGE_INDEX_USE, &cons);
	test(listlength(cons) == 1, "unuse_package1");

	RETURN;
}


/*
 *  build_package
 */
static int test_nil_t(void)
{
	addr check, package;

	build_package();

	internchar(LISP_COMMON, "NIL", &check);
	test(check == Nil, "nil_t1");
	internchar(LISP_COMMON, "T", &check);
	test(check == T, "nil_t2");

	find_char_package_(LISP_COMMON, &check);
	GetPackageSymbol(Nil, &package);
	test(check == package, "nil_t3");
	GetPackageSymbol(T, &package);
	test(check == package, "nil_t4");

	test(GetType(Nil) == LISPTYPE_NIL, "nil_t5");
	test(GetType(T) == LISPTYPE_T, "nil_t5");

	RETURN;
}


/*
 *  test
 */
static int test_usepackage_export(void)
{
	enum PACKAGE_TYPE type;
	addr pos1, pos2, symbol, value, temp;
	Execute ptr;

	ptr = Execute_Thread;
	/* export -> use-package */
	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	type = internchar("BBB", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_NIL, "usepackage_export1");

	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	export_package(pos2, symbol);
	type = internchar("BBB", "HELLO", &temp);
	test(type == PACKAGE_TYPE_EXTERNAL, "usepackage_export2");

	use_package(pos1, pos2);
	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INHERITED, "usepackage_export3");
	value = 0;
	getspecialcheck_local(ptr, symbol, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "usepackage_export4");

	/* use-package -> export */
	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");

	use_package(pos1, pos2);
	type = internchar("BBB", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_NIL, "usepackage_export5");
	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	export_package(pos2, symbol);
	type = internchar("BBB", "HELLO", &temp);
	test(type == PACKAGE_TYPE_EXTERNAL, "usepackage_export6");

	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INHERITED, "usepackage_export7");
	value = 0;
	getspecialcheck_local(ptr, symbol, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "usepackage_export8");

	RETURN;
}

static int test_usepackage_import(void)
{
	enum PACKAGE_TYPE type;
	addr pos1, pos2, symbol, value;
	Execute ptr;

	ptr = Execute_Thread;
	/* export -> use-package */
	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	type = internchar("BBB", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_NIL, "usepackage_import1");

	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	export_package(pos2, symbol);
	use_package(pos1, pos2);
	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INHERITED, "usepackage_import2");
	value = 0;
	getspecialcheck_local(ptr, symbol, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "usepackage_import3");
	/* import bbb::hello */
	type = internchar("BBB", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_EXTERNAL, "usepackage_import4");
	import_package(pos1, symbol);
	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INTERNAL, "usepackage_import5");

	/* export -> use-package */
	force_delete_package("AAA");
	force_delete_package("BBB");
	internpackage(&pos1, "AAA");
	internpackage(&pos2, "BBB");
	type = internchar("BBB", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_NIL, "usepackage_import6");

	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	export_package(pos2, symbol);
	use_package(pos1, pos2);
	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INHERITED, "usepackage_import7");
	value = 0;
	getspecialcheck_local(ptr, symbol, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "usepackage_import8");
	/* import aaa::hello */
	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INHERITED, "usepackage_import9");
	import_package(pos1, symbol);
	type = internchar("AAA", "HELLO", &symbol);
	test(type == PACKAGE_TYPE_INTERNAL, "usepackage_import10");

	RETURN;
}


/*
 *  main
 */
static int testbreak_package(void)
{
	TestBreak(test_packagetable);
	TestBreak(test_alloc_bitpackage);
	TestBreak(test_make_bitpackage);
	TestBreak(test_importbitpackage);
	TestBreak(test_inheritbitpackage);
	TestBreak(test_shadowintern_bitpackage);
	TestBreak(test_shadowimport_bitpackage);
	TestBreak(test_string_designer);
	TestBreak(test_findstringlocal);
	TestBreak(test_find_package);
	TestBreak(test_package_designer);
	TestBreak(test_package_heap);
	TestBreak(test_package_char_heap);
	//TestBreak(test_specialsymbol);
	TestBreak(test_interndefault);
	TestBreak(test_constantintern);
	TestBreak(test_set_default_package);
	TestBreak(test_initpackage);
	TestBreak(test_getpackage);
	/* make_package */
	TestBreak(test_pushlist_package);
	TestBreak(test_pushnewlist);
	TestBreak(test_check_nicknames);
	TestBreak(test_check_listconflict);
	TestBreak(test_check_first_usepackage);
	TestBreak(test_append_nicknames);
	TestBreak(test_append_exportname);
	TestBreak(test_append_usepackage);
	TestBreak(test_make_package);
	/* delete_package */
	TestBreak(test_delete_eqlist);
	TestBreak(test_remove_eqpackage);
	TestBreak(test_allunintern_inherited);
	TestBreak(test_allunintern_uselist);
	TestBreak(test_allunintern);
	TestBreak(test_delete_package);
	/* rename_package */
	TestBreak(test_check_renameone);
	TestBreak(test_check_rename);
	TestBreak(test_delete_renameone);
	TestBreak(test_delete_allnames);
	TestBreak(test_intern_renameone);
	TestBreak(test_intern_allnames);
	TestBreak(test_rename_package);
	/* package function */
	TestBreak(test_getname_package);
	TestBreak(test_getnickname_package);
	TestBreak(test_getuselist_package);
	TestBreak(test_getusedbylist_package);
	TestBreak(test_getshadow_package);
	/* find_symbol_package */
	TestBreak(test_find_bitpackage);
	TestBreak(test_find_symbol_package);
	/* find_allsymbols_package */
	TestBreak(test_push_basesymbol);
	TestBreak(test_find_allsymbols_package);
	/* list_all_packages */
	TestBreak(test_push_basepackage);
	TestBreak(test_list_all_packages);

	/* intern */
	TestBreak(test_intern_bitpackage);
	TestBreak(test_intern_package_table);
	TestBreak(test_intern_package);
	/* unintern */
	TestBreak(test_check_export_unintern);
	TestBreak(test_check_shadowing_unintern);
	TestBreak(test_uninterncheck);
	TestBreak(test_remove_package_unintern);
	TestBreak(test_intern_inherited_unintern);
	TestBreak(test_remove_shadowing_symbols);
	TestBreak(test_uninternsymbol);
	TestBreak(test_unintern);
	/* import */
	TestBreak(test_import_bitpackage);
	TestBreak(test_importsymbol);
	TestBreak(test_importlist);
	TestBreak(test_import_package);
	/* shadow */
	TestBreak(test_shadowsymbol);
	TestBreak(test_shadowlist);
	TestBreak(test_shadow_package);
	/* shadowing-import */
	TestBreak(test_shadowimportsymbol);
	TestBreak(test_shadowimportlist);
	TestBreak(test_shadowing_import_package);
	/* export */
	TestBreak(test_check_exportsymbol);
	TestBreak(test_exportsymbol_nocheck);
	TestBreak(test_exportsymbol);
	TestBreak(test_exportlist);
	TestBreak(test_export_package);
	/* unexport */
	TestBreak(test_delete_stringlist);
	TestBreak(test_remove_export_list);
	TestBreak(test_check_unexportsymbol);
	TestBreak(test_unexport_usedbylist);
	TestBreak(test_unexport_symboltype);
	TestBreak(test_unexportsymbol);
	TestBreak(test_unexportlist);
	TestBreak(test_unexport_package);
	/* use_pacakge */
	TestBreak(test_check_alreadyuse);
	TestBreak(test_check_useconflict);
	TestBreak(test_check_usepackage);
	TestBreak(test_usepackageoperator);
	TestBreak(test_check_usepackagelist);
	TestBreak(test_usepackagelist);
	TestBreak(test_use_package);
	/* unuse_pacakge */
	TestBreak(test_check_uselist);
	TestBreak(test_unusepackageoperator);
	TestBreak(test_unusepackagelist);
	TestBreak(test_unuse_package);
	/* build_package */
	TestBreak(test_nil_t);
	/* test */
	TestBreak(test_usepackage_export);
	TestBreak(test_usepackage_import);

	return 0;
}
#endif

int test_package(void)
{
#if 0
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_reader();
		lisp_initialize = 1;
		result = testbreak_package();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
#endif
	return 0;
}

