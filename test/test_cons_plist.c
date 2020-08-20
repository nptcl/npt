#include "cons_plist.c"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "package.h"
#include "package_symbol.h"
#include "sequence.h"
#include "type.h"
#include "type_table.h"

static int test_getplist(void)
{
	addr list, a, b, c, d, e, f, pos;

	test(getplist(Nil, T, &pos), "getplist.1");
	test(getplist(Nil, Nil, &pos), "getplist.2");
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	fixnum_heap(&d, 40);
	fixnum_heap(&e, 50);
	fixnum_heap(&f, 60);
	list_heap(&list, a, b, c, d, e, f, NULL);
	test(getplist(list, a, &pos) == 0, "getplist.3");
	test(pos == b, "getplist.4");
	test(getplist(list, b, &pos), "getplist.5");
	test(getplist(list, c, &pos) == 0, "getplist.6");
	test(pos == d, "getplist.7");
	test(getplist(list, e, &pos) == 0, "getplist.8");
	test(pos == f, "getplist.9");
	test(getplist(list, T, &pos), "getplist.10");

	RETURN;
}

static int test_getplist_safe(void)
{
	addr list, a, b, c, d, e, f, pos;

	test(getplist_safe(Nil, T, &pos), "getplist_safe.1");
	test(getplist_safe(Nil, Nil, &pos), "getplist_safe.2");
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	fixnum_heap(&d, 40);
	fixnum_heap(&e, 50);
	fixnum_heap(&f, 60);
	list_heap(&list, a, b, c, d, e, f, NULL);
	test(getplist_safe(list, a, &pos) == 0, "getplist_safe.3");
	test(pos == b, "getplist_safe.4");
	test(getplist_safe(list, b, &pos), "getplist_safe.5");
	test(getplist_safe(list, c, &pos) == 0, "getplist_safe.6");
	test(pos == d, "getplist_safe.7");
	test(getplist_safe(list, e, &pos) == 0, "getplist_safe.8");
	test(pos == f, "getplist_safe.9");
	test(getplist_safe(list, T, &pos), "getplist_safe.10");

	RETURN;
}

static int test_setplist_alloc(void)
{
	addr pos, list, check, sym1, sym2, sym3, sym4;

	internchar_debug(LISP_PACKAGE, "HELLO1", &sym1);
	internchar_debug(LISP_PACKAGE, "HELLO2", &sym2);
	internchar_debug(LISP_PACKAGE, "HELLO3", &sym3);
	internchar_debug(LISP_PACKAGE, "HELLO4", &sym4);
	test(setplist_alloc(NULL, Nil, sym1, sym2, &list), "setplist_alloc.1");
	test(GetType(list) == LISPTYPE_CONS, "setplist_alloc.2");
	GetCons(list, &pos, &check);
	test(pos == sym1, "setplist_alloc.3");
	GetCons(check, &pos, &check);
	test(pos == sym2, "setplist_alloc.4");
	test(check == Nil, "setplist_alloc.5");
	test(setplist_alloc(NULL, list, sym1, T, &list) == 0, "setplist_alloc.6");
	test(setplist_alloc(NULL, list, sym3, sym4, &list), "setplist_alloc.7");
	test(getplist(list, sym1, &pos) == 0, "setplist_alloc.8");
	test(pos == T, "setplist_alloc.9");
	test(getplist(list, sym3, &pos) == 0, "setplist_alloc.10");
	test(pos == sym4, "setplist_alloc.11");
	test(getplist(list, sym4, &pos), "setplist_alloc.12");

	RETURN;
}

static int test_setplist_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	test(setplist_local(local, Nil, T, Nil, &pos), "setplist_local.1");
	test(GetType(pos) == LISPTYPE_CONS, "setplist_local.2");
	test(GetStatusDynamic(pos), "setplist_local.3");
	rollback_local(local, stack);

	RETURN;
}

static int test_setplist_heap(void)
{
	addr pos;

	test(setplist_heap(Nil, T, Nil, &pos), "setplist_heap.1");
	test(GetType(pos) == LISPTYPE_CONS, "setplist_heap.2");
	test(! GetStatusDynamic(pos), "setplist_heap.3");

	RETURN;
}

static int test_setplist_error(void)
{
	addr list, pos, key1, key2, key3;

	list = Nil;
	fixnum_heap(&key1, 10);
	setplist_heap(list, key1, key1, &list);
	fixnum_heap(&key2, 20);
	setplist_heap(list, key2, key2, &list);
	fixnum_heap(&key3, 30);
	setplist_heap(list, key3, key3, &list);
	test(getplist(list, key1, &pos) == 0, "setplist_error.1");
	test(getplist(list, key2, &pos) == 0, "setplist_error.2");
	test(getplist(list, key3, &pos) == 0, "setplist_error.3");
	test(getplist(list, T, &pos), "setplist_error.4");

	RETURN;
}

static int test_pushnewplist_alloc(void)
{
	addr pos, list, check, sym1, sym2, sym3, sym4;

	internchar_debug(LISP_PACKAGE, "HELLO1", &sym1);
	internchar_debug(LISP_PACKAGE, "HELLO2", &sym2);
	internchar_debug(LISP_PACKAGE, "HELLO3", &sym3);
	internchar_debug(LISP_PACKAGE, "HELLO4", &sym4);
	test(pushnewplist_alloc(NULL, Nil, sym1, sym2, &list), "pushnewplist_alloc.1");
	test(GetType(list) == LISPTYPE_CONS, "pushnewplist_alloc.2");
	GetCons(list, &pos, &check);
	test(pos == sym1, "pushnewplist_alloc.3");
	GetCons(check, &pos, &check);
	test(check == Nil, "pushnewplist_alloc.4");
	GetCons(pos, &pos, &check);
	test(pos == sym2, "pushnewplist_alloc.5");
	test(check == Nil, "pushnewplist_alloc.6");

	test(! pushnewplist_alloc(NULL, list, sym1, T, &list), "pushnewplist_alloc.7");
	test(! pushnewplist_alloc(NULL, list, sym1, T, &list), "pushnewplist_alloc.8");
	test(! pushnewplist_alloc(NULL, list, sym1, sym2, &list), "pushnewplist_alloc.9");
	test(! pushnewplist_alloc(NULL, list, sym1, sym2, &list), "pushnewplist_alloc.10");
	test(getplist(list, sym1, &pos) == 0, "pushnewplist_alloc.11");
	GetCons(pos, &check, &pos);
	test(check == T, "pushnewplist_alloc.12");
	GetCons(pos, &check, &pos);
	test(check == sym2, "pushnewplist_alloc.13");
	test(pos == Nil, "pushnewplist_allo.14");

	test(pushnewplist_alloc(NULL, list, sym3, sym4, &list), "pushnewplist_alloc.15");
	test(getplist(list, sym1, &pos) == 0, "pushnewplist_alloc.16");
	test(getplist(list, sym3, &pos) == 0, "pushnewplist_alloc.17");
	test(getplist(list, sym4, &pos), "pushnewplist_alloc.18");

	RETURN;
}

static int test_pushnewplist_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	test(pushnewplist_local(local, Nil, T, Nil, &pos), "pushnewplist_local1");
	test(GetType(pos) == LISPTYPE_CONS, "pushnewplist_local2");
	test(GetStatusDynamic(pos), "pushnewplist_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_pushnewplist_heap(void)
{
	addr pos;

	test(pushnewplist_heap(Nil, T, Nil, &pos), "pushnewplist_heap1");
	test(GetType(pos) == LISPTYPE_CONS, "pushnewplist_heap2");
	test(! GetStatusDynamic(pos), "pushnewplist_heap3");

	RETURN;
}

static enum RemPlist remplist_debug(addr list, addr key, addr *ret)
{
	enum RemPlist value;
	Error(remplist_safe_(list, key, ret, &value));
	return value;
}

static int test_remplist_safe(void)
{
	addr pos, key, key1, key2, key3, value, list;

	GetConstant(CONSTANT_COMMON_SPECIAL, &key);
	test(remplist_debug(Nil, key, &pos) == RemPlist_NotFound, "remplist_safe.1");
	test(pos == Nil, "remplist_safe.2");

	list_heap(&list, key, T, NULL);
	fixnum_heap(&key1, 10);
	test(remplist_debug(list, key1, &pos) == RemPlist_NotFound, "remplist_safe.3");
	test(pos == list, "remplist_safe.4");
	test(remplist_debug(list, key, &pos) == RemPlist_Update, "remplist_safe.5");
	test(pos == Nil, "remplist_safe.6");

	interncommon_debug("CAR", &key1);
	interncommon_debug("CDR", &key2);
	interncommon_debug("CONS", &key3);
	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_debug(list, T, &pos) == RemPlist_NotFound, "remplist_safe.7");
	test(pos == list, "remplist_safe.8");
	test(remplist_debug(list, key1, &pos) == RemPlist_Update, "remplist_safe.9");
	test(length_list_unsafe(pos) == 4, "remplist_safe.10");
	test(getplist(pos, key1, &value), "remplist_safe.11");
	test(getplist(pos, key2, &value) == 0, "remplist_safe.12");
	test(RefFixnum(value) == 20, "remplist_safe.13");
	test(getplist(pos, key3, &value) == 0, "remplist_safe.14");
	test(RefFixnum(value) == 30, "remplist_safe.15");

	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_debug(list, key2, &pos) == RemPlist_Delete, "remplist_safe.16");
	test(length_list_unsafe(pos) == 4, "remplist_safe.17");
	test(getplist(pos, key1, &value) == 0, "remplist_safe.18");
	test(RefFixnum(value) == 10, "remplist_safe.19");
	test(getplist(pos, key2, &value), "remplist_safe.20");
	test(getplist(pos, key3, &value) == 0, "remplist_safe.21");
	test(RefFixnum(value) == 30, "remplist_safe.22");

	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_debug(list, key3, &pos) == RemPlist_Delete, "remplist_safe.23");
	test(length_list_unsafe(pos) == 4, "remplist_safe.24");
	test(getplist(pos, key1, &value) == 0, "remplist_safe.25");
	test(RefFixnum(value) == 10, "remplist_safe.26");
	test(getplist(pos, key2, &value) == 0, "remplist_safe.27");
	test(RefFixnum(value) == 20, "remplist_safe.28");
	test(getplist(pos, key3, &value), "remplist_safe.29");

	RETURN;
}

static int test_remplist_check(void)
{
	addr pos, key, key1, key2, key3, value, list;

	GetConstant(CONSTANT_COMMON_SPECIAL, &key);
	test(remplist_check(Nil, key, &pos) == RemPlist_NotFound, "remplist_check.1");
	test(pos == Nil, "remplist_check.2");

	list_heap(&list, key, T, NULL);
	fixnum_heap(&key1, 10);
	test(remplist_check(list, key1, &pos) == RemPlist_NotFound, "remplist_check.3");
	test(pos == list, "remplist_check.4");
	test(remplist_check(list, key, &pos) == RemPlist_Update, "remplist_check.5");
	test(pos == Nil, "remplist_check.6");

	interncommon_debug("CAR", &key1);
	interncommon_debug("CDR", &key2);
	interncommon_debug("CONS", &key3);
	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_check(list, T, &pos) == RemPlist_NotFound, "remplist_check.7");
	test(pos == list, "remplist_check.8");
	test(remplist_check(list, key1, &pos) == RemPlist_Update, "remplist_check.9");
	test(length_list_unsafe(pos) == 4, "remplist_check.10");
	test(getplist(pos, key1, &value), "remplist_check.11");
	test(getplist(pos, key2, &value) == 0, "remplist_check.12");
	test(RefFixnum(value) == 20, "remplist_check.13");
	test(getplist(pos, key3, &value) == 0, "remplist_check.14");
	test(RefFixnum(value) == 30, "remplist_check.15");

	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_check(list, key2, &pos) == RemPlist_Delete, "remplist_check.16");
	test(length_list_unsafe(pos) == 4, "remplist_check.17");
	test(getplist(pos, key1, &value) == 0, "remplist_check.18");
	test(RefFixnum(value) == 10, "remplist_check.19");
	test(getplist(pos, key2, &value), "remplist_check.20");
	test(getplist(pos, key3, &value) == 0, "remplist_check.21");
	test(RefFixnum(value) == 30, "remplist_check.22");

	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_check(list, key3, &pos) == RemPlist_Delete, "remplist_check.23");
	test(length_list_unsafe(pos) == 4, "remplist_check.24");
	test(getplist(pos, key1, &value) == 0, "remplist_check.25");
	test(RefFixnum(value) == 10, "remplist_check.26");
	test(getplist(pos, key2, &value) == 0, "remplist_check.27");
	test(RefFixnum(value) == 20, "remplist_check.28");
	test(getplist(pos, key3, &value), "remplist_check.29");

	RETURN;
}

static int test_remplist(void)
{
	addr pos, list, key1, key2, key3;

	pos = NULL;
	test(! remplist(Nil, T, &pos), "remplist.1");
	test(pos == Nil, "remplist.2");

	interncommon_debug("CAR", &key1);
	interncommon_debug("CDR", &key2);
	interncommon_debug("CONS", &key3);
	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist(list, key1, &pos), "remplist.3");
	test(! remplist(pos, key3, &pos), "remplist.4");

	RETURN;
}

static int test_getplist_constant(void)
{
	addr pos, list, sym1, sym2, sym3, sym4;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);
	GetConstant(CONSTANT_COMMON_EQUALP, &sym4);
	list_heap(&list, sym1, sym2, sym3, sym4, NULL);
	test(getplist_constant(list, CONSTANT_COMMON_EQUAL, &pos) == 0,
			"getplist_constant.1");
	test(pos == sym4, "getplist_constant.2");

	RETURN;
}

static int test_setplist_constant_alloc(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(setplist_constant_alloc(NULL, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"setplist_constant_alloc.1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"setplist_constant_alloc.2");
	test(pos == symbol, "setplist_constant_alloc.3");

	RETURN;
}

static int test_setplist_constant_local(void)
{
	addr pos, list, symbol;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(setplist_constant_local(local, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"setplist_constant_local.1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"setplist_constant_local.2");
	test(pos == symbol, "setplist_constant_local.3");
	rollback_local(local, stack);

	RETURN;
}

static int test_setplist_constant_heap(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(setplist_constant_heap(Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"setplist_constant_heap.1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"setplist_constant_heap.2");
	test(pos == symbol, "setplist_constant_heap.3");

	RETURN;
}

static int test_remplist_constant(void)
{
	int result;
	addr pos;

	GetConstant(CONSTANT_COMMON_SPECIAL, &pos);
	list_heap(&pos, pos, T, NULL);
	result = remplist_constant(pos, CONSTANT_COMMON_SPECIAL, &pos);
	test(result, "remplist_constant.1");
	test(pos == Nil, "remplist_constant.2");

	pos = NULL;
	result = remplist_constant(Nil, CONSTANT_COMMON_SPECIAL, &pos);
	test(! result, "remplist_constant.3");
	test(pos == Nil, "remplist_constant.4");

	RETURN;
}

static int test_getpplist(void)
{
	addr list, sym1, sym2, sym3, pos;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);
	list_heap(&list, sym1, sym2, NULL);
	list_heap(&list, sym3, list, sym1, Nil, NULL);
	test(getpplist(list, sym3, sym1, &pos) == 0, "getpplist.1");
	test(pos == sym2, "getpplist.2");
	test(getpplist(list, sym3, sym2, &pos), "getpplist.3");
	test(getpplist(list, sym1, sym2, &pos), "getpplist.4");
	test(getpplist(list, sym2, sym2, &pos), "getpplist.5");

	RETURN;
}

static int test_setpplist_alloc(void)
{
	addr list, pos, sym1, sym2, sym3, sym4;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);
	GetConstant(CONSTANT_COMMON_EQUALP, &sym4);

	test(setpplist_alloc(NULL, Nil, sym1, sym2, sym3, &list), "setpplist_alloc.1");
	test(getpplist(list, sym1, sym2, &pos) == 0, "setpplist_alloc.2");
	test(pos == sym3, "setpplist_alloc.3");

	test(setpplist_alloc(NULL, list, sym1, sym2, sym4, &list) == 0, "setpplist_alloc.4");
	test(getpplist(list, sym1, sym2, &pos) == 0, "setpplist_alloc.5");
	test(pos == sym4, "setpplist_alloc.6");

	test(setpplist_alloc(NULL, list, sym1, sym3, sym1, &list) == 0, "setpplist_alloc.7");
	test(getpplist(list, sym1, sym2, &pos) == 0, "setpplist_alloc.8");
	test(pos == sym4, "setpplist_alloc.9");
	test(getpplist(list, sym1, sym3, &pos) == 0, "setpplist_alloc.10");
	test(pos == sym1, "setpplist_alloc.11");

	test(setpplist_alloc(NULL, list, sym2, sym3, sym1, &list), "setpplist_alloc.12");
	test(getpplist(list, sym2, sym3, &pos) == 0, "setpplist_alloc.13");
	test(pos == sym1, "setpplist_alloc.14");

	test(getpplist(list, sym1, sym2, &pos) == 0, "setpplist_alloc.15");
	test(pos == sym4, "setpplist_alloc.16");

	RETURN;
}

static int test_setpplist_local(void)
{
	addr list, pos, sym1, sym2, sym3;
	LocalRoot local;
	LocalStack stack;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);

	local = Local_Thread;
	push_local(local, &stack);
	test(setpplist_local(local, Nil, sym1, sym2, sym3, &list), "setpplist_local.1");
	test(getpplist(list, sym1, sym2, &pos) == 0, "setpplist_local.2");
	test(pos == sym3, "setpplist_local.3");
	rollback_local(local, stack);

	RETURN;
}

static int test_setpplist_heap(void)
{
	addr list, pos, sym1, sym2, sym3;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);

	test(setpplist_heap(Nil, sym1, sym2, sym3, &list), "setpplist_heap.1");
	test(getpplist(list, sym1, sym2, &pos) == 0, "setpplist_heap.2");
	test(pos == sym3, "setpplist_heap.3");

	RETURN;
}


/*
 *  cons_plist
 */
static int testcase_cons_plist(void)
{
	TestBreak(test_getplist);
	TestBreak(test_getplist_safe);
	TestBreak(test_setplist_alloc);
	TestBreak(test_setplist_local);
	TestBreak(test_setplist_heap);
	TestBreak(test_setplist_error);
	TestBreak(test_pushnewplist_alloc);
	TestBreak(test_pushnewplist_local);
	TestBreak(test_pushnewplist_heap);
	TestBreak(test_remplist_safe);
	TestBreak(test_remplist_check);
	TestBreak(test_remplist);
	TestBreak(test_getplist_constant);
	TestBreak(test_setplist_constant_alloc);
	TestBreak(test_setplist_constant_local);
	TestBreak(test_setplist_constant_heap);
	TestBreak(test_remplist_constant);
	TestBreak(test_getpplist);
	TestBreak(test_setpplist_alloc);
	TestBreak(test_setpplist_local);
	TestBreak(test_setpplist_heap);

	return 0;
}

static void testinit_cons_plist(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_package();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_common();
}

int test_cons_plist(void)
{
	DegradeTitle;
	return DegradeCode(cons_plist);
}

