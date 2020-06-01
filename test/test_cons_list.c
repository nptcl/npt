#include "cons_list.c"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "package.h"
#include "sequence.h"
#include "type.h"
#include "type_table.h"

static int test_length_list_safe(void)
{
	addr cons;

	test(length_list_safe(Nil) == 0, "length_list_safe.1");
	conscar_heap(&cons, T);
	test(length_list_safe(cons) == 1, "length_list_safe.2");
	list_heap(&cons, T, Nil, Nil, T, NULL);
	test(length_list_safe(cons) == 4, "length_list_safe.3");

	RETURN;
}

static int test_length_list_unsafe(void)
{
	addr cons;

	test(length_list_unsafe(Nil) == 0, "length_list_unsafe.1");
	conscar_heap(&cons, T);
	test(length_list_unsafe(cons) == 1, "length_list_unsafe.2");
	list_heap(&cons, T, Nil, Nil, T, NULL);
	test(length_list_unsafe(cons) == 4, "length_list_unsafe.3");

	RETURN;
}

static int test_append2_alloc_unsafe(void)
{
	addr check, cons, next, v1, v2, v3, v4, v5;

	append2_alloc_unsafe(NULL, Nil, Nil, &check);
	test(check == Nil, "append2_alloc_unsafe.1");
	consnil_heap(&cons);
	append2_alloc_unsafe(NULL, cons, Nil, &check);
	test(check == cons, "append2_alloc_unsafe.2");
	append2_alloc_unsafe(NULL, Nil, cons, &check);
	test(check == cons, "append2_alloc_unsafe.3");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	fixnum_heap(&v5, 50);

	list_heap(&cons, v1, NULL);
	list_heap(&next, v3, v4, v5, NULL);
	append2_alloc_unsafe(NULL, cons, next, &check);
	test(check != cons, "append2_alloc_unsafe.4");
	test(check != next, "append2_alloc_unsafe.5");
	GetCons(check, &cons, &check);
	test(cons == v1, "append2_alloc_unsafe.6");
	test(check == next, "append2_alloc_unsafe.7");

	list_heap(&cons, v1, v2, NULL);
	list_heap(&next, v3, v4, v5, NULL);
	append2_alloc_unsafe(NULL, cons, next, &check);
	test(check != cons, "append2_alloc_unsafe.8");
	test(check != next, "append2_alloc_unsafe.9");
	GetCons(check, &cons, &check);
	test(cons == v1, "append2_alloc_unsafe.10");
	GetCons(check, &cons, &check);
	test(cons == v2, "append2_alloc_unsafe.11");
	test(check == next, "append2_alloc_unsafe.12");

	RETURN;
}

static int test_find_list_eq_unsafe(void)
{
	addr cons, value1, value2, value3;

	test(find_list_eq_unsafe(T, Nil) == 0, "find_list_eq_unsafe.1");

	consnil_heap(&cons);
	test(find_list_eq_unsafe(T, cons) == 0, "find_list_eq_unsafe.2");
	conscar_heap(&cons, T);
	test(find_list_eq_unsafe(T, cons), "find_list_eq_unsafe.3");

	fixnum_heap(&value1, 1);
	fixnum_heap(&value2, 22);
	fixnum_heap(&value3, 333);
	list_heap(&cons, value3, value2, value1, NULL);

	test(find_list_eq_unsafe(Nil, cons) == 0, "find_list_eq_unsafe.4");
	test(find_list_eq_unsafe(T, cons) == 0, "find_list_eq_unsafe.5");
	test(find_list_eq_unsafe(value1, cons), "find_list_eq_unsafe.6");
	test(find_list_eq_unsafe(value2, cons), "find_list_eq_unsafe.7");
	test(find_list_eq_unsafe(value3, cons), "find_list_eq_unsafe.8");
	make_fixnum_heap(&value2, 22);
	test(find_list_eq_unsafe(value2, cons) == 0, "find_list_eq_unsafe.9");

	RETURN;
}

static int test_pushnew_alloc(void)
{
	addr sym1, sym2, sym3, list, check;

	interncommon("CAR", &sym1);
	interncommon("CDR", &sym2);
	interncommon("CONS", &sym3);
	test(pushnew_alloc(NULL, Nil, sym1, &list), "pushnew_alloc.1");
	test(length_list_safe(list) == 1, "pushnew_alloc.2");
	GetCar(list, &check);
	test(check == sym1, "pushnew_alloc.3");

	test(pushnew_alloc(NULL, list, sym2, &list), "pushnew_alloc.4");
	test(length_list_safe(list) == 2, "pushnew_alloc.5");
	GetCar(list, &check);
	test(check == sym2, "pushnew_alloc.6");

	test(pushnew_alloc(NULL, list, sym2, &list) == 0, "pushnew_alloc.7");
	test(pushnew_alloc(NULL, list, sym1, &list) == 0, "pushnew_alloc.8");
	test(length_list_safe(list) == 2, "pushnew_alloc.9");

	RETURN;
}


/*
 *  reverse
 */
static int test_nreconc_unsafe(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	/* nil */
	result = 0;
	nreconc_unsafe(&result, Nil, T);
	test(result == T, "nreconc_unsafe.1");

	/* single */
	fixnum_heap(&v1, 10);
	list_heap(&root, v1, NULL);
	nreconc_unsafe(&result, root, T);
	test(result == root, "nreconc_unsafe.2");
	GetCons(result, &result, &right);
	test(result == v1, "nreconc_unsafe.3");
	test(right == T, "nreconc_unsafe.4");

	/* list2 */
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	list_heap(&root, v1, v2, NULL);
	nreconc_unsafe(&result, root, T);
	test(result != root, "nreconc_unsafe.5");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "nreconc_unsafe.6");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "nreconc_unsafe.7");
	test(right == T, "nreconc_unsafe.8");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	nreconc_unsafe(&result, root, T);
	test(result != root, "nreconc_unsafe.9");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "nreconc_unsafe.10");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "nreconc_unsafe.11");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "nreconc_unsafe.12");
	test(right == T, "nreconc_unsafe.13");

	RETURN;
}

static int test_nreverse_list_unsafe(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	/* nil */
	result = 0;
	nreverse_list_unsafe(&result, Nil);
	test(result == Nil, "nreverse_list_unsafe.1");

	/* single */
	consnil_heap(&root);
	SetCar(root, T);
	nreverse_list_unsafe(&result, root);
	test(result == root, "nreverse_list_unsafe.2");

	/* list2 */
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	list_heap(&root, v1, v2, NULL);
	nreverse_list_unsafe(&result, root);
	test(result != root, "nreverse_list_unsafe.3");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "nreverse_list_unsafe.4");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "nreverse_list_unsafe.5");
	test(right == Nil, "nreverse_list_unsafe.6");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	nreverse_list_unsafe(&result, root);
	test(result != root, "nreverse_list_unsafe.7");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "nreverse_list_unsafe.8");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "nreverse_list_unsafe.9");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "nreverse_list_unsafe.10");
	test(right == Nil, "nreverse_list_unsafe.11");

	RETURN;
}

static int test_reverse_list_heap_unsafe(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	/* nil */
	result = 0;
	reverse_list_heap_unsafe(&result, Nil);
	test(result == Nil, "reverse_list_heap_unsafe.1");

	/* single */
	consnil_heap(&root);
	SetCar(root, T);
	reverse_list_heap_unsafe(&result, root);
	test(result != root, "reverse_list_heap_unsafe.2");

	/* list2 */
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	list_heap(&root, v1, v2, NULL);
	reverse_list_heap_unsafe(&result, root);
	test(result != root, "reverse_list_heap_unsafe.3");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "reverse_list_heap_unsafe.4");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "reverse_list_heap_unsafe.5");
	test(right == Nil, "reverse_list_heap_unsafe.6");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	reverse_list_heap_unsafe(&result, root);
	test(result != root, "reverse_list_heap_unsafe.7");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "reverse_list_heap_unsafe.8");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "reverse_list_heap_unsafe.9");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "reverse_list_heap_unsafe.10");
	test(right == Nil, "reverse_list_heap_unsafe.11");
	test(! GetStatusDynamic(result), "reverse_list_heap_unsafe.12");

	RETURN;
}

static int test_reverse_list_local_unsafe(void)
{
	addr left, right, root, result;
	fixnum value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* nil */
	result = 0;
	reverse_list_local_unsafe(local, &result, Nil);
	test(result == Nil, "reverse_list_local_unsafe.1");

	/* single */
	consnil_local(local, &root);
	SetCar(root, T);
	reverse_list_local_unsafe(local, &result, root);
	test(result != root, "reverse_list_local_unsafe.2");

	/* list2 */
	consnil_local(local, &root);
	fixnum_local(local, &left, 10);
	consnil_local(local, &right);
	SetCons(root, left, right);
	fixnum_local(local, &left, 20);
	SetCar(right, left);
	reverse_list_local_unsafe(local, &result, root);
	test(result != root, "reverse_list_local_unsafe.3");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "reverse_list_local_unsafe.4");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "reverse_list_local_unsafe.5");
	test(right == Nil, "reverse_list_local_unsafe.6");

	/* list3 */
	consnil_local(local, &root);
	fixnum_local(local, &left, 100);
	consnil_local(local, &right);
	SetCons(root, left, right);
	fixnum_local(local, &left, 200);
	consnil_local(local, &result);
	SetCons(right, left, result);
	fixnum_local(local, &left, 300);
	SetCar(result, left);
	result = 0;
	reverse_list_alloc_unsafe(local, &result, root);
	test(result != root, "reverse_list_local_unsafe.7");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "reverse_list_local_unsafe.8");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "reverse_list_local_unsafe.9");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "reverse_list_local_unsafe.10");
	test(right == Nil, "reverse_list_local_unsafe.11");
	test(GetStatusDynamic(result), "reverse_list_local_unsafe.12");

	/* local */
	rollback_local(local, stack);

	RETURN;
}


/*
 *  unsafe
 */
static int test_copy_list_alloc_unsafe(void)
{
	addr cons, root, left, value1, value2, value3, check;

	copy_list_alloc_unsafe(NULL, &cons, Nil);
	test(cons == Nil, "copy_list_alloc_unsafe.1");

	conscar_heap(&cons, T);
	copy_list_alloc_unsafe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_unsafe.2");
	test(root != Nil, "copy_list_alloc_unsafe.3");
	GetCons(root, &left, &root);
	test(left == T, "copy_list_alloc_unsafe.4");
	test(root == Nil, "copy_list_alloc_unsafe.5");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value3, value2, value1, NULL);
	copy_list_alloc_unsafe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_unsafe.6");
	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_unsafe.7");
	test(left == value3, "copy_list_alloc_unsafe.8");
	test(check == value3, "copy_list_alloc_unsafe.9");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_unsafe.10");
	test(left == value2, "copy_list_alloc_unsafe.11");
	test(check == value2, "copy_list_alloc_unsafe.12");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(left == value1, "copy_list_alloc_unsafe.13");
	test(check == value1, "copy_list_alloc_unsafe.14");
	test(cons == Nil, "copy_list_alloc_unsafe.15");
	test(root == Nil, "copy_list_alloc_unsafe.16");

	RETURN;
}

static int test_copy_list_heap_unsafe(void)
{
	addr cons;

	consnil_heap(&cons);
	copy_list_heap_unsafe(&cons, cons);
	test(GetType(cons) == LISPTYPE_CONS, "copy_list_heap_unsafe.1");
	test(! GetStatusDynamic(cons), "copy_list_heap_unsafe.2");

	RETURN;
}

static int test_copy_list_local_unsafe(void)
{
	addr cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	consnil_local(local, &cons);
	copy_list_local_unsafe(local, &cons, cons);
	test(GetType(cons) == LISPTYPE_CONS, "copy_list_local_unsafe.1");
	test(GetStatusDynamic(cons), "copy_list_local_unsafe.2");
	rollback_local(local, stack);

	RETURN;
}

static int test_copy_list_alloc_safe(void)
{
	addr cons, root, left, value1, value2, value3, check;

	copy_list_alloc_safe(NULL, &cons, Nil);
	test(cons == Nil, "copy_list_alloc_safe.1");

	conscar_heap(&cons, T);
	copy_list_alloc_safe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_safe.2");
	test(root != Nil, "copy_list_alloc_safe.3");
	GetCons(root, &left, &root);
	test(left == T, "copy_list_alloc_safe.4");
	test(root == Nil, "copy_list_alloc_safe.5");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value3, value2, value1, NULL);
	copy_list_alloc_safe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_safe.6");
	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_safe.7");
	test(left == value3, "copy_list_alloc_safe.8");
	test(check == value3, "copy_list_alloc_safe.9");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_safe.10");
	test(left == value2, "copy_list_alloc_safe.11");
	test(check == value2, "copy_list_alloc_safe.12");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(left == value1, "copy_list_alloc_safe.13");
	test(check == value1, "copy_list_alloc_safe.14");
	test(cons == Nil, "copy_list_alloc_safe.15");
	test(root == Nil, "copy_list_alloc_safe.16");

	copy_list_alloc_safe(NULL, &cons, T);
	test(cons == T, "copy_list_alloc_safe.17");

	lista_heap(&cons, value1, value2, NULL);
	copy_list_alloc_safe(NULL, &cons, cons);
	GetCons(cons, &left, &cons);
	test(left == value1, "copy_list_alloc_safe.18");
	test(cons == value2, "copy_list_alloc_safe.19");

	lista_heap(&cons, value1, value2, value3, NULL);
	copy_list_alloc_safe(NULL, &cons, cons);
	GetCons(cons, &left, &cons);
	test(left == value1, "copy_list_alloc_safe.20");
	GetCons(cons, &left, &cons);
	test(left == value2, "copy_list_alloc_safe.21");
	test(cons == value3, "copy_list_alloc_safe.22");

	RETURN;
}


/*
 *  cons_list
 */
static int testcase_cons_list(void)
{
	TestBreak(test_length_list_safe);
	TestBreak(test_length_list_unsafe);
	TestBreak(test_append2_alloc_unsafe);
	TestBreak(test_find_list_eq_unsafe);
	TestBreak(test_pushnew_alloc);
	TestBreak(test_nreconc_unsafe);
	TestBreak(test_nreverse_list_unsafe);
	TestBreak(test_reverse_list_heap_unsafe);
	TestBreak(test_reverse_list_local_unsafe);
	TestBreak(test_copy_list_alloc_unsafe);
	TestBreak(test_copy_list_heap_unsafe);
	TestBreak(test_copy_list_local_unsafe);
	TestBreak(test_copy_list_alloc_safe);

	return 0;
}

static void testinit_cons_list(Execute ptr)
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

int test_cons_list(void)
{
	TITLE;
	return degrade_code(
			testinit_cons_list,
			testcase_cons_list);
}

