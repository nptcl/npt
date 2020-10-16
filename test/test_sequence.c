#include "sequence.c"
#include "cons.h"
#include "degrade.h"

static int test_make_vector_from_list(void)
{
	addr pos, check;

	make_vector_from_list_(&pos, Nil);
	test(GetType(pos) == LISPTYPE_VECTOR, "make_vector_from_list.1");
	test(lenarrayr(pos) == 0, "make_vector_from_list.2");

	list_heap(&pos, Nil, T, fixnumh(10), NULL);
	make_vector_from_list_(&pos, pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "make_vector_from_list.3");
	test(lenarrayr(pos) == 3, "make_vector_from_list.4");
	getarray(pos, 0, &check);
	test(check == Nil, "make_vector_from_list.5");
	getarray(pos, 1, &check);
	test(check == T, "make_vector_from_list.6");
	getarray(pos, 2, &check);
	test(RefFixnum(check) == 10, "make_vector_from_list.7");

	RETURN;
}

static int test_delete_list_eq_unsafe(void)
{
	addr cons, check, v1, v2, v3, v4;

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_list_eq_unsafe(v4, cons, &cons) == 0, "delete_list_eq_unsafe.1");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete_list_eq_unsafe.2");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete_list_eq_unsafe.3");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete_list_eq_unsafe.4");
	test(cons == Nil, "delete_list_eq_unsafe.5");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_list_eq_unsafe(v1, cons, &cons) != 0, "delete_list_eq_unsafe.6");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete_list_eq_unsafe.7");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete_list_eq_unsafe.8");
	test(cons == Nil, "delete_list_eq_unsafe.9");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_list_eq_unsafe(v2, cons, &cons) != 0, "delete_list_eq_unsafe.10");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete_list_eq_unsafe.11");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete_list_eq_unsafe.12");
	test(cons == Nil, "delete_list_eq_unsafe.13");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_list_eq_unsafe(v3, cons, &cons) != 0, "delete_list_eq_unsafe.14");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete_list_eq_unsafe.15");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete_list_eq_unsafe.16");
	test(cons == Nil, "delete_list_eq_unsafe.17");

	list_heap(&cons, v1, v1, v1, NULL);
	test(delete_list_eq_unsafe(v1, cons, &cons) != 0, "delete_list_eq_unsafe.18");
	test(cons == Nil, "delete_list_eq_unsafe.19");

	RETURN;
}

static int test_delete1_list_eq_unsafe(void)
{
	addr cons, check, v1, v2, v3, v4;

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_list_eq_unsafe(v4, cons, &cons) == 0, "delete1_list_eq_unsafe.1");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_list_eq_unsafe.2");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete1_list_eq_unsafe.3");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete1_list_eq_unsafe.4");
	test(cons == Nil, "delete1_list_eq_unsafe.5");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_list_eq_unsafe(v1, cons, &cons) == 1, "delete1_list_eq_unsafe.6");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete1_list_eq_unsafe.7");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete1_list_eq_unsafe.8");
	test(cons == Nil, "delete1_list_eq_unsafe.9");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_list_eq_unsafe(v2, cons, &cons) == 1, "delete1_list_eq_unsafe.10");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_list_eq_unsafe.11");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete1_list_eq_unsafe.12");
	test(cons == Nil, "delete1_list_eq_unsafe.13");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_list_eq_unsafe(v3, cons, &cons) == 1, "delete1_list_eq_unsafe.14");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_list_eq_unsafe.15");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete1_list_eq_unsafe.16");
	test(cons == Nil, "delete1_list_eq_unsafe.17");

	list_heap(&cons, v1, v1, v1, NULL);
	test(delete1_list_eq_unsafe(v1, cons, &cons) == 1, "delete1_list_eq_unsafe.18");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_list_eq_unsafe.19");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_list_eq_unsafe.20");
	test(cons == Nil, "delete1_list_eq_unsafe.21");

	RETURN;
}


/*
 *  sequence
 */
static int testcase_sequence(void)
{
	TestBreak(test_make_vector_from_list);
	TestBreak(test_delete_list_eq_unsafe);
	TestBreak(test_delete1_list_eq_unsafe);

	return 0;
}

static void testinit_sequence(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

int test_sequence(void)
{
	DegradeTitle;
	return DegradeCode(sequence);
}

