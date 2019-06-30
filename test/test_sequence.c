#include "sequence.c"
#include "cons.h"
#include "degrade.h"

/*
 *  unsafe
 */
static int test_copy_list_alloc_unsafe(void)
{
	addr cons, root, left, value1, value2, value3, check;

	copy_list_alloc_unsafe(NULL, &cons, Nil);
	test(cons == Nil, "copy_list_alloc_unsafe1");

	conscar_heap(&cons, T);
	copy_list_alloc_unsafe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_unsafe1");
	test(root != Nil, "copy_list_alloc_unsafe2");
	GetCons(root, &left, &root);
	test(left == T, "copy_list_alloc_unsafe3");
	test(root == Nil, "copy_list_alloc_unsafe4");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value3, value2, value1, NULL);
	copy_list_alloc_unsafe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_unsafe5");
	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_unsafe6");
	test(left == value3, "copy_list_alloc_unsafe7");
	test(check == value3, "copy_list_alloc_unsafe8");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_unsafe9");
	test(left == value2, "copy_list_alloc_unsafe10");
	test(check == value2, "copy_list_alloc_unsafe11");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(left == value1, "copy_list_alloc_unsafe12");
	test(check == value1, "copy_list_alloc_unsafe13");
	test(cons == Nil, "copy_list_alloc_unsafe14");
	test(root == Nil, "copy_list_alloc_unsafe15");

	RETURN;
}

static int test_copy_list_heap_unsafe(void)
{
	addr cons;

	consnil_heap(&cons);
	copy_list_heap_unsafe(&cons, cons);
	test(GetType(cons) == LISPTYPE_CONS, "copy_list_heap_unsafe1");
	test(! GetStatusDynamic(cons), "copy_list_heap_unsafe2");

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
	test(GetType(cons) == LISPTYPE_CONS, "copy_list_local_unsafe1");
	test(GetStatusDynamic(cons), "copy_list_local_unsafe2");
	rollback_local(local, stack);

	RETURN;
}

static int test_copy_list_alloc_safe(void)
{
	addr cons, root, left, value1, value2, value3, check;

	copy_list_alloc_safe(NULL, &cons, Nil);
	test(cons == Nil, "copy_list_alloc_safe1");

	conscar_heap(&cons, T);
	copy_list_alloc_safe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_safe1");
	test(root != Nil, "copy_list_alloc_safe2");
	GetCons(root, &left, &root);
	test(left == T, "copy_list_alloc_safe3");
	test(root == Nil, "copy_list_alloc_safe4");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value3, value2, value1, NULL);
	copy_list_alloc_safe(NULL, &root, cons);
	test(cons != root, "copy_list_alloc_safe5");
	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_safe6");
	test(left == value3, "copy_list_alloc_safe7");
	test(check == value3, "copy_list_alloc_safe8");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(cons != root, "copy_list_alloc_safe9");
	test(left == value2, "copy_list_alloc_safe10");
	test(check == value2, "copy_list_alloc_safe11");

	GetCons(cons, &left, &cons);
	GetCons(root, &check, &root);
	test(left == value1, "copy_list_alloc_safe12");
	test(check == value1, "copy_list_alloc_safe13");
	test(cons == Nil, "copy_list_alloc_safe14");
	test(root == Nil, "copy_list_alloc_safe15");

	copy_list_alloc_safe(NULL, &cons, T);
	test(cons == T, "copy_list_alloc_safe16");

	lista_heap(&cons, value1, value2, NULL);
	copy_list_alloc_safe(NULL, &cons, cons);
	GetCons(cons, &left, &cons);
	test(left == value1, "copy_list_alloc_safe17");
	test(cons == value2, "copy_list_alloc_safe18");

	lista_heap(&cons, value1, value2, value3, NULL);
	copy_list_alloc_safe(NULL, &cons, cons);
	GetCons(cons, &left, &cons);
	test(left == value1, "copy_list_alloc_safe19");
	GetCons(cons, &left, &cons);
	test(left == value2, "copy_list_alloc_safe20");
	test(cons == value3, "copy_list_alloc_safe21");

	RETURN;
}

static int test_make_vector_from_list(void)
{
	addr pos, check;

	make_vector_from_list(&pos, Nil);
	test(GetType(pos) == LISPTYPE_VECTOR, "make_vector_from_list1");
	test(lenarrayr(pos) == 0, "make_vector_from_list2");

	list_heap(&pos, Nil, T, fixnum_heapr(10), NULL);
	make_vector_from_list(&pos, pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "make_vector_from_list3");
	test(lenarrayr(pos) == 3, "make_vector_from_list4");
	getarray(pos, 0, &check);
	test(check == Nil, "make_vector_from_list5");
	getarray(pos, 1, &check);
	test(check == T, "make_vector_from_list6");
	getarray(pos, 2, &check);
	test(RefFixnum(check) == 10, "make_vector_from_list7");

	RETURN;
}

static int test_compare2(addr left, addr right)
{
	fixnum value1, value2;
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	return value1 < value2;
}

static int test_simplesort_cons_unsafe(void)
{
	addr cons, value1, value2, value3, left, left2, left3;

	simplesort_cons_unsafe(&cons, Nil, test_compare2);
	test(cons == Nil, "simplesort_cons_unsafe1");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value1, NULL);
	conscar_heap(&cons, value1);
	simplesort_cons_unsafe(&cons, cons, test_compare2);
	test(cons != Nil, "simplesort_cons_unsafe2");
	GetCons(cons, &left, &cons);
	test(left == value1, "simplesort_cons_unsafe3");
	test(cons == Nil, "simplesort_cons_unsafe4");

	/* test */
	list_heap(&cons, value1, value2, value3, NULL);
	simplesort_cons_unsafe(&cons, cons, test_compare2);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_cons_unsafe5");

	/* test */
	list_heap(&cons, value3, value2, value1, NULL);
	conscar_heap(&cons, value1);
	cons_heap(&cons, value2, cons);
	cons_heap(&cons, value3, cons);
	simplesort_cons_unsafe(&cons, cons, test_compare2);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_cons_unsafe6");

	/* test */
	list_heap(&cons, value2, value3, value1, NULL);
	simplesort_cons_unsafe(&cons, cons, test_compare2);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_cons_unsafe7");

	RETURN;
}

static int test_compare3(addr info, addr left, addr right)
{
	fixnum value1, value2;
	if (info == T) return 1;
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	return value1 < value2;
}

static int test_simplesort_info_cons_unsafe(void)
{
	addr cons, value1, value2, value3, left, left2, left3;

	simplesort_info_cons_unsafe(&cons, Nil, Nil, test_compare3);
	test(cons == Nil, "simplesort_info_cons_unsafe1");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value1, NULL);
	simplesort_info_cons_unsafe(&cons, cons, Nil, test_compare3);
	test(cons != Nil, "simplesort_info_cons_unsafe2");
	GetCons(cons, &left, &cons);
	test(left == value1, "simplesort_info_cons_unsafe3");
	test(cons == Nil, "simplesort_info_cons_unsafe4");

	/* test */
	list_heap(&cons, value1, value2, value3, NULL);
	simplesort_info_cons_unsafe(&cons, cons, Nil, test_compare3);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_info_cons_unsafe5");

	/* test */
	list_heap(&cons, value3, value2, value1, NULL);
	simplesort_info_cons_unsafe(&cons, cons, Nil, test_compare3);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_info_cons_unsafe6");

	/* test */
	list_heap(&cons, value2, value3, value1, NULL);
	simplesort_info_cons_unsafe(&cons, cons, Nil, test_compare3);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_info_cons_unsafe7");

	/* test */
	list_heap(&cons, value2, value3, value1, NULL);
	simplesort_info_cons_unsafe(&cons, cons, T, test_compare3);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value2 && left2 == value3 && left3 == value1 && cons == Nil,
			"simplesort_info_cons_unsafe8");

	RETURN;
}

static int test_nth_unsafe(void)
{
	addr cons, check, value1, value2;

	nth_unsafe(&cons, 0, Nil);
	test(cons == Nil, "nth_unsafe1");
	nth_unsafe(&cons, 3, Nil);
	test(cons == Nil, "nth_unsafe1");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	list_heap(&cons, value1, value2, NULL);
	nth_unsafe(&check, 0, cons);
	test(check == value1, "nth_unsafe3");
	nth_unsafe(&check, 1, cons);
	test(check == value2, "nth_unsafe4");
	nth_unsafe(&check, 2, cons);
	test(check == Nil, "nth_unsafe5");
	nth_unsafe(&check, 3, cons);
	test(check == Nil, "nth_unsafe6");
	nth_unsafe(&check, 999999, cons);
	test(check == Nil, "nth_unsafe7");

	RETURN;
}

static int test_append_cons_alloc_unsafe(void)
{
	addr check, cons, next, v1, v2, v3, v4, v5;

	append_cons_alloc_unsafe(NULL, &check, Nil, Nil);
	test(check == Nil, "append_cons_alloc_unsafe1");
	consnil_heap(&cons);
	append_cons_alloc_unsafe(NULL, &check, cons, Nil);
	test(check == cons, "append_cons_alloc_unsafe2");
	append_cons_alloc_unsafe(NULL, &check, Nil, cons);
	test(check == cons, "append_cons_alloc_unsafe3");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	fixnum_heap(&v5, 50);

	list_heap(&cons, v1, NULL);
	list_heap(&next, v3, v4, v5, NULL);
	append_cons_alloc_unsafe(NULL, &check, cons, next);
	test(check != cons, "append_cons_alloc_unsafe4");
	test(check != next, "append_cons_alloc_unsafe5");
	GetCons(check, &cons, &check);
	test(cons == v1, "append_cons_alloc_unsafe6");
	test(check == next, "append_cons_alloc_unsafe7");

	list_heap(&cons, v1, v2, NULL);
	list_heap(&next, v3, v4, v5, NULL);
	append_cons_alloc_unsafe(NULL, &check, cons, next);
	test(check != cons, "append_cons_alloc_unsafe8");
	test(check != next, "append_cons_alloc_unsafe9");
	GetCons(check, &cons, &check);
	test(cons == v1, "append_cons_alloc_unsafe10");
	GetCons(check, &cons, &check);
	test(cons == v2, "append_cons_alloc_unsafe11");
	test(check == next, "append_cons_alloc_unsafe12");

	RETURN;
}

static int test_delete_cons_eq_unsafe(void)
{
	addr cons, check, v1, v2, v3, v4;

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_cons_eq_unsafe(v4, cons, &cons) == 0, "delete_cons_eq_unsafe1");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete_cons_eq_unsafe2");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete_cons_eq_unsafe3");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete_cons_eq_unsafe4");
	test(cons == Nil, "delete_cons_eq_unsafe5");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_cons_eq_unsafe(v1, cons, &cons) == 1, "delete_cons_eq_unsafe6");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete_cons_eq_unsafe7");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete_cons_eq_unsafe8");
	test(cons == Nil, "delete_cons_eq_unsafe9");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_cons_eq_unsafe(v2, cons, &cons) == 1, "delete_cons_eq_unsafe10");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete_cons_eq_unsafe11");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete_cons_eq_unsafe12");
	test(cons == Nil, "delete_cons_eq_unsafe13");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete_cons_eq_unsafe(v3, cons, &cons) == 1, "delete_cons_eq_unsafe14");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete_cons_eq_unsafe15");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete_cons_eq_unsafe16");
	test(cons == Nil, "delete_cons_eq_unsafe17");

	list_heap(&cons, v1, v1, v1, NULL);
	test(delete_cons_eq_unsafe(v1, cons, &cons) == 3, "delete_cons_eq_unsafe18");
	test(cons == Nil, "delete_cons_eq_unsafe19");

	RETURN;
}

static int test_delete1_cons_eq_unsafe(void)
{
	addr cons, check, v1, v2, v3, v4;

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_cons_eq_unsafe(v4, cons, &cons) == 0, "delete1_cons_eq_unsafe1");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_cons_eq_unsafe2");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete1_cons_eq_unsafe3");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete1_cons_eq_unsafe4");
	test(cons == Nil, "delete1_cons_eq_unsafe5");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_cons_eq_unsafe(v1, cons, &cons) == 1, "delete1_cons_eq_unsafe6");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete1_cons_eq_unsafe7");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete1_cons_eq_unsafe8");
	test(cons == Nil, "delete1_cons_eq_unsafe9");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_cons_eq_unsafe(v2, cons, &cons) == 1, "delete1_cons_eq_unsafe10");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_cons_eq_unsafe11");
	GetCons(cons, &check, &cons);
	test(check == v3, "delete1_cons_eq_unsafe12");
	test(cons == Nil, "delete1_cons_eq_unsafe13");

	list_heap(&cons, v1, v2, v3, NULL);
	test(delete1_cons_eq_unsafe(v3, cons, &cons) == 1, "delete1_cons_eq_unsafe14");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_cons_eq_unsafe15");
	GetCons(cons, &check, &cons);
	test(check == v2, "delete1_cons_eq_unsafe16");
	test(cons == Nil, "delete1_cons_eq_unsafe17");

	list_heap(&cons, v1, v1, v1, NULL);
	test(delete1_cons_eq_unsafe(v1, cons, &cons) == 1, "delete1_cons_eq_unsafe18");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_cons_eq_unsafe19");
	GetCons(cons, &check, &cons);
	test(check == v1, "delete1_cons_eq_unsafe20");
	test(cons == Nil, "delete1_cons_eq_unsafe21");

	RETURN;
}


/*
 *  main
 */
static int testgroup_sequence(void)
{
	/* unsafe */
	TestBreak(test_copy_list_alloc_unsafe);
	TestBreak(test_copy_list_heap_unsafe);
	TestBreak(test_copy_list_local_unsafe);
	TestBreak(test_copy_list_alloc_safe);
	TestBreak(test_make_vector_from_list);
	TestBreak(test_simplesort_cons_unsafe);
	TestBreak(test_simplesort_info_cons_unsafe);
	TestBreak(test_nth_unsafe);
	TestBreak(test_append_cons_alloc_unsafe);
	TestBreak(test_delete_cons_eq_unsafe);
	TestBreak(test_delete1_cons_eq_unsafe);

	return 0;
}

int test_sequence(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		//build_package();
		lisp_initialize = 1;
		result = testgroup_sequence();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

