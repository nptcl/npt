#include "cons.c"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "package.h"
#include "sequence.h"
#include "type.h"
#include "type_table.h"

static int test_consp_getcons(void)
{
	addr left, right, cons, check1, check2;

	check1 = check2 = Nil;

	fixnum_heap(&left, 100);
	fixnum_heap(&right, 200);
	cons_heap(&cons, left, right);
	test(consp_getcons(cons, &check1, &check2), "consp_getcons.1");
	test(check1 == left, "consp_getcons.2");
	test(check2 == right, "consp_getcons.3");
	test(! consp_getcons(Nil, &check1, &check2), "consp_getcons.4");
	test(! consp_getcons(left, &check1, &check2), "consp_getcons.5");

	test(consp_getcar(cons, &check1), "consp_getcar.1");
	test(check1 == left, "consp_getcar.2");
	test(! consp_getcar(Nil, &check1), "consp_getcar.3");
	test(! consp_getcar(left, &check1), "consp_getcar.4");

	test(consp_getcdr(cons, &check2), "consp_getcdr.1");
	test(check2 == right, "consp_getcdr.2");
	test(! consp_getcdr(Nil, &check1), "consp_getcdr.3");
	test(! consp_getcdr(left, &check1), "consp_getcdr.4");

	RETURN;
}

static int test_getcons(void)
{
	addr left, right, cons, check1, check2;

	check1 = check2 = Unbound;

	fixnum_heap(&left, 100);
	fixnum_heap(&right, 200);
	cons_heap(&cons, left, right);
	getcons_(cons, &check1, &check2);
	test(check1 == left, "getcons.1");
	test(check2 == right, "getcons.2");
	getcar_(cons, &check1);
	getcdr_(cons, &check2);
	test(check1 == left, "getcar_.1");
	test(check2 == right, "getcdr_.1");

	RETURN;
}

static int test_setcons(void)
{
	addr a, b, c, d, cons, check1, check2;

	check1 = check2 = Unbound;

	fixnum_heap(&a, 100);
	fixnum_heap(&b, 200);
	fixnum_heap(&c, 100);
	fixnum_heap(&d, 200);
	cons_heap(&cons, a, b);
	setcons_(cons, c, d);
	getcons_(cons, &check1, &check2);
	test(check1 == c, "setcons.1");
	test(check2 == d, "setcons.2");

	setcar_(cons, a);
	getcar_(cons, &check1);
	test(check1 == a, "setcar.1");

	setcdr_(cons, b);
	getcdr_(cons, &check2);
	test(check2 == b, "setcdr.1");

	RETURN;
}

static int test_list_alloc(void)
{
	addr cons, left, value;

	list_alloc(NULL, &cons, NULL);
	test(cons == Nil, "list_alloc.1");
	list_alloc(NULL, &cons, T, NULL);
	test(cons != Nil, "list_alloc.2");
	GetCons(cons, &left, &cons);
	test(left == T, "list_alloc.3");
	test(cons == Nil, "list_alloc.4");
	consnil_heap(&value);

	list_alloc(NULL, &cons, T, Nil, value, NULL);
	GetCons(cons, &left, &cons);
	test(left == T, "list_alloc.5");
	GetCons(cons, &left, &cons);
	test(left == Nil, "list_alloc.6");
	GetCons(cons, &left, &cons);
	test(left == value, "list_alloc.7");
	test(cons == Nil, "list_alloc.8");

	RETURN;
}

static int test_lista_safe_alloc(void)
{
	addr cons, pos, v1, v2, v3;

	lista_safe_alloc_(NULL, &cons, Nil, Nil);
	test(cons == Nil, "lista_safe_alloc.1");
	lista_safe_alloc_(NULL, &cons, T, Nil);
	test(cons == T, "lista_safe_alloc.2");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	list_heap(&cons, v2, NULL);
	lista_safe_alloc_(NULL, &cons, v1, cons);
	GetCons(cons, &pos, &cons);
	test(pos == v1, "lista_safe_alloc.3");
	test(cons == v2, "lista_safe_alloc.4");

	list_heap(&cons, v2, v3, NULL);
	lista_safe_alloc_(NULL, &cons, v1, cons);
	GetCons(cons, &pos, &cons);
	test(pos == v1, "lista_safe_alloc.5");
	GetCons(cons, &pos, &cons);
	test(pos == v2, "lista_safe_alloc.6");
	test(cons == v3, "lista_safe_alloc.7");

	RETURN;
}

static int test_lista_alloc(void)
{
	addr list, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	lista_alloc(local, &list, T, NULL);
	test(list == T, "lista_alloc.1");

	lista_alloc(local, &list, fixnuma(local, 10), fixnuma(local, 20), NULL);
	test(GetStatusDynamic(list), "lista_alloc.2");
	test(consp(list), "lista_alloc.3");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_alloc.4");
	test(RefFixnum(list) == 20, "lista_alloc.5");

	lista_local(local, &list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	test(GetStatusDynamic(list), "lista_alloc.6");
	test(consp(list), "lista_alloc.7");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_alloc.8");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_alloc.9");
	test(RefFixnum(list) == 30, "lista_alloc.10");

	lista_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30),
			fixnumh(40), fixnumh(50), fixnumh(60), NULL);
	test(! GetStatusDynamic(list), "lista_allocr.11");
	test(consp(list), "lista_allocr.12");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_allocr.13");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_allocr.14");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 30, "lista_allocr.15");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 40, "lista_allocr.16");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 50, "lista_allocr.17");
	test(RefFixnum(list) == 60, "lista_allocr.18");

	rollback_local(local, stack);

	RETURN;
}

static int test_List_bind(void)
{
	addr list, pos1, pos2, pos3;

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	List_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "List_bind.1");
	test(RefFixnum(pos2) == 20, "List_bind.2");
	test(RefFixnum(pos3) == 30, "List_bind.3");

	pos1 = pos2 = pos3 = NULL;
	list_bind_(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "list_bind.1");
	test(RefFixnum(pos2) == 20, "list_bind.2");
	test(RefFixnum(pos3) == 30, "list_bind.3");

	RETURN;
}

static int test_Lista_bind(void)
{
	addr list, pos1, pos2, pos3;

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	Lista_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "Lista_bind.1");
	test(RefFixnum(pos2) == 20, "Lista_bind.2");
	test(consp(pos3), "Lista_bind.3");
	GetCons(pos3, &pos3, &pos1);
	test(RefFixnum(pos3) == 30, "Lista_bind.4");
	test(pos1 == Nil, "Lista_bind.5");

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	Lista_bind(list, &pos1, &pos2, NULL);
	test(RefFixnum(pos1) == 10, "Lista_bind.6");
	test(consp(pos2), "Lista_bind.7");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 20, "Lista_bind.8");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 30, "Lista_bind.9");
	test(pos2 == Nil, "Lista_bind.10");

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	lista_bind_(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "lista_bind.1");
	test(RefFixnum(pos2) == 20, "lista_bind.2");
	test(consp(pos3), "lista_bind.3");
	GetCons(pos3, &pos3, &pos1);
	test(RefFixnum(pos3) == 30, "lista_bind.4");
	test(pos1 == Nil, "lista_bind.5");

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	lista_bind_(list, &pos1, &pos2, NULL);
	test(RefFixnum(pos1) == 10, "lista_bind.6");
	test(consp(pos2), "lista_bind.7");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 20, "lista_bind.8");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 30, "lista_bind.9");
	test(pos2 == Nil, "lista_bind.10");

	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	pos1 = pos2 = pos3 = NULL;
	lista_bind_(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "lista_bind.11");
	test(RefFixnum(pos2) == 20, "lista_bind.12");
	test(pos3 == Nil, "lista_bind.13");

	RETURN;
}


/*
 *  cons
 */
static int testcase_cons(void)
{
	TestBreak(test_consp_getcons);
	TestBreak(test_getcons);
	TestBreak(test_setcons);
	TestBreak(test_list_alloc);
	TestBreak(test_lista_safe_alloc);
	TestBreak(test_lista_alloc);
	TestBreak(test_List_bind);
	TestBreak(test_Lista_bind);

	return 0;
}

static void testinit_cons(Execute ptr)
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

int test_cons(void)
{
	TITLE;
	return degrade_code(
			testinit_cons,
			testcase_cons);
}

