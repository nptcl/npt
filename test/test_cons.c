#include "cons.c"
#include "cons_list.h"
#include "cons_plist.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "package.h"
#include "sequence.h"
#include "type.h"
#include "type_table.h"

/*
 *  safe
 */
static int test_getcons(void)
{
	addr left, right, cons, check1, check2;

	fixnum_heap(&left, 100);
	fixnum_heap(&right, 200);
	cons_heap(&cons, left, right);
	getcons(cons, &check1, &check2);
	test(check1 == left, "getcons1");
	test(check2 == right, "getcons2");
	getcar(cons, &check1);
	getcdr(cons, &check2);
	test(check1 == left, "getcar1");
	test(check2 == right, "getcdr1");

	RETURN;
}

static int test_length_list_safe(void)
{
	addr cons;

	test(length_list_safe(Nil) == 0, "length_list_safe1");
	conscar_heap(&cons, T);
	test(length_list_safe(cons) == 1, "length_list_safe2");
	list_heap(&cons, T, Nil, Nil, T, NULL);
	test(length_list_safe(cons) == 4, "length_list_safe3");

	RETURN;
}

static int test_length_list_unsafe(void)
{
	addr cons;

	test(length_list_unsafe(Nil) == 0, "length_list_unsafe1");
	conscar_heap(&cons, T);
	test(length_list_unsafe(cons) == 1, "length_list_unsafe2");
	list_heap(&cons, T, Nil, Nil, T, NULL);
	test(length_list_unsafe(cons) == 4, "length_list_unsafe3");

	RETURN;
}

static int test_append2_alloc_unsafe(void)
{
	addr check, cons, next, v1, v2, v3, v4, v5;

	append2_alloc_unsafe(NULL, Nil, Nil, &check);
	test(check == Nil, "append2_alloc_unsafe1");
	consnil_heap(&cons);
	append2_alloc_unsafe(NULL, cons, Nil, &check);
	test(check == cons, "append2_alloc_unsafe2");
	append2_alloc_unsafe(NULL, Nil, cons, &check);
	test(check == cons, "append2_alloc_unsafe3");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	fixnum_heap(&v4, 40);
	fixnum_heap(&v5, 50);

	list_heap(&cons, v1, NULL);
	list_heap(&next, v3, v4, v5, NULL);
	append2_alloc_unsafe(NULL, cons, next, &check);
	test(check != cons, "append2_alloc_unsafe4");
	test(check != next, "append2_alloc_unsafe5");
	GetCons(check, &cons, &check);
	test(cons == v1, "append2_alloc_unsafe6");
	test(check == next, "append2_alloc_unsafe7");

	list_heap(&cons, v1, v2, NULL);
	list_heap(&next, v3, v4, v5, NULL);
	append2_alloc_unsafe(NULL, cons, next, &check);
	test(check != cons, "append2_alloc_unsafe8");
	test(check != next, "append2_alloc_unsafe9");
	GetCons(check, &cons, &check);
	test(cons == v1, "append2_alloc_unsafe10");
	GetCons(check, &cons, &check);
	test(cons == v2, "append2_alloc_unsafe11");
	test(check == next, "append2_alloc_unsafe12");

	RETURN;
}

static int test_find_list_eq_unsafe(void)
{
	addr cons, value1, value2, value3;

	test(find_list_eq_unsafe(T, Nil) == 0, "find_list_eq_unsafe1");

	consnil_heap(&cons);
	test(find_list_eq_unsafe(T, cons) == 0, "find_list_eq_unsafe2");
	conscar_heap(&cons, T);
	test(find_list_eq_unsafe(T, cons), "find_list_eq_unsafe3");

	fixnum_heap(&value1, 1);
	fixnum_heap(&value2, 22);
	fixnum_heap(&value3, 333);
	list_heap(&cons, value3, value2, value1, NULL);

	test(find_list_eq_unsafe(Nil, cons) == 0, "find_list_eq_unsafe4");
	test(find_list_eq_unsafe(T, cons) == 0, "find_list_eq_unsafe5");
	test(find_list_eq_unsafe(value1, cons), "find_list_eq_unsafe6");
	test(find_list_eq_unsafe(value2, cons), "find_list_eq_unsafe7");
	test(find_list_eq_unsafe(value3, cons), "find_list_eq_unsafe8");
	make_fixnum_heap(&value2, 22);
	test(find_list_eq_unsafe(value2, cons) == 0, "find_list_eq_unsafe9");

	RETURN;
}


/*
 *  reverse
 */
static int test_nreverse_list_unsafe(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	/* nil */
	result = 0;
	nreverse_list_unsafe(&result, Nil);
	test(result == Nil, "nreverse_list_unsafe1");

	/* single */
	consnil_heap(&root);
	SetCar(root, T);
	nreverse_list_unsafe(&result, root);
	test(result == root, "nreverse_list_unsafe2");

	/* list2 */
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	list_heap(&root, v1, v2, NULL);
	nreverse_list_unsafe(&result, root);
	test(result != root, "nreverse_list_unsafe3");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "nreverse_list_unsafe4");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "nreverse_list_unsafe5");
	test(right == Nil, "nreverse_list_unsafe6");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	nreverse_list_unsafe(&result, root);
	test(result != root, "nreverse_list_unsafe7");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "nreverse_list_unsafe8");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "nreverse_list_unsafe9");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "nreverse_list_unsafe10");
	test(right == Nil, "nreverse_list_unsafe11");

	RETURN;
}

static int test_nreverse_list_unsafe_inplace(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	test(nreverse_list_unsafe_inplace(Nil) == Nil, "ureverse_list_unsafe_inplace1");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	result = nreverse_list_unsafe_inplace(root);
	test(result != root, "nreverse_list_unsafe_inplace1");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "nreverse_list_unsafe_inplace2");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "nreverse_list_unsafe_inplace3");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "nreverse_list_unsafe_inplace4");
	test(right == Nil, "nreverse_list_unsafe_inplace5");

	RETURN;
}

static int test_nreverse_list_unsafe_dotted(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	/* nil */
	result = 0;
	nreverse_list_unsafe_dotted(&result, Nil, T);
	test(result == T, "nreverse_list_unsafe_dotted1");

	/* single */
	fixnum_heap(&v1, 10);
	list_heap(&root, v1, NULL);
	nreverse_list_unsafe_dotted(&result, root, T);
	test(result == root, "nreverse_list_unsafe_dotted2");
	GetCons(result, &result, &right);
	test(result == v1, "nreverse_list_unsafe_dotted3");
	test(right == T, "nreverse_list_unsafe_dotted4");

	/* list2 */
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	list_heap(&root, v1, v2, NULL);
	nreverse_list_unsafe_dotted(&result, root, T);
	test(result != root, "nreverse_list_unsafe_dotted5");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "nreverse_list_unsafe_dotted6");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "nreverse_list_unsafe_dotted7");
	test(right == T, "nreverse_list_unsafe_dotted8");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	nreverse_list_unsafe_dotted(&result, root, T);
	test(result != root, "nreverse_list_unsafe_dotted9");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "nreverse_list_unsafe_dotted10");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "nreverse_list_unsafe_dotted11");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "nreverse_list_unsafe_dotted12");
	test(right == T, "nreverse_list_unsafe_dotted13");

	RETURN;
}

static int test_reverse_list_heap_unsafe(void)
{
	addr left, right, root, result, v1, v2, v3;
	fixnum value;

	/* nil */
	result = 0;
	reverse_list_heap_unsafe(&result, Nil);
	test(result == Nil, "reverse_list1");

	/* single */
	consnil_heap(&root);
	SetCar(root, T);
	reverse_list_heap_unsafe(&result, root);
	test(result != root, "reverse_list2");

	/* list2 */
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	list_heap(&root, v1, v2, NULL);
	reverse_list_heap_unsafe(&result, root);
	test(result != root, "reverse_list3");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "reverse_list4");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "reverse_list5");
	test(right == Nil, "reverse_list6");

	/* list3 */
	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	fixnum_heap(&v3, 300);
	list_heap(&root, v1, v2, v3, NULL);
	reverse_list_heap_unsafe(&result, root);
	test(result != root, "reverse_list7");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "reverse_list8");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "reverse_list9");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "reverse_list10");
	test(right == Nil, "reverse_list11");
	test(! GetStatusDynamic(result), "reverse_list12");

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
	test(result == Nil, "reverse_list1");

	/* single */
	consnil_local(local, &root);
	SetCar(root, T);
	reverse_list_local_unsafe(local, &result, root);
	test(result != root, "reverse_list2");

	/* list2 */
	consnil_local(local, &root);
	fixnum_local(local, &left, 10);
	consnil_local(local, &right);
	SetCons(root, left, right);
	fixnum_local(local, &left, 20);
	SetCar(right, left);
	reverse_list_local_unsafe(local, &result, root);
	test(result != root, "reverse_list3");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 20, "reverse_list4");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 10, "reverse_list5");
	test(right == Nil, "reverse_list6");

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
	test(result != root, "reverse_list7");
	GetCons(result, &left, &right);
	GetFixnum(left, &value);
	test(value == 300, "reverse_list8");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 200, "reverse_list9");
	GetCons(right, &left, &right);
	GetFixnum(left, &value);
	test(value == 100, "reverse_list10");
	test(right == Nil, "reverse_list11");
	test(GetStatusDynamic(result), "reverse_list12");

	/* local */
	rollback_local(local, stack);

	RETURN;
}


/*
 *  list
 */
static int test_lista_alloc_safe(void)
{
	addr cons, pos, v1, v2, v3;

	lista_alloc_safe(NULL, &cons, Nil, Nil);
	test(cons == Nil, "lista_alloc_safe1");
	lista_alloc_safe(NULL, &cons, T, Nil);
	test(cons == T, "lista_alloc_safe2");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	list_heap(&cons, v2, NULL);
	lista_alloc_safe(NULL, &cons, v1, cons);
	GetCons(cons, &pos, &cons);
	test(pos == v1, "lista_alloc_safe3");
	test(cons == v2, "lista_alloc_safe4");

	list_heap(&cons, v2, v3, NULL);
	lista_alloc_safe(NULL, &cons, v1, cons);
	GetCons(cons, &pos, &cons);
	test(pos == v1, "lista_alloc_safe5");
	GetCons(cons, &pos, &cons);
	test(pos == v2, "lista_alloc_safe6");
	test(cons == v3, "lista_alloc_safe7");

	RETURN;
}

static int test_lista_allocr(void)
{
	addr list, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	list = lista_allocr(local, T, NULL);
	test(list == T, "lista_allocr1");

	list = lista_allocr(local, fixnuma(local, 10), fixnuma(local, 20), NULL);
	test(GetStatusDynamic(list), "lista_allocr2");
	test(consp(list), "lista_allocr3");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_allocr4");
	test(RefFixnum(list) == 20, "lista_allocr5");

	list = lista_localr(local, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	test(GetStatusDynamic(list), "lista_allocr6");
	test(consp(list), "lista_allocr7");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_allocr8");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_allocr9");
	test(RefFixnum(list) == 30, "lista_allocr10");

	list = lista_heapr(T, NULL);
	test(list == T, "lista_heapr1");

	list = lista_heapr(fixnumh(10), fixnumh(20), NULL);
	test(! GetStatusDynamic(list), "lista_heapr2");
	test(consp(list), "lista_heapr3");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_heapr4");
	test(RefFixnum(list) == 20, "lista_heapr5");

	list = lista_heapr(fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	test(! GetStatusDynamic(list), "lista_heapr6");
	test(consp(list), "lista_heapr7");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_heapr8");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_heapr9");
	test(RefFixnum(list) == 30, "lista_heapr10");

	list = lista_heapr(fixnumh(10), fixnumh(20), fixnumh(30),
			fixnumh(40), fixnumh(50), fixnumh(60), NULL);
	test(! GetStatusDynamic(list), "lista_heapr11");
	test(consp(list), "lista_heapr12");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_heapr13");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_heapr14");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 30, "lista_heapr15");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 40, "lista_heapr16");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 50, "lista_heapr17");
	test(RefFixnum(list) == 60, "lista_heapr18");

	rollback_local(local, stack);

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
	test(list == T, "lista_alloc1");

	lista_alloc(local, &list, fixnuma(local, 10), fixnuma(local, 20), NULL);
	test(GetStatusDynamic(list), "lista_alloc2");
	test(consp(list), "lista_alloc3");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_alloc4");
	test(RefFixnum(list) == 20, "lista_alloc5");

	lista_local(local, &list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	test(GetStatusDynamic(list), "lista_alloc6");
	test(consp(list), "lista_alloc7");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_alloc8");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_alloc9");
	test(RefFixnum(list) == 30, "lista_alloc10");

	lista_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30),
			fixnumh(40), fixnumh(50), fixnumh(60), NULL);
	test(! GetStatusDynamic(list), "lista_allocr11");
	test(consp(list), "lista_allocr12");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 10, "lista_allocr13");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 20, "lista_allocr14");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 30, "lista_allocr15");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 40, "lista_allocr16");
	GetCons(list, &pos, &list);
	test(RefFixnum(pos) == 50, "lista_allocr17");
	test(RefFixnum(list) == 60, "lista_allocr18");

	rollback_local(local, stack);

	RETURN;
}

static int test_List_bind(void)
{
	addr list, pos1, pos2, pos3;

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	List_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "List_bind1");
	test(RefFixnum(pos2) == 20, "List_bind2");
	test(RefFixnum(pos3) == 30, "List_bind3");

	pos1 = pos2 = pos3 = NULL;
	list_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "list_bind1");
	test(RefFixnum(pos2) == 20, "list_bind2");
	test(RefFixnum(pos3) == 30, "list_bind3");

	RETURN;
}

static int test_Lista_bind(void)
{
	addr list, pos1, pos2, pos3;

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	Lista_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "Lista_bind1");
	test(RefFixnum(pos2) == 20, "Lista_bind2");
	test(consp(pos3), "Lista_bind3");
	GetCons(pos3, &pos3, &pos1);
	test(RefFixnum(pos3) == 30, "Lista_bind4");
	test(pos1 == Nil, "Lista_bind5");

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	Lista_bind(list, &pos1, &pos2, NULL);
	test(RefFixnum(pos1) == 10, "Lista_bind5");
	test(consp(pos2), "Lista_bind6");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 20, "Lista_bind6");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 30, "Lista_bind7");
	test(pos2 == Nil, "Lista_bind8");

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	lista_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "lista_bind1");
	test(RefFixnum(pos2) == 20, "lista_bind2");
	test(consp(pos3), "lista_bind3");
	GetCons(pos3, &pos3, &pos1);
	test(RefFixnum(pos3) == 30, "lista_bind4");
	test(pos1 == Nil, "lista_bind5");

	list_heap(&list, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	pos1 = pos2 = pos3 = NULL;
	lista_bind(list, &pos1, &pos2, NULL);
	test(RefFixnum(pos1) == 10, "lista_bind5");
	test(consp(pos2), "lista_bind6");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 20, "lista_bind6");
	GetCons(pos2, &pos1, &pos2);
	test(RefFixnum(pos1) == 30, "lista_bind7");
	test(pos2 == Nil, "lista_bind8");

	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	pos1 = pos2 = pos3 = NULL;
	lista_bind(list, &pos1, &pos2, &pos3, NULL);
	test(RefFixnum(pos1) == 10, "lista_bind9");
	test(RefFixnum(pos2) == 20, "lista_bind10");
	test(pos3 == Nil, "lista_bind11");

	RETURN;
}


/*
 *  plist
 */
static int test_pushnew_alloc(void)
{
	addr sym1, sym2, sym3, list, check;

	interncommon("CAR", &sym1);
	interncommon("CDR", &sym2);
	interncommon("CONS", &sym3);
	test(pushnew_alloc(NULL, Nil, sym1, &list), "pushnew_alloc1");
	test(length_list_safe(list) == 1, "pushnew_alloc2");
	GetCar(list, &check);
	test(check == sym1, "pushnew_alloc3");

	test(pushnew_alloc(NULL, list, sym2, &list), "pushnew_alloc4");
	test(length_list_safe(list) == 2, "pushnew_alloc5");
	GetCar(list, &check);
	test(check == sym2, "pushnew_alloc6");

	test(pushnew_alloc(NULL, list, sym2, &list) == 0, "pushnew_alloc7");
	test(pushnew_alloc(NULL, list, sym1, &list) == 0, "pushnew_alloc8");
	test(length_list_safe(list) == 2, "pushnew_alloc9");

	RETURN;
}

static int test_plist(void)
{
	addr list, pos, key, key1, key2, key3;

	test(getplist(Nil, T, &pos), "plist1");
	test(getplist(Nil, Nil, &pos), "plist2");
	list_heap(&list, T, fixnumh(10), Nil, fixnumh(20), NULL);
	test(getplist(list, T, &pos) == 0, "plist3");
	test(RefFixnum(pos) == 10, "plist4");
	test(getplist(list, Nil, &pos) == 0, "plist5");
	test(RefFixnum(pos) == 20, "plist6");
	test(getplist(list, list, &pos), "plist7");

	test(setplist_heap(list, T, fixnumh(30), &list) == 0, "plist8");
	test(getplist(list, T, &pos) == 0, "plist9");
	test(RefFixnum(pos) == 30, "plist10");
	test(setplist_heap(list, Nil, fixnumh(40), &list) == 0, "plist11");
	test(getplist(list, Nil, &pos) == 0, "plist12");
	test(RefFixnum(pos) == 40, "plist13");

	fixnum_heap(&key, 999);
	test(setplist_heap(list, key, fixnumh(50), &list), "plist14");
	test(getplist(list, key, &pos) == 0, "plist15");
	test(RefFixnum(pos) == 50, "plist16");

	test(pushplist_heap(list, key, fixnumh(60), &list) == 0, "plist17");
	test(getplist(list, key, &pos) == 0, "plist18");
	test(GetType(pos) == LISPTYPE_CONS, "plist19");
	GetCar(pos, &key);
	test(RefFixnum(key) == 60, "plist20");
	GetCdr(pos, &key);
	test(RefFixnum(key) == 50, "plist21");

	list = Nil;
	fixnum_heap(&key1, 10);
	setplist_heap(list, key1, key1, &list);
	fixnum_heap(&key2, 20);
	setplist_heap(list, key2, key2, &list);
	fixnum_heap(&key3, 30);
	setplist_heap(list, key3, key3, &list);
	test(getplist(list, key1, &pos) == 0, "plist22-error");
	test(getplist(list, key2, &pos) == 0, "plist23-error");
	test(getplist(list, key3, &pos) == 0, "plist24-error");
	test(getplist(list, T, &pos), "plist25-error");

	list = Nil;
	fixnum_heap(&key1, 10);
	pushplist_heap(list, key1, key1, &list);
	fixnum_heap(&key2, 20);
	pushplist_heap(list, key2, key2, &list);
	fixnum_heap(&key3, 30);
	pushplist_heap(list, key3, key3, &list);
	test(getplist(list, key1, &pos) == 0, "plist26-error");
	test(getplist(list, key2, &pos) == 0, "plist27-error");
	test(getplist(list, key3, &pos) == 0, "plist28-error");
	test(getplist(list, T, &pos), "plist29-error");

	RETURN;
}

static int test_getplist(void)
{
	addr pos, list, sym1, sym2, sym3, sym4;

	test(getplist(Nil, T, &pos), "getplist1");
	test(pos == Nil, "getplist2");

	internchar(LISP_PACKAGE, "HELLO1", &sym1);
	internchar(LISP_PACKAGE, "HELLO2", &sym2);
	internchar(LISP_PACKAGE, "HELLO3", &sym3);
	internchar(LISP_PACKAGE, "HELLO4", &sym4);
	list_heap(&list, sym1, sym2, sym3, sym4, NULL);
	test(getplist(list, sym1, &pos) == 0, "getplist3");
	test(pos == sym2, "getplist4");
	test(getplist(list, sym3, &pos) == 0, "getplist5");
	test(pos == sym4, "getplist6");
	test(getplist(list, sym2, &pos), "getplist7");
	test(getplist(list, Nil, &pos), "getplist8");

	RETURN;
}

static int test_setplist_alloc(void)
{
	addr pos, list, check, sym1, sym2, sym3, sym4;

	internchar(LISP_PACKAGE, "HELLO1", &sym1);
	internchar(LISP_PACKAGE, "HELLO2", &sym2);
	internchar(LISP_PACKAGE, "HELLO3", &sym3);
	internchar(LISP_PACKAGE, "HELLO4", &sym4);
	test(setplist_alloc(NULL, Nil, sym1, sym2, &list), "setplist_alloc1");
	test(GetType(list) == LISPTYPE_CONS, "setplist_alloc2");
	GetCons(list, &pos, &check);
	test(pos == sym1, "setplist_alloc3");
	GetCons(check, &pos, &check);
	test(pos == sym2, "setplist_alloc4");
	test(check == Nil, "setplist_alloc5");
	test(setplist_alloc(NULL, list, sym1, T, &list) == 0, "setplist_alloc6");
	test(setplist_alloc(NULL, list, sym3, sym4, &list), "setplist_alloc7");
	test(getplist(list, sym1, &pos) == 0, "setplist_alloc8");
	test(pos == T, "setplist_alloc9");
	test(getplist(list, sym3, &pos) == 0, "setplist_alloc10");
	test(pos == sym4, "setplist_alloc11");
	test(getplist(list, sym4, &pos), "setplist_alloc12");

	RETURN;
}

static int test_setplist_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	test(setplist_local(local, Nil, T, Nil, &pos), "setplist_local1");
	test(GetType(pos) == LISPTYPE_CONS, "setplist_local2");
	test(GetStatusDynamic(pos), "setplist_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_setplist_heap(void)
{
	addr pos;

	test(setplist_heap(Nil, T, Nil, &pos), "setplist_heap1");
	test(GetType(pos) == LISPTYPE_CONS, "setplist_heap2");
	test(! GetStatusDynamic(pos), "setplist_heap3");

	RETURN;
}

static int test_pushplist_alloc(void)
{
	addr pos, list, check, sym1, sym2, sym3, sym4;

	internchar(LISP_PACKAGE, "HELLO1", &sym1);
	internchar(LISP_PACKAGE, "HELLO2", &sym2);
	internchar(LISP_PACKAGE, "HELLO3", &sym3);
	internchar(LISP_PACKAGE, "HELLO4", &sym4);
	test(pushplist_alloc(NULL, Nil, sym1, sym2, &list), "pushplist_alloc1");
	test(GetType(list) == LISPTYPE_CONS, "pushplist_alloc2");
	GetCons(list, &pos, &check);
	test(pos == sym1, "pushplist_alloc3");
	GetCons(check, &pos, &check);
	test(check == Nil, "pushplist_alloc4");
	GetCons(pos, &pos, &check);
	test(pos == sym2, "pushplist_alloc5");
	test(check == Nil, "pushplist_alloc6");

	test(pushplist_alloc(NULL, list, sym1, T, &list) == 0, "pushplist_alloc7");
	test(getplist(list, sym1, &pos) == 0, "pushplist_alloc8");
	GetCons(pos, &check, &pos);
	test(check == T, "pushplist_alloc9");
	GetCons(pos, &check, &pos);
	test(check == sym2, "pushplist_alloc10");
	test(pos == Nil, "pushplist_allo11");

	test(pushplist_alloc(NULL, list, sym3, sym4, &list), "pushplist_alloc12");
	test(getplist(list, sym1, &pos) == 0, "pushplist_alloc13");
	test(getplist(list, sym3, &pos) == 0, "pushplist_alloc14");
	test(getplist(list, sym4, &pos), "pushplist_alloc15");

	RETURN;
}

static int test_pushplist_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	test(pushplist_local(local, Nil, T, Nil, &pos), "pushplist_local1");
	test(GetType(pos) == LISPTYPE_CONS, "pushplist_local2");
	test(GetStatusDynamic(pos), "pushplist_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_pushplist_heap(void)
{
	addr pos;

	test(pushplist_heap(Nil, T, Nil, &pos), "pushplist_heap1");
	test(GetType(pos) == LISPTYPE_CONS, "pushplist_heap2");
	test(! GetStatusDynamic(pos), "pushplist_heap3");

	RETURN;
}

static int test_pushnewplist_alloc(void)
{
	addr pos, list, check, sym1, sym2, sym3, sym4;

	internchar(LISP_PACKAGE, "HELLO1", &sym1);
	internchar(LISP_PACKAGE, "HELLO2", &sym2);
	internchar(LISP_PACKAGE, "HELLO3", &sym3);
	internchar(LISP_PACKAGE, "HELLO4", &sym4);
	test(pushnewplist_alloc(NULL, Nil, sym1, sym2, &list),
			"pushnewplist_alloc1");
	test(GetType(list) == LISPTYPE_CONS, "pushnewplist_alloc2");
	GetCons(list, &pos, &check);
	test(pos == sym1, "pushnewplist_alloc3");
	GetCons(check, &pos, &check);
	test(check == Nil, "pushnewplist_alloc4");
	GetCons(pos, &pos, &check);
	test(pos == sym2, "pushnewplist_alloc5");
	test(check == Nil, "pushnewplist_alloc6");

	test(pushnewplist_alloc(NULL, list, sym1, T, &list) == 0,
			"pushnewplist_alloc7a");
	test(pushnewplist_alloc(NULL, list, sym1, T, &list) == 0,
			"pushnewplist_alloc7b");
	test(pushnewplist_alloc(NULL, list, sym1, sym2, &list) == 0,
			"pushnewplist_alloc7c");
	test(pushnewplist_alloc(NULL, list, sym1, sym2, &list) == 0,
			"pushnewplist_alloc7d");
	test(getplist(list, sym1, &pos) == 0, "pushnewplist_alloc8");
	GetCons(pos, &check, &pos);
	test(check == T, "pushnewplist_alloc9");
	GetCons(pos, &check, &pos);
	test(check == sym2, "pushnewplist_alloc10");
	test(pos == Nil, "pushnewplist_allo11");

	test(pushnewplist_alloc(NULL, list, sym3, sym4, &list),
			"pushnewplist_alloc12");
	test(getplist(list, sym1, &pos) == 0, "pushnewplist_alloc13");
	test(getplist(list, sym3, &pos) == 0, "pushnewplist_alloc14");
	test(getplist(list, sym4, &pos), "pushnewplist_alloc15");

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

static int test_remplist_check(void)
{
	addr pos, key, key1, key2, key3, value, list;

	GetConstant(CONSTANT_COMMON_SPECIAL, &key);
	test(remplist_check(Nil, key, &pos) == RemPlist_NotFound, "remplist_check1");
	test(pos == Nil, "remplist_check2");

	list_heap(&list, key, T, NULL);
	fixnum_heap(&key1, 10);
	test(remplist_check(list, key1, &pos) == RemPlist_NotFound, "remplist_check3");
	test(pos == list, "remplist_check4");
	test(remplist_check(list, key, &pos) == RemPlist_Update, "remplist_check5");
	test(pos == Nil, "remplist_check6");

	interncommon("CAR", &key1);
	interncommon("CDR", &key2);
	interncommon("CONS", &key3);
	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_check(list, T, &pos) == RemPlist_NotFound, "remplist_check7");
	test(pos == list, "remplist_check8");
	test(remplist_check(list, key1, &pos) == RemPlist_Update, "remplist_check9");
	test(length_list_unsafe(pos) == 4, "remplist_check10");
	test(getplist(pos, key1, &value), "remplist_check11");
	test(getplist(pos, key2, &value) == 0, "remplist_check12");
	test(RefFixnum(value) == 20, "remplist_check13");
	test(getplist(pos, key3, &value) == 0, "remplist_check14");
	test(RefFixnum(value) == 30, "remplist_check15");

	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_check(list, key2, &pos) == RemPlist_Delete, "remplist_check16");
	test(length_list_unsafe(pos) == 4, "remplist_check17");
	test(getplist(pos, key1, &value) == 0, "remplist_check18");
	test(RefFixnum(value) == 10, "remplist_check19");
	test(getplist(pos, key2, &value), "remplist_check20");
	test(getplist(pos, key3, &value) == 0, "remplist_check21");
	test(RefFixnum(value) == 30, "remplist_check22");

	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist_check(list, key3, &pos) == RemPlist_Delete, "remplist_check23");
	test(length_list_unsafe(pos) == 4, "remplist_check24");
	test(getplist(pos, key1, &value) == 0, "remplist_check25");
	test(RefFixnum(value) == 10, "remplist_check26");
	test(getplist(pos, key2, &value) == 0, "remplist_check27");
	test(RefFixnum(value) == 20, "remplist_check28");
	test(getplist(pos, key3, &value), "remplist_check29");

	RETURN;
}

static int test_remplist(void)
{
	addr pos, list, key1, key2, key3;

	pos = NULL;
	test(! remplist(Nil, T, &pos), "remplist1");
	test(pos == Nil, "remplist2");

	interncommon("CAR", &key1);
	interncommon("CDR", &key2);
	interncommon("CONS", &key3);
	list_heap(&list,
			key1, fixnumh(10),
			key2, fixnumh(20),
			key3, fixnumh(30), NULL);
	test(remplist(list, key1, &pos), "remplist3");
	test(! remplist(pos, key3, &pos), "remplist4");

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
			"getplist_constant1");
	test(pos == sym4, "getplist_constant2");

	RETURN;
}

static int test_setplist_constant_alloc(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(setplist_constant_alloc(NULL, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"setplist_constant_alloc1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"setplist_constant_alloc2");
	test(pos == symbol, "setplist_constant_alloc3");

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
			"setplist_constant_local1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"setplist_constant_local2");
	test(pos == symbol, "setplist_constant_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_setplist_constant_heap(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(setplist_constant_heap(Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"setplist_constant_heap1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"setplist_constant_heap2");
	test(pos == symbol, "setplist_constant_heap3");

	RETURN;
}

static int test_pushplist_constant_alloc(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(pushplist_constant_alloc(NULL, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"pushplist_constant_alloc1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"pushplist_constant_alloc2");
	test(GetType(pos) == LISPTYPE_CONS, "pushplist_constant_alloc3");

	RETURN;
}

static int test_pushplist_constant_local(void)
{
	addr pos, list, symbol;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(pushplist_constant_local(local, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"pushplist_constant_local1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"pushplist_constant_local2");
	test(GetType(pos) == LISPTYPE_CONS, "pushplist_constant_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_pushplist_constant_heap(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(pushplist_constant_heap(Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"pushplist_constant_heap1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"pushplist_constant_heap2");
	test(GetType(pos) == LISPTYPE_CONS, "pushplist_constant_heap3");

	RETURN;
}

static int test_pushnewplist_constant_alloc(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(pushnewplist_constant_alloc(NULL, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"pushnewplist_constant_alloc1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"pushnewplist_constant_alloc2");
	test(GetType(pos) == LISPTYPE_CONS, "pushnewplist_constant_alloc3");

	RETURN;
}

static int test_pushnewplist_constant_local(void)
{
	addr pos, list, symbol;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(pushnewplist_constant_local(local, Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"pushnewplist_constant_local1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"pushnewplist_constant_local2");
	test(GetType(pos) == LISPTYPE_CONS, "pushnewplist_constant_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_pushnewplist_constant_heap(void)
{
	addr pos, list, symbol;

	GetConstant(CONSTANT_COMMON_EQL, &symbol);
	test(pushnewplist_constant_heap(Nil, CONSTANT_COMMON_EQ, symbol, &list),
			"pushnewplist_constant_heap1");
	test(getplist_constant(list, CONSTANT_COMMON_EQ, &pos) == 0,
			"pushnewplist_constant_heap2");
	test(GetType(pos) == LISPTYPE_CONS, "pushnewplist_constant_heap3");

	RETURN;
}

static int test_remplist_check_constant(void)
{
	enum RemPlist result;
	addr pos;

	GetConstant(CONSTANT_COMMON_SPECIAL, &pos);
	list_heap(&pos, pos, T, NULL);
	result = remplist_check_constant(pos, CONSTANT_COMMON_SPECIAL, &pos);
	test(result == RemPlist_Update, "remplist_check_constant1");
	test(pos == Nil, "remplist_check_constant2");

	pos = NULL;
	result = remplist_check_constant(Nil, CONSTANT_COMMON_SPECIAL, &pos);
	test(result == RemPlist_NotFound, "remplist_check_constant3");
	test(pos == Nil, "remplist_check_constant4");

	RETURN;
}

static int test_remplist_constant(void)
{
	int result;
	addr pos;

	GetConstant(CONSTANT_COMMON_SPECIAL, &pos);
	list_heap(&pos, pos, T, NULL);
	result = remplist_constant(pos, CONSTANT_COMMON_SPECIAL, &pos);
	test(result, "remplist_constant1");
	test(pos == Nil, "remplist_constant2");

	pos = NULL;
	result = remplist_constant(Nil, CONSTANT_COMMON_SPECIAL, &pos);
	test(! result, "remplist_constant3");
	test(pos == Nil, "remplist_constant4");

	RETURN;
}

static int test_getplistplist(void)
{
	addr list, sym1, sym2, sym3, pos;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);
	list_heap(&list, sym1, sym2, NULL);
	list_heap(&list, sym3, list, sym1, Nil, NULL);
	test(getplistplist(list, sym3, sym1, &pos) == 0, "getplistplist1");
	test(pos == sym2, "getplistplist2");
	test(getplistplist(list, sym3, sym2, &pos), "getplistplist3");
	test(getplistplist(list, sym1, sym2, &pos), "getplistplist4");
	test(getplistplist(list, sym2, sym2, &pos), "getplistplist5");

	RETURN;
}

static int test_setplistplist_alloc(void)
{
	addr list, pos, sym1, sym2, sym3, sym4;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);
	GetConstant(CONSTANT_COMMON_EQUALP, &sym4);

	test(setplistplist_alloc(NULL, Nil, sym1, sym2, sym3, &list),
			"setplistplist_alloc1");
	test(getplistplist(list, sym1, sym2, &pos) == 0, "setplistplist_alloc2");
	test(pos == sym3, "setplistplist_alloc3");

	test(setplistplist_alloc(NULL, list, sym1, sym2, sym4, &list) == 0,
			"setplistplist_alloc4");
	test(getplistplist(list, sym1, sym2, &pos) == 0, "setplistplist_alloc5");
	test(pos == sym4, "setplistplist_alloc6");

	test(setplistplist_alloc(NULL, list, sym1, sym3, sym1, &list) == 0,
			"setplistplist_alloc7");
	test(getplistplist(list, sym1, sym2, &pos) == 0, "setplistplist_alloc8");
	test(pos == sym4, "setplistplist_alloc9");
	test(getplistplist(list, sym1, sym3, &pos) == 0, "setplistplist_alloc10");
	test(pos == sym1, "setplistplist_alloc11");

	test(setplistplist_alloc(NULL, list, sym2, sym3, sym1, &list),
			"setplistplist_alloc12");
	test(getplistplist(list, sym2, sym3, &pos) == 0, "setplistplist_alloc13");
	test(pos == sym1, "setplistplist_alloc14");

	test(getplistplist(list, sym1, sym2, &pos) == 0, "setplistplist_alloc15");
	test(pos == sym4, "setplistplist_alloc16");

	RETURN;
}

static int test_setplistplist_local(void)
{
	addr list, pos, sym1, sym2, sym3;
	LocalRoot local;
	LocalStack stack;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);

	local = Local_Thread;
	push_local(local, &stack);
	test(setplistplist_local(local, Nil, sym1, sym2, sym3, &list),
			"setplistplist_local1");
	test(getplistplist(list, sym1, sym2, &pos) == 0, "setplistplist_local2");
	test(pos == sym3, "setplistplist_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_setplistplist_heap(void)
{
	addr list, pos, sym1, sym2, sym3;

	GetConstant(CONSTANT_COMMON_EQ, &sym1);
	GetConstant(CONSTANT_COMMON_EQL, &sym2);
	GetConstant(CONSTANT_COMMON_EQUAL, &sym3);

	test(setplistplist_heap(Nil, sym1, sym2, sym3, &list),
			"setplistplist_heap1");
	test(getplistplist(list, sym1, sym2, &pos) == 0, "setplistplist_heap2");
	test(pos == sym3, "setplistplist_heap3");

	RETURN;
}

static int testbreak_cons(void)
{
	TestBreak(test_getcons);
	TestBreak(test_length_list_safe);
	TestBreak(test_length_list_unsafe);
	TestBreak(test_append2_alloc_unsafe);
	TestBreak(test_find_list_eq_unsafe);
	/* reverse */
	TestBreak(test_nreverse_list_unsafe);
	TestBreak(test_nreverse_list_unsafe_inplace);
	TestBreak(test_nreverse_list_unsafe_dotted);
	TestBreak(test_reverse_list_heap_unsafe);
	TestBreak(test_reverse_list_local_unsafe);
	/* list */
	TestBreak(test_lista_alloc_safe);
	TestBreak(test_lista_allocr);
	TestBreak(test_lista_alloc);
	TestBreak(test_List_bind);
	TestBreak(test_Lista_bind);
	/* plist */
	TestBreak(test_pushnew_alloc);
	TestBreak(test_plist);
	TestBreak(test_getplist);
	TestBreak(test_setplist_alloc);
	TestBreak(test_setplist_local);
	TestBreak(test_setplist_heap);
	TestBreak(test_pushplist_alloc);
	TestBreak(test_pushplist_local);
	TestBreak(test_pushplist_heap);
	TestBreak(test_pushnewplist_alloc);
	TestBreak(test_pushnewplist_local);
	TestBreak(test_pushnewplist_heap);
	TestBreak(test_remplist_check);
	TestBreak(test_remplist);
	TestBreak(test_getplist_constant);
	TestBreak(test_setplist_constant_alloc);
	TestBreak(test_setplist_constant_local);
	TestBreak(test_setplist_constant_heap);
	TestBreak(test_pushplist_constant_alloc);
	TestBreak(test_pushplist_constant_local);
	TestBreak(test_pushplist_constant_heap);
	TestBreak(test_pushnewplist_constant_alloc);
	TestBreak(test_pushnewplist_constant_local);
	TestBreak(test_pushnewplist_constant_heap);
	TestBreak(test_remplist_check_constant);
	TestBreak(test_remplist_constant);
	TestBreak(test_getplistplist);
	TestBreak(test_setplistplist_alloc);
	TestBreak(test_setplistplist_local);
	TestBreak(test_setplistplist_heap);

	return 0;
}

int test_cons(void)
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
		lisp_initialize = 1;
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_common();
		result = testbreak_cons();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

