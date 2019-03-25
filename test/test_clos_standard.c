#include "clos_standard.c"
#include "build.h"
#include "character.h"
#include "clos.h"
#include "cons.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "hashtable.h"
#include "heap.h"
#include "local.h"
#include "object.h"
#include "package.h"
#include "stream.h"

/*
 *  class-precedence-list
 */
static addr test_class_call(Execute ptr, addr pos)
{
	clos_elt(pos, Clos_class_slots, &pos);
	return pos;
}

static int test_call_class_direct_superclasses(void)
{
	addr clos, value, check;
	Execute ptr;

	ptr = Execute_Thread;
	vector4_heap(&clos, Clos_class_size);
	clos_heap(&clos, clos);
	fixnum_heap(&value, 100);
	setf_clos_elt(clos, Clos_class_direct_superclasses, value);
	setf_clos_elt(clos, Clos_class_slots, T);
	call_class_direct_superclasses(ptr, NULL, clos, &check);
	test(check == value, "call_class_direct_superclasses1");
	call_class_direct_superclasses(ptr, test_class_call, clos, &check);
	test(check == T, "call_class_direct_superclasses2");

	RETURN;
}

static void clos_supers_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr super, cons, clos, slots;

	/* make cons */
	cons = Nil;
	for (;;) {
		super = va_arg(args, addr);
		if (super == NULL) break;
		cons_alloc(local, &cons, super, cons);
	}

	/* make clos */
	vector4_alloc(local, &slots, Clos_class_size);
	clos_alloc(local, &clos, slots);
	nreverse_list_unsafe(&cons, cons);
	setf_clos_elt(clos, Clos_class_direct_superclasses, cons);
	*ret = clos;
}
static void clos_supers_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	clos_supers_alloc(local, ret, args);
	va_end(args);
}
static void clos_supers_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	clos_supers_alloc(NULL, ret, args);
	va_end(args);
}

static int test_direct_superclasses_list(void)
{
	addr clos, left, right;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* test */
	clos_supers_local(local, &clos, NULL);
	direct_superclasses_list(ptr, NULL, clos, &right);
	test(GetType(right) == LISPTYPE_CONS, "direct_superclasses_list1");
	GetCons(right, &left, &right);
	test(left == clos, "direct_superclasses_list2");
	test(right != Nil, "direct_superclasses_list3");
	GetCons(right, &left, &right);
	test(left == Unbound, "direct_superclasses_list4");
	test(right == Nil, "direct_superclasses_list5");

	clos_supers_local(local, &clos, Nil, T, NULL);
	direct_superclasses_list(ptr, NULL, clos, &right);
	test(GetType(right) == LISPTYPE_CONS, "direct_superclasses_list6");
	GetCons(right, &left, &right);
	test(left == clos, "direct_superclasses_list7");
	test(right != Nil, "direct_superclasses_list8");
	GetCons(right, &left, &right);
	test(left == Nil, "direct_superclasses_list9");
	test(right != Nil, "direct_superclasses_list10");
	GetCons(right, &left, &right);
	test(left == T, "direct_superclasses_list11");
	test(right != Nil, "direct_superclasses_list12");
	GetCons(right, &left, &right);
	test(left == Unbound, "direct_superclasses_list13");
	test(right == Nil, "direct_superclasses_list14");

	rollback_local(local, stack);

	RETURN;
}

static int test_direct_superclasses_chain(void)
{
	addr clos, left, right, cons, a, b;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* (clos unbound) -> ((clos . unbound)) */
	clos_supers_local(local, &clos, NULL);
	direct_superclasses_chain(ptr, NULL, clos, &right);

	test(GetType(right) == LISPTYPE_CONS, "direct_superclasses_chain1");
	GetCons(right, &left, &right);
	test(GetType(left) == LISPTYPE_CONS, "direct_superclasses_chain2");
	test(right == Nil, "direct_superclasses_chain3");
	GetCons(left, &left, &right);
	test(left == clos, "direct_superclasses_chain4");
	test(right == Unbound, "direct_superclasses_chain5");

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	direct_superclasses_chain(ptr, NULL, clos, &cons);
	GetCons(cons, &left, &cons);
	GetCons(left, &left, &right);
	test(left == b, "direct_superclasses_chain6");
	test(right == Unbound, "direct_superclasses_chain7");
	GetCons(cons, &left, &cons);
	GetCons(left, &left, &right);
	test(left == a, "direct_superclasses_chain8");
	test(right == b, "direct_superclasses_chain9");
	GetCons(cons, &left, &cons);
	GetCons(left, &left, &right);
	test(left == clos, "direct_superclasses_chain10");
	test(right == a, "direct_superclasses_chain11");
	test(cons == Nil, "direct_superclasses_chain12");

	rollback_local(local, stack);

	RETURN;
}

static int test_all_superclass_list(void)
{
	addr clos, left, right, a, b, c, d, e;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	clos_supers_local(local, &clos, NULL);
	all_superclass_list(ptr, NULL, clos, &right);
	test(right != Nil, "all_superclass_list1");
	GetCons(right, &left, &right);
	test(left == clos, "all_superclass_list2");
	test(right == Nil, "all_superclass_list3");

	clos_supers_local(local, &a, NULL);
	clos_supers_local(local, &b, a, NULL);
	clos_supers_local(local, &c, b, NULL);
	clos_supers_local(local, &d, NULL);
	clos_supers_local(local, &e, d, NULL);
	clos_supers_local(local, &clos, c, d, e, NULL);
	all_superclass_list(ptr, NULL, clos, &right);
	test(length_list_unsafe(right) == 6, "all_superclass_list4");
	test(find_list_eq_unsafe(a, right) &&
			find_list_eq_unsafe(b, right) &&
			find_list_eq_unsafe(c, right) &&
			find_list_eq_unsafe(d, right) &&
			find_list_eq_unsafe(e, right) &&
			find_list_eq_unsafe(clos, right), "all_superclass_list5");

	rollback_local(local, stack);

	RETURN;
}

static int test_find_chain_cons(void)
{
	addr clos, left, right, a, b;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	direct_superclasses_chain(ptr, NULL, clos, &right);

	cons_local(local, &left, a, b);
	test(find_chain_cons(left, right), "find_chain_cons1");
	cons_local(local, &left, a, Nil);
	test(! find_chain_cons(left, right), "find_chain_cons2");
	cons_local(local, &left, Nil, b);
	test(! find_chain_cons(left, right), "find_chain_cons3");
	cons_local(local, &left, b, a);
	test(! find_chain_cons(left, right), "find_chain_cons4");
	cons_local(local, &left, b, Unbound);
	test(find_chain_cons(left, right), "find_chain_cons5");
	cons_local(local, &left, clos, a);
	test(find_chain_cons(left, right), "find_chain_cons6");

	rollback_local(local, stack);

	RETURN;
}

static int test_find_cons_chain_check(addr car, addr cdr, addr cons)
{
	cons_local(Local_Thread, &cdr, car, cdr);
	return find_chain_cons(cdr, cons);
}

static int test_superclasses_chain(void)
{
	addr clos, cons, a, b, c, d, e, t;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* (t . Unbound) */
	clos_supers_local(local, &t, NULL);
	superclasses_chain(ptr, NULL, t, &cons);
	test(length_list_unsafe(cons) == 1, "superclasses_chain1");
	test(test_find_cons_chain_check(t, Unbound, cons), "superclasses_chain2");

	clos_supers_local(local, &t, NULL);
	clos_supers_local(local, &a, t, NULL);
	clos_supers_local(local, &b, t, NULL);
	clos_supers_local(local, &c, a, b, t, NULL);
	clos_supers_local(local, &d, t, NULL);
	clos_supers_local(local, &e, t, NULL);
	clos_supers_local(local, &clos, d, c, e, t, NULL);
	superclasses_chain(ptr, NULL, clos, &cons);
	/* (t . Unbound) (a . t) (b . t) (c . a) (a . b)
	   (d . t) (e . t) (clos . d) (d . c) (c . e) */
	test(length_list_unsafe(cons) == 10, "superclasses_chain3");
	test(test_find_cons_chain_check(t, Unbound, cons), "superclasses_chain4");
	test(test_find_cons_chain_check(a, t, cons), "superclasses_chain5");
	test(test_find_cons_chain_check(b, t, cons), "superclasses_chain6");
	test(test_find_cons_chain_check(c, a, cons), "superclasses_chain7");
	test(test_find_cons_chain_check(a, b, cons), "superclasses_chain8");
	test(test_find_cons_chain_check(d, t, cons), "superclasses_chain9");
	test(test_find_cons_chain_check(e, t, cons), "superclasses_chain10");
	test(test_find_cons_chain_check(clos, d, cons), "superclasses_chain11");
	test(test_find_cons_chain_check(d, c, cons), "superclasses_chain12");
	test(test_find_cons_chain_check(c, e, cons), "superclasses_chain13");

	rollback_local(local, stack);

	RETURN;
}

static int test_find_top_superclass(void)
{
	addr clos, left, right, a, b;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	direct_superclasses_chain(ptr, NULL, clos, &right);
	find_top_superclass(right, &left);
	test(left == clos, "find_top_supercalss1");

	rollback_local(local, stack);

	RETURN;
}

static int test_remove_top_superclass(void)
{
	addr clos, left, right, a, b, c, check;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	clos_supers_local(local, &clos, a, b, NULL);
	direct_superclasses_chain(ptr, NULL, clos, &right);
	remove_top_superclass(b, right, &right);
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == a, "remove_top_class1");
	test(check == b, "remove_top_class2");
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == clos, "remove_top_class3");
	test(check == a, "remove_top_class4");
	test(right == Nil, "remove_top_class5");

	/* ((A . b)) */
	cons_local(local, &right, a, b);
	conscar_local(local, &right, right);
	remove_top_superclass(a, right, &right);
	test(right == Nil, "remove_top_class6");

	/* ((A . b) (b . a)) */
	cons_local(local, &left, b, a);
	conscar_local(local, &right, left);
	cons_local(local, &left, a, b);
	cons_local(local, &right, left, right);
	remove_top_superclass(a, right, &right);
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == b, "remove_top_class7");
	test(check == a, "remove_top_class8");
	test(right == Nil, "remove_top_class9");

	/* ((b . a) (A . b)) */
	cons_local(local, &left, a, b);
	conscar_local(local, &right, left);
	cons_local(local, &left, b, a);
	cons_local(local, &right, left, right);
	remove_top_superclass(a, right, &right);
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == b, "remove_top_class10");
	test(check == a, "remove_top_class11");
	test(right == Nil, "remove_top_class12");

	/* ((A . b) (b . c) (c . unbound)) */
	/* ((b . c) (A . b) (c . unbound)) */
	/* ((b . c) (c . a) (A . b)) */

	rollback_local(local, stack);

	RETURN;
}

static int no_dynamic_check(addr right)
{
	while (right != Nil) {
		if (GetStatusDynamic(right)) return 0;
		GetCdr(right, &right);
	}

	return 1;
}

static int test_result_class_precedence_list(void)
{
	addr clos, left, right;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	clos_supers_heap(&clos, NULL);
	result_class_precedence_list(ptr, NULL, clos, &right);
	test(right != Nil, "result_class_precedence_list1");
	test(no_dynamic_check(right), "result_class_precedence_list2");
	GetCons(right, &left, &right);
	test(left == clos, "result_class_precedence_list3");
	test(right == Nil, "result_class_precedence_list4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Common Lisp the Language, 2nd Edition
 *    28. Common Lisp Object System
 *    28.1.5.2. Examples
 *      (defclass pie (apple cinnamon) ())
 *      (defclass apple (fruit) ())
 *      (defclass cinnamon (spice) ())
 *      (defclass fruit (food) ())
 *      (defclass spice (food) ())
 *      (defclass food () ())
 *         -> (pie apple fruit cinnamon spice food standard-object t)
 *
 *      (defclass pie (apple cinnamon) ())
 *      (defclass pastry (cinnamon apple) ())
 *      (defclass apple () ())
 *      (defclass cinnamon () ())
 *         -> (pie apple cinnamon standard-object t)
 *         -> (pastry cinnamon apple standard-object t)
 */
static int cons_check(addr right, ...)
{
	addr left, check;
	va_list args;

	va_start(args, right);
	while (right != Nil) {
		check = va_arg(args, addr);
		if (check == NULL) return 0;
		GetCons(right, &left, &right);
		if (check != left) return 0;
	}
	va_end(args);

	return 1;
}

static int test_std_compute_class_precedence_list(void)
{
	addr pie, apple, cinnamon, fruit, spice, food, pastry, t, object;
	addr cons;
	Execute ptr;

	ptr = Execute_Thread;
	clos_supers_heap(&t, NULL);
	clos_supers_heap(&object, t, NULL);
	clos_supers_heap(&food, object, NULL);
	clos_supers_heap(&spice, food, object, NULL);
	clos_supers_heap(&fruit, food, object, NULL);
	clos_supers_heap(&cinnamon, spice, object, NULL);
	clos_supers_heap(&apple, fruit, object, NULL);
	clos_supers_heap(&pie, apple, cinnamon, object, NULL);

	std_compute_class_precedence_list(ptr, pie, &cons);
	test(no_dynamic_check(cons), "std_compute_class_precedence_list1");
	test(cons_check(cons, pie, apple, fruit, cinnamon, spice, food, object, t, NULL),
			"std_compute_class_precedence_list2");

	clos_supers_heap(&t, NULL);
	clos_supers_heap(&object, t, NULL);
	clos_supers_heap(&cinnamon, object, NULL);
	clos_supers_heap(&apple, object, NULL);
	clos_supers_heap(&pastry, cinnamon, apple, object, NULL);
	clos_supers_heap(&pie, apple, cinnamon, object, NULL);

	std_compute_class_precedence_list(ptr, pie, &cons);
	test(no_dynamic_check(cons), "std_compute_class_precedence_list3");
	test(cons_check(cons, pie, apple, cinnamon, object, t, NULL),
			"std_compute_class_precedence_list4");

	std_compute_class_precedence_list(ptr, pastry, &cons);
	test(no_dynamic_check(cons), "std_compute_class_precedence_list5");
	test(cons_check(cons, pastry, cinnamon, apple, object, t, NULL),
			"std_compute_class_precedence_list6");

	RETURN;
}


/*
 *  compute-slots
 */
static int test_call_class_precedence_list(void)
{
	addr clos, value, check;
	Execute ptr;

	ptr = Execute_Thread;
	vector4_heap(&clos, Clos_class_size);
	clos_heap(&clos, clos);
	fixnum_heap(&value, 100);
	setf_clos_elt(clos, Clos_class_precedence_list, value);
	setf_clos_elt(clos, Clos_class_slots, T);
	call_class_precedence_list(ptr, NULL, clos, &check);
	test(check == value, "call_class_precedence_list1");
	call_class_precedence_list(ptr, test_class_call, clos, &check);
	test(check == T, "call_class_precedence_list2");

	RETURN;
}

static int test_call_class_direct_slots(void)
{
	addr clos, value, check;
	Execute ptr;

	ptr = Execute_Thread;
	vector4_heap(&clos, Clos_class_size);
	clos_heap(&clos, clos);
	fixnum_heap(&value, 100);
	setf_clos_elt(clos, Clos_class_direct_slots, value);
	setf_clos_elt(clos, Clos_class_slots, T);
	call_class_direct_slots(ptr, NULL, clos, &check);
	test(check == value, "call_class_direct_slots1");
	call_class_direct_slots(ptr, test_class_call, clos, &check);
	test(check == T, "call_class_direct_slots2");

	RETURN;
}

static int test_member_slotname(void)
{
	addr slot1, slot2, slot3, pos, cons;

	slot_heap(&slot1);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	SetSlot(slot1, SLOT_INDEX_NAME, pos);

	member_slotname(&cons, slot1, Nil);
	test(cons == Nil, "member_slotname1");

	slot_heap(&slot2);
	internchar(LISP_PACKAGE, "AAA", &pos);
	SetSlot(slot2, SLOT_INDEX_NAME, pos);

	slot_heap(&slot3);
	internchar(LISP_PACKAGE, "BBB", &pos);
	SetSlot(slot3, SLOT_INDEX_NAME, pos);

	list_heap(&cons, slot2, slot3, NULL);
	member_slotname(&cons, slot1, cons);
	test(cons == Nil, "member_slotname2");

	slot_heap(&slot3);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	SetSlot(slot3, SLOT_INDEX_NAME, pos);

	list_heap(&cons, slot2, slot3, NULL);
	member_slotname(&cons, slot1, cons);
	test(cons != Nil, "member_slotname3");

	RETURN;
}

static void slotname_heap(addr *ret, const char *name)
{
	addr symbol;
	slot_heap(ret);
	internchar(LISP_PACKAGE, name, &symbol);
	SetSlot(*ret, SLOT_INDEX_NAME, symbol);
}

static void test_makeclos_heap(addr *ret, ...)
{
	const char *name;
	addr cons, slot, slots, clos, temp;
	size_t i;
	va_list args;

	/* make list */
	cons = Nil;
	va_start(args, ret);
	for (i = 0; ; i++) {
		name = va_arg(args, const char *);
		if (name == NULL) break;
		slotname_heap(&slot, name);
		cons_heap(&cons, slot, cons);
	}
	va_end(args);
	nreverse_list_unsafe(&cons, cons);

	/* make clos */
	vector4_heap(&slots, i);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &slot, &cons);
		SetArrayA4(slots, i, slot);
	}
	vector4_heap(&temp, Clos_class_size);
	clos_heap(&clos, temp);
	setf_clos_elt(clos, Clos_class_direct_slots, slots);
	list_heap(&temp, clos, NULL);
	setf_clos_elt(clos, Clos_class_precedence_list, temp);
	*ret = clos;
}

static int slotnamecheck(addr slot, const char *name)
{
	addr check;
	GetSlot(slot, SLOT_INDEX_NAME, &slot);
	internchar(LISP_PACKAGE, name, &check);
	return check == slot;
}

static int test_gather_slots_heap(void)
{
	Execute ptr;
	addr clos1, clos2, cons, check, aaa;
	size_t size;

	ptr = Execute_Thread;
	test_makeclos_heap(&clos1, "HELLO", "AAA", "BBB", NULL);
	gather_slots_heap(ptr, NULL, NULL, clos1, &cons, &size);
	test(cons != Nil, "gather_slots_heap1");
	test(size == 3, "gather_slots_heap2");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "gather_slots_heap3");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "AAA"), "gather_slots_heap4");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "gather_slots_heap5");
	test(cons == Nil, "gather_slots_heap6");

	test_makeclos_heap(&clos2, "AAA", "CCC", NULL);
	list_heap(&cons, clos2, clos1, NULL);
	setf_clos_elt(clos2, Clos_class_precedence_list, cons);
	gather_slots_heap(ptr, NULL, NULL, clos2, &cons, &size);
	test(size == 4, "gather_slots_heap7");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "gather_slots_heap8");
	GetCons(cons, &check, &cons);
	aaa = check;
	test(slotnamecheck(check, "AAA"), "gather_slots_heap9");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "gather_slots_heap10");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "CCC"), "gather_slots_heap11");
	test(cons == Nil, "gather_slots_heap12");

	clos_elt(clos2, Clos_class_direct_slots, &cons);
	GetArrayA4(cons, 0, &cons);
	test(cons == aaa, "gather_slots_heap13");

	RETURN;
}

static int test_std_compute_slots_heap(void)
{
	Execute ptr;
	addr clos1, clos2, cons, check, aaa;

	ptr = Execute_Thread;
	test_makeclos_heap(&clos1, "HELLO", "AAA", "BBB", NULL);
	std_compute_slots_heap(ptr, clos1, &cons);
	test(cons != Nil, "std_compute_slots_heap1");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "std_compute_slots_heap2");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "AAA"), "std_compute_slots_heap3");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "std_compute_slots_heap4");
	test(cons == Nil, "std_compute_slots_heap5");

	test_makeclos_heap(&clos2, "AAA", "CCC", NULL);
	list_heap(&cons, clos2, clos1, NULL);
	setf_clos_elt(clos2, Clos_class_precedence_list, cons);
	std_compute_slots_heap(ptr, clos2, &cons);
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "std_compute_slots_heap6");
	GetCons(cons, &check, &cons);
	aaa = check;
	test(slotnamecheck(check, "AAA"), "std_compute_slots_heap7");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "std_compute_slots_heap8");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "CCC"), "std_compute_slots_heap9");
	test(cons == Nil, "std_compute_slots_heap10");

	clos_elt(clos2, Clos_class_direct_slots, &cons);
	GetArrayA4(cons, 0, &cons);
	test(cons != aaa, "std_compute_slots_heap11");

	RETURN;
}

static int test_std_compute_slots_vector_heap(void)
{
	Execute ptr;
	addr clos1, clos2, cons, check, aaa;
	size_t size;

	ptr = Execute_Thread;
	test_makeclos_heap(&clos1, "HELLO", "AAA", "BBB", NULL);
	std_compute_slots_vector_heap(ptr, clos1, &cons);
	test(cons != Nil, "std_compute_slots_vector_heap1");
	GetArrayA4(cons, 0, &check);
	test(slotnamecheck(check, "HELLO"), "std_compute_slots_vector_heap2");
	GetArrayA4(cons, 1, &check);
	test(slotnamecheck(check, "AAA"), "std_compute_slots_vector_heap3");
	GetArrayA4(cons, 2, &check);
	test(slotnamecheck(check, "BBB"), "std_compute_slots_vector_heap4");
	LenArrayA4(cons, &size);
	test(size == 3, "std_compute_slots_vector_heap5");

	test_makeclos_heap(&clos2, "AAA", "CCC", NULL);
	list_heap(&cons, clos2, clos1, NULL);
	setf_clos_elt(clos2, Clos_class_precedence_list, cons);
	std_compute_slots_vector_heap(ptr, clos2, &cons);
	GetArrayA4(cons, 0, &check);
	test(slotnamecheck(check, "HELLO"), "std_compute_slots_vector_heap6");
	GetArrayA4(cons, 1, &check);
	aaa = check;
	test(slotnamecheck(check, "AAA"), "std_compute_slots_vector_heap7");
	GetArrayA4(cons, 2, &check);
	test(slotnamecheck(check, "BBB"), "std_compute_slots_vector_heap8");
	GetArrayA4(cons, 3, &check);
	test(slotnamecheck(check, "CCC"), "std_compute_slots_vector_heap9");
	LenArrayA4(cons, &size);
	test(size == 4, "std_compute_slots_vector_heap10");

	clos_elt(clos2, Clos_class_direct_slots, &cons);
	GetArrayA4(cons, 0, &cons);
	test(cons != aaa, "std_compute_slots_vector_heap11");

	RETURN;
}


/*
 *  standard-class
 */
static int test_default_slot_nametype(void)
{
	addr pos, check, type;

	vector4_heap(&pos, Clos_class_size);
	default_slot_nametype(pos, Clos_class_name, CONSTANT_CLOSNAME_NAME);
	GetArrayA4(pos, Clos_class_name, &pos);
	GetSlot(pos, SLOT_INDEX_TYPE, &type);
	GetSlot(pos, SLOT_INDEX_NAME, &pos);
	GetConstant(CONSTANT_CLOSNAME_NAME, &check);
	test(pos == check, "default_slot_nametype1");
	GetConstant(CONSTANT_COMMON_SYMBOL, &check);
	test(type == check, "default_slot_nametype2");

	RETURN;
}

static int test_default_slot_name(void)
{
	addr pos, check;

	vector4_heap(&pos, Clos_class_size);
	default_slot_name(pos, Clos_class_slots, CONSTANT_CLOSNAME_EFFECTIVE_SLOTS);
	GetArrayA4(pos, Clos_class_slots, &pos);
	GetSlot(pos, SLOT_INDEX_NAME, &pos);
	GetConstant(CONSTANT_CLOSNAME_EFFECTIVE_SLOTS, &check);
	test(pos == check, "default_slot_name1");

	RETURN;
}

static int test_default_slot_initform(void)
{
	addr pos, check, value;

	vector4_heap(&pos, Clos_class_size);
	default_slot_initform(pos, Clos_class_name, CONSTANT_CLOSNAME_NAME, T);
	GetArrayA4(pos, Clos_class_name, &pos);
	GetSlot(pos, SLOT_INDEX_INITFORM, &value);
	GetSlot(pos, SLOT_INDEX_NAME, &pos);
	GetConstant(CONSTANT_CLOSNAME_NAME, &check);
	test(pos == check, "default_slot_initform1");
	test(value == T, "default_slot_initform2");

	RETURN;
}

static int test_set_slots_localtion(void)
{
	addr slots, slot;
	size_t check;

	vector4_heap(&slots, 3);
	slot_heap(&slot);
	SetArrayA4(slots, 0, slot);
	slot_heap(&slot);
	SetArrayA4(slots, 1, slot);
	slot_heap(&slot);
	SetArrayA4(slots, 2, slot);
	set_slots_localtion(slots);

	GetArrayA4(slots, 0, &slot);
	GetSlot(slot, SLOT_INDEX_LOCATION, &slot);
	GetIndex(slot, &check);
	test(check == 0, "set_slots_location1");
	GetArrayA4(slots, 1, &slot);
	GetSlot(slot, SLOT_INDEX_LOCATION, &slot);
	GetIndex(slot, &check);
	test(check == 1, "set_slots_location2");
	GetArrayA4(slots, 2, &slot);
	GetSlot(slot, SLOT_INDEX_LOCATION, &slot);
	GetIndex(slot, &check);
	test(check == 2, "set_slots_location3");

	RETURN;
}

static int test_slotname(addr slots, int index, const char *name)
{
	addr pos, check;

	GetArrayA4(slots, index, &pos);
	GetSlot(pos, SLOT_INDEX_NAME, &pos);
	internchar_keyword(name, &check);

	return check == pos;
}

static int test_make_standard_class_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	make_standard_class_slots(&slots);
	LenArrayA4(slots, &size);
	test(size == Clos_class_size, "make_standard_class_slots1");

	for (check = 1, i = 0; i < Clos_class_size; i++) {
		GetArrayA4(slots, i, &pos);
		GetSlot(pos, SLOT_INDEX_LOCATION, &pos);
		GetIndex(pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "make_standard_class_slots2");
	test(test_slotname(slots, Clos_class_name, "NAME"),
			"make_standard_class_slots3");
	test(test_slotname(slots, Clos_class_direct_slots, "DIRECT-SLOTS"),
			"make_standard_class_slots4");
	test(test_slotname(slots, Clos_class_direct_subclasses, "DIRECT-SUBCLASSES"),
			"make_standard_class_slots5");
	test(test_slotname(slots, Clos_class_direct_superclasses, "DIRECT-SUPERCLASSES"),
			"make_standard_class_slots6");
	test(test_slotname(slots, Clos_class_precedence_list, "CLASS-PRECEDENCE-LIST"),
			"make_standard_class_slots7");
	test(test_slotname(slots, Clos_class_slots, "EFFECTIVE-SLOTS"),
			"make_standard_class_slots8");
	test(test_slotname(slots, Clos_class_finalized_p, "FINALIZED-P"),
			"make_standard_class_slots9");
	test(test_slotname(slots, Clos_class_prototype, "PROTOTYPE"),
			"make_standard_class_slots10");
	test(test_slotname(slots, Clos_class_direct_methods, "DIRECT-METHODS"),
			"make_standard_class_slots11");
	test(test_slotname(slots, Clos_class_direct_shared, "DIRECT-SHARED"),
			"make_standard_class_slots12");
	test(test_slotname(slots, Clos_class_default_initargs, "DEFAULT-INITARGS"),
			"make_standard_class_slots13");
	test(test_slotname(slots, Clos_class_direct_default_initargs,
				"DIRECT-DEFAULT-INITARGS"),
			"make_standard_class_slots14");
	test(test_slotname(slots, Clos_class_version, "VERSION"),
			"make_standard_class_slots15");
	test(test_slotname(slots, Clos_class_update_info, "UPDATE-INFO"),
			"make_standard_class_slots16");

	RETURN;
}

static int test_dummy_standard_class(void)
{
	addr slots, clos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	make_standard_class_slots(&slots);
	dummy_standard_class(NULL, &clos, slots);
	test(closp(clos), "dummy_standard_class1");
	clos_elt(clos, Clos_class_direct_slots, &check);
	test(check == slots, "dummy_standard_class2");
	clos_elt(clos, Clos_class_slots, &check);
	test(check == slots, "dummy_standard_class3");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_class_slots(void)
{
	addr slots, clos, name, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	make_standard_class_slots(&slots);
	dummy_standard_class(NULL, &clos, slots);
	vector4_heap(&slots, 4);
	internchar(LISP_PACKAGE, "HELLO", &name);
	make_class_slots(&clos, clos, name, slots);
	clos_elt(clos, Clos_class_name, &check);
	test(check == name, "make_class_slots1");
	clos_elt(clos, Clos_class_prototype, &check);
	test(check == clos, "make_class_slots2");
	clos_elt(clos, Clos_class_finalized_p, &check);
	test(check == T, "make_class_slots3");
	clos_elt(clos, Clos_class_direct_slots, &check);
	test(check == slots, "make_class_slots4");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_direct_slots_empty(void)
{
	addr slots, clos, name, check;
	size_t size;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	make_standard_class_slots(&slots);
	dummy_standard_class(NULL, &clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);
	make_direct_slots_empty(&clos, clos, name);
	clos_elt(clos, Clos_class_direct_slots, &check);
	LenArrayA4(check, &size);
	test(size == 0, "make_direct_slots_empty1");
	rollback_local(local, stack);

	RETURN;
}

static int test_std_update_class_of(void)
{
	addr slots, clos, instance, name, version, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	make_standard_class_slots(&slots);
	dummy_standard_class(NULL, &clos, slots);
	internchar(LISP_PACKAGE, "AAA", &name);
	make_class_slots(&clos, clos, name, slots);

	fixnum_heap(&version, 100);
	setf_clos_elt(clos, Clos_class_version, version);
	clos_heap(&instance, slots);
	std_update_class_of(instance, clos);
	clos_class_of(instance, &check);
	test(check == clos, "std_update_class_of1");
	clos_version(instance, &check);
	test(check == version, "std_update_class_of2");

	rollback_local(local, stack);

	RETURN;
}

static int test_set_inheritance(void)
{
	addr slots, metaclass, clos, name, super, supers, check, left;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	make_standard_class_slots(&slots);
	dummy_standard_class(NULL, &metaclass, slots);
	internchar(LISP_PACKAGE, "METACLASS", &name);
	make_class_slots(&metaclass, metaclass, name, slots);
	set_inheritance(ptr, metaclass, metaclass, Nil);

	internchar(LISP_PACKAGE, "SUPER", &name);
	make_class_slots(&super, metaclass, name, slots);
	set_inheritance(ptr, super, metaclass, Nil);

	internchar(LISP_PACKAGE, "AAA", &name);
	make_class_slots(&clos, metaclass, name, slots);
	list_heap(&supers, super, NULL);
	set_inheritance(ptr, clos, metaclass, supers);

	clos_version(clos, &check);
	GetFixnum(check, &value);
	test(value == 0, "set_inheritance1");
	clos_class_of(clos, &check);
	test(check == metaclass, "set_inheritance2");
	clos_elt(clos, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == super, "set_inheritance3");
	test(check == Nil, "set_inheritance4");
	clos_elt(clos, Clos_class_precedence_list, &check);
	test(GetType(check) == LISPTYPE_CONS, "set_inheritance5");
	clos_elt(clos, Clos_class_slots, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "set_inheritance6");
	clos_elt(super, Clos_class_direct_subclasses, &check);
	GetCons(check, &left, &check);
	test(left == clos, "set_inheritance7");
	test(check == Nil, "set_inheritance8");
	clos_elt(clos, Clos_class_name, &check);
	test(check == name, "set_inheritance9");
	check = find_class(name);
	test(check == clos, "set_inheritance10");

	rollback_local(local, stack);

	RETURN;
}

static int test_set_inheritance_single(void)
{
	addr slots, metaclass, clos, name, super, check, left;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	make_standard_class_slots(&slots);
	dummy_standard_class(NULL, &metaclass, slots);
	internchar(LISP_PACKAGE, "METACLASS", &name);
	make_class_slots(&metaclass, metaclass, name, slots);
	set_inheritance(ptr, metaclass, metaclass, Nil);

	internchar(LISP_PACKAGE, "SUPER", &name);
	make_class_slots(&super, metaclass, name, slots);
	set_inheritance(ptr, super, metaclass, Nil);

	internchar(LISP_PACKAGE, "AAA", &name);
	make_class_slots(&clos, metaclass, name, slots);
	set_inheritance_single(ptr, clos, metaclass, super);

	clos_version(clos, &check);
	GetFixnum(check, &value);
	test(value == 0, "set_inheritance_single1");
	clos_class_of(clos, &check);
	test(check == metaclass, "set_inheritance_single2");
	clos_elt(clos, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == super, "set_inheritance_single3");
	test(check == Nil, "set_inheritance_single4");
	clos_elt(clos, Clos_class_precedence_list, &check);
	test(GetType(check) == LISPTYPE_CONS, "set_inheritance_single5");
	clos_elt(clos, Clos_class_slots, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "set_inheritance_single6");
	clos_elt(super, Clos_class_direct_subclasses, &check);
	GetCons(check, &left, &check);
	test(left == clos, "set_inheritance_single7");
	test(check == Nil, "set_inheritance_single8");
	clos_elt(clos, Clos_class_name, &check);
	test(check == name, "set_inheritance_single9");
	check = find_class(name);
	test(check == clos, "set_inheritance_single10");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_standard_class(void)
{
	addr metaclass, tclass, object, classclass, check, left;
	Execute ptr;

	ptr = Execute_Thread;
	make_standard_class(ptr, &metaclass);

	/* t */
	tclass = find_class(T);
	clos_class_of(tclass, &check);
	GetConstant(CONSTANT_COMMON_BUILT_IN_CLASS, &left);
	test(check == find_class(left), "make_standard_class1");
	clos_elt(tclass, Clos_class_direct_superclasses, &check);
	test(check == Nil, "make_standard_class2");

	/* object */
	GetConstant(CONSTANT_COMMON_STANDARD_OBJECT, &object);
	object = find_class(object);
	clos_class_of(object, &check);
	test(check == metaclass, "make_standard_class3");
	clos_elt(object, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == tclass, "make_standard_class4");
	test(check == Nil, "make_standard_class5");

	/* class */
	GetConstant(CONSTANT_COMMON_CLASS, &classclass);
	classclass = find_class(classclass);
	clos_class_of(classclass, &check);
	test(check == metaclass, "make_standard_class6");
	clos_elt(classclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == object, "make_standard_class7");
	test(check == Nil, "make_standard_class8");

	/* standard-class */
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == metaclass, "make_standard_class9");
	clos_class_of(check, &check);
	test(check == metaclass, "make_standard_class10");
	clos_elt(metaclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == classclass, "make_standard_class11");
	test(check == Nil, "make_standard_class12");

	RETURN;
}

static int test_make_type_class_slots(void)
{
	addr instance, metaclass, name, slots, slot, symbol, supers, check;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	make_standard_class(ptr, &metaclass);
	internchar(LISP_PACKAGE, "HELLO", &name);
	list_heap(&supers, find_class(T), NULL);
	vector4_heap(&slots, 2);
	slot_heap(&slot);
	internchar(LISP_PACKAGE, "AAA", &symbol);
	SetSlot(slot, SLOT_INDEX_NAME, symbol);
	SetArrayA4(slots, 0, slot);
	internchar(LISP_PACKAGE, "BBB", &symbol);
	SetSlot(slot, SLOT_INDEX_NAME, symbol);
	SetArrayA4(slots, 1, slot);
	make_type_class_slots(ptr, &instance, metaclass, name, slots, supers);
	clos_class_of(instance, &check);
	test(check == metaclass, "make_type_class_slots1");
	clos_elt(instance, Clos_class_direct_superclasses, &supers);
	GetCons(supers, &check, &supers);
	test(check == find_class(T), "make_type_class_slots2");
	test(supers == Nil, "make_type_class_slots3");
	clos_elt(instance, Clos_class_direct_slots, &check);
	LenArrayA4(check, &size);
	test(size == 2, "make_type_class_slots4");

	RETURN;
}

static int test_make_type_class(void)
{
	addr instance, metaclass, name, supers, check;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	make_standard_class(ptr, &metaclass);
	internchar(LISP_PACKAGE, "HELLO", &name);
	list_heap(&supers, find_class(T), NULL);
	make_type_class(ptr, &instance, metaclass, name, supers);
	clos_class_of(instance, &check);
	test(check == metaclass, "make_type_class1");
	clos_elt(instance, Clos_class_direct_superclasses, &supers);
	GetCons(supers, &check, &supers);
	test(check == find_class(T), "make_type_class2");
	test(supers == Nil, "make_type_class3");
	clos_elt(instance, Clos_class_direct_slots, &check);
	LenArrayA4(check, &size);
	test(size == 0, "make_type_class4");

	RETURN;
}

static int test_make_type_class_constant(void)
{
	addr metaclass, clos, check, name;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	forget_all_classes();
	make_standard_class(ptr, &metaclass);
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_BUILT_IN_CLASS,
			CONSTANT_COMMON_CLASS,
			CONSTANT_EMPTY);
	GetConstant(CONSTANT_COMMON_BUILT_IN_CLASS, &clos);
	clos = find_class(clos);
	clos_class_of(clos, &check);
	test(check == metaclass, "make_type_class_constant1");
	clos_elt(clos, Clos_class_direct_superclasses, &check);
	GetCar(check, &check);
	GetConstant(CONSTANT_COMMON_CLASS, &name);
	test(check == find_class(name), "make_type_class_constant2");
	clos_elt(clos, Clos_class_direct_slots, &check);
	LenArrayA4(check, &size);
	test(size == 0, "make_type_class_constant3");

	RETURN;
}

static int test_make_type_class_slots_constant(void)
{
	addr metaclass, clos, check, slot, slots, name;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;

	vector4_heap(&slots, 2);
	slot_heap(&slot);
	internchar(LISP_PACKAGE, "AAA", &name);
	SetSlot(slot, SLOT_INDEX_NAME, name);
	SetArrayA4(slots, 0, slot);
	internchar(LISP_PACKAGE, "BBB", &name);
	SetSlot(slot, SLOT_INDEX_NAME, name);
	SetArrayA4(slots, 1, slot);

	forget_all_classes();
	make_standard_class(ptr, &metaclass);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_BUILT_IN_CLASS,
			CONSTANT_COMMON_CLASS);
	GetConstant(CONSTANT_COMMON_BUILT_IN_CLASS, &clos);
	clos = find_class(clos);
	clos_class_of(clos, &check);
	test(check == metaclass, "make_type_class_constant1");
	clos_elt(clos, Clos_class_direct_superclasses, &check);
	GetCar(check, &check);
	GetConstant(CONSTANT_COMMON_CLASS, &name);
	test(check == find_class(name), "make_type_class_constant2");
	clos_elt(clos, Clos_class_direct_slots, &check);
	LenArrayA4(check, &size);
	test(size == 2, "make_type_class_constant3");
	forget_all_classes();

	RETURN;
}


static int test_build_standard_class(void)
{
	addr check;
	addr trueclass, classclass, builtinclass, metaclass, objectclass;
	addr classname, builtinname, metaname, objectname;
	Execute ptr;

	ptr = Execute_Thread;
	forget_all_classes();
	build_standard_class(ptr);

	interncommon("CLASS", &classname);
	interncommon("BUILT-IN-CLASS", &builtinname);
	interncommon("STANDARD-CLASS", &metaname);
	interncommon("STANDARD-OBJECT", &objectname);
	trueclass = find_class(T);
	classclass = find_class(classname);
	builtinclass = find_class(builtinname);
	metaclass = find_class(metaname);
	objectclass = find_class(objectname);
	test(closp(trueclass), "build_standard_class1");
	test(closp(classclass), "build_standard_class2");
	test(closp(builtinclass), "build_standard_class3");
	test(closp(metaclass), "build_standard_class4");
	test(closp(objectclass), "build_standard_class5");
	clos_elt(trueclass, Clos_class_name, &check);
	test(check == T, "build_standard_class6");
	clos_elt(classclass, Clos_class_name, &check);
	test(check == classname, "build_standard_class7");
	clos_elt(builtinclass, Clos_class_name, &check);
	test(check == builtinname, "build_standard_class8");
	clos_elt(metaclass, Clos_class_name, &check);
	test(check == metaname, "build_standard_class9");
	clos_elt(objectclass, Clos_class_name, &check);
	test(check == objectname, "build_standard_class10");

	/* t */
	clos_class_of(trueclass, &check);
	test(check == builtinclass, "build_standard_class11");
	clos_elt(trueclass, Clos_class_direct_superclasses, &check);
	test(check == Nil, "build_standard_class12");
	clos_elt(trueclass, Clos_class_direct_subclasses, &check);
	test(find_list_eq_unsafe(objectclass, check), "build_standard_class13");

	/* standard-object */
	clos_class_of(objectclass, &check);
	test(metaclass == check, "build_standard_class14");
	test(std_subclass_p(objectclass, trueclass), "build_standard_class15");
	clos_elt(objectclass, Clos_class_direct_subclasses, &check);
	test(find_list_eq_unsafe(classclass, check), "build_standard_class16");

	/* class */
	clos_class_of(classclass, &check);
	test(metaclass == check, "build_standard_class17");
	test(std_subclass_p(classclass, objectclass), "build_standard_class18");
	test(std_subclass_p(classclass, trueclass), "build_standard_class19");
	clos_elt(classclass, Clos_class_direct_subclasses, &check);
	test(find_list_eq_unsafe(metaclass, check), "build_standard_class20");
	test(find_list_eq_unsafe(builtinclass, check), "build_standard_class21");

	/* standard-class */
	clos_class_of(metaclass, &check);
	test(metaclass == check, "build_standard_class22");
	test(std_subclass_p(metaclass, classclass), "build_standard_class23");
	test(std_subclass_p(metaclass, objectclass), "build_standard_class24");
	test(std_subclass_p(metaclass, trueclass), "build_standard_class25");
	clos_elt(metaclass, Clos_class_direct_subclasses, &check);
	test(check == Nil, "build_standard_class26");

	/* built-in-class */
	clos_class_of(builtinclass, &check);
	test(metaclass == check, "build_standard_class27");
	test(std_subclass_p(builtinclass, objectclass), "build_standard_class28");
	test(std_subclass_p(builtinclass, classclass), "build_standard_class29");
	test(std_subclass_p(builtinclass, trueclass), "build_standard_class30");
	clos_elt(builtinclass, Clos_class_direct_subclasses, &check);
	test(check == Nil, "build_standard_class31");

	/* constant */
	GetConstant(CONSTANT_CLOS_CLASS, &check);
	test(classclass == check, "build_standard_class32");
	GetConstant(CONSTANT_CLOS_STANDARD_CLASS, &check);
	test(metaclass == check, "build_standard_class33");

	/* finalize */
	forget_all_classes();

	RETURN;
}


/*
 *  standard-generic-function
 */
static int test_make_standard_generic_function_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	make_standard_generic_function_slots(&slots);
	LenArrayA4(slots, &size);
	test(size == Clos_generic_size, "make_standard_generic_function_slots1");

	for (check = 1, i = 0; i < Clos_generic_size; i++) {
		GetArrayA4(slots, i, &pos);
		GetSlot(pos, SLOT_INDEX_LOCATION, &pos);
		GetIndex(pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "make_standard_generic_function_slots2");
	test(test_slotname(slots, Clos_generic_name, "NAME"),
			"make_standard_generic_function_slots3");
	test(test_slotname(slots, Clos_generic_lambda_list, "LAMBDA-LIST"),
			"make_standard_generic_function_slots4");
	test(test_slotname(slots, Clos_generic_methods, "METHODS"),
			"make_standard_generic_function_slots5");
	test(test_slotname(slots, Clos_generic_method_class, "METHOD-CLASS"),
			"make_standard_generic_function_slots6");
	test(test_slotname(slots, Clos_generic_argument_precedence_order,
				"ARGUMENT-PRECEDENCE-ORDER"),
			"make_standard_generic_function_slots7");
	test(test_slotname(slots, Clos_generic_declarations, "DECLARATIONS"),
			"make_standard_generic_function_slots8");
	test(test_slotname(slots, Clos_generic_method_combination, "METHOD-COMBINATION"),
			"make_standard_generic_function_slots9");
	test(test_slotname(slots, Clos_generic_combination_arguments,
				"COMBINATION-ARGUMENTS"),
			"make_standard_generic_function_slots10");
	test(test_slotname(slots, Clos_generic_eqlcheck, "EQLCHECK"),
			"make_standard_generic_function_slots11");
	test(test_slotname(slots, Clos_generic_cache, "CACHE"),
			"make_standard_generic_function_slots12");
	test(test_slotname(slots, Clos_generic_call, "CALL"),
			"make_standard_generic_function_slots13");

	RETURN;
}

static int test_build_standard_generic_function(void)
{
	addr pos, fclass, gclass, sgclass, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	forget_all_classes();
	build_standard_class(ptr);
	build_standard_generic_function(ptr);

	/* function */
	GetConstant(CONSTANT_COMMON_FUNCTION, &fclass);
	fclass = find_class(fclass);
	test(closp(fclass), "build_standard_generic_function1");
	clos_class_of(fclass, &pos);
	GetConstant(CONSTANT_COMMON_BUILT_IN_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_standard_generic_function2");
	clos_elt(fclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == find_class(T), "build_standard_generic_function3");
	test(check == Nil, "build_standard_generic_function4");

	/* generic-function */
	GetConstant(CONSTANT_COMMON_GENERIC_FUNCTION, &gclass);
	gclass = find_class(gclass);
	test(closp(gclass), "build_standard_generic_function5");
	clos_class_of(gclass, &pos);
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_standard_generic_function6");
	clos_elt(gclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == fclass, "build_standard_generic_function7");
	test(check != Nil, "build_standard_generic_function8");
	GetCons(check, &left, &check);
	test(check == Nil, "build_standard_generic_function9");
	GetConstant(CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT, &check);
	check = find_class(check);
	test(left == check, "build_standard_generic_function10");

	/* standard-generic-function */
	GetConstant(CONSTANT_COMMON_STANDARD_GENERIC_FUNCTION, &sgclass);
	sgclass = find_class(sgclass);
	test(closp(sgclass), "build_standard_generic_function11");
	clos_class_of(sgclass, &pos);
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_standard_generic_function12");
	clos_elt(sgclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(left == gclass, "build_standard_generic_function13");
	test(check == Nil, "build_standard_generic_function14");

	/* constant */
	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	test(pos == sgclass, "build_standard_generic_function15");

	RETURN;
}

static int test_generic_function_instance(void)
{
	addr pos, name;

	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	make_instance_restrict_heap(pos, &pos);
	GetConstant(CONSTANT_CLOSNAME_EQLCHECK, &name);
	test(clos_slot_exists_p(pos, name), "generic_function_instance1");

	RETURN;
}


/*
 *  standard-method
 */
static int test_make_standard_method_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	make_standard_method_slots(&slots);
	LenArrayA4(slots, &size);
	test(size == Clos_method_size, "make_standard_method_slots1");

	for (check = 1, i = 0; i < Clos_method_size; i++) {
		GetArrayA4(slots, i, &pos);
		GetSlot(pos, SLOT_INDEX_LOCATION, &pos);
		GetIndex(pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "make_standard_method_slots2");
	test(test_slotname(slots, Clos_method_function, "FUNCTION"),
			"make_standard_method_slots3");
	test(test_slotname(slots, Clos_method_generic_function, "GENERIC-FUNCTION"),
			"make_standard_method_slots4");
	test(test_slotname(slots, Clos_method_lambda_list, "LAMBDA-LIST"),
			"make_standard_method_slots5");
	test(test_slotname(slots, Clos_method_lambda_parse, "LAMBDA-PARSE"),
			"make_standard_method_slots6");
	test(test_slotname(slots, Clos_method_qualifiers, "QUALIFIERS"),
			"make_standard_method_slots7");
	test(test_slotname(slots, Clos_method_specializers, "SPECIALIZERS"),
			"make_standard_method_slots8");

	RETURN;
}

static int test_build_standard_method(void)
{
	addr pos, mclass, smclass, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	forget_all_classes();
	build_standard_class(ptr);
	build_standard_generic_function(ptr);
	build_standard_method(ptr);

	/* method */
	GetConstant(CONSTANT_COMMON_METHOD, &mclass);
	mclass = find_class(mclass);
	test(closp(mclass), "build_standard_method1");
	clos_class_of(mclass, &pos);
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_standard_method2");
	clos_elt(mclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_standard_method3");
	GetConstant(CONSTANT_COMMON_STANDARD_OBJECT, &check);
	check = find_class(check);
	test(left == check, "build_standard_method4");

	/* standard-method */
	GetConstant(CONSTANT_COMMON_STANDARD_METHOD, &smclass);
	smclass = find_class(smclass);
	test(closp(smclass), "build_standard_method5");
	clos_class_of(smclass, &pos);
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_standard_method6");
	clos_elt(smclass, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_standard_method7");
	test(left == mclass, "build_standard_method8");

	RETURN;
}


/*
 *  method-combination
 */
static int test_make_method_combination_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	make_method_combination_slots(&slots);
	LenArrayA4(slots, &size);
	test(size == Clos_combination_size, "make_method_combination_slots1");

	for (check = 1, i = 0; i < Clos_combination_size; i++) {
		GetArrayA4(slots, i, &pos);
		GetSlot(pos, SLOT_INDEX_LOCATION, &pos);
		GetIndex(pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "make_method_combination_slots2");
	test(test_slotname(slots, Clos_combination_name, "NAME"),
			"make_method_combination_slots3");
	test(test_slotname(slots, Clos_combination_long_p, "LONG-P"),
			"make_method_combination_slots4");
	test(test_slotname(slots, Clos_combination_document, "DOCUMENT"),
			"make_method_combination_slots5");
	test(test_slotname(slots, Clos_combination_identity, "IDENTITY"),
			"make_method_combination_slots6");
	test(test_slotname(slots, Clos_combination_operator, "OPERATOR"),
			"make_method_combination_slots7");
	test(test_slotname(slots, Clos_combination_lambda_list, "LAMBDA-LIST"),
			"make_method_combination_slots8");
	test(test_slotname(slots, Clos_combination_qualifiers, "QUALIFIERS"),
			"make_method_combination_slots9");
	test(test_slotname(slots, Clos_combination_arguments, "ARGUMENTS"),
			"make_method_combination_slots10");
	test(test_slotname(slots, Clos_combination_generic, "GENERIC"),
			"make_method_combination_slots11");
	test(test_slotname(slots, Clos_combination_form, "FORM"),
			"make_method_combination_slots12");
	test(test_slotname(slots, Clos_combination_function, "FUNCTION"),
			"make_method_combination_slots13");

	RETURN;
}

static int test_build_method_combination(void)
{
	addr pos, clos, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	forget_all_classes();
	build_standard_class(ptr);
	build_standard_generic_function(ptr);
	build_method_combination(ptr);

	/* method_combination */
	GetConstant(CONSTANT_COMMON_METHOD_COMBINATION, &clos);
	clos = find_class(clos);
	test(closp(clos), "build_method_combination1");
	clos_class_of(clos, &pos);
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_method_combination2");
	clos_elt(clos, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_method_combination3");
	GetConstant(CONSTANT_COMMON_STANDARD_OBJECT, &check);
	check = find_class(check);
	test(left == check, "build_method_combination4");

	RETURN;
}


/*
 *  eql-specializer
 */
static int test_make_eql_specializer_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	make_eql_specializer_slots(&slots);
	LenArrayA4(slots, &size);
	test(size == Clos_specializer_size, "make_eql_specializer_slots1");

	for (check = 1, i = 0; i < Clos_specializer_size; i++) {
		GetArrayA4(slots, i, &pos);
		GetSlot(pos, SLOT_INDEX_LOCATION, &pos);
		GetIndex(pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "make_eql_specializer_slots2");
	test(test_slotname(slots, Clos_specializer_object, "OBJECT"),
			"make_eql_specializer_slots3");
	test(test_slotname(slots, Clos_specializer_type, "TYPE"),
			"make_eql_specializer_slots4");

	RETURN;
}

static int test_build_eql_specializer(void)
{
	addr pos, clos, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	forget_all_classes();
	build_standard_class(ptr);
	build_standard_generic_function(ptr);
	build_method_combination(ptr);
	build_eql_specializer(ptr);

	/* eql_specializer */
	GetConstant(CONSTANT_CLOSNAME_EQL_SPECIALIZER, &clos);
	clos = find_class(clos);
	test(closp(clos), "build_eql_specializer1");
	clos_class_of(clos, &pos);
	GetConstant(CONSTANT_COMMON_STANDARD_CLASS, &check);
	check = find_class(check);
	test(check == pos, "build_eql_specializer2");
	clos_elt(clos, Clos_class_direct_superclasses, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_eql_specializer3");
	GetConstant(CONSTANT_COMMON_STANDARD_OBJECT, &check);
	check = find_class(check);
	test(left == check, "build_eql_specializer4");

	RETURN;
}

static int test_intern_eql_specializer(void)
{
	addr value, check, check2;

	forget_all_eql_specializer();
	fixnum_heap(&value, 100);
	intern_eql_specializer(value, &check);
	test(closp(check), "intern_eql_specializer1");
	intern_eql_specializer(value, &check2);
	test(check == check2, "intern_eql_specializer2");

	fixnum_heap(&value, 101);
	intern_eql_specializer(value, &check2);
	test(check != check2, "intern_eql_specializer3");

	character_heap(&value, 100);
	intern_eql_specializer(value, &check);
	character_heap(&value, 100);
	intern_eql_specializer(value, &check2);
	test(check == check2, "intern_eql_specializer4");

	character_heap(&value, 101);
	intern_eql_specializer(value, &check2);
	test(check != check2, "intern_eql_specializer5");

	fixnum_heap(&value, 100);
	intern_eql_specializer(value, &check);
	test(eql_specializer_p(check), "eql_specializer_p1");
	GetConstant(CONSTANT_CLOS_STANDARD_CLASS, &check);
	test(! eql_specializer_p(check), "eql_specializer_p2");

	forget_all_eql_specializer();

	RETURN;
}


/*
 *  check function
 */
static int test_std_subclass_p(void)
{
	addr t, a, b, cons;
	Execute ptr;

	ptr = Execute_Thread;
	clos_supers_heap(&t, NULL);
	clos_supers_heap(&a, NULL);
	clos_supers_heap(&b, t, NULL);
	std_compute_class_precedence_list(ptr, t, &cons);
	setf_clos_elt(t, Clos_class_precedence_list, cons);
	std_compute_class_precedence_list(ptr, a, &cons);
	setf_clos_elt(a, Clos_class_precedence_list, cons);
	std_compute_class_precedence_list(ptr, b, &cons);
	setf_clos_elt(b, Clos_class_precedence_list, cons);

	test(std_subclass_p(t, t), "std_subclass_p1");
	test(! std_subclass_p(t, a), "std_subclass_p2");
	test(! std_subclass_p(a, t), "std_subclass_p3");
	test(std_subclass_p(b, t), "std_subclass_p4");
	test(! std_subclass_p(t, b), "std_subclass_p5");
	test(! std_subclass_p(t, b), "std_subclass_p6");

	clos_supers_heap(&t, NULL);
	clos_supers_heap(&a, t, NULL);
	clos_supers_heap(&b, a, NULL);
	std_compute_class_precedence_list(ptr, t, &cons);
	setf_clos_elt(t, Clos_class_precedence_list, cons);
	std_compute_class_precedence_list(ptr, b, &cons);
	setf_clos_elt(b, Clos_class_precedence_list, cons);
	test(std_subclass_p(b, t), "std_subclass_p7");
	test(! std_subclass_p(t, b), "std_subclass_p8");

	RETURN;
}

static int test_std_subtype_p(void)
{
	addr metaclass, clos, name, instance;
	Execute ptr;

	ptr = Execute_Thread;
	GetConstant(CONSTANT_CLOS_STANDARD_CLASS, &metaclass);
	internchar(LISP_PACKAGE, "HELLO", &name);
	make_type_class(ptr, &clos, metaclass, name, Nil);
	make_instance_restrict_heap(clos, &instance);

	test(std_subtype_p(instance, clos), "std_subtype_p1");
	test(! std_subtype_p(clos, instance), "std_subtype_p2");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_standard(void)
{
	/* class-precedence-list */
	TestBreak(test_call_class_direct_superclasses);
	TestBreak(test_direct_superclasses_list);
	TestBreak(test_direct_superclasses_chain);
	TestBreak(test_all_superclass_list);
	TestBreak(test_find_chain_cons);
	TestBreak(test_superclasses_chain);
	TestBreak(test_find_top_superclass);
	TestBreak(test_remove_top_superclass);
	TestBreak(test_result_class_precedence_list);
	TestBreak(test_std_compute_class_precedence_list);
	/* compute-slots */
	TestBreak(test_call_class_precedence_list);
	TestBreak(test_call_class_direct_slots);
	TestBreak(test_member_slotname);
	TestBreak(test_gather_slots_heap);
	TestBreak(test_std_compute_slots_heap);
	TestBreak(test_std_compute_slots_vector_heap);
	/* standard-class */
	TestBreak(test_default_slot_nametype);
	TestBreak(test_default_slot_name);
	TestBreak(test_default_slot_initform);
	TestBreak(test_set_slots_localtion);
	TestBreak(test_make_standard_class_slots);
	TestBreak(test_dummy_standard_class);
	TestBreak(test_make_class_slots);
	TestBreak(test_make_direct_slots_empty);
	TestBreak(test_std_update_class_of);
	TestBreak(test_set_inheritance);
	TestBreak(test_set_inheritance_single);
	TestBreak(test_make_standard_class);
	TestBreak(test_make_type_class_slots);
	TestBreak(test_make_type_class);
	TestBreak(test_make_type_class_constant);
	TestBreak(test_make_type_class_slots_constant);
	TestBreak(test_build_standard_class);
	/* standard-generic-function */
	TestBreak(test_make_standard_generic_function_slots);
	TestBreak(test_build_standard_generic_function);
	TestBreak(test_generic_function_instance);
	/* standard-method */
	TestBreak(test_make_standard_method_slots);
	TestBreak(test_build_standard_method);
	/* method-combination */
	TestBreak(test_make_method_combination_slots);
	TestBreak(test_build_method_combination);
	/* eql-specializer */
	TestBreak(test_make_eql_specializer_slots);
	TestBreak(test_build_eql_specializer);
	TestBreak(test_intern_eql_specializer);
	/* check function */
	TestBreak(test_std_subclass_p);
	TestBreak(test_std_subtype_p);

	return 0;
}

int test_clos_standard(void)
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
		build_character();
		build_package();
		build_stream();
		build_clos_table(ptr);
		lisp_init = 1;
		result = testbreak_clos_standard();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

