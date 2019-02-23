#include "clos_object.c"
#include "clos.h"
#include "degrade.h"
#include "object.h"
#include "memory.h"


/*
 *  find-class
 */
static int test_find_class_nil(void)
{
	addr name, pos, table;

	internchar(LISP_PACKAGE, "HELLO", &name);
	pos = find_class_nil(name);
	test(pos == Nil, "find_class_nil1");

	table = Root(LISPINDEX_CLOS);
	intern_hashheap(table, name, &pos);
	SetCdr(pos, T);
	pos = find_class_nil(name);
	test(pos == T, "find_class_nil2");

	clear_hashtable_heap(table);
	pos = find_class_nil(name);
	test(pos == Nil, "find_class_nil3");

	RETURN;
}


/*
 *  slot
 */
static int test_slot_heap(void)
{
	addr slot, pos, instance;
	size_t size;

	slot_heap(&slot);
	test(! GetStatusDynamic(slot), "slot_heap1");
	test(GetStatusSize(slot) == LISPSIZE_ARRAY2, "slot_heap2");
	test(GetType(slot) == LISPTYPE_SYSTEM, "slot_heap3");
	GetSlot(slot, SLOT_INDEX_NAME, &pos);
	test(pos == Unbound, "slot_heap4");
	GetSlot(slot, SLOT_INDEX_INITFORM, &pos);
	test(pos == Unbound, "slot_heap5");
	GetSlot(slot, SLOT_INDEX_INITFUNCTION, &pos);
	test(pos == Nil, "slot_heap6");
	GetSlot(slot, SLOT_INDEX_TYPE, &pos);
	test(pos == T, "slot_heap7");
	GetSlot(slot, SLOT_INDEX_ALLOCATION, &pos);
	internchar_keyword("INSTANCE", &instance);
	test(pos == instance, "slot_heap8");
	LenArrayA2(slot, &size);
	test(size == SLOT_INDEX_SIZE, "slot_heap9");

	RETURN;
}

static int test_slot_local(void)
{
	addr slot, pos, instance;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);

	slot_local(local, &slot);
	test(GetStatusDynamic(slot), "slot_local1");
	test(GetStatusSize(slot) == LISPSIZE_ARRAY2, "slot_local2");
	test(GetType(slot) == LISPTYPE_SYSTEM, "slot_local3");
	GetSlot(slot, SLOT_INDEX_NAME, &pos);
	test(pos == Unbound, "slot_local4");
	GetSlot(slot, SLOT_INDEX_INITFORM, &pos);
	test(pos == Unbound, "slot_local5");
	GetSlot(slot, SLOT_INDEX_INITFUNCTION, &pos);
	test(pos == Nil, "slot_local6");
	GetSlot(slot, SLOT_INDEX_TYPE, &pos);
	test(pos == T, "slot_local7");
	GetSlot(slot, SLOT_INDEX_ALLOCATION, &pos);
	internchar_keyword("INSTANCE", &instance);
	test(pos == instance, "slot_local8");
	LenArrayA2(slot, &size);
	test(size == SLOT_INDEX_SIZE, "slot_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_slot_copy_alloc(void)
{
	addr slot, copy;

	slot_alloc(NULL, &slot);
	SetSlot(slot, SLOT_INDEX_ALLOCATION, T);
	slot_copy_alloc(NULL, &copy, slot);
	test(slot != copy, "slot_copy_alloc1");
	GetSlot(slot, SLOT_INDEX_ALLOCATION, &copy);
	test(copy == T, "slot_copy_alloc2");

	RETURN;
}


/*
 *  clos
 */
static int test_clos_heap(void)
{
	int check;
	addr slots, clos, pos, left, right;
	size_t size, i;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	test(GetType(clos) == LISPTYPE_CLOS, "clos_heap1");
	test(GetStatusSize(clos) == LISPSIZE_ARRAY2, "clos_heap2");
	test(! GetStatusDynamic(clos), "clos_heap3");
	LenArrayA2(clos, &size);
	test(size == CLOS_INDEX_SIZE, "clos_heap4");

	GetClos(clos, CLOS_INDEX_CLASS_OF, &pos);
	test(pos == Unbound, "clos_heap5");

	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "clos_heap6");
	test(! GetStatusDynamic(pos), "clos_heap7");

	GetClos(clos, CLOS_INDEX_SLOTS, &pos);
	test(slots == pos, "clos_heap8");

	GetClos(clos, CLOS_INDEX_DATA, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "clos_heap9");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY4, "clos_heap10");
	test(! GetStatusDynamic(pos), "clos_heap11");
	LenArrayA4(pos, &size);
	test(size == 3, "clos_heap12");
	test(GetUser(clos) == 0, "clos_heap13");

	for (check = 1, i = 0; i < 3; i++) {
		GetArrayA4(pos, i, &right);
		if (GetType(right) != LISPTYPE_CONS) { check = 0; break; }
		GetCons(right, &left, &right);
		if (left != Nil || right != Unbound) { check = 0; break; }
	}
	test(check, "clos_heap14");

	RETURN;
}

static int test_closp(void)
{
	addr pos, slots;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	vector4_heap(&slots, 3);
	fixnum_heap(&pos, 100);
	test(! closp(pos), "closp1");
	clos_heap(&pos, slots);
	test(closp(pos), "closp2");
	clos_local(local, &pos, slots);
	test(closp(pos), "closp3");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_swap(void)
{
	addr clos1, clos2, slots1, slots2, table1, table2, check;

	vector4_heap(&slots1, 1);
	vector4_heap(&slots2, 2);
	clos_heap(&clos1, slots1);
	clos_heap(&clos2, slots2);
	GetClos(clos1, CLOS_INDEX_TABLE, &table1);
	GetClos(clos2, CLOS_INDEX_TABLE, &table2);
	clos_swap(clos1, clos2);

	GetClos(clos1, CLOS_INDEX_TABLE, &check);
	test(check == table2, "clos_swap1");
	GetClos(clos2, CLOS_INDEX_TABLE, &check);
	test(check == table1, "clos_swap2");
	GetClos(clos1, CLOS_INDEX_SLOTS, &check);
	test(check == slots2, "clos_swap3");
	GetClos(clos2, CLOS_INDEX_SLOTS, &check);
	test(check == slots1, "clos_swap4");

	RETURN;
}

static int test_clos_destroy(void)
{
	addr clos, slots, check;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	clos_destroy(clos);
	GetClos(clos, CLOS_INDEX_CLASS_OF, &check);
	test(check == Unbound, "clos_destroy1");
	GetClos(clos, CLOS_INDEX_VERSION, &check);
	test(check == Unbound, "clos_destroy2");
	GetClos(clos, CLOS_INDEX_TABLE, &check);
	test(check == Unbound, "clos_destroy3");
	GetClos(clos, CLOS_INDEX_SLOTS, &check);
	test(check == Nil, "clos_destroy4");
	GetClos(clos, CLOS_INDEX_DATA, &check);
	test(check == Nil, "clos_destroy5");

	RETURN;
}

static int test_clos_class_of(void)
{
	addr clos, parent, check;

	vector4_heap(&check, 3);
	clos_heap(&parent, check);
	vector4_heap(&check, 4);
	clos_heap(&clos, check);
	SetClos(clos, CLOS_INDEX_CLASS_OF, parent);
	clos_class_of(clos, &check);
	test(check == parent, "clos_class_of1");

	RETURN;
}

static int test_setf_clos_class_of(void)
{
	addr clos, parent, check;

	vector4_heap(&check, 1);
	clos_heap(&parent, check);
	vector4_heap(&check, 2);
	clos_heap(&clos, check);
	setf_clos_class_of(clos, parent);
	clos_class_of(clos, &check);
	test(check == parent, "setf_clos_class_of2");

	RETURN;
}

static int test_clos_version(void)
{
	addr clos, check, value;

	vector4_heap(&check, 1);
	clos_heap(&clos, check);
	fixnum_heap(&value, 10);
	SetClos(clos, CLOS_INDEX_VERSION, value);
	clos_version(clos, &check);
	test(check == value, "clos_version1");

	RETURN;
}

static int test_setf_clos_version(void)
{
	addr clos, check, value;

	vector4_heap(&check, 1);
	clos_heap(&clos, check);
	fixnum_heap(&value, 10);
	setf_clos_version(clos, value);
	clos_version(clos, &check);
	test(check == value, "setf_clos_version1");

	RETURN;
}


/*
 *  clos-elt
 */
static int test_clos_elt_unbound(void)
{
	addr clos, slots, pos;

	/* clos_elt_unbound */
	vector4_heap(&slots, 4);
	clos_heap(&clos, slots);
	GetClos(clos, CLOS_INDEX_DATA, &pos);
	GetArrayA4(pos, 3, &pos);
	SetCdr(pos, T);
	clos_elt_unbound(clos, 1, &pos);
	test(pos == Unbound, "clos_elt_unbound1");
	clos_elt_unbound(clos, 3, &pos);
	test(pos == T, "clos_elt_unbound2");

	/* clos_elt_unbound_p */
	test(clos_elt_unbound_p(clos, 1), "clos_elt_unbound_p1");
	test(! clos_elt_unbound_p(clos, 3), "clos_elt_unbound_p2");

	/* clos_elt_boundp */
	test(! clos_elt_boundp(clos, 1), "clos_elt_boundp1");
	test(clos_elt_boundp(clos, 3), "clos_elt_boundp2");

	RETURN;
}

static int test_clos_elt(void)
{
	addr clos, data, cons, pos;
	fixnum value;

	vector4_heap(&pos, 3);
	clos_heap(&clos, pos);
	GetClos(clos, CLOS_INDEX_DATA, &data);
	/* index 0 */
	GetArrayA4(data, 0, &cons);
	fixnum_heap(&pos, 0);
	SetCdr(cons, pos);
	/* index 1 */
	GetArrayA4(data, 1, &cons);
	fixnum_heap(&pos, 1111);
	SetCdr(cons, pos);
	/* index 2 */
	GetArrayA4(data, 2, &cons);
	fixnum_heap(&pos, 2222);
	SetCdr(cons, pos);
	/* check */
	clos_elt(clos, 0, &pos);
	GetFixnum(pos, &value);
	test(value == 0, "clos_elt1");
	clos_elt(clos, 1, &pos);
	GetFixnum(pos, &value);
	test(value == 1111, "clos_elt2");
	clos_elt(clos, 2, &pos);
	GetFixnum(pos, &value);
	test(value == 2222, "clos_elt3");

	RETURN;
}

static int test_setf_clos_elt(void)
{
	addr clos, pos;
	fixnum value;

	vector4_heap(&pos, 3);
	clos_heap(&clos, pos);
	fixnum_heap(&pos, 10);
	setf_clos_elt(clos, 0, pos);
	fixnum_heap(&pos, 20);
	setf_clos_elt(clos, 1, pos);
	fixnum_heap(&pos, 30);
	setf_clos_elt(clos, 2, pos);

	clos_elt(clos, 0, &pos);
	GetFixnum(pos, &value);
	test(value == 10, "setf_clos_elt1");
	clos_elt(clos, 1, &pos);
	GetFixnum(pos, &value);
	test(value == 20, "setf_clos_elt2");
	clos_elt(clos, 2, &pos);
	GetFixnum(pos, &value);
	test(value == 30, "setf_clos_elt3");

	RETURN;
}


/*
 *  clos-value
 */
static int test_clos_value_check(void)
{
	addr clos, table, data, name, cons, pos, check;

	vector4_heap(&pos, 3);
	clos_heap(&clos, pos);
	GetClos(clos, CLOS_INDEX_TABLE, &table);
	GetClos(clos, CLOS_INDEX_DATA, &data);

	internchar(LISP_PACKAGE, "HELLO", &name);
	test(clos_value_check(clos, name, &check), "clos_value_check1");

	intern_hashheap(table, name, &cons);
	index_heap(&pos, 1);
	SetCdr(cons, pos);
	GetArrayA4(data, 1, &cons);
	fixnum_heap(&pos, 111);

	test(! clos_value_check(clos, name, &check), "clos_value_check2");
	test(check == Unbound, "clos_value_check3");

	SetCdr(cons, pos);
	test(! clos_value_check(clos, name, &check), "clos_value_check4");
	test(check == pos, "clos_value_check5");

	RETURN;
}

static int test_clos_value_unbound(void)
{
	addr clos, table, data, name, cons, pos, check;

	vector4_heap(&pos, 3);
	clos_heap(&clos, pos);
	GetClos(clos, CLOS_INDEX_TABLE, &table);
	GetClos(clos, CLOS_INDEX_DATA, &data);
	internchar(LISP_PACKAGE, "HELLO", &name);
	intern_hashheap(table, name, &cons);
	index_heap(&pos, 1);
	SetCdr(cons, pos);
	GetArrayA4(data, 1, &cons);
	fixnum_heap(&pos, 111);

	clos_value_unbound(clos, name, &check);
	test(check == Unbound, "clos_value_unbound1");

	SetCdr(cons, pos);
	clos_value_unbound(clos, name, &check);
	test(check == pos, "clos_value_unbound2");

	RETURN;
}


static int test_clos_value(void)
{
	addr clos, table, data, name, cons, pos;
	fixnum value;

	vector4_heap(&pos, 3);
	clos_heap(&clos, pos);
	GetClos(clos, CLOS_INDEX_TABLE, &table);
	GetClos(clos, CLOS_INDEX_DATA, &data);

	internchar(LISP_PACKAGE, "HELLO", &name);
	intern_hashheap(table, name, &cons);
	index_heap(&pos, 1);
	SetCdr(cons, pos);
	GetArrayA4(data, 1, &cons);
	fixnum_heap(&pos, 111);
	SetCdr(cons, pos);

	internchar(LISP_PACKAGE, "AAABBB", &name);
	intern_hashheap(table, name, &cons);
	index_heap(&pos, 2);
	SetCdr(cons, pos);
	GetArrayA4(data, 2, &cons);
	fixnum_heap(&pos, 222);
	SetCdr(cons, pos);

	/* test */
	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "clos_value1");

	internchar(LISP_PACKAGE, "AAABBB", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "clos_value2");

	RETURN;
}

static int test_setf_clos_value(void)
{
	addr clos, name, table, cons, pos;
	fixnum value;

	vector4_heap(&pos, 3);
	clos_heap(&clos, pos);
	GetClos(clos, CLOS_INDEX_TABLE, &table);

	internchar(LISP_PACKAGE, "HELLO", &name);
	intern_hashheap(table, name, &cons);
	index_heap(&pos, 1);
	SetCdr(cons, pos);

	internchar(LISP_PACKAGE, "AAABBB", &name);
	intern_hashheap(table, name, &cons);
	index_heap(&pos, 2);
	SetCdr(cons, pos);

	/* test */
	fixnum_heap(&pos, 111);
	internchar(LISP_PACKAGE, "HELLO", &name);
	setf_clos_value(clos, name, pos);

	fixnum_heap(&pos, 222);
	internchar(LISP_PACKAGE, "AAABBB", &name);
	setf_clos_value(clos, name, pos);

	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "setf_clos_value1");

	internchar(LISP_PACKAGE, "AAABBB", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "setf_clos_value2");

	RETURN;
}


/*
 *  make-instance-restrict
 */
static int test_make_instance_restrict_heap(void)
{
	addr slots, pos, name, one, clos;
	fixnum value;

	/* direct-slots */
	vector4_heap(&slots, 3);

	slot_heap(&pos);
	internchar_keyword("HELLO", &name);
	SetSlot(pos, SLOT_INDEX_NAME, name);
	fixnum_heap(&one, 111);
	SetSlot(pos, SLOT_INDEX_INITFORM, one);
	index_heap(&one, 0);
	SetSlot(pos, SLOT_INDEX_LOCATION, one);
	SetArrayA4(slots, 0, pos);

	slot_heap(&pos);
	internchar_keyword("AAA", &name);
	SetSlot(pos, SLOT_INDEX_NAME, name);
	fixnum_heap(&one, 222);
	SetSlot(pos, SLOT_INDEX_INITFORM, one);
	index_heap(&one, 1);
	SetSlot(pos, SLOT_INDEX_LOCATION, one);
	SetArrayA4(slots, 1, pos);

	slot_heap(&pos);
	internchar_keyword("BBB", &name);
	SetSlot(pos, SLOT_INDEX_NAME, name);
	fixnum_heap(&one, 333);
	SetSlot(pos, SLOT_INDEX_INITFORM, one);
	index_heap(&one, 2);
	SetSlot(pos, SLOT_INDEX_LOCATION, one);
	SetArrayA4(slots, 2, pos);

	/* clos */
	vector4_heap(&pos, Clos_class_size);
	clos_heap(&clos, pos);
	setf_clos_elt(clos, Clos_class_slots, slots);

	/* test */
	make_instance_restrict_heap(clos, &clos);
	clos_elt(clos, 0, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "make_instance_restrict_heap1");
	clos_elt(clos, 1, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "make_instance_restrict_heap2");
	clos_elt(clos, 2, &pos);
	GetFixnum(pos, &value);
	test(value == 333, "make_instance_restrict_heap3");

	internchar_keyword("HELLO", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "make_instance_restrict_heap4");
	internchar_keyword("AAA", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "make_instance_restrict_heap5");
	internchar_keyword("BBB", &name);
	clos_value(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 333, "make_instance_restrict_heap6");

	clos_version(clos, &pos);
	GetFixnum(pos, &value);
	test(value == 0, "make_instance_restrict_heap7");

	RETURN;
}


/*
 *  slot check
 */
static int test_clos_slot_exists_p(void)
{
	addr clos, slots, name, pos;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);
	test(! clos_slot_exists_p(clos, name), "clos_slot_exists_p1");

	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	intern_hashheap(pos, name, &pos);
	test(clos_slot_exists_p(clos, name), "clos_slot_exists_p2");

	RETURN;
}

static int test_clos_slot_boundp_nil(void)
{
	addr clos, slots, name, pos, index;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);
	test(clos_slot_boundp_nil(clos, name) == -1, "clos_slot_boundp_nil1");

	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	intern_hashheap(pos, name, &pos);
	index_heap(&index, 0);
	SetCdr(pos, index);

	test(! clos_slot_boundp_nil(clos, name), "clos_slot_boundp_nil2");
	GetClos(clos, CLOS_INDEX_DATA, &pos);
	GetArrayA4(pos, 0, &pos);
	SetCdr(pos, T);
	test(clos_slot_boundp_nil(clos, name), "clos_slot_boundp_nil3");

	RETURN;
}

static int test_clos_slot_boundp(void)
{
	addr clos, slots, name, pos, index;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);

	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	intern_hashheap(pos, name, &pos);
	index_heap(&index, 0);
	SetCdr(pos, index);

	test(! clos_slot_boundp(clos, name), "clos_slot_boundp1");
	GetClos(clos, CLOS_INDEX_DATA, &pos);
	GetArrayA4(pos, 0, &pos);
	SetCdr(pos, T);
	test(clos_slot_boundp(clos, name), "clos_slot_boundp2");

	RETURN;
}

static int test_clos_slot_makunbound_nil(void)
{
	addr clos, slots, name, pos, index;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);
	test(clos_slot_makunbound_nil(clos, name), "clos_slot_makunbound_nil1");

	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	intern_hashheap(pos, name, &pos);
	index_heap(&index, 0);
	SetCdr(pos, index);

	test(! clos_slot_makunbound_nil(clos, name), "clos_slot_makunbound_nil2");
	GetClos(clos, CLOS_INDEX_DATA, &pos);
	GetArrayA4(pos, 0, &pos);
	SetCdr(pos, T);
	test(! clos_slot_makunbound_nil(clos, name), "clos_slot_makunbound_nil3");
	GetCdr(pos, &pos);
	test(pos == Unbound, "clos_slot_makunbound_nil4");

	RETURN;
}

static int test_clos_slot_makunbound(void)
{
	addr clos, slots, name, pos, index;

	vector4_heap(&slots, 3);
	clos_heap(&clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);
	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	intern_hashheap(pos, name, &pos);
	index_heap(&index, 0);
	SetCdr(pos, index);

	clos_slot_makunbound(clos, name);
	GetClos(clos, CLOS_INDEX_DATA, &pos);
	GetArrayA4(pos, 0, &pos);
	SetCdr(pos, T);
	clos_slot_makunbound(clos, name);
	GetCdr(pos, &pos);
	test(pos == Unbound, "clos_slot_makunbound1");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_object(void)
{
	TestBreak(test_find_class_nil);
	/* slot */
	TestBreak(test_slot_heap);
	TestBreak(test_slot_local);
	TestBreak(test_slot_copy_alloc);
	/* clos */
	TestBreak(test_clos_heap);
	TestBreak(test_closp);
	TestBreak(test_clos_swap);
	TestBreak(test_clos_destroy);
	TestBreak(test_clos_class_of);
	TestBreak(test_setf_clos_class_of);
	TestBreak(test_clos_version);
	TestBreak(test_setf_clos_version);
	/* clos-elt */
	TestBreak(test_clos_elt_unbound);
	TestBreak(test_clos_elt);
	TestBreak(test_setf_clos_elt);
	/* clos-value */
	TestBreak(test_clos_value_check);
	TestBreak(test_clos_value_unbound);
	TestBreak(test_clos_value);
	TestBreak(test_setf_clos_value);
	/* make-instance-restrict */
	TestBreak(test_make_instance_restrict_heap);
	/* slot check */
	TestBreak(test_clos_slot_exists_p);
	TestBreak(test_clos_slot_boundp_nil);
	TestBreak(test_clos_slot_boundp);
	TestBreak(test_clos_slot_makunbound_nil);
	TestBreak(test_clos_slot_makunbound);

	return 0;
}

int test_clos_object(void)
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
		build_package();
		build_clos_table(ptr);
		lisp_init = 1;
		result = testbreak_clos_object();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

