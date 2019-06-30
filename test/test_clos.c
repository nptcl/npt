#include "clos.c"
#include "character.h"
#include "common.h"
#include "real.h"
#include "degrade.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "syscall.h"
#include "type.h"

/*
 *  access
 */
static int test_getslot(void)
{
	int n;
	addr pos, check, v;
	struct slot_struct *str;
	size_t s;

	slot_heap(&pos);
	str = SlotStruct(pos);
	test(str, "SlotStruct1");

	fixnum_heap(&v, 10);
	setarray(pos, SLOT_INDEX_NAME, T);
	GetNameSlot(pos, &check);
	test(check == T, "GetNameSlot1");
	SetNameSlot(pos, v);
	GetNameSlot(pos, &check);
	test(check == v, "GetNameSlot2");

	setarray(pos, SLOT_INDEX_TYPE, T);
	GetTypeSlot(pos, &check);
	test(check == T, "GetTypeSlot1");
	SetTypeSlot(pos, v);
	GetTypeSlot(pos, &check);
	test(check == v, "GetTypeSlot2");

	setarray(pos, SLOT_INDEX_INITARGS, T);
	GetArgsSlot(pos, &check);
	test(check == T, "GetArgsSlot1");
	SetArgsSlot(pos, v);
	GetArgsSlot(pos, &check);
	test(check == v, "GetArgsSlot2");

	setarray(pos, SLOT_INDEX_INITFORM, T);
	GetFormSlot(pos, &check);
	test(check == T, "GetFormSlot1");
	SetFormSlot(pos, v);
	GetFormSlot(pos, &check);
	test(check == v, "GetFormSlot2");

	setarray(pos, SLOT_INDEX_INITFUNCTION, T);
	GetFunctionSlot(pos, &check);
	test(check == T, "GetFunctionSlot1");
	SetFunctionSlot(pos, v);
	GetFunctionSlot(pos, &check);
	test(check == v, "GetFunctionSlot2");

	setarray(pos, SLOT_INDEX_READERS, T);
	GetReadersSlot(pos, &check);
	test(check == T, "GetReadersSlot1");
	SetReadersSlot(pos, v);
	GetReadersSlot(pos, &check);
	test(check == v, "GetReadersSlot2");

	setarray(pos, SLOT_INDEX_WRITERS, T);
	GetWritersSlot(pos, &check);
	test(check == T, "GetWritersSlot1");
	SetWritersSlot(pos, v);
	GetWritersSlot(pos, &check);
	test(check == v, "GetWritersSlot2");

	test(GetUser(pos) == 0, "GetAllocationSlot1");
	SetAllocationSlot(pos, 100);
	test(GetUser(pos) == 1, "GetAllocationSlot2");
	GetAllocationSlot(pos, &n);
	test(n == 1, "GetAllocationSlot3");

	test(str->location == 0, "GetLocationSlot1");
	SetLocationSlot(pos, 200);
	test(str->location == 200, "GetLocationSlot2");
	GetLocationSlot(pos, &s);
	test(s == 200, "GetLocationSlot3");

	RETURN;
}

static int test_getclos(void)
{
	int n;
	addr pos, check, v;
	struct clos_struct *str;
	fixnum s;

	slot_vector_heap(&pos, 10);
	clos_heap(&pos, pos);
	str = ClosStruct(pos);
	test(str, "ClosStruct1");

	fixnum_heap(&v, 10);
	setarray(pos, CLOS_INDEX_CLASS_OF, T);
	GetClassOfClos(pos, &check);
	test(check == T, "GetClassOfClos1");
	SetClassOfClos(pos, v);
	GetClassOfClos(pos, &check);
	test(check == v, "GetClassOfClos2");

	setarray(pos, CLOS_INDEX_SLOT, T);
	GetSlotClos(pos, &check);
	test(check == T, "GetSlotClos1");
	SetSlotClos(pos, v);
	GetSlotClos(pos, &check);
	test(check == v, "GetSlotClos2");

	setarray(pos, CLOS_INDEX_VALUE, T);
	GetValueClos(pos, &check);
	test(check == T, "GetValueClos1");
	SetValueClos(pos, v);
	GetValueClos(pos, &check);
	test(check == v, "GetValueClos2");

	test(GetUser(pos) == 0, "GetFuncallClos1");
	SetFuncallClos(pos, 100);
	test(GetUser(pos) == 1, "GetFuncallClos2");
	GetFuncallClos(pos, &n);
	test(n == 1, "GetFuncallClos3");

	test(str->version == 0, "GetVersionClos1");
	SetVersionClos(pos, 200);
	test(str->version == 200, "GetVersionClos2");
	GetVersionClos(pos, &s);
	test(s == 200, "GetVersionClos3");

	RETURN;
}

static int test_getslotvector(void)
{
	addr pos, check, v;
	size_t size;

	slot_vector_heap(&pos, 10);
	LenSlotVector(pos, &size);
	test(size = 10, "getslotvector1");

	setarray(pos, 3, T);
	GetSlotVector(pos, 3, &check);
	test(check == T, "getslotvector2");
	fixnum_heap(&v, 10);
	SetSlotVector(pos, 4, v);
	GetSlotVector(pos, 4, &check);
	test(check == v, "getslotvector3");

	RETURN;
}

static int test_getclosvalue(void)
{
	addr pos, check, v;
	size_t size;

	clos_value_unsafe(NULL, &pos, 10);
	LenClosValue(pos, &size);
	test(size = 10, "getclosvalue1");

	setarray(pos, 3, T);
	GetClosValue(pos, 3, &check);
	test(check == T, "getclosvalue2");
	fixnum_heap(&v, 10);
	SetClosValue(pos, 4, v);
	GetClosValue(pos, 4, &check);
	test(check == v, "getclosvalue3");

	RETURN;
}


/*
 *  allocate
 */
static int test_slot_alloc(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	slot_alloc(local, &pos);
	test(GetType(pos) == LISPSYSTEM_SLOT, "slot_alloc1");
	test(GetStatusDynamic(pos), "slot_alloc2");
	test(slot_instance_p(pos), "slot_alloc3");
	GetNameSlot(pos, &check);
	test(check == Unbound, "slot_alloc4");
	GetFormSlot(pos, &check);
	test(check == Unbound, "slot_alloc5");

	slot_alloc(NULL, &pos);
	test(GetType(pos) == LISPSYSTEM_SLOT, "slot_alloc6");
	test(! GetStatusDynamic(pos), "slot_alloc7");

	slot_local(local, &pos);
	test(GetType(pos) == LISPSYSTEM_SLOT, "slot_alloc8");
	test(GetStatusDynamic(pos), "slot_alloc9");

	slot_heap(&pos);
	test(GetType(pos) == LISPSYSTEM_SLOT, "slot_alloc10");
	test(! GetStatusDynamic(pos), "slot_alloc11");

	rollback_local(local, stack);

	RETURN;
}

static int test_slot_copy_alloc(void)
{
	int v;
	addr pos1, pos2, check;
	LocalRoot local;
	LocalStack stack;
	size_t s;

	local = Local_Thread;
	push_local(local, &stack);

	slot_heap(&pos1);
	SetAllocationSlot(pos1, 1);
	SetLocationSlot(pos1, 22);
	SetFormSlot(pos1, T);

	slot_copy_alloc(local, &pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT, "slot_copy_alloc1");
	test(GetStatusDynamic(pos2), "slot_copy_alloc2");
	GetAllocationSlot(pos2, &v);
	test(v, "slot_copy_alloc3");
	GetLocationSlot(pos2, &s);
	test(s == 22, "slot_copy_alloc4");
	GetFormSlot(pos2, &check);
	test(check == T, "slot_copy_alloc5");

	slot_copy_alloc(NULL, &pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT, "slot_copy_alloc6");
	test(! GetStatusDynamic(pos2), "slot_copy_alloc7");

	slot_copy_local(local, &pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT, "slot_copy_alloc8");
	test(GetStatusDynamic(pos2), "slot_copy_alloc9");

	slot_copy_heap(&pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT, "slot_copy_alloc10");
	test(! GetStatusDynamic(pos2), "slot_copy_alloc11");

	rollback_local(local, stack);

	RETURN;
}

static int test_slot_vector_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	slot_vector_alloc(local, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_alloc1");
	test(GetStatusDynamic(pos), "slot_vector_alloc2");
	test(lenarrayr(pos) == 10, "slot_vector_alloc3");

	slot_vector_alloc(NULL, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_alloc4");
	test(! GetStatusDynamic(pos), "slot_vector_alloc5");

	slot_vector_local(local, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_alloc6");
	test(GetStatusDynamic(pos), "slot_vector_alloc7");

	slot_vector_heap(&pos, 10);
	test(GetType(pos) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_alloc8");
	test(! GetStatusDynamic(pos), "slot_vector_alloc9");

	rollback_local(local, stack);

	RETURN;
}

static int test_slot_vector_copy_alloc(void)
{
	addr pos1, pos2, pos;
	LocalRoot local;
	LocalStack stack;
	size_t i;

	local = Local_Thread;
	push_local(local, &stack);

	slot_vector_heap(&pos1, 10);
	for (i = 0; i < 10; i++) {
		slot_heap(&pos);
		SetSlotVector(pos1, i, pos);
	}
	slot_vector_copy_alloc(local, &pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_copy_alloc1");
	test(GetStatusDynamic(pos2), "slot_vector_copy_alloc2");
	test(lenarrayr(pos2) == 10, "slot_vector_copy_alloc3");

	slot_vector_copy_alloc(NULL, &pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_copy_alloc4");
	test(! GetStatusDynamic(pos2), "slot_vector_copy_alloc5");

	slot_vector_copy_local(local, &pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_copy_alloc6");
	test(GetStatusDynamic(pos2), "slot_vector_copy_alloc7");

	slot_vector_copy_heap(&pos2, pos1);
	test(GetType(pos2) == LISPSYSTEM_SLOT_VECTOR, "slot_vector_copy_alloc8");
	test(! GetStatusDynamic(pos2), "slot_vector_copy_alloc9");

	GetSlotVector(pos2, 4, &pos);
	test(GetType(pos) == LISPSYSTEM_SLOT, "slot_vector_copy_alloc10");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_alloc(void)
{
	int v;
	addr slots, pos, check;
	LocalRoot local;
	LocalStack stack;
	fixnum s;

	local = Local_Thread;
	push_local(local, &stack);

	slot_vector_heap(&slots, 10);
	clos_alloc(local, &pos, slots);
	test(GetType(pos) == LISPTYPE_CLOS, "clos_alloc1");
	test(GetStatusDynamic(pos), "clos_alloc2");

	GetClassOfClos(pos, &check);
	test(check == Unbound, "clos_alloc3");
	GetSlotClos(pos, &check);
	test(check == slots, "clos_alloc4");
	GetFuncallClos(pos, &v);
	test(v == 0, "clos_alloc5");
	GetVersionClos(pos, &s);
	test(s == 0, "clos_alloc6");
	GetValueClos(pos, &check);
	test(GetType(check) == LISPSYSTEM_CLOS_VALUE, "clos_alloc7");
	test(lenarrayr(check) == 10, "clos_alloc8");
	getarray(check, 3, &check);
	test(check == Unbound, "clos_alloc9");

	clos_alloc(NULL, &pos, slots);
	test(GetType(pos) == LISPTYPE_CLOS, "clos_alloc10");
	test(! GetStatusDynamic(pos), "clos_alloc11");

	clos_local(local, &pos, slots);
	test(GetType(pos) == LISPTYPE_CLOS, "clos_alloc12");
	test(GetStatusDynamic(pos), "clos_alloc13");

	clos_alloc(NULL, &pos, slots);
	test(GetType(pos) == LISPTYPE_CLOS, "clos_alloc14");
	test(! GetStatusDynamic(pos), "clos_alloc15");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  control
 */
static int test_closp(void)
{
	addr slots, pos;

	slot_vector_heap(&slots, 10);
	clos_heap(&pos, slots);
	test(closp(pos), "closp1");
	test(! closp(slots), "closp2");
	test(! closp(T), "closp3");

	RETURN;
}

static int test_slotp(void)
{
	addr pos;

	slot_heap(&pos);
	test(slotp(pos), "slotp1");
	test(! slotp(T), "slotp2");

	RETURN;
}

static int test_clos_funcall_p(void)
{
	addr pos;

	slot_vector_heap(&pos, 10);
	clos_heap(&pos, pos);
	test(! clos_funcall_p(pos), "clos_funcall_p1");
	SetFuncallClos(pos, 1);
	test(clos_funcall_p(pos), "clos_funcall_p2");
	SetFuncallClos(pos, 0);
	test(! clos_funcall_p(pos), "clos_funcall_p3");
	clos_set_funcall(pos);
	test(clos_funcall_p(pos), "clos_funcall_p4");

	RETURN;
}

static int test_slot_class_p(void)
{
	addr pos;

	slot_heap(&pos);
	test(! slot_class_p(pos), "slot_class_p1");
	test(slot_instance_p(pos), "slot_class_p2");
	SetAllocationSlot(pos, 1);
	test(slot_class_p(pos), "slot_class_p3");
	test(! slot_instance_p(pos), "slot_class_p4");

	slot_set_instance(pos);
	test(! slot_class_p(pos), "slot_class_p5");
	test(slot_instance_p(pos), "slot_class_p6");
	slot_set_class(pos);
	test(slot_class_p(pos), "slot_class_p7");
	test(! slot_instance_p(pos), "slot_class_p8");

	RETURN;
}

static int test_clos_getp(void)
{
	addr slots, pos, name, value, v;
	constindex index;

	slot_vector_heap(&slots, 4);
	slot_heap(&pos);
	SetNameSlot(pos, readr("aaa"));
	SetSlotVector(slots, 0, pos);
	slot_heap(&pos);
	SetNameSlot(pos, readr("bbb"));
	SetSlotVector(slots, 1, pos);
	slot_heap(&pos);
	SetNameSlot(pos, readr("standard-class"));
	SetSlotVector(slots, 2, pos);
	slot_heap(&pos);
	SetNameSlot(pos, readr("ddd"));
	SetSlotVector(slots, 3, pos);

	clos_heap(&pos, slots);
	index = CONSTANT_COMMON_STANDARD_CLASS;
	test(clos_errorp(pos, 1, index), "clos_errorp1");
	test(! clos_errorp(pos, 2, index), "clos_errorp2");
	test(clos_errorp(pos, 100, index), "clos_errorp3");
	index = CONSTANT_COMMON_STANDARD_OBJECT;
	test(clos_errorp(pos, 3, index), "clos_errorp4");

	test(! clos_getp(pos, readr("zzz"), &name), "clos_getp1");
	test(clos_getp(pos, readr("aaa"), &name), "clos_getp2");
	test(name == Unbound, "clos_getp3");
	GetValueClos(pos, &value);
	SetClosValue(value, 1, T);
	test(clos_getp(pos, readr("bbb"), &name), "clos_getp4");
	test(name == T, "clos_getp5");

	fixnum_heap(&v, 10);
	test(! clos_setp(pos, readr("zzz"), v), "clos_setp1");
	test(clos_setp(pos, readr("ddd"), v), "clos_setp2");
	test(clos_getp(pos, readr("ddd"), &name), "clos_setp3");
	test(name == v, "clos_setp4");

	name = 0;
	test(! clos_checkp(pos, readr("zzz"), &name), "clos_checkp1");
	test(clos_checkp(pos, readr("ddd"), &name), "clos_checkp2");
	test(name == v, "clos_checkp3");

	clos_get(pos, readr("bbb"), &name);
	test(name == T, "clos_get1");
	clos_set(pos, readr("bbb"), v);
	clos_get(pos, readr("bbb"), &name);
	test(name == v, "clos_set1");
	name = 0;
	clos_check(pos, readr("bbb"), &name);
	test(name == v, "clos_check1");

	name = 0;
	clos_getelt(pos, 3, &name);
	test(name == v, "clos_getelt1");
	clos_setelt(pos, 3, T);
	name = 0;
	clos_getelt(pos, 3, &name);
	test(name == T, "clos_setelt1");
	name = 0;
	clos_checkelt(pos, 3, &name);
	test(name == T, "clos_checkelt1");

	RETURN;
}


/*
 *  check
 */
static int test_clos_slot_exists_p(void)
{
	addr slots, pos;

	slot_vector_heap(&slots, 4);
	slot_heap(&pos);
	SetNameSlot(pos, readr("aaa"));
	SetSlotVector(slots, 0, pos);
	slot_heap(&pos);
	SetNameSlot(pos, readr("bbb"));
	SetSlotVector(slots, 1, pos);
	slot_heap(&pos);
	SetNameSlot(pos, readr("standard-class"));
	SetSlotVector(slots, 2, pos);
	slot_heap(&pos);
	SetNameSlot(pos, readr("ddd"));
	SetSlotVector(slots, 3, pos);
	clos_heap(&pos, slots);

	test(clos_slot_exists_p(pos, readr("bbb")), "clos_slot_exists_p1");
	test(! clos_slot_exists_p(pos, readr("zzz")), "clos_slot_exists_p2");
	test(! clos_slot_boundp_nil(pos, readr("bbb")), "clos_slot_boundp_nil1");
	clos_setp(pos, readr("bbb"), Nil);
	test(clos_slot_boundp_nil(pos, readr("bbb")), "clos_slot_boundp_nil2");
	test(clos_slot_boundp_nil(pos, readr("zzz")) < 0, "clos_slot_boundp_nil3");

	clos_setp(pos, readr("bbb"), Unbound);
	test(! clos_slot_boundp(pos, readr("bbb")), "clos_slot_boundp1");
	clos_setp(pos, readr("bbb"), Nil);
	test(clos_slot_boundp(pos, readr("bbb")), "clos_slot_boundp2");

	test(clos_slot_makunbound_nil(pos, readr("zzz")), "clos_slot_makunbound_nil1");
	clos_setp(pos, readr("bbb"), Nil);
	test(! clos_slot_makunbound_nil(pos, readr("bbb")), "clos_slot_makunbound_nil2");
	test(! clos_slot_boundp(pos, readr("bbb")), "clos_slot_makunbound_nil3");

	clos_setp(pos, readr("bbb"), Nil);
	clos_slot_makunbound(pos, readr("bbb"));
	test(! clos_slot_boundp(pos, readr("bbb")), "clos_slot_makunbound1");

	RETURN;
}


/*
 *  table
 */
static int test_clos_find_class(void)
{
	addr pos;

	clos_find_class_nil(readr("aaa"), &pos);
	test(pos == Nil, "clos_find_class1");
	clos_find_class_nil(readr("method"), &pos);
	test(closp(pos), "clos_find_class2");
	clos_find_class(readr("class"), &pos);
	test(closp(pos), "clos_find_class3");

	clos_define_class(readr("aaa"), pos);
	pos = 0;
	clos_find_class(readr("aaa"), &pos);
	test(closp(pos), "clos_find_class4");

	RETURN;
}

static int test_clos_find_generic(void)
{
	addr pos;

	clos_find_generic_nil(readr("aaa"), &pos);
	test(pos == Nil, "clos_find_generic1");
	clos_find_class(readr("method"), &pos);
	clos_define_generic(readr("aaa"), pos);
	pos = 0;
	clos_find_generic_nil(readr("aaa"), &pos);
	test(closp(pos), "clos_find_generic2");
	pos = 0;
	clos_find_generic(readr("aaa"), &pos);
	test(closp(pos), "clos_find_generic3");

	RETURN;
}

static int test_clos_find_combination(void)
{
	addr pos;

	clos_find_combination_nil(readr("aaa"), &pos);
	test(pos == Nil, "clos_find_combination1");
	clos_find_combination_nil(readr("progn"), &pos);
	test(closp(pos), "clos_find_combination2");
	clos_find_combination(readr("+"), &pos);
	test(closp(pos), "clos_find_combination3");

	clos_define_combination(readr("aaa"), pos);
	pos = 0;
	clos_find_combination(readr("aaa"), &pos);
	test(closp(pos), "clos_find_combination4");

	RETURN;
}

static int test_clos_find_specializer(void)
{
	addr pos;

	clos_find_specializer_nil(readr("aaa"), &pos);
	test(pos == Nil, "clos_find_specializer1");
	clos_find_class(readr("method"), &pos);
	clos_define_specializer(readr("aaa"), pos);
	pos = 0;
	clos_find_specializer_nil(readr("aaa"), &pos);
	test(closp(pos), "clos_find_specializer2");
	pos = 0;
	clos_find_specializer(readr("aaa"), &pos);
	test(closp(pos), "clos_find_specializer3");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos(void)
{
	/* access */
	TestBreak(test_getslot);
	TestBreak(test_getclos);
	TestBreak(test_getslotvector);
	TestBreak(test_getclosvalue);
	/* allocate */
	TestBreak(test_slot_alloc);
	TestBreak(test_slot_copy_alloc);
	TestBreak(test_slot_vector_alloc);
	TestBreak(test_slot_vector_copy_alloc);
	TestBreak(test_clos_alloc);
	TestBreak(test_clos_alloc);
	/* control */
	TestBreak(test_closp);
	TestBreak(test_slotp);
	TestBreak(test_clos_funcall_p);
	TestBreak(test_slot_class_p);
	TestBreak(test_clos_getp);
	/* check */
	TestBreak(test_clos_slot_exists_p);
	/* table */
	TestBreak(test_clos_find_class);
	TestBreak(test_clos_find_generic);
	TestBreak(test_clos_find_combination);
	TestBreak(test_clos_find_specializer);

	return 0;
}

int test_clos(void)
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
		build_real();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_readtable();
		lisp_initialize = 1;
		result = testbreak_clos();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

