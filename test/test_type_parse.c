#include "type_parse.c"
#include "calltype.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"

static int test_typeonly(addr pos, enum LISPDECL type)
{
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		type = RefLispDecl(pos);
		degrade_printf("listdecl error\n");
		return 0;
	}
	if (RefNotDecl(pos)) {
		degrade_printf("notdecl error\n");
		return 0;
	}

	return 1;
}

static int test_typecheck(addr pos, enum LISPDECL type, size_t size)
{
	if (! test_typeonly(pos, type)) return 0;
	if (lenarrayr(pos) != size) {
		degrade_printf("array size error\n");
		return 0;
	}

	return 1;
}

static int test_typeonly_not(addr pos, enum LISPDECL type)
{
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("listdecl error\n");
		return 0;
	}
	if (! RefNotDecl(pos)) {
		degrade_printf("notdecl error\n");
		return 0;
	}

	return 1;
}

static int test_typecheck_not(addr pos, enum LISPDECL type, size_t size)
{
	if (! test_typeonly_not(pos, type)) return 0;
	if (lenarrayr(pos) != size) {
		degrade_printf("array size error\n");
		return 0;
	}

	return 1;
}

static int test_type_heap(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	pos = type_allocr(NULL, LISPDECL_CONS, 10);
	test(test_typecheck(pos, LISPDECL_CONS, 10), "type_heap1");
	test(! GetStatusDynamic(pos), "type_heap2");

	pos = type_localr(local, LISPDECL_CONS, 10);
	test(test_typecheck(pos, LISPDECL_CONS, 10), "type_heap3");
	test(GetStatusDynamic(pos), "type_heap4");

	pos = type_heapr(LISPDECL_CONS, 10);
	test(test_typecheck(pos, LISPDECL_CONS, 10), "type_heap5");
	test(! GetStatusDynamic(pos), "type_heap6");

	type_alloc(local, &pos, LISPDECL_CONS, 10);
	test(test_typecheck(pos, LISPDECL_CONS, 10), "type_heap7");
	test(GetStatusDynamic(pos), "type_heap8");

	type_local(local, &pos, LISPDECL_CONS, 10);
	test(test_typecheck(pos, LISPDECL_CONS, 10), "type_heap9");
	test(GetStatusDynamic(pos), "type_heap10");

	type_heap(&pos, LISPDECL_CONS, 10);
	test(test_typecheck(pos, LISPDECL_CONS, 10), "type_heap11");
	test(! GetStatusDynamic(pos), "type_heap12");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_no_recursive_type_heap(void)
{
	addr pos, pos1, pos2, check, one;
	LocalRoot local;
	LocalStack stack;

	type_heap(&pos, LISPDECL_CONS, 3);
	fixnum_heap(&pos1, 10);
	fixnum_heap(&pos2, 20);
	SetArrayType(pos, 0, T);
	SetArrayType(pos, 1, pos1);
	SetArrayType(pos, 2, pos2);

	copy_no_recursive_type_heap(&check, pos);
	test(test_typecheck(check, LISPDECL_CONS, 3), "copy_no_recursive_type_heap1");
	GetArrayType(check, 0, &one);
	test(one == T, "copy_no_recursive_type_heap2");
	GetArrayType(check, 1, &one);
	test(one == pos1, "copy_no_recursive_type_heap3");
	GetArrayType(check, 2, &one);
	test(one == pos2, "copy_no_recursive_type_heap4");

	SetNotDecl(pos, 1);
	copy_no_recursive_type_heap(&check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_type_heap5");
	test(RefNotDecl(check), "copy_no_recursive_type_heap6");
	test(lenarrayr(check) == 3, "copy_no_recursive_type_heap7");
	GetArrayType(check, 0, &one);
	test(one == T, "copy_no_recursive_type_heap8");
	GetArrayType(check, 1, &one);
	test(one == pos1, "copy_no_recursive_type_heap9");
	GetArrayType(check, 2, &one);
	test(one == pos2, "copy_no_recursive_type_heap10");

	local = Local_Thread;
	push_local(local, &stack);

	copy_no_recursive_type_alloc(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_type_heap11");
	test(GetStatusDynamic(check), "copy_no_recursive_type_heap12");
	copy_no_recursive_type_local(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_type_heap13");
	test(GetStatusDynamic(check), "copy_no_recursive_type_heap14");
	copy_no_recursive_type_heap(&check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_type_heap15");
	test(! GetStatusDynamic(check), "copy_no_recursive_type_heap16");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_no_recursive_typeonly_heap(void)
{
	addr pos, pos1, pos2, check, one;
	LocalRoot local;
	LocalStack stack;

	type_heap(&pos, LISPDECL_CONS, 3);
	fixnum_heap(&pos1, 10);
	fixnum_heap(&pos2, 20);
	SetArrayType(pos, 0, T);
	SetArrayType(pos, 1, pos1);
	SetArrayType(pos, 2, pos2);

	copy_no_recursive_typeonly_heap(&check, pos);
	test(test_typecheck(check, LISPDECL_CONS, 3), "copy_no_recursive_typeonly_heap1");
	GetArrayType(check, 0, &one);
	test(one == T, "copy_no_recursive_typeonly_heap2");
	GetArrayType(check, 1, &one);
	test(one == pos1, "copy_no_recursive_typeonly_heap3");
	GetArrayType(check, 2, &one);
	test(one == pos2, "copy_no_recursive_typeonly_heap4");

	SetNotDecl(pos, 1);
	copy_no_recursive_typeonly_heap(&check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_typeonly_heap5");
	test(! RefNotDecl(check), "copy_no_recursive_typeonly_heap6");
	test(lenarrayr(check) == 3, "copy_no_recursive_typeonly_heap7");
	GetArrayType(check, 0, &one);
	test(one == T, "copy_no_recursive_typeonly_heap8");
	GetArrayType(check, 1, &one);
	test(one == pos1, "copy_no_recursive_typeonly_heap9");
	GetArrayType(check, 2, &one);
	test(one == pos2, "copy_no_recursive_typeonly_heap10");

	local = Local_Thread;
	push_local(local, &stack);

	copy_no_recursive_typeonly_alloc(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_typeonly_heap11");
	test(GetStatusDynamic(check), "copy_no_recursive_typeonly_heap12");

	copy_no_recursive_typeonly_local(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_typeonly_heap13");
	test(GetStatusDynamic(check), "copy_no_recursive_typeonly_heap14");

	copy_no_recursive_typeonly_heap(&check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "copy_no_recursive_typeonly_heap15");
	test(! GetStatusDynamic(check), "copy_no_recursive_typeonly_heap16");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_object1(void)
{
	addr cons, pos;

	consnil_heap(&cons);
	type_object1(NULL, LISPDECL_SYMBOL, cons, &pos);
	test(test_typecheck(pos, LISPDECL_SYMBOL, 1), "type_object1-1");
	GetArrayType(pos, 0, &pos);
	test(pos == cons, "type_object1-2");

	RETURN;
}

static int test_type_object2(void)
{
	addr pos, cons, check;

	consnil_heap(&cons);
	type_object2(NULL, LISPDECL_ASTERISK, T, cons, &pos);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 2), "type_object2-1");
	GetArrayType(pos, 0, &check);
	test(check == T, "type_object2-2");
	GetArrayType(pos, 1, &check);
	test(check == cons, "type_object2-3");

	RETURN;
}

static int test_type_object3(void)
{
	addr pos, value, check;

	fixnum_heap(&value, 10);
	type_object3(NULL, LISPDECL_CONS, T, value, Nil, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 3), "type_object3-1");
	GetArrayType(pos, 0, &check);
	test(check == T, "type_object3-2");
	GetArrayType(pos, 1, &check);
	test(check == value, "type_object3-3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_object3-4");

	RETURN;
}

static int test_type_object4(void)
{
	addr pos, value1, value2, check;

	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	type_object4(NULL, LISPDECL_CONS, T, value1, Nil, value2, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 4), "type_object4-1");
	GetArrayType(pos, 0, &check);
	test(check == T, "type_object4-2");
	GetArrayType(pos, 1, &check);
	test(check == value1, "type_object4-3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_object4-4");
	GetArrayType(pos, 3, &check);
	test(check == value2, "type_object4-5");

	RETURN;
}

static int test_type_object1_not(void)
{
	addr cons, pos;

	consnil_heap(&cons);
	type_object1_not(NULL, LISPDECL_SYMBOL, cons, &pos);
	test(test_typecheck_not(pos, LISPDECL_SYMBOL, 1), "type_object1_not1");
	GetArrayType(pos, 0, &pos);
	test(pos == cons, "type_object1_not2");

	RETURN;
}

static int test_type_object2_not(void)
{
	addr pos, cons, check;

	consnil_heap(&cons);
	type_object2_not(NULL, LISPDECL_ASTERISK, T, cons, &pos);
	test(test_typecheck_not(pos, LISPDECL_ASTERISK, 2), "type_object2_not1");
	GetArrayType(pos, 0, &check);
	test(check == T, "type_object2_not2");
	GetArrayType(pos, 1, &check);
	test(check == cons, "type_object2_not3");

	RETURN;
}

static int test_type_object3_not(void)
{
	addr pos, value, check;

	fixnum_heap(&value, 10);
	type_object3_not(NULL, LISPDECL_CONS, T, value, Nil, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 3), "type_object3_not1");
	GetArrayType(pos, 0, &check);
	test(check == T, "type_object3_not2");
	GetArrayType(pos, 1, &check);
	test(check == value, "type_object3_not3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_object3_not4");

	RETURN;
}

static int test_type_object4_not(void)
{
	addr pos, value1, value2, check;

	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	type_object4_not(NULL, LISPDECL_CONS, T, value1, Nil, value2, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 4), "type_object4_not1");
	GetArrayType(pos, 0, &check);
	test(check == T, "type_object4_not2");
	GetArrayType(pos, 1, &check);
	test(check == value1, "type_object4_not3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_object4_not4");
	GetArrayType(pos, 3, &check);
	test(check == value2, "type_object4_not5");

	RETURN;
}

static int test_type_empty(void)
{
	addr pos;

	pos = type_emptyr(NULL, LISPDECL_CONS);
	test(test_typecheck(pos, LISPDECL_CONS, 0), "type_empty1");
	type_empty(NULL, LISPDECL_NULL, &pos);
	test(test_typecheck(pos, LISPDECL_NULL, 0), "type_empty2");

	RETURN;
}

static int test_type_empty_not(void)
{
	addr pos;
	type_empty_not(NULL, LISPDECL_CONS, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 0), "type_empty_not1");
	RETURN;
}

static int test_type_asterisk_heap(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	pos = type_asterisk_allocr(NULL);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_asterisk_heap1");
	test(! GetStatusDynamic(pos), "type_asterisk_heap2");

	pos = type_asterisk_localr(local);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_asterisk_heap3");
	test(GetStatusDynamic(pos), "type_asterisk_heap4");

	pos = type_asterisk_heapr();
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_asterisk_heap5");
	test(! GetStatusDynamic(pos), "type_asterisk_heap6");

	type_asterisk_alloc(local, &pos);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_asterisk_heap7");
	test(GetStatusDynamic(pos), "type_asterisk_heap8");

	type_asterisk_local(local, &pos);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_asterisk_heap9");
	test(GetStatusDynamic(pos), "type_asterisk_heap10");

	type_asterisk_heap(&pos);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_asterisk_heap11");
	test(! GetStatusDynamic(pos), "type_asterisk_heap12");

	rollback_local(local, stack);

	RETURN;
}

static int test_asterisk_p(void)
{
	addr pos;

	type_asterisk_heap(&pos);
	test(asterisk_p(pos), "asterisk_p1");
	type_empty(NULL, LISPDECL_CONS, &pos);
	test(! asterisk_p(pos), "asterisk_p2");

	RETURN;
}

static int test_type_nil_heap(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	pos = type_nil_allocr(NULL);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_nil_heap1");
	test(! GetStatusDynamic(pos), "type_nil_heap2");
	pos = type_nil_localr(local);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_nil_heapr3");
	test(GetStatusDynamic(pos), "type_nil_heap4");
	pos = type_nil_heapr();
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_nil_heapr5");
	test(! GetStatusDynamic(pos), "type_nil_heap6");

	type_nil_alloc(local, &pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_nil_heap7");
	test(GetStatusDynamic(pos), "type_nil_heap8");
	type_nil_local(local, &pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_nil_heap9");
	test(GetStatusDynamic(pos), "type_nil_heap10");
	type_nil_heap(&pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_nil_heap11");
	test(! GetStatusDynamic(pos), "type_nil_heap12");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_t_heap(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	pos = type_t_allocr(NULL);
	test(test_typecheck(pos, LISPDECL_T, 0), "type_t_heap1");
	test(! GetStatusDynamic(pos), "type_t_heap2");
	pos = type_t_localr(local);
	test(test_typecheck(pos, LISPDECL_T, 0), "type_t_heapr3");
	test(GetStatusDynamic(pos), "type_t_heap4");
	pos = type_t_heapr();
	test(test_typecheck(pos, LISPDECL_T, 0), "type_t_heapr5");
	test(! GetStatusDynamic(pos), "type_t_heap6");

	type_t_alloc(local, &pos);
	test(test_typecheck(pos, LISPDECL_T, 0), "type_t_heap7");
	test(GetStatusDynamic(pos), "type_t_heap8");
	type_t_local(local, &pos);
	test(test_typecheck(pos, LISPDECL_T, 0), "type_t_heap9");
	test(GetStatusDynamic(pos), "type_t_heap10");
	type_t_heap(&pos);
	test(test_typecheck(pos, LISPDECL_T, 0), "type_t_heap11");
	test(! GetStatusDynamic(pos), "type_t_heap12");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_bool_heap(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	type_bool_heap(0, &pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_bool_heap1");
	type_bool_heap(100, &pos);
	test(test_typecheck(pos, LISPDECL_T, 0), "type_bool_heap2");

	local = Local_Thread;
	push_local(local, &stack);

	type_bool_alloc(NULL, 0, &pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_bool_heap3");
	test(! GetStatusDynamic(pos), "type_bool_heap4");
	type_bool_local(local, 0, &pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_bool_heap5");
	test(GetStatusDynamic(pos), "type_bool_heap6");
	type_bool_heap(0, &pos);
	test(test_typecheck(pos, LISPDECL_NIL, 0), "type_bool_heap7");
	test(! GetStatusDynamic(pos), "type_bool_heap8");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_realvalue(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	type_realvalue(NULL, LISPDECL_INTEGER, pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type_realvalue1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_realvalue2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 10, "type_realvalue3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_realvalue4");
	GetArrayType(pos, 3, &check);
	test(RefFixnum(check) == 10, "type_realvalue5");

	RETURN;
}

static int test_type_aster1(void)
{
	addr pos;

	type_aster1(NULL, LISPDECL_CONS, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 1), "type_aster1-1");
	GetArrayType(pos, 0, &pos);
	test(asterisk_p(pos), "type_aster1-2");

	RETURN;
}

static int test_type_aster2(void)
{
	addr pos, check;

	type_aster2(NULL, LISPDECL_CONS, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_aster2-1");
	GetArrayType(pos, 0, &check);
	test(asterisk_p(check), "type_aster2-2");
	GetArrayType(pos, 1, &check);
	test(asterisk_p(check), "type_aster2-3");

	RETURN;
}

static int test_type_aster3(void)
{
	addr pos, check;

	type_aster3(NULL, LISPDECL_CONS, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 3), "type_aster3-1");
	GetArrayType(pos, 0, &check);
	test(asterisk_p(check), "type_aster3-2");
	GetArrayType(pos, 1, &check);
	test(asterisk_p(check), "type_aster3-3");
	GetArrayType(pos, 2, &check);
	test(asterisk_p(check), "type_aster3-4");

	RETURN;
}

static int test_type_aster4(void)
{
	addr pos, check;

	type_aster4(NULL, LISPDECL_CONS, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 4), "type_aster4-1");
	GetArrayType(pos, 0, &check);
	test(asterisk_p(check), "type_aster4-2");
	GetArrayType(pos, 1, &check);
	test(asterisk_p(check), "type_aster4-3");
	GetArrayType(pos, 2, &check);
	test(asterisk_p(check), "type_aster4-4");
	GetArrayType(pos, 3, &check);
	test(asterisk_p(check), "type_aster4-5");

	RETURN;
}

static int test_function_asterisk(void)
{
	addr pos, check;

	type_function_asterisk(NULL, &pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function_asterisk1");
	GetArrayType(pos, 0, &check);
	test(asterisk_p(check), "type_function_asterisk2");
	GetArrayType(pos, 1, &check);
	test(asterisk_p(check), "type_function_asterisk3");
	GetArrayType(pos, 2, &check);
	test(asterisk_p(check), "type_function_asterisk4");

	RETURN;
}

static int test_type_and_call(void)
{
	addr left, right, aster, truetype, niltype, check;

	type_empty(NULL, LISPDECL_CONS, &left);
	type_empty(NULL, LISPDECL_NULL, &right);
	type_asterisk_heap(&aster);
	type_t_heap(&truetype);
	type_nil_heap(&niltype);

	type_and(NULL, aster, right, &check);
	test(check == right, "type_and_call1");
	type_and(NULL, truetype, right, &check);
	test(check == right, "type_and_call2");
	type_and(NULL, niltype, right, &check);
	test(test_typecheck(check, LISPDECL_NIL, 0), "type_and_call3");

	type_and(NULL, left, aster, &check);
	test(check == left, "type_and_call4");
	type_and(NULL, left, truetype, &check);
	test(check == left, "type_and_call5");
	type_and(NULL, left, niltype, &check);
	test(test_typecheck(check, LISPDECL_NIL, 0), "type_and_call6");

	type_and(NULL, left, right, &check);
	test(test_typecheck(check, LISPDECL_AND, 1), "type_and_call7");
	GetArrayType(check, 0, &check);
	GetArrayA4(check, 0, &aster);
	test(aster == left, "type_and_call8");
	GetArrayA4(check, 1, &aster);
	test(aster == right, "type_and_call9");

	RETURN;
}

static int test_type_or_call(void)
{
	addr left, right, aster, truetype, niltype, check;

	type_empty(NULL, LISPDECL_CONS, &left);
	type_empty(NULL, LISPDECL_NULL, &right);
	type_asterisk_heap(&aster);
	type_t_heap(&truetype);
	type_nil_heap(&niltype);

	type_or(NULL, aster, right, &check);
	test(test_typecheck(check, LISPDECL_T, 0), "type_or_call1");
	type_or(NULL, truetype, right, &check);
	test(test_typecheck(check, LISPDECL_T, 0), "type_or_call2");
	type_or(NULL, niltype, right, &check);
	test(check == right, "type_or_call3");

	type_or(NULL, left, aster, &check);
	test(test_typecheck(check, LISPDECL_T, 0), "type_or_call4");
	type_or(NULL, left, truetype, &check);
	test(test_typecheck(check, LISPDECL_T, 0), "type_or_call5");
	type_or(NULL, left, niltype, &check);
	test(check == left, "type_or_call6");

	type_or(NULL, left, right, &check);
	test(test_typecheck(check, LISPDECL_OR, 1), "type_or_call7");
	GetArrayType(check, 0, &check);
	GetArrayA4(check, 0, &aster);
	test(aster == left, "type_or_call8");
	GetArrayA4(check, 1, &aster);
	test(aster == right, "type_or_call9");

	RETURN;
}

static int test_setnotdecl_value(void)
{
	addr pos;

	type_empty(NULL, LISPDECL_CONS, &pos);
	setnotdecl_value(pos, 0);
	test(GetUser(pos) == LISPDECL_CONS, "setnotdecl_value1");
	test(! RefNotDecl(pos), "setnotdecl_value2");
	setnotdecl_value(pos, 1);
	test(GetUser(pos) != LISPDECL_CONS, "setnotdecl_value3");
	test(RefLispDecl(pos) == LISPDECL_CONS, "setnotdecl_value4");
	test(RefNotDecl(pos), "setnotdecl_value5");

	RETURN;
}

static int test_setnotdecl_object(void)
{
	addr left, right;

	type_empty(NULL, LISPDECL_CONS, &left);
	type_empty_not(NULL, LISPDECL_CONS, &right);
	setnotdecl_object(left, right);
	test(RefNotDecl(left), "setnotdecl_object1");
	setnotdecl_value(right, 0);
	setnotdecl_object(left, right);
	test(! RefNotDecl(left), "setnotdecl_object2");

	RETURN;
}

static int test_reversenotdecl(void)
{
	addr pos;

	type_empty(NULL, LISPDECL_CONS, &pos);
	reversenotdecl(pos);
	test(RefNotDecl(pos), "reversenotdecl1");
	test(RefLispDecl(pos) == LISPDECL_CONS, "reversenotdecl2");
	reversenotdecl(pos);
	test(! RefNotDecl(pos), "reversenotdecl3");
	test(RefLispDecl(pos) == LISPDECL_CONS, "reversenotdecl4");

	RETURN;
}

static int test_float_value_p(void)
{
	test(float_value_p(LISPDECL_FLOAT), "float_value_p1");
	test(float_value_p(LISPDECL_DOUBLE_FLOAT), "float_value_p2");
	test(! float_value_p(LISPDECL_CONS), "float_value_p3");

	RETURN;
}

static int test_range_value_p(void)
{
	test(range_value_p(LISPDECL_INTEGER), "range_value_p1");
	test(range_value_p(LISPDECL_REAL), "range_value_p2");
	test(range_value_p(LISPDECL_SHORT_FLOAT), "range_value_p3");
	test(! range_value_p(LISPDECL_SYMBOL), "range_value_p4");

	RETURN;
}

static int test_range_type_p(void)
{
	addr pos;

	type_empty(NULL, LISPDECL_INTEGER, &pos);
	test(range_type_p(pos), "range_type_p1");
	type_empty(NULL, LISPDECL_REAL, &pos);
	test(range_type_p(pos), "range_type_p2");
	type_empty(NULL, LISPDECL_SHORT_FLOAT, &pos);
	test(range_type_p(pos), "range_type_p3");
	type_empty(NULL, LISPDECL_SYMBOL, &pos);
	test(! range_type_p(pos), "range_type_p4");

	RETURN;
}

static int test_subtype_real_p(void)
{
	test(subtype_real_p(LISPDECL_INTEGER, LISPDECL_INTEGER), "subtype_real_p1");
	test(! subtype_real_p(LISPDECL_RATIONAL, LISPDECL_INTEGER), "subtype_real_p2");
	test(subtype_real_p(LISPDECL_INTEGER, LISPDECL_RATIONAL), "subtype_real_p3");
	test(subtype_real_p(LISPDECL_FLOAT, LISPDECL_REAL), "subtype_real_p4");
	test(subtype_real_p(LISPDECL_FLOAT, LISPDECL_FLOAT), "subtype_real_p5");
	test(! subtype_real_p(LISPDECL_INTEGER, LISPDECL_FLOAT), "subtype_real_p6");
	test(subtype_real_p(LISPDECL_LONG_FLOAT, LISPDECL_FLOAT), "subtype_real_p7");
	test(subtype_real_p(LISPDECL_LONG_FLOAT, LISPDECL_LONG_FLOAT), "subtype_real_p8");
	test(! subtype_real_p(LISPDECL_LONG_FLOAT, LISPDECL_SYMBOL), "subtype_real_p9");

	RETURN;
}

/* upgraded-array-element-type */
static int test_type_signed_byte(void)
{
	addr pos;

	type_signed_byte(NULL, &pos, 16);
	test(GetType(pos) == LISPTYPE_TYPE, "type_signed_byte1");
	test(RefLispDecl(pos) == LISPDECL_SIGNED_BYTE, "type_signed_byte2");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "type_signed_byte3");
	test(RefFixnum(pos) == 16, "type_signed_byte4");

	RETURN;
}

static int test_type_unsigned_byte(void)
{
	addr pos;

	type_unsigned_byte(NULL, &pos, 16);
	test(GetType(pos) == LISPTYPE_TYPE, "type_unsigned_byte1");
	test(RefLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "type_unsigned_byte2");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "type_unsigned_byte3");
	test(RefFixnum(pos) == 16, "type_unsigned_byte4");

	RETURN;
}

static int test_upgraded_array_unsigned(void)
{
	test(upgraded_array_unsigned(0) == 8, "upgraded_array_unsigned1");
	test(upgraded_array_unsigned(1) == 8, "upgraded_array_unsigned2");
	test(upgraded_array_unsigned(0xFF) == 8, "upgraded_array_unsigned3");
	test(upgraded_array_unsigned(0x100) == 16, "upgraded_array_unsigned4");
	test(upgraded_array_unsigned(0xFFFE) == 16, "upgraded_array_unsigned5");
	test(upgraded_array_unsigned(0xFFFF) == 16, "upgraded_array_unsigned6");
	test(upgraded_array_unsigned(0x10000) == 32, "upgraded_array_unsigned7");
	test(upgraded_array_unsigned(0xFFFFFFFF) == 32, "upgraded_array_unsigned8");
#ifdef LISP_64BIT
	test(upgraded_array_unsigned(0x100000000ULL) == 64, "upgraded_array_unsigned9");
	test(upgraded_array_unsigned(0x800000000ULL) == 64, "upgraded_array_unsigned10");
#endif

	RETURN;
}

static int test_upgraded_array_signed(void)
{
	int p, m;

	p = signplus_bignum;
	m = signminus_bignum;
	test(upgraded_array_signed(p,0) == 8, "upgraded_array_signed1p");
	test(upgraded_array_signed(p,1) == 8, "upgraded_array_signed2p");
	test(upgraded_array_signed(p,0x7F) == 8, "upgraded_array_signed3p");
	test(upgraded_array_signed(p,0x80) == 16, "upgraded_array_signed4p");
	test(upgraded_array_signed(p,0x7FFF) == 16, "upgraded_array_signed5p");
	test(upgraded_array_signed(p,0x8000) == 32, "upgraded_array_signed6p");
	test(upgraded_array_signed(p,0x7FFFFFFF) == 32, "upgraded_array_signed7p");
#ifdef LISP_64BIT
	test(upgraded_array_signed(p,0x80000000) == 64, "upgraded_array_signed8p");
	test(upgraded_array_signed(p,0x100000000ULL) == 64, "upgraded_array_signed9p");
	test(upgraded_array_signed(p,0x7FFFFFFFFFFFFFFFULL) == 64,
			"upgraded_array_signed10p");
	test(upgraded_array_signed(p,0x8000000000000000ULL) == 0,
			"upgraded_array_signed11p");
#else
	test(upgraded_array_signed(p,0x80000000) == 0, "upgraded_array_signed8p");
#endif
	test(upgraded_array_signed(m,0) == 8, "upgraded_array_signed1m");
	test(upgraded_array_signed(m,10) == 8, "upgraded_array_signed2m");
	test(upgraded_array_signed(m,0x80) == 8, "upgraded_array_signed3m");
	test(upgraded_array_signed(m,0x81) == 16, "upgraded_array_signed4m");
	test(upgraded_array_signed(m,0x8000) == 16, "upgraded_array_signed5m");
	test(upgraded_array_signed(m,0x8001) == 32, "upgraded_array_signed6m");
	test(upgraded_array_signed(m,0x80000000) == 32, "upgraded_array_signed7m");
#ifdef LISP_64BIT
	test(upgraded_array_signed(m,0x80000001) == 64, "upgraded_array_signed8m");
	test(upgraded_array_signed(m,0x100000000ULL) == 64, "upgraded_array_signed9m");
	test(upgraded_array_signed(m,0x8000000000000000ULL) == 64,
			"upgraded_array_signed10m");
	test(upgraded_array_signed(m,0x8000000000000001ULL) == 0,
			"upgraded_array_signed11m");
#else
	test(upgraded_array_signed(m,0x80000001) == 0, "upgraded_array_signed8m");
#endif

	RETURN;
}

static int test_upgraded_array_integer(void)
{
	enum ARRAY_TYPE result;
	int size;
	addr pos;

	/* asterisk */
	size = -1;
	type_intrange_right(Nil, 0, &pos);
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer1");
	type_intrange_left(Nil, 0, &pos);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer2");

	/* left */
	type_object4(NULL, LISPDECL_INTEGER, Nil, Nil, Nil, fixnum_heapr(10), &pos);
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer3");

	/* right */
	type_object4(NULL, LISPDECL_INTEGER, Nil, fixnum_heapr(10), Nil, Nil, &pos);
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer4");

	/* signed */
	type_intrange(Nil, -10, Nil, 0xAAAA, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_SIGNED, "upgraded_array_integer5");
	test(size == 32, "upgraded_array_integer6");

	type_intrange(T, -0xAAAA, T, 20, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_SIGNED, "upgraded_array_integer7");
	test(size == 32, "upgraded_array_integer8");

	/* unsigned */
	type_intrange(Nil, 0, Nil, 1, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_BIT, "upgraded_array_integer9");

	type_intrange(T, 10, T, 20, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_UNSIGNED, "upgraded_array_integer10");
	test(size == 8, "upgraded_array_integer11");

	type_intrange(T, 4000, T, 20, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_UNSIGNED, "upgraded_array_integer12");
	test(size == 16, "upgraded_array_integer13");

	RETURN;
}

static int test_upgraded_array_inplace(void)
{
	enum ARRAY_TYPE type;
	int size;
	addr pos;

	type_empty(NULL, LISPDECL_CHARACTER, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_inplace1");

	type_empty(NULL, LISPDECL_BASE_CHAR, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_inplace2");

	type_empty(NULL, LISPDECL_STANDARD_CHAR, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_inplace3");

	type_intrange(Nil, 0, Nil, 1, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_BIT, "upgraded_array_inplace4");

	type_intrange(Nil, 0, Nil, 40, &pos);
	size = -1;
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_UNSIGNED, "upgraded_array_inplace5");
	test(size == 8, "upgraded_array_inplace6");

	type_intrange(Nil, -70000, Nil, 40, &pos);
	size = -1;
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_SIGNED, "upgraded_array_inplace7");
	test(size == 32, "upgraded_array_inplace8");

	type_aster4(NULL, LISPDECL_SINGLE_FLOAT, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_SINGLE_FLOAT, "upgraded_array_inplace9");

	type_aster4(NULL, LISPDECL_DOUBLE_FLOAT, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_DOUBLE_FLOAT, "upgraded_array_inplace10");

	type_aster4(NULL, LISPDECL_LONG_FLOAT, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_LONG_FLOAT, "upgraded_array_inplace11");

	type_empty(NULL, LISPDECL_SEQUENCE, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_T, "upgraded_array_inplace12");

	type_empty_not(NULL, LISPDECL_STANDARD_CHAR, &pos);
	type = upgraded_array_inplace(pos, &size);
	test(type == ARRAY_TYPE_T, "upgraded_array_inplace13");

	RETURN;
}

static int test_upgraded_array_direct(void)
{
	enum ARRAY_TYPE type;
	int size;
	addr pos;

	type_empty(NULL, LISPDECL_CHARACTER, &pos);
	type = upgraded_array_direct(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_direct1");

	RETURN;
}

static int test_upgraded_array_type_alloc(void)
{
	addr pos, check;

	type_empty(NULL, LISPDECL_STANDARD_CHAR, &pos);
	upgraded_array_type_alloc(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CHARACTER, "upgraded_array_type_alloc1");

	type_empty(NULL, LISPDECL_NULL, &pos);
	upgraded_array_type_alloc(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_T, "upgraded_array_type_alloc2");

	RETURN;
}

static int test_type_object2_array(void)
{
	addr pos, type, cons, check;

	type_empty(NULL, LISPDECL_PACKAGE, &type);
	consnil_heap(&cons);
	type_object2_array(NULL, LISPDECL_ARRAY, type, cons, &pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_object2_array-1");
	GetArrayType(pos, 0, &check);
	test(check != type, "type_object2_array-2");
	test(RefLispDecl(check) == LISPDECL_T, "type_object2_array-3");
	GetArrayType(pos, 1, &check);
	test(check == cons, "type_object2_array-4");

	type_asterisk_heap(&type);
	consnil_heap(&cons);
	type_object2_array(NULL, LISPDECL_ARRAY, type, cons, &pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_object2_array-5");
	GetArrayType(pos, 0, &check);
	test(asterisk_p(type), "type_object2_array-6");
	GetArrayType(pos, 1, &check);
	test(check == cons, "type_object2_array-7");

	RETURN;
}

static int test_equal_array_type(void)
{
	addr left, right;

	type_empty(NULL, LISPDECL_NULL, &left);
	upgraded_array_type_alloc(NULL, &left, left);
	type_empty(NULL, LISPDECL_SYMBOL, &right);
	upgraded_array_type_alloc(NULL, &right, right);
	test(equal_array_type(left, right), "equal_array_type1");

	type_empty(NULL, LISPDECL_CHARACTER, &right);
	upgraded_array_type_alloc(NULL, &right, right);
	test(! equal_array_type(left, right), "equal_array_type2");

	type_object4(NULL, LISPDECL_INTEGER,
			Nil, fixnum_heapr(10),
			Nil, fixnum_heapr(20), &left);
	upgraded_array_type_alloc(NULL, &left, left);
	type_object4(NULL, LISPDECL_INTEGER,
			Nil, fixnum_heapr(1),
			Nil, fixnum_heapr(200), &right);
	upgraded_array_type_alloc(NULL, &right, right);
	test(equal_array_type(left, right), "equal_array_type3");

	type_object4(NULL, LISPDECL_INTEGER,
			Nil, fixnum_heapr(1),
			Nil, fixnum_heapr(256), &right);
	upgraded_array_type_alloc(NULL, &right, right);
	test(! equal_array_type(left, right), "equal_array_type4");

	type_object4(NULL, LISPDECL_INTEGER,
			Nil, fixnum_heapr(-1000),
			Nil, fixnum_heapr(20), &left);
	upgraded_array_type_alloc(NULL, &left, left);
	type_object4(NULL, LISPDECL_INTEGER,
			Nil, fixnum_heapr(-2000),
			Nil, fixnum_heapr(200), &right);
	upgraded_array_type_alloc(NULL, &right, right);
	test(equal_array_type(left, right), "equal_array_type5");

	RETURN;
}


/*
 *  symbol-type
 */
static int test_emptycheck(const char *name, enum LISPDECL type)
{
	addr pos;

	internchar(LISP_COMMON, name, &pos);
	parse_type_heap(&pos, pos);
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != 0) {
		degrade_printf("array size error\n");
		return 0;
	}

	return 1;
}

static int type_symbol_empty(void)
{
	test(test_emptycheck("NIL", LISPDECL_NIL), "symbol_empty_nil");
	test(test_emptycheck("T", LISPDECL_T), "symbol_empty_t");
	test(test_emptycheck("ATOM", LISPDECL_ATOM), "symbol_empty_atom");
	test(test_emptycheck("LIST", LISPDECL_LIST), "symbol_empty_list");
	test(test_emptycheck("BOOLEAN", LISPDECL_BOOLEAN), "symbol_empty_boolean");
	test(test_emptycheck("NULL", LISPDECL_NULL), "symbol_empty_null");
	test(test_emptycheck("HASH-TABLE", LISPDECL_HASH_TABLE), "symbol_empty_hash_table");
	test(test_emptycheck("SYMBOL", LISPDECL_SYMBOL), "symbol_empty_symbol");
	test(test_emptycheck("KEYWORD", LISPDECL_KEYWORD), "symbol_empty_keyword");
	test(test_emptycheck("PACKAGE", LISPDECL_PACKAGE), "symbol_empty_package");
	test(test_emptycheck("RANDOM-STATE", LISPDECL_RANDOM_STATE),
			"symbol_empty_random_state");
	test(test_emptycheck("READTABLE", LISPDECL_READTABLE), "symbol_empty_readtable");
	test(test_emptycheck("PATHNAME", LISPDECL_PATHNAME), "symbol_empty_pathname");
	test(test_emptycheck("LOGICAL-PATHNAME", LISPDECL_LOGICAL_PATHNAME),
			"symbol_empty_logical_pathname");
	test(test_emptycheck("SEQUENCE", LISPDECL_SEQUENCE), "symbol_empty_sequence");
	test(test_emptycheck("CHARACTER", LISPDECL_CHARACTER), "symbol_empty_character");
	test(test_emptycheck("BASE-CHAR", LISPDECL_BASE_CHAR), "symbol_empty_base_char");
	test(test_emptycheck("EXTENDED-CHAR", LISPDECL_EXTENDED_CHAR),
			"symbol_empty_extended_char");
	test(test_emptycheck("STANDARD-CHAR", LISPDECL_STANDARD_CHAR),
			"symbol_empty_standard_char");
	test(test_emptycheck("NUMBER", LISPDECL_NUMBER), "symbol_empty_number");
	test(test_emptycheck("RATIO", LISPDECL_RATIO), "symbol_empty_ratio");
	test(test_emptycheck("BIT", LISPDECL_BIT), "symbol_empty_bit");
	test(test_emptycheck("FIXNUM", LISPDECL_FIXNUM), "symbol_empty_fixnum");
	test(test_emptycheck("BIGNUM", LISPDECL_BIGNUM), "symbol_empty_bignum");

	RETURN;
}

static int test_aster1check(const char *name, enum LISPDECL type)
{
	addr pos;

	internchar(LISP_COMMON, name, &pos);
	parse_type_heap(&pos, pos);
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != 1) {
		degrade_printf("array size error\n");
		return 0;
	}
	GetArrayType(pos, 0, &pos);
	if (! asterisk_p(pos)) {
		degrade_printf("asterisk error\n");
		return 0;
	}

	return 1;
}

static int type_symbol_aster1(void)
{
	test(test_aster1check("SIMPLE-VECTOR",  LISPDECL_SIMPLE_VECTOR),
			"symbol_aster1_vector");
	test(test_aster1check("BIT-VECTOR",  LISPDECL_BIT_VECTOR),
			"symbol_aster1_bit_vector");
	test(test_aster1check("SIMPLE-BIT-VECTOR",  LISPDECL_SIMPLE_BIT_VECTOR),
			"symbol_aster1_simple_bit_vector");
	test(test_aster1check("STRING",  LISPDECL_STRING),
			"symbol_aster1_string");
	test(test_aster1check("BASE-STRING",  LISPDECL_BASE_STRING),
			"symbol_aster1_base_string");
	test(test_aster1check("SIMPLE-STRING",  LISPDECL_SIMPLE_STRING),
			"symbol_aster1_simple_string");
	test(test_aster1check("SIMPLE-BASE-STRING",  LISPDECL_SIMPLE_BASE_STRING),
			"symbol_aster1_simple_base_string");
	test(test_aster1check("SIGNED-BYTE",  LISPDECL_SIGNED_BYTE),
			"symbol_aster1_signed_byte");
	test(test_aster1check("UNSIGNED-BYTE",  LISPDECL_UNSIGNED_BYTE),
			"symbol_aster1_unsigned_byte");
	test(test_aster1check("COMPLEX",  LISPDECL_COMPLEX),
			"symbol_aster1_complex");

	RETURN;
}

static int test_aster2check(const char *name, enum LISPDECL type)
{
	const int size = 2;
	int i;
	addr pos, check;

	internchar(LISP_COMMON, name, &pos);
	parse_type_heap(&pos, pos);
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != (size_t)size) {
		degrade_printf("array size error\n");
		return 0;
	}
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &check);
		if (! asterisk_p(check)) {
			degrade_printf("asterisk error\n");
			return 0;
		}
	}

	return 1;
}

static int type_symbol_aster2(void)
{
	test(test_aster2check("CONS", LISPDECL_CONS),
			"symbol_aster2_cons");
	test(test_aster2check("ARRAY", LISPDECL_ARRAY),
			"symbol_aster2_array");
	test(test_aster2check("SIMPLE-ARRAY", LISPDECL_SIMPLE_ARRAY),
			"symbol_aster2_simple_array");
	test(test_aster2check("VECTOR", LISPDECL_VECTOR),
			"symbol_aster2_vector");

	RETURN;
}

static int test_aster3check(const char *name, enum LISPDECL type)
{
	const int size = 3;
	int i;
	addr pos, check;

	internchar(LISP_COMMON, name, &pos);
	parse_type_heap(&pos, pos);
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != (size_t)size) {
		degrade_printf("array size error\n");
		return 0;
	}
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &check);
		if (! asterisk_p(check)) {
			degrade_printf("asterisk error\n");
			return 0;
		}
	}

	return 1;
}

static int type_symbol_aster3(void)
{
	test(test_aster3check("FUNCTION", LISPDECL_FUNCTION),
			"symbol_empty_function");
	test(test_aster3check("COMPILED-FUNCTION", LISPDECL_COMPILED_FUNCTION),
			"symbol_empty_compiled_function");

	RETURN;
}

static int test_numbercheck(const char *name, enum LISPDECL type)
{
	const int size = 4;
	int i;
	addr pos, check;

	internchar(LISP_COMMON, name, &pos);
	parse_type_heap(&pos, pos);
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != (size_t)size) {
		degrade_printf("array size error\n");
		return 0;
	}
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &check);
		if (! asterisk_p(check)) {
			degrade_printf("asterisk error\n");
			return 0;
		}
	}

	return 1;
}

static int type_symbol_number(void)
{
	test(test_numbercheck("REAL", LISPDECL_REAL), "symbol_number_real");
	test(test_numbercheck("RATIONAL", LISPDECL_RATIONAL), "symbol_number_rational");
	test(test_numbercheck("INTEGER", LISPDECL_INTEGER), "symbol_number_integer");
	test(test_numbercheck("FLOAT", LISPDECL_FLOAT), "symbol_number_float");
	test(test_numbercheck("SHORT-FLOAT", LISPDECL_SHORT_FLOAT),
			"symbol_number_short_float");
	test(test_numbercheck("SINGLE-FLOAT", LISPDECL_SINGLE_FLOAT),
			"symbol_number_single_float");
	test(test_numbercheck("DOUBLE-FLOAT", LISPDECL_DOUBLE_FLOAT),
			"symbol_number_double_float");
	test(test_numbercheck("LONG-FLOAT", LISPDECL_LONG_FLOAT),
			"symbol_number_long_float");

	RETURN;
}


static int test_build_type_symbol(void)
{
	addr pos, check;
	struct type_symbol *str;

	build_type_symbol();
	pos = Root(LISPINDEX_TYPE_SYMBOL);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "build_type_symbol1");

	interncommon("*", &check);
	findvalue_hashtable(pos, check, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "build_type_symbol2");
	str = &TypeSymbol[(int)RefFixnum(check)];
	test(str->name == CONSTANT_COMMON_ASTERISK, "build_type_symbol3");

	interncommon("INTEGER", &check);
	findvalue_hashtable(pos, check, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "build_type_symbol4");
	str = &TypeSymbol[(int)RefFixnum(check)];
	test(str->name == CONSTANT_COMMON_INTEGER, "build_type_symbol5");

	RETURN;
}


/*
 *  Compound-type
 */
static int test_type_array4(void)
{
	addr symbol, pos, cons;

	build_type_symbol();

	interncommon("AND", &symbol);
	type_array4(NULL, LISPDECL_AND, symbol, Nil, &pos);
	test(test_typecheck(pos, LISPDECL_AND, 1), "type_array4-1");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "type_array4-2");
	test(lenarrayr(pos) == 0, "type_array4-3");

	interncommon("FIXNUM", &pos);
	conscar_heap(&cons, pos);
	interncommon("CONS", &pos);
	cons_heap(&cons, pos, cons);
	interncommon("STRING", &pos);
	cons_heap(&cons, pos, cons);
	type_array4(NULL, LISPDECL_AND, symbol, cons, &pos);
	test(test_typecheck(pos, LISPDECL_AND, 1), "type_array4-4");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "type_array4-5");
	test(lenarrayr(pos) == 3, "type_array4-6");

	GetArrayA4(pos, 2, &cons);
	test(test_typecheck(cons, LISPDECL_FIXNUM, 0), "type_array4-7");
	GetArrayA4(pos, 1, &cons);
	test(test_typecheck(cons, LISPDECL_CONS, 2), "type_array4-8");
	GetArrayA4(pos, 0, &cons);
	test(test_typecheck(cons, LISPDECL_STRING, 1), "type_array4-9");

	RETURN;
}

static int test_type_and(void)
{
	addr pos, pos1, pos2, pos3;

	interncommon("AND", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("STRING", &pos3);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_AND, 1), "type_and1");

	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_AND, 1), "type_and2");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type_and3");
	GetArrayA4(pos, 0, &pos1);
	test(test_typeonly(pos1, LISPDECL_FIXNUM), "type_and4");
	GetArrayA4(pos, 1, &pos1);
	test(test_typeonly(pos1, LISPDECL_STRING), "type_and5");

	RETURN;
}

static int test_type_eql(void)
{
	addr pos;

	interncommon("EQL", &pos);
	list_heap(&pos, pos, T, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_EQL, 1), "type_eql1");
	GetArrayType(pos, 0, &pos);
	test(pos == T, "type_eql2");

	RETURN;
}

static int test_type_member(void)
{
	addr pos, pos1, check;

	interncommon("MEMBER", &pos);
	fixnum_heap(&pos1, 100);
	list_heap(&pos, pos, pos1, T, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_MEMBER, 1), "type_member1");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type_member2");
	GetArrayA4(pos, 0, &check);
	test(check == pos1, "type_member3");
	GetArrayA4(pos, 1, &pos1);
	test(pos1 == T, "type_member4");

	RETURN;
}

static int test_type_mod(void)
{
	addr pos, pos1;

	interncommon("MOD", &pos);
	fixnum_heap(&pos1, 100);
	list_heap(&pos, pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_MOD, 1), "type_mod1");
	GetArrayType(pos, 0, &pos);
	test(pos == pos1, "type_mod2");

	RETURN;
}

static int test_type_not(void)
{
	addr pos, pos1;

	interncommon("NOT", &pos);
	interncommon("BIGNUM", &pos1);
	list_heap(&pos, pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_NOT, 1), "type_not1");
	GetArrayType(pos, 0, &pos);
	test(test_typeonly(pos, LISPDECL_BIGNUM), "type_not2");

	RETURN;
}

static int test_type_or(void)
{
	addr pos, pos1, pos2, pos3;

	interncommon("OR", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("STRING", &pos3);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_OR, 1), "type_or1");

	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_OR, 1), "type_or2");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type_or3");
	GetArrayA4(pos, 0, &pos1);
	test(test_typeonly(pos1, LISPDECL_FIXNUM), "type_or4");
	GetArrayA4(pos, 1, &pos1);
	test(test_typeonly(pos1, LISPDECL_STRING), "type_or6");

	RETURN;
}

static int test_type_satisfies(void)
{
	addr pos, pos1;

	interncommon("SATISFIES", &pos);
	internchar(LISP_PACKAGE, "HELLO", &pos1);
	list_heap(&pos, pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SATISFIES, 1), "type_satisfies1");
	GetArrayType(pos, 0, &pos);
	test(pos == pos1, "type_satisfies2");

	RETURN;
}

static int test_values_var(void)
{
	addr pos, pos1, pos2, pos3, check;

	interncommon("VALUES", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_var1");
	GetArrayType(pos, 0, &pos);
	test(pos == Nil, "type_values_var2");

	internchar(LISP_PACKAGE, "FIXNUM", &pos2);
	internchar(LISP_PACKAGE, "STRING", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_var3");
	GetArrayType(pos, 0, &pos);
	test(pos != Nil, "type_values_var4");
	GetCons(pos, &check, &pos);
	test(test_typeonly(check, LISPDECL_FIXNUM), "type_values_var5");
	GetCons(pos, &check, &pos);
	test(test_typeonly(check, LISPDECL_STRING), "type_values_var6");
	test(pos == Nil, "type_values_var7");

	RETURN;
}

static int test_values_opt(void)
{
	addr pos, pos1, pos2, pos3, pos4, left, right;

	interncommon("VALUES", &pos1);
	internchar(LISP_PACKAGE, "FIXNUM", &pos2);
	interncommon("&OPTIONAL", &pos3);
	internchar(LISP_PACKAGE, "STRING", &pos4);
	list_heap(&pos, pos1, pos2, pos3, pos4, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_opt1");
	GetArrayType(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_values_opt2");
	test(right == Nil, "type_values_opt3");
	GetArrayType(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_values_opt4");
	test(right == Nil, "type_values_opt5");

	interncommon("&OPTIONAL", &pos2);
	internchar(LISP_PACKAGE, "FIXNUM", &pos3);
	internchar(LISP_PACKAGE, "STRING", &pos4);
	list_heap(&pos, pos1, pos2, pos3, pos4, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_opt6");
	GetArrayType(pos, 0, &right);
	test(right == Nil, "type_values_opt7");
	GetArrayType(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_values_opt8");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_values_opt9");
	test(right == Nil, "type_values_opt10");

	RETURN;
}

static int test_values_rest(void)
{
	addr pos, pos1, pos2, pos3, pos4, left, right;

	interncommon("VALUES", &pos1);
	internchar(LISP_PACKAGE, "FIXNUM", &pos2);
	interncommon("&REST", &pos3);
	internchar(LISP_PACKAGE, "LIST", &pos4);
	list_heap(&pos, pos1, pos2, pos3, pos4, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_rest1");
	GetArrayType(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_values_rest2");
	test(right == Nil, "type_values_rest3");
	GetArrayType(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_LIST), "type_values_rest4");

	interncommon("VALUES", &pos1);
	interncommon("&REST", &pos2);
	internchar(LISP_PACKAGE, "LIST", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_rest5");
	GetArrayType(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_LIST), "type_values_rest6");

	RETURN;
}

static int test_type_values(void)
{
	addr pos, pos1, pos2, left, right;

	interncommon("VALUES", &pos1);
	internchar(LISP_PACKAGE, "FIXNUM", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_values_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_type_values1");
	GetArrayType(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_type_values2");
	test(right == Nil, "type_type_values3");
	GetArrayType(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_T), "type_type_values4");

	RETURN;
}


/*
 *  Atomic-type
 */
static int test_type_cons(void)
{
	addr pos, pos1, pos2, pos3, check;

	interncommon("CONS", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_cons1");
	test(lenarrayr(pos) == 2, "type_cons2");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons3");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons4");

	interncommon("CONS", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_cons5");
	test(lenarrayr(pos) == 2, "type_cons6");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons7");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons8");

	interncommon("CONS", &pos1);
	interncommon("*", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_cons9");
	test(lenarrayr(pos) == 2, "type_cons10");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons11");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons12");

	interncommon("CONS", &pos1);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_cons13");
	test(lenarrayr(pos) == 2, "type_cons14");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "type_cons15");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons16");

	interncommon("CONS", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_cons17");
	test(lenarrayr(pos) == 2, "type_cons18");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "type_cons19");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_cons20");

	interncommon("CONS", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("INTEGER", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type_cons21");
	test(lenarrayr(pos) == 2, "type_cons22");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "type_cons23");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_INTEGER), "type_cons24");

	RETURN;
}

static int test_type_function_lambda_var(void)
{
	addr pos, pos1, pos2, left, right;

	type_function_lambda(NULL, &pos, Nil);
	GetArrayA2(pos, 0, &pos);
	test(pos == Nil, "type_function_lambda_var1");

	interncommon("FIXNUM", &pos1);
	interncommon("STRING", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_var2");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_lambda_var3");
	test(right == Nil, "type_function_lambda_var4");

	RETURN;
}

static int test_type_function_lambda_opt(void)
{
	addr pos, pos1, pos2, pos3, left, right;

	interncommon("&OPTIONAL", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("STRING", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 0, &right);
	test(right == Nil, "type_function_lambda_opt1");
	GetArrayA2(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_opt2");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_lambda_opt3");
	test(right == Nil, "type_function_lambda_opt4");

	interncommon("FIXNUM", &pos1);
	interncommon("&OPTIONAL", &pos2);
	interncommon("STRING", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_opt5");
	test(right == Nil, "type_function_lambda_opt6");
	GetArrayA2(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_lambda_opt7");
	test(right == Nil, "type_function_lambda_opt8");

	RETURN;
}

static int test_type_function_lambda_rest(void)
{
	addr pos, pos1, pos2, pos3, left, right;

	interncommon("FIXNUM", &pos1);
	interncommon("&REST", &pos2);
	interncommon("STRING", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_rest1");
	test(right == Nil, "type_function_lambda_rest2");
	GetArrayA2(pos, 1, &right);
	test(right == Nil, "type_function_lambda_rest3");
	GetArrayA2(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_STRING), "type_function_lambda_rest4");

	interncommon("&REST", &pos1);
	interncommon("NIL", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_NIL), "type_function_lambda_rest5");

	RETURN;
}

static int test_type_function_lambda_key(void)
{
	addr pos, pos1, pos2, key, left, right;

	interncommon("&KEY", &pos1);
	internchar(LISP_PACKAGE, "HELLO", &key);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos2, key, pos2, NULL);
	list_heap(&pos, pos1, pos2, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 3, &right);
	GetCons(right, &left, &right);
	test(right == Nil, "type_function_lambda_key1");
	GetCons(left, &left, &right);
	test(left == key, "type_function_lambda_key2");
	test(test_typeonly(right, LISPDECL_FIXNUM), "type_function_lambda_key3");

	RETURN;
}

static int test_type_function_cons(void)
{
	addr pos, pos1, pos2, left, right;

	interncommon("*", &pos);
	type_function_cons(NULL, &pos, pos);
	test(test_typeonly(pos, LISPDECL_ASTERISK), "type_function_cons1");

	interncommon("FIXNUM", &pos1);
	interncommon("STRING", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	type_function_lambda(NULL, &pos, pos);
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_cons2");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_cons3");
	test(right == Nil, "type_function_cons4");

	RETURN;
}

static int test_type_function(void)
{
	addr pos, pos1, pos2, pos3, check, left, right;

	interncommon("FUNCTION", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function1");
	test(lenarrayr(pos) == 3, "type_function2");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function3");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function4");

	interncommon("FUNCTION", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function5");
	test(lenarrayr(pos) == 3, "type_function6");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function7");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function8");

	interncommon("FUNCTION", &pos1);
	interncommon("*", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function9");
	test(lenarrayr(pos) == 3, "type_function10");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function11");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function12");

	interncommon("FUNCTION", &pos1);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos2, pos2, NULL);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function13");
	test(lenarrayr(pos) == 3, "type_function14");
	GetArrayType(pos, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_function15");
	getarray(check, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function16");
	test(right == Nil, "type_function17");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function18");

	interncommon("FUNCTION", &pos1);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos2, pos2, NULL);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function19");
	test(lenarrayr(pos) == 3, "type_function20");
	GetArrayType(pos, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_function21");
	getarray(check, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function22");
	test(right == Nil, "type_function23");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "type_function24");

	interncommon("FUNCTION", &pos1);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos2, pos2, NULL);
	interncommon("VALUES", &pos3);
	list_heap(&pos3, pos3, NULL);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function25");
	test(lenarrayr(pos) == 3, "type_function26");
	GetArrayType(pos, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_function27");
	getarray(check, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function28");
	test(right == Nil, "type_function29");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_VALUES), "type_function30");

	RETURN;
}

static int test_asterisk_length(void)
{
	addr cons, pos, aster;
	size_t size;

	interncommon("*", &aster);
	fixnum_heap(&pos, 10);

	list_heap(&cons, aster, NULL);
	test(asterisk_length(cons, &size), "asterisk_length1");
	test(size == 1, "asterisk_length2");

	list_heap(&cons, pos, NULL);
	test(! asterisk_length(cons, &size), "asterisk_length3");

	list_heap(&cons, aster, aster, aster, aster, NULL);
	test(asterisk_length(cons, &size), "asterisk_length4");
	test(size == 4, "asterisk_length5");

	list_heap(&cons, aster, aster, pos, aster, aster, NULL);
	test(! asterisk_length(cons, &size), "asterisk_length6");

	RETURN;
}

static int test_fixnum_check(void)
{
	addr pos;

	fixnum_heap(&pos, 0);
	fixnum_check(pos);
	test(1, "fixnum_check1");

	fixnum_heap(&pos, 1);
	fixnum_check(pos);
	test(1, "fixnum_check2");

	fixnum_heap(&pos, 100);
	fixnum_check(pos);
	test(1, "fixnum_check3");

	RETURN;
}

static int test_dimension_array(void)
{
	addr aster, pos, check;

	interncommon("*", &aster);
	list_heap(&pos, aster, fixnum_heapr(10), fixnum_heapr(20), NULL);
	dimension_array(NULL, &pos, pos);
	getarray(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "dimension_array1");
	getarray(pos, 1, &check);
	test(RefFixnum(check) == 10, "dimension_array2");
	getarray(pos, 2, &check);
	test(RefFixnum(check) == 20, "dimension_array3");
	test(lenarrayr(pos) == 3, "dimension_array4");

	RETURN;
}

static int test_parse_typearray(void)
{
	addr pos;

	parse_typearray(NULL, &pos, Nil);
	test(RefFixnum(pos) == 0, "parse_typearray1");

	fixnum_heap(&pos, 3);
	parse_typearray(NULL, &pos, pos);
	test(RefFixnum(pos) == 3, "parse_typearray2");

	interncommon("*", &pos);
	list_heap(&pos, pos, pos, pos, pos, pos, NULL);
	parse_typearray(NULL, &pos, pos);
	test(RefFixnum(pos) == 5, "parse_typearray3");

	fixnum_heap(&pos, 11);
	list_heap(&pos, pos, pos, NULL);
	parse_typearray(NULL, &pos, pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "parse_typearray4");
	test(lenarrayr(pos) == 2, "parse_typearray5");
	getarray(pos, 0, &pos);
	test(RefFixnum(pos) == 11, "parse_typearray6");

	RETURN;
}

static int test_type_array(void)
{
	addr pos, pos1, pos2, pos3, left;

	interncommon("ARRAY", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array1");
	test(lenarrayr(pos) == 2, "type_array2");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array3");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array4");

	interncommon("ARRAY", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array5");
	test(lenarrayr(pos) == 2, "type_array6");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array7");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array8");

	interncommon("ARRAY", &pos1);
	interncommon("*", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array9");
	test(lenarrayr(pos) == 2, "type_array10");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array11");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array12");

	interncommon("ARRAY", &pos1);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array13");
	test(lenarrayr(pos) == 2, "type_array14");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_SIGNED_BYTE), "type_array15");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array16");

	interncommon("ARRAY", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array17");
	test(lenarrayr(pos) == 2, "type_array18");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_SIGNED_BYTE), "type_array19");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array20");

	interncommon("ARRAY", &pos1);
	interncommon("FIXNUM", &pos2);
	fixnum_heap(&pos3, 44);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array21");
	test(lenarrayr(pos) == 2, "type_array22");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_SIGNED_BYTE), "type_array23");
	GetArrayType(pos, 1, &left);
	test(RefFixnum(left) == 44, "type_array24");

	interncommon("ARRAY", &pos1);
	interncommon("*", &pos2);
	fixnum_heap(&pos3, 44);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array25");
	test(lenarrayr(pos) == 2, "type_array26");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_array27");
	GetArrayType(pos, 1, &left);
	test(RefFixnum(left) == 44, "type_array28");

	interncommon("ARRAY", &pos1);
	interncommon("T", &pos2);
	fixnum_heap(&pos3, 44);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "type_array29");
	test(lenarrayr(pos) == 2, "type_array30");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_T), "type_array31");
	GetArrayType(pos, 1, &left);
	test(RefFixnum(left) == 44, "type_array32");

	RETURN;
}

static int test_type_simple_array(void)
{
	addr pos, pos1, left;

	interncommon("SIMPLE-ARRAY", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SIMPLE_ARRAY, 2), "type_simple_array1");
	test(lenarrayr(pos) == 2, "type_simple_array2");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_simple_array3");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_simple_array4");

	RETURN;
}

static int test_type_vector(void)
{
	addr pos, pos1, pos2, pos3, left;

	interncommon("VECTOR", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector1");
	test(lenarrayr(pos) == 2, "type_vector2");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector3");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector4");

	interncommon("VECTOR", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector5");
	test(lenarrayr(pos) == 2, "type_vector6");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector7");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector8");

	interncommon("VECTOR", &pos1);
	interncommon("*", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector9");
	test(lenarrayr(pos) == 2, "type_vector10");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector11");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector12");

	interncommon("VECTOR", &pos1);
	interncommon("FIXNUM", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector13");
	test(lenarrayr(pos) == 2, "type_vector14");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_SIGNED_BYTE), "type_vector15");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector16");

	interncommon("VECTOR", &pos1);
	interncommon("FIXNUM", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector17");
	test(lenarrayr(pos) == 2, "type_vector18");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_SIGNED_BYTE), "type_vector19");
	GetArrayType(pos, 1, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector20");

	interncommon("VECTOR", &pos1);
	interncommon("FIXNUM", &pos2);
	fixnum_heap(&pos3, 44);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector21");
	test(lenarrayr(pos) == 2, "type_vector22");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_SIGNED_BYTE), "type_vector23");
	GetArrayType(pos, 1, &left);
	test(RefFixnum(left) == 44, "type_vector24");

	interncommon("VECTOR", &pos1);
	interncommon("*", &pos2);
	fixnum_heap(&pos3, 44);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector25");
	test(lenarrayr(pos) == 2, "type_vector26");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_vector27");
	GetArrayType(pos, 1, &left);
	test(RefFixnum(left) == 44, "type_vector28");

	interncommon("VECTOR", &pos1);
	interncommon("T", &pos2);
	fixnum_heap(&pos3, 44);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "type_vector29");
	test(lenarrayr(pos) == 2, "type_vector30");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_T), "type_vector31");
	GetArrayType(pos, 1, &left);
	test(RefFixnum(left) == 44, "type_vector32");

	RETURN;
}

static int test_type_simple_vector(void)
{
	addr pos, pos1, pos2, left;

	interncommon("SIMPLE-VECTOR", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SIMPLE_VECTOR, 1), "type_simple_vector1");
	test(lenarrayr(pos) == 1, "type_simple_vector2");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_simple_vector3");

	interncommon("SIMPLE-VECTOR", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SIMPLE_VECTOR, 1), "type_simple_vector4");
	test(lenarrayr(pos) == 1, "type_simple_vector5");
	GetArrayType(pos, 0, &left);
	test(test_typeonly(left, LISPDECL_ASTERISK), "type_simple_vector6");

	interncommon("SIMPLE-VECTOR", &pos1);
	fixnum_heap(&pos2, 0);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SIMPLE_VECTOR, 1), "type_simple_vector7");
	test(lenarrayr(pos) == 1, "type_simple_vector8");
	GetArrayType(pos, 0, &left);
	test(RefFixnum(left) == 0, "type_simple_vector9");

	RETURN;
}

static int test_size_check_args(const char *name, enum LISPDECL type)
{
	addr pos, pos1, pos2, left;

	interncommon(name, &pos1);
	fixnum_heap(&pos2, 10);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	if (! test_typecheck(pos, type, 1)) {
		degrade_printf("typecheck error.\n");
		return 0;
	}
	if (lenarrayr(pos) != 1) {
		degrade_printf("lenarray error.\n");
		return 0;
	}
	GetArrayType(pos, 0, &left);
	if (RefFixnum(left) != 10) {
		degrade_printf("fixnum error.\n");
		return 0;
	}

	return 1;
}

#define sizecheck(a,b,c) test(test_size_check_args(a, b), "type_size_" c)
static int test_type_size_check(void)
{
	sizecheck("SIMPLE-VECTOR", LISPDECL_SIMPLE_VECTOR, "simple_vector");
	sizecheck("BIT-VECTOR", LISPDECL_BIT_VECTOR, "bit_vector");
	sizecheck("SIMPLE-BIT-VECTOR", LISPDECL_SIMPLE_BIT_VECTOR, "simple_bit_vector");
	sizecheck("STRING", LISPDECL_STRING, "string");
	sizecheck("BASE-STRING", LISPDECL_BASE_STRING, "base_string");
	sizecheck("SIMPLE-STRING", LISPDECL_SIMPLE_STRING, "simple_string");
	sizecheck("SIMPLE-BASE-STRING", LISPDECL_SIMPLE_BASE_STRING, "simple_base_string");

	RETURN;
}
#undef sizecheck

static int test_range_element(void)
{
	addr pos, pos1, pos2;

	fixnum_heap(&pos, 100);
	range_element(realp_type, &pos1, &pos2, pos);
	test(pos1 == Nil, "range_element1");
	test(pos2 == pos, "range_element2");

	list_heap(&pos1, pos, NULL);
	range_element(realp_type, &pos1, &pos2, pos1);
	test(pos1 == T, "range_element3");
	test(pos2 == pos, "range_element4");

	RETURN;
}

static int test_type_range(void)
{
	addr pos, pos1, pos2, pos3;

	interncommon("REAL", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_REAL, 4), "type_range1");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_range2");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_range3");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_range4");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_range5");

	interncommon("RATIONAL", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_RATIONAL, 4), "type_range6");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_range7");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_range8");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_range9");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_range10");

	interncommon("INTEGER", &pos1);
	interncommon("*", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "type_range11");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_range12");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_range13");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_range14");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_range15");

	interncommon("REAL", &pos1);
	fixnum_heap(&pos2, 100);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_REAL, 4), "type_range16");
	test(RefArrayType(pos, 0) == Nil, "type_range17");
	test(RefFixnum(RefArrayType(pos, 1)) == 100, "type_range18");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_range19");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_range20");

	interncommon("REAL", &pos1);
	fixnum_heap(&pos2, 100);
	list_heap(&pos2, pos2, NULL);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_REAL, 4), "type_range21");
	test(RefArrayType(pos, 0) == T, "type_range22");
	test(RefFixnum(RefArrayType(pos, 1)) == 100, "type_range23");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_range24");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_range25");

	interncommon("REAL", &pos1);
	fixnum_heap(&pos2, 100);
	list_heap(&pos2, pos2, NULL);
	fixnum_heap(&pos3, 200);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_REAL, 4), "type_range26");
	test(RefArrayType(pos, 0) == T, "type_range27");
	test(RefFixnum(RefArrayType(pos, 1)) == 100, "type_range28");
	test(RefArrayType(pos, 2) == Nil, "type_range29");
	test(RefFixnum(RefArrayType(pos, 3)) == 200, "type_range30");

	interncommon("REAL", &pos1);
	interncommon("*", &pos2);
	fixnum_heap(&pos3, 200);
	list_heap(&pos3, pos3, NULL);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_REAL, 4), "type_range31");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_range32");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_range33");
	test(RefArrayType(pos, 2) == T, "type_range34");
	test(RefFixnum(RefArrayType(pos, 3)) == 200, "type_range35");

	RETURN;
}

static int test_type_byte(void)
{
	addr pos, pos1, pos2;

	interncommon("SIGNED-BYTE", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SIGNED_BYTE, 1), "type_byte1");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_byte2");

	interncommon("UNSIGNED-BYTE", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "type_byte3");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_byte4");

	interncommon("UNSIGNED-BYTE", &pos1);
	fixnum_heap(&pos2, 111);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "type_byte5");
	test(RefFixnum(RefArrayType(pos, 0)) == 111, "type_byte6");

	RETURN;
}

static int test_type_complex(void)
{
	addr pos, pos1, pos2;

	interncommon("COMPLEX", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_COMPLEX, 1), "type_complex1");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_complex2");

	interncommon("COMPLEX", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_COMPLEX, 1), "type_complex3");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_complex4");

	interncommon("COMPLEX", &pos1);
	interncommon("INTEGER", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_COMPLEX, 1), "type_complex5");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_INTEGER), "type_complex6");

	RETURN;
}

static int test_type_float(void)
{
	addr pos, pos1, pos2, pos3;

	interncommon("FLOAT", &pos1);
	list_heap(&pos, pos1, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_FLOAT, 4), "type_float1");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_float2");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_float3");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_float4");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_float5");

	interncommon("SHORT-FLOAT", &pos1);
	interncommon("*", &pos2);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SHORT_FLOAT, 4), "type_float6");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_float7");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_float8");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_float9");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_float10");

	interncommon("SINGLE-FLOAT", &pos1);
	interncommon("*", &pos2);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_SINGLE_FLOAT, 4), "type_float11");
	test(test_typeonly(RefArrayType(pos, 0), LISPDECL_ASTERISK), "type_float12");
	test(test_typeonly(RefArrayType(pos, 1), LISPDECL_ASTERISK), "type_float13");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_float14");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_float15");

	interncommon("DOUBLE-FLOAT", &pos1);
	double_float_heap(&pos2, 100.0);
	list_heap(&pos, pos1, pos2, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_DOUBLE_FLOAT, 4), "type_float16");
	test(RefArrayType(pos, 0) == Nil, "type_float17");
	test(RefDoubleFloat(RefArrayType(pos, 1)) == 100.0, "type_float18");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_float19");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_float20");

	interncommon("LONG-FLOAT", &pos1);
	long_float_heap(&pos2, 100.0L);
	list_heap(&pos2, pos2, NULL);
	interncommon("*", &pos3);
	list_heap(&pos, pos1, pos2, pos3, NULL);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_LONG_FLOAT, 4), "type_float21");
	test(RefArrayType(pos, 0) == T, "type_float22");
	test(RefLongFloat(RefArrayType(pos, 1)) == 100.0L, "type_float23");
	test(test_typeonly(RefArrayType(pos, 2), LISPDECL_ASTERISK), "type_float24");
	test(test_typeonly(RefArrayType(pos, 3), LISPDECL_ASTERISK), "type_float25");

	RETURN;
}

static int test_parse_type(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	interncommon("NULL", &pos);
	parse_type_alloc(local, &pos, pos);
	test(test_typecheck(pos, LISPDECL_NULL, 0), "parse_type1");
	test(GetStatusDynamic(pos), "parse_type2");

	interncommon("INTEGER", &pos);
	list_local(local, &pos, pos, NULL);
	parse_type_local(local, &pos, pos);
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "parse_type3");
	test(GetStatusDynamic(pos), "parse_type4");

	interncommon("NULL", &pos);
	parse_type_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_NULL, 0), "parse_type5");
	test(! GetStatusDynamic(pos), "parse_type6");

	interncommon("NULL", &pos);
	parse_type_not_alloc(local, &pos, pos);
	test(test_typecheck_not(pos, LISPDECL_NULL, 0), "parse_type7");
	test(GetStatusDynamic(pos), "parse_type8");

	interncommon("INTEGER", &pos);
	list_local(local, &pos, pos, NULL);
	parse_type_not_local(local, &pos, pos);
	test(test_typecheck_not(pos, LISPDECL_INTEGER, 4), "parse_type9");
	test(GetStatusDynamic(pos), "parse_type10");

	interncommon("NULL", &pos);
	parse_type_not_heap(&pos, pos);
	test(test_typecheck_not(pos, LISPDECL_NULL, 0), "parse_type11");
	test(! GetStatusDynamic(pos), "parse_type12");

	interncommon("NULL", &pos);
	parse_type_no_asterisk_alloc(local, &pos, pos);
	test(test_typecheck(pos, LISPDECL_NULL, 0), "parse_type13");
	test(GetStatusDynamic(pos), "parse_type14");

	interncommon("INTEGER", &pos);
	list_local(local, &pos, pos, NULL);
	parse_type_no_asterisk_local(local, &pos, pos);
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "parse_type15");
	test(GetStatusDynamic(pos), "parse_type16");

	interncommon("NULL", &pos);
	parse_type_no_asterisk_heap(&pos, pos);
	test(test_typecheck(pos, LISPDECL_NULL, 0), "parse_type17");
	test(! GetStatusDynamic(pos), "parse_type18");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_throw_heap(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	type_aster1(NULL, LISPDECL_CONS, &left);
	type_throw_heap(&right, left);
	test(left == right, "type_throw_heap1");

	interncommon("SYMBOL", &left);
	type_throw_heap(&right, left);
	test(test_typeonly(right, LISPDECL_SYMBOL), "type_throw_heap2");

	interncommon("NIL", &left);
	type_throw_heap(&right, left);
	test(test_typeonly(right, LISPDECL_NIL), "type_throw_heap3");

	interncommon("T", &left);
	type_throw_heap(&right, left);
	test(test_typeonly(right, LISPDECL_T), "type_throw_heap4");

	interncommon("INTEGER", &left);
	conscar_heap(&left, left);
	type_throw_heap(&right, left);
	test(test_typeonly(right, LISPDECL_INTEGER), "type_throw_heap5");

	local = Local_Thread;
	push_local(local, &stack);

	interncommon("T", &left);
	type_throw_alloc(local, &right, left);
	test(test_typeonly(right, LISPDECL_T), "type_throw_heap6");
	test(GetStatusDynamic(right), "type_throw_heap7");

	type_throw_local(local, &right, left);
	test(test_typeonly(right, LISPDECL_T), "type_throw_heap8");
	test(GetStatusDynamic(right), "type_throw_heap9");

	type_throw_heap(&right, left);
	test(test_typeonly(right, LISPDECL_T), "type_throw_heap10");
	test(! GetStatusDynamic(right), "type_throw_heap11");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  check
 */
static int test_type_symbol_p(void)
{
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	test(! type_symbol_p(pos), "type_symbol_p1");
	interncommon("SYMBOL", &pos);
	test(type_symbol_p(pos), "type_symbol_p2");
	interncommon("STANDARD-CLASS", &pos);
	test(type_symbol_p(pos), "type_symbol_p3");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_parse(void)
{
	TestBreak(test_type_heap);
	TestBreak(test_copy_no_recursive_type_heap);
	TestBreak(test_copy_no_recursive_typeonly_heap);
	TestBreak(test_type_object1);
	TestBreak(test_type_object2);
	TestBreak(test_type_object3);
	TestBreak(test_type_object4);
	TestBreak(test_type_object1_not);
	TestBreak(test_type_object2_not);
	TestBreak(test_type_object3_not);
	TestBreak(test_type_object4_not);
	TestBreak(test_type_empty);
	TestBreak(test_type_empty_not);
	TestBreak(test_type_asterisk_heap);
	TestBreak(test_asterisk_p);
	TestBreak(test_type_nil_heap);
	TestBreak(test_type_t_heap);
	TestBreak(test_type_bool_heap);
	TestBreak(test_type_realvalue);
	TestBreak(test_type_aster1);
	TestBreak(test_type_aster2);
	TestBreak(test_type_aster3);
	TestBreak(test_type_aster4);
	TestBreak(test_function_asterisk);
	TestBreak(test_type_and_call);
	TestBreak(test_type_or_call);
	TestBreak(test_setnotdecl_value);
	TestBreak(test_setnotdecl_object);
	TestBreak(test_reversenotdecl);
	TestBreak(test_float_value_p);
	TestBreak(test_range_value_p);
	TestBreak(test_range_type_p);
	TestBreak(test_subtype_real_p);
	/* upgraded_array_element_type */
	TestBreak(test_type_signed_byte);
	TestBreak(test_type_unsigned_byte);
	TestBreak(test_upgraded_array_unsigned);
	TestBreak(test_upgraded_array_signed);
	TestBreak(test_upgraded_array_integer);
	TestBreak(test_upgraded_array_inplace);
	TestBreak(test_upgraded_array_direct);
	TestBreak(test_upgraded_array_type_alloc);
	TestBreak(test_type_object2_array);
	TestBreak(test_equal_array_type);
	/* symbol-type */
	TestBreak(type_symbol_empty);
	TestBreak(type_symbol_aster1);
	TestBreak(type_symbol_aster2);
	TestBreak(type_symbol_aster3);
	TestBreak(type_symbol_number);
	TestBreak(test_build_type_symbol);
	/* Compound-type */
	TestBreak(test_type_array4);
	TestBreak(test_type_and);
	TestBreak(test_type_eql);
	TestBreak(test_type_member);
	TestBreak(test_type_mod);
	TestBreak(test_type_not);
	TestBreak(test_type_or);
	TestBreak(test_type_satisfies);
	TestBreak(test_values_var);
	TestBreak(test_values_opt);
	TestBreak(test_values_rest);
	TestBreak(test_type_values);
	/* Atomic-type */
	TestBreak(test_type_cons);
	TestBreak(test_type_function_lambda_var);
	TestBreak(test_type_function_lambda_opt);
	TestBreak(test_type_function_lambda_rest);
	TestBreak(test_type_function_lambda_key);
	TestBreak(test_type_function_cons);
	TestBreak(test_type_function);
	TestBreak(test_asterisk_length);
	TestBreak(test_fixnum_check);
	TestBreak(test_dimension_array);
	TestBreak(test_parse_typearray);
	TestBreak(test_type_array);
	TestBreak(test_type_simple_array);
	TestBreak(test_type_vector);
	TestBreak(test_type_simple_vector);
	TestBreak(test_type_size_check);
	TestBreak(test_range_element);
	TestBreak(test_type_range);
	TestBreak(test_type_byte);
	TestBreak(test_type_complex);
	TestBreak(test_type_float);
	TestBreak(test_parse_type);
	TestBreak(test_type_throw_heap);
	/* check */
	TestBreak(test_type_symbol_p);

	return 0;
}

int test_type_parse(void)
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
		build_clos(ptr);
		build_condition(ptr);
		build_calltype();
		build_common();
		build_clos(ptr);
		build_type();
		lisp_init = 1;
		result = testbreak_type_parse();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

