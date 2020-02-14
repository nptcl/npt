#include "type.c"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "cons.h"
#include "condition.h"
#include "degrade.h"
#include "package.h"
#include "type_table.h"

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


/*
 *  allocate
 */
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

static int test_getlispdecl(void)
{
	enum LISPDECL type;
	addr pos;

	type_heap(&pos, LISPDECL_CONS, 10);
	GetLispDecl(pos, &type);
	test(type == LISPDECL_CONS, "getlispdecl1");
	test(GetUser(pos) == LISPDECL_CONS, "getlispdecl2");
	SetLispDecl(pos, LISPDECL_SYMBOL);
	GetLispDecl(pos, &type);
	test(type == LISPDECL_SYMBOL, "getlispdecl3");
	test(GetUser(pos) == LISPDECL_SYMBOL, "getlispdecl4");
	test(RefLispDecl(pos) == LISPDECL_SYMBOL, "getlispdecl5");

	RETURN;
}

static int test_getnotdecl(void)
{
	int check;
	addr pos;

	type_heap(&pos, LISPDECL_KEYWORD, 5);
	GetNotDecl(pos, &check);
	test(! check, "getnotdecl1");
	test(! RefNotDecl(pos), "getnotdecl2");
	SetNotDecl(pos, 0);
	GetNotDecl(pos, &check);
	test(! check, "getnotdecl3");
	test(! RefNotDecl(pos), "getnotdecl4");
	SetNotDecl(pos, 100);
	GetNotDecl(pos, &check);
	test(check, "getnotdecl5");
	test(RefNotDecl(pos), "getnotdecl6");
	test(RefLispDecl(pos) == LISPDECL_KEYWORD, "getnotdecl7");
	SetNotDecl(pos, 0);
	GetNotDecl(pos, &check);
	test(! check, "getnotdecl8");
	test(! RefNotDecl(pos), "getnotdecl9");
	test(RefLispDecl(pos) == LISPDECL_KEYWORD, "getnotdecl10");

	SetNotDecl(pos, 1);
	SetLispDecl(pos, LISPDECL_SYMBOL);
	test(! RefNotDecl(pos), "getnotdecl11");

	RETURN;
}

static int test_type_setnotdecl(void)
{
	addr pos;

	type_heap(&pos, LISPDECL_CONS, 2);
	type_setnotdecl(pos, 0);
	test(GetUser(pos) == LISPDECL_CONS, "type_setnotdecl1");
	test(! RefNotDecl(pos), "type_setnotdecl2");
	type_setnotdecl(pos, 1);
	test(GetUser(pos) != LISPDECL_CONS, "type_setnotdecl3");
	test(RefLispDecl(pos) == LISPDECL_CONS, "type_setnotdecl4");
	test(RefNotDecl(pos), "type_setnotdecl5");

	RETURN;
}

static int test_type_revnotdecl(void)
{
	addr pos;

	type_heap(&pos, LISPDECL_CONS, 2);
	type_revnotdecl(pos);
	test(RefNotDecl(pos), "type_revnotdecl1");
	test(RefLispDecl(pos) == LISPDECL_CONS, "type_revnotdecl2");
	type_revnotdecl(pos);
	test(! RefNotDecl(pos), "type_revnotdecl3");
	test(RefLispDecl(pos) == LISPDECL_CONS, "type_revnotdecl4");

	RETURN;
}

static int test_type_setnotobject(void)
{
	addr left, right;

	type_heap(&left, LISPDECL_CONS, 1);
	type_heap(&right, LISPDECL_CONS, 2);
	SetNotDecl(right, 1);
	type_setnotobject(left, right);
	test(RefNotDecl(left), "type_setnotobject1");
	type_setnotdecl(right, 0);
	type_setnotobject(left, right);
	test(! RefNotDecl(left), "type_setnotobject2");

	RETURN;
}

static int test_type_getarraytype(void)
{
	addr pos, check;

	type_heap(&pos, LISPDECL_ARRAY, 10);
	GetArrayType(pos, 3, &check);
	test(check == Nil, "getarraytype1");
	test(RefArrayType(pos, 3) == Nil, "getarraytype2");
	SetArrayType(pos, 3, T);
	GetArrayType(pos, 3, &check);
	test(check == T, "getarraytype3");
	test(RefArrayType(pos, 3) == T, "getarraytype4");

	RETURN;
}

static int test_type_getvalues1(void)
{
	addr pos, aster, type;

	/* (values) */
	type_heap(&aster, LISPDECL_T, 0);
	type_values_heap(Nil, Nil, aster, Nil, &pos);
	type_getvalues1(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type_getvalues1-1");

	/* (values &rest integer) */
	type_heap(&pos, LISPDECL_INTEGER, 0);
	type_values_heap(Nil, Nil, pos, Nil, &pos);
	type_getvalues1(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type_getvalues1-2");

	/* (values integer) */
	type_heap(&pos, LISPDECL_INTEGER, 0);
	list_heap(&pos, pos, NULL);
	type_values_heap(pos, Nil, aster, Nil, &pos);
	type_getvalues1(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type_getvalues1-3");

	/* (values &optional integer) */
	type_heap(&pos, LISPDECL_INTEGER, 0);
	list_heap(&pos, pos, NULL);
	type_values_heap(Nil, pos, aster, Nil, &pos);
	type_getvalues1(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type_getvalues1-4");

	/* (values null integer) */
	type_heap(&pos, LISPDECL_NULL, 0);
	type_heap(&type, LISPDECL_INTEGER, 0);
	list_heap(&pos, pos, type, NULL);
	type_values_heap(pos, Nil, aster, Nil, &pos);
	type_getvalues1(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_NULL, "type_getvalues1-5");

	RETURN;
}


/*
 *  check
 */
static int test_decl_character_p(void)
{
	test(decl_character_p(LISPDECL_CHARACTER), "decl_character_p1");
	test(decl_character_p(LISPDECL_EXTENDED_CHAR), "decl_character_p2");
	test(! decl_character_p(LISPDECL_CONS), "decl_character_p3");

	RETURN;
}

static int test_decl_float_p(void)
{
	test(decl_float_p(LISPDECL_FLOAT), "decl_float_p1");
	test(decl_float_p(LISPDECL_DOUBLE_FLOAT), "decl_float_p2");
	test(! decl_float_p(LISPDECL_CONS), "decl_float_p3");

	RETURN;
}

static int test_decl_range_p(void)
{
	test(decl_range_p(LISPDECL_INTEGER), "decl_range_p1");
	test(decl_range_p(LISPDECL_REAL), "decl_range_p2");
	test(decl_range_p(LISPDECL_SHORT_FLOAT), "decl_range_p3");
	test(! decl_range_p(LISPDECL_SYMBOL), "decl_range_p4");

	RETURN;
}

static int test_decl_subtypep_real(void)
{
	int check;

	check = decl_subtypep_real(LISPDECL_INTEGER, LISPDECL_INTEGER);
	test(check, "decl_subtypep_real1");
	check = decl_subtypep_real(LISPDECL_RATIONAL, LISPDECL_INTEGER);
	test(! check, "decl_subtypep_real2");
	check = decl_subtypep_real(LISPDECL_INTEGER, LISPDECL_RATIONAL);
	test(check, "decl_subtypep_real3");
	check = decl_subtypep_real(LISPDECL_FLOAT, LISPDECL_REAL);
	test(check, "decl_subtypep_real4");
	check = decl_subtypep_real(LISPDECL_FLOAT, LISPDECL_FLOAT);
	test(check, "decl_subtypep_real5");
	check = decl_subtypep_real(LISPDECL_INTEGER, LISPDECL_FLOAT);
	test(! check, "decl_subtypep_real6");
	check = decl_subtypep_real(LISPDECL_LONG_FLOAT, LISPDECL_FLOAT);
	test(check, "decl_subtypep_real7");
	check = decl_subtypep_real(LISPDECL_LONG_FLOAT, LISPDECL_LONG_FLOAT);
	test(check, "decl_subtypep_real8");
	check = decl_subtypep_real(LISPDECL_LONG_FLOAT, LISPDECL_SYMBOL);
	test(! check, "decl_subtypep_real9");

	RETURN;
}

static int test_type_function_p(void)
{
	addr pos;

	type_heap(&pos, LISPDECL_FUNCTION, 3);
	test(type_function_p(pos), "type_function_p1");
	type_heap(&pos, LISPDECL_CONS, 3);
	test(! type_function_p(pos), "type_function_p2");
	fixnum_heap(&pos, 11);
	test(! type_function_p(pos), "type_function_p3");

	RETURN;
}

static int test_type_astert_p(void)
{
	addr pos;

	type_heap(&pos, LISPDECL_ASTERISK, 3);
	test(type_astert_p(pos), "type_astert_p1");
	type_heap(&pos, LISPDECL_T, 3);
	test(type_astert_p(pos), "type_astert_p2");
	type_heap(&pos, LISPDECL_NIL, 3);
	test(! type_astert_p(pos), "type_astert_p3");

	RETURN;
}

static int test_type_function_aster_p(void)
{
	addr pos, aster;

	type0_heap(LISPDECL_ASTERISK, &aster);
	type3_heap(LISPDECL_FUNCTION, aster, aster, Nil, &pos);
	test(type_function_aster_p(pos), "type_function_aster_p1");
	type3_heap(LISPDECL_COMPILED_FUNCTION, aster, aster, aster, &pos);
	test(type_function_aster_p(pos), "type_function_aster_p2");
	type0_heap(LISPDECL_SYMBOL, &pos);
	test(! type_function_aster_p(pos), "type_function_aster_p3");
	fixnum_heap(&pos, 11);
	test(! type_function_aster_p(pos), "type_function_aster_p4");

	RETURN;
}

static int test_type_asterisk_p(void)
{
	addr pos;

	type_heap(&pos, LISPDECL_ASTERISK, 3);
	test(type_asterisk_p(pos), "type_asterisk_p1");
	type_heap(&pos, LISPDECL_CONS, 2);
	test(! type_asterisk_p(pos), "type_asterisk_p2");
	fixnum_heap(&pos, 11);
	test(! type_asterisk_p(pos), "type_asterisk_p3");

	RETURN;
}


/*
 *  copy
 */
static int test_type_copy_unsafe_alloc(void)
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

	type_copy_unsafe_alloc(NULL, &check, pos);
	test(test_typecheck(check, LISPDECL_CONS, 3), "type_copy_unsafe_alloc1");
	GetArrayType(check, 0, &one);
	test(one == T, "type_copy_unsafe_alloc2");
	GetArrayType(check, 1, &one);
	test(one == pos1, "type_copy_unsafe_alloc3");
	GetArrayType(check, 2, &one);
	test(one == pos2, "type_copy_unsafe_alloc4");

	SetNotDecl(pos, 1);
	type_copy_unsafe_alloc(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copy_unsafe_alloc5");
	test(RefNotDecl(check), "type_copy_unsafe_alloc6");
	test(lenarrayr(check) == 3, "type_copy_unsafe_alloc7");
	GetArrayType(check, 0, &one);
	test(one == T, "type_copy_unsafe_alloc8");
	GetArrayType(check, 1, &one);
	test(one == pos1, "type_copy_unsafe_alloc9");
	GetArrayType(check, 2, &one);
	test(one == pos2, "type_copy_unsafe_alloc10");

	local = Local_Thread;
	push_local(local, &stack);

	type_copy_unsafe_alloc(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copy_unsafe_alloc11");
	test(GetStatusDynamic(check), "type_copy_unsafe_alloc12");
	type_copy_unsafe_alloc(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copy_unsafe_alloc13");
	test(GetStatusDynamic(check), "type_copy_unsafe_alloc14");
	type_copy_unsafe_alloc(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copy_unsafe_alloc15");
	test(! GetStatusDynamic(check), "type_copy_unsafe_alloc16");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_copydecl_unsafe_alloc(void)
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

	type_copydecl_unsafe_alloc(NULL, &check, pos);
	test(test_typecheck(check, LISPDECL_CONS, 3), "type_copydecl_unsafe_alloc1");
	GetArrayType(check, 0, &one);
	test(one == T, "type_copydecl_unsafe_alloc2");
	GetArrayType(check, 1, &one);
	test(one == pos1, "type_copydecl_unsafe_alloc3");
	GetArrayType(check, 2, &one);
	test(one == pos2, "type_copydecl_unsafe_alloc4");

	SetNotDecl(pos, 1);
	type_copydecl_unsafe_alloc(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copydecl_unsafe_alloc5");
	test(! RefNotDecl(check), "type_copydecl_unsafe_alloc6");
	test(lenarrayr(check) == 3, "type_copydecl_unsafe_alloc7");
	GetArrayType(check, 0, &one);
	test(one == T, "type_copydecl_unsafe_alloc8");
	GetArrayType(check, 1, &one);
	test(one == pos1, "type_copydecl_unsafe_alloc9");
	GetArrayType(check, 2, &one);
	test(one == pos2, "type_copydecl_unsafe_alloc10");

	local = Local_Thread;
	push_local(local, &stack);

	type_copydecl_unsafe_alloc(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copydecl_unsafe_alloc11");
	test(GetStatusDynamic(check), "type_copydecl_unsafe_alloc12");

	type_copydecl_unsafe_alloc(local, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copydecl_unsafe_alloc13");
	test(GetStatusDynamic(check), "type_copydecl_unsafe_alloc14");

	type_copydecl_unsafe_alloc(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_CONS, "type_copydecl_unsafe_alloc15");
	test(! GetStatusDynamic(check), "type_copydecl_unsafe_alloc16");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  object
 */
static int test_type0_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0_alloc(local, LISPDECL_CONS, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 0), "type0_alloc1");
	test(GetStatusDynamic(pos), "type0_alloc2");

	type0_alloc(NULL, LISPDECL_SYMBOL, &pos);
	test(test_typecheck(pos, LISPDECL_SYMBOL, 0), "type0_alloc3");
	test(! GetStatusDynamic(pos), "type0_alloc4");

	type0_local(local, LISPDECL_LIST, &pos);
	test(test_typecheck(pos, LISPDECL_LIST, 0), "type0_local1");
	test(GetStatusDynamic(pos), "type0_local2");

	type0_heap(LISPDECL_STRING, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 0), "type0_heap1");
	test(! GetStatusDynamic(pos), "type0_heap2");

	rollback_local(local, stack);

	RETURN;
}

static int test_type1_alloc(void)
{
	addr pos, pos1;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0_alloc(NULL, LISPDECL_KEYWORD, &pos1);

	type1_alloc(local, LISPDECL_CONS, T, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 1), "type1_alloc1");
	test(GetStatusDynamic(pos), "type1_alloc2");
	test(RefArrayType(pos, 0) == T, "type1_alloc3");

	type1_alloc(NULL, LISPDECL_SYMBOL, pos1, &pos);
	test(test_typecheck(pos, LISPDECL_SYMBOL, 1), "type1_alloc4");
	test(! GetStatusDynamic(pos), "type1_alloc5");
	test(RefArrayType(pos, 0) == pos1, "type1_alloc6");

	type1_local(local, LISPDECL_LIST, Nil, &pos);
	test(test_typecheck(pos, LISPDECL_LIST, 1), "type1_local1");
	test(GetStatusDynamic(pos), "type1_local2");
	test(RefArrayType(pos, 0) == Nil, "type1_local3");

	type1_heap(LISPDECL_STRING, pos1, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 1), "type1_heap1");
	test(! GetStatusDynamic(pos), "type1_heap2");
	test(RefArrayType(pos, 0) == pos1, "type1_heap3");

	rollback_local(local, stack);

	RETURN;
}

static int test_type2_alloc(void)
{
	addr pos, pos1, pos2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0_alloc(NULL, LISPDECL_KEYWORD, &pos1);
	type0_alloc(NULL, LISPDECL_SYMBOL, &pos2);

	type2_alloc(local, LISPDECL_CONS, pos1, pos2, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 2), "type2_alloc1");
	test(GetStatusDynamic(pos), "type2_alloc2");
	test(RefArrayType(pos, 0) == pos1, "type2_alloc3");
	test(RefArrayType(pos, 1) == pos2, "type2_alloc4");

	type2_alloc(NULL, LISPDECL_SYMBOL, pos1, pos2, &pos);
	test(test_typecheck(pos, LISPDECL_SYMBOL, 2), "type2_alloc5");
	test(! GetStatusDynamic(pos), "type2_alloc6");
	test(RefArrayType(pos, 0) == pos1, "type2_alloc7");
	test(RefArrayType(pos, 1) == pos2, "type2_alloc8");

	type2_local(local, LISPDECL_LIST, T, pos2, &pos);
	test(test_typecheck(pos, LISPDECL_LIST, 2), "type2_local1");
	test(GetStatusDynamic(pos), "type2_local2");
	test(RefArrayType(pos, 0) == T, "type2_local3");
	test(RefArrayType(pos, 1) == pos2, "type2_local4");

	type2_heap(LISPDECL_STRING, pos1, T, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 2), "type2_heap1");
	test(! GetStatusDynamic(pos), "type2_heap2");
	test(RefArrayType(pos, 0) == pos1, "type2_heap3");
	test(RefArrayType(pos, 1) == T, "type2_heap4");

	rollback_local(local, stack);

	RETURN;
}

static int test_type3_alloc(void)
{
	addr pos, pos1, pos2, pos3;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0_alloc(NULL, LISPDECL_KEYWORD, &pos1);
	type0_alloc(NULL, LISPDECL_SYMBOL, &pos2);
	type0_alloc(NULL, LISPDECL_ATOM, &pos3);

	type3_alloc(local, LISPDECL_CONS, pos1, pos2, pos3, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 3), "type3_alloc1");
	test(GetStatusDynamic(pos), "type3_alloc2");
	test(RefArrayType(pos, 0) == pos1, "type3_alloc3");
	test(RefArrayType(pos, 1) == pos2, "type3_alloc4");
	test(RefArrayType(pos, 2) == pos3, "type3_alloc5");

	type3_alloc(NULL, LISPDECL_SYMBOL, pos1, pos2, pos3, &pos);
	test(test_typecheck(pos, LISPDECL_SYMBOL, 3), "type3_alloc6");
	test(! GetStatusDynamic(pos), "type3_alloc7");
	test(RefArrayType(pos, 0) == pos1, "type3_alloc8");
	test(RefArrayType(pos, 1) == pos2, "type3_alloc9");
	test(RefArrayType(pos, 2) == pos3, "type3_alloc10");

	type3_local(local, LISPDECL_LIST, T, pos2, pos3, &pos);
	test(test_typecheck(pos, LISPDECL_LIST, 3), "type3_local1");
	test(GetStatusDynamic(pos), "type3_local2");
	test(RefArrayType(pos, 0) == T, "type3_local3");
	test(RefArrayType(pos, 1) == pos2, "type3_local4");
	test(RefArrayType(pos, 2) == pos3, "type3_local5");

	type3_heap(LISPDECL_STRING, pos1, pos2, T, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 3), "type3_heap1");
	test(! GetStatusDynamic(pos), "type3_heap2");
	test(RefArrayType(pos, 0) == pos1, "type3_heap3");
	test(RefArrayType(pos, 1) == pos2, "type3_heap4");
	test(RefArrayType(pos, 2) == T, "type3_heap5");

	rollback_local(local, stack);

	RETURN;
}

static int test_type4_alloc(void)
{
	addr pos, pos1, pos2, pos3, pos4;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0_alloc(NULL, LISPDECL_KEYWORD, &pos1);
	type0_alloc(NULL, LISPDECL_SYMBOL, &pos2);
	type0_alloc(NULL, LISPDECL_ATOM, &pos3);
	type0_alloc(NULL, LISPDECL_STRING, &pos4);

	type4_alloc(local, LISPDECL_CONS, pos1, pos2, pos3, pos4, &pos);
	test(test_typecheck(pos, LISPDECL_CONS, 4), "type4_alloc1");
	test(GetStatusDynamic(pos), "type4_alloc2");
	test(RefArrayType(pos, 0) == pos1, "type4_alloc3");
	test(RefArrayType(pos, 1) == pos2, "type4_alloc4");
	test(RefArrayType(pos, 2) == pos3, "type4_alloc5");
	test(RefArrayType(pos, 3) == pos4, "type4_alloc6");

	type4_alloc(NULL, LISPDECL_SYMBOL, pos1, pos2, pos3, pos4, &pos);
	test(test_typecheck(pos, LISPDECL_SYMBOL, 4), "type4_alloc7");
	test(! GetStatusDynamic(pos), "type4_alloc8");
	test(RefArrayType(pos, 0) == pos1, "type4_alloc9");
	test(RefArrayType(pos, 1) == pos2, "type4_alloc10");
	test(RefArrayType(pos, 2) == pos3, "type4_alloc11");
	test(RefArrayType(pos, 3) == pos4, "type4_alloc12");

	type4_local(local, LISPDECL_LIST, T, pos2, pos3, pos4, &pos);
	test(test_typecheck(pos, LISPDECL_LIST, 4), "type4_local1");
	test(GetStatusDynamic(pos), "type4_local2");
	test(RefArrayType(pos, 0) == T, "type4_local3");
	test(RefArrayType(pos, 1) == pos2, "type4_local4");
	test(RefArrayType(pos, 2) == pos3, "type4_local5");
	test(RefArrayType(pos, 3) == pos4, "type4_local6");

	type4_heap(LISPDECL_STRING, pos1, pos2, pos3, T, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 4), "type4_heap1");
	test(! GetStatusDynamic(pos), "type4_heap2");
	test(RefArrayType(pos, 0) == pos1, "type4_heap3");
	test(RefArrayType(pos, 1) == pos2, "type4_heap4");
	test(RefArrayType(pos, 2) == pos3, "type4_heap5");
	test(RefArrayType(pos, 3) == T, "type4_heap6");

	rollback_local(local, stack);

	RETURN;
}

static int test_type0not_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0not_alloc(local, LISPDECL_CONS, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 0), "type0not_alloc1");
	test(GetStatusDynamic(pos), "type0not_alloc2");

	type0not_alloc(NULL, LISPDECL_SYMBOL, &pos);
	test(test_typecheck_not(pos, LISPDECL_SYMBOL, 0), "type0not_alloc3");
	test(! GetStatusDynamic(pos), "type0not_alloc4");

	type0not_local(local, LISPDECL_LIST, &pos);
	test(test_typecheck_not(pos, LISPDECL_LIST, 0), "type0not_local1");
	test(GetStatusDynamic(pos), "type0not_local2");

	type0not_heap(LISPDECL_STRING, &pos);
	test(test_typecheck_not(pos, LISPDECL_STRING, 0), "type0not_heap1");
	test(! GetStatusDynamic(pos), "type0not_heap2");

	rollback_local(local, stack);

	RETURN;
}

static int test_type1not_alloc(void)
{
	addr pos, pos1;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0not_alloc(NULL, LISPDECL_KEYWORD, &pos1);

	type1not_alloc(local, LISPDECL_CONS, T, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 1), "type1not_alloc1");
	test(GetStatusDynamic(pos), "type1not_alloc2");
	test(RefArrayType(pos, 0) == T, "type1not_alloc3");

	type1not_alloc(NULL, LISPDECL_SYMBOL, pos1, &pos);
	test(test_typecheck_not(pos, LISPDECL_SYMBOL, 1), "type1not_alloc4");
	test(! GetStatusDynamic(pos), "type1not_alloc5");
	test(RefArrayType(pos, 0) == pos1, "type1not_alloc6");

	type1not_local(local, LISPDECL_LIST, Nil, &pos);
	test(test_typecheck_not(pos, LISPDECL_LIST, 1), "type1not_local1");
	test(GetStatusDynamic(pos), "type1not_local2");
	test(RefArrayType(pos, 0) == Nil, "type1not_local3");

	type1not_heap(LISPDECL_STRING, pos1, &pos);
	test(test_typecheck_not(pos, LISPDECL_STRING, 1), "type1not_heap1");
	test(! GetStatusDynamic(pos), "type1not_heap2");
	test(RefArrayType(pos, 0) == pos1, "type1not_heap3");

	rollback_local(local, stack);

	RETURN;
}

static int test_type2not_alloc(void)
{
	addr pos, pos1, pos2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0not_alloc(NULL, LISPDECL_KEYWORD, &pos1);
	type0not_alloc(NULL, LISPDECL_SYMBOL, &pos2);

	type2not_alloc(local, LISPDECL_CONS, pos1, pos2, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 2), "type2not_alloc1");
	test(GetStatusDynamic(pos), "type2not_alloc2");
	test(RefArrayType(pos, 0) == pos1, "type2not_alloc3");
	test(RefArrayType(pos, 1) == pos2, "type2not_alloc4");

	type2not_alloc(NULL, LISPDECL_SYMBOL, pos1, pos2, &pos);
	test(test_typecheck_not(pos, LISPDECL_SYMBOL, 2), "type2not_alloc5");
	test(! GetStatusDynamic(pos), "type2not_alloc6");
	test(RefArrayType(pos, 0) == pos1, "type2not_alloc7");
	test(RefArrayType(pos, 1) == pos2, "type2not_alloc8");

	type2not_local(local, LISPDECL_LIST, T, pos2, &pos);
	test(test_typecheck_not(pos, LISPDECL_LIST, 2), "type2not_local1");
	test(GetStatusDynamic(pos), "type2not_local2");
	test(RefArrayType(pos, 0) == T, "type2not_local3");
	test(RefArrayType(pos, 1) == pos2, "type2not_local4");

	type2not_heap(LISPDECL_STRING, pos1, T, &pos);
	test(test_typecheck_not(pos, LISPDECL_STRING, 2), "type2not_heap1");
	test(! GetStatusDynamic(pos), "type2not_heap2");
	test(RefArrayType(pos, 0) == pos1, "type2not_heap3");
	test(RefArrayType(pos, 1) == T, "type2not_heap4");

	rollback_local(local, stack);

	RETURN;
}

static int test_type3not_alloc(void)
{
	addr pos, pos1, pos2, pos3;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0not_alloc(NULL, LISPDECL_KEYWORD, &pos1);
	type0not_alloc(NULL, LISPDECL_SYMBOL, &pos2);
	type0not_alloc(NULL, LISPDECL_ATOM, &pos3);

	type3not_alloc(local, LISPDECL_CONS, pos1, pos2, pos3, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 3), "type3not_alloc1");
	test(GetStatusDynamic(pos), "type3not_alloc2");
	test(RefArrayType(pos, 0) == pos1, "type3not_alloc3");
	test(RefArrayType(pos, 1) == pos2, "type3not_alloc4");
	test(RefArrayType(pos, 2) == pos3, "type3not_alloc5");

	type3not_alloc(NULL, LISPDECL_SYMBOL, pos1, pos2, pos3, &pos);
	test(test_typecheck_not(pos, LISPDECL_SYMBOL, 3), "type3not_alloc6");
	test(! GetStatusDynamic(pos), "type3not_alloc7");
	test(RefArrayType(pos, 0) == pos1, "type3not_alloc8");
	test(RefArrayType(pos, 1) == pos2, "type3not_alloc9");
	test(RefArrayType(pos, 2) == pos3, "type3not_alloc10");

	type3not_local(local, LISPDECL_LIST, T, pos2, pos3, &pos);
	test(test_typecheck_not(pos, LISPDECL_LIST, 3), "type3not_local1");
	test(GetStatusDynamic(pos), "type3not_local2");
	test(RefArrayType(pos, 0) == T, "type3not_local3");
	test(RefArrayType(pos, 1) == pos2, "type3not_local4");
	test(RefArrayType(pos, 2) == pos3, "type3not_local5");

	type3not_heap(LISPDECL_STRING, pos1, pos2, T, &pos);
	test(test_typecheck_not(pos, LISPDECL_STRING, 3), "type3not_heap1");
	test(! GetStatusDynamic(pos), "type3not_heap2");
	test(RefArrayType(pos, 0) == pos1, "type3not_heap3");
	test(RefArrayType(pos, 1) == pos2, "type3not_heap4");
	test(RefArrayType(pos, 2) == T, "type3not_heap5");

	rollback_local(local, stack);

	RETURN;
}

static int test_type4not_alloc(void)
{
	addr pos, pos1, pos2, pos3, pos4;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type0not_alloc(NULL, LISPDECL_KEYWORD, &pos1);
	type0not_alloc(NULL, LISPDECL_SYMBOL, &pos2);
	type0not_alloc(NULL, LISPDECL_ATOM, &pos3);
	type0not_alloc(NULL, LISPDECL_STRING, &pos4);

	type4not_alloc(local, LISPDECL_CONS, pos1, pos2, pos3, pos4, &pos);
	test(test_typecheck_not(pos, LISPDECL_CONS, 4), "type4not_alloc1");
	test(GetStatusDynamic(pos), "type4not_alloc2");
	test(RefArrayType(pos, 0) == pos1, "type4not_alloc3");
	test(RefArrayType(pos, 1) == pos2, "type4not_alloc4");
	test(RefArrayType(pos, 2) == pos3, "type4not_alloc5");
	test(RefArrayType(pos, 3) == pos4, "type4not_alloc6");

	type4not_alloc(NULL, LISPDECL_SYMBOL, pos1, pos2, pos3, pos4, &pos);
	test(test_typecheck_not(pos, LISPDECL_SYMBOL, 4), "type4not_alloc7");
	test(! GetStatusDynamic(pos), "type4not_alloc8");
	test(RefArrayType(pos, 0) == pos1, "type4not_alloc9");
	test(RefArrayType(pos, 1) == pos2, "type4not_alloc10");
	test(RefArrayType(pos, 2) == pos3, "type4not_alloc11");
	test(RefArrayType(pos, 3) == pos4, "type4not_alloc12");

	type4not_local(local, LISPDECL_LIST, T, pos2, pos3, pos4, &pos);
	test(test_typecheck_not(pos, LISPDECL_LIST, 4), "type4not_local1");
	test(GetStatusDynamic(pos), "type4not_local2");
	test(RefArrayType(pos, 0) == T, "type4not_local3");
	test(RefArrayType(pos, 1) == pos2, "type4not_local4");
	test(RefArrayType(pos, 2) == pos3, "type4not_local5");
	test(RefArrayType(pos, 3) == pos4, "type4not_local6");

	type4not_heap(LISPDECL_STRING, pos1, pos2, pos3, T, &pos);
	test(test_typecheck_not(pos, LISPDECL_STRING, 4), "type4not_heap1");
	test(! GetStatusDynamic(pos), "type4not_heap2");
	test(RefArrayType(pos, 0) == pos1, "type4not_heap3");
	test(RefArrayType(pos, 1) == pos2, "type4not_heap4");
	test(RefArrayType(pos, 2) == pos3, "type4not_heap5");
	test(RefArrayType(pos, 3) == T, "type4not_heap6");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_aster_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type_aster_local(local, &pos);
	test(test_typecheck(pos, LISPDECL_ASTERISK, 0), "type_aster_local1");
	test(GetStatusDynamic(pos), "type_aster_local2");

	rollback_local(local, stack);

	RETURN;
}

static int test_type1aster_localall(void)
{
	addr pos, pos1;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type1aster_localall(local, LISPDECL_STRING, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 1), "type1aster_localall1");
	test(GetStatusDynamic(pos), "type1aster_localall2");

	GetArrayType(pos, 0, &pos1);
	test(test_typecheck(pos1, LISPDECL_ASTERISK, 0), "type1aster_localall3");
	test(GetStatusDynamic(pos1), "type1aster_localall4");

	rollback_local(local, stack);

	RETURN;
}

static int test_type2aster_localall(void)
{
	addr pos, pos1, pos2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type2aster_localall(local, LISPDECL_STRING, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 2), "type2aster_localall1");
	test(GetStatusDynamic(pos), "type2aster_localall2");

	GetArrayType(pos, 0, &pos1);
	test(test_typecheck(pos1, LISPDECL_ASTERISK, 0), "type2aster_localall3");
	test(GetStatusDynamic(pos1), "type2aster_localall4");

	GetArrayType(pos, 1, &pos2);
	test(test_typecheck(pos2, LISPDECL_ASTERISK, 0), "type2aster_localall5");
	test(GetStatusDynamic(pos2), "type2aster_localall6");
	test(pos1 != pos2, "type2aster_localall7");

	rollback_local(local, stack);

	RETURN;
}

static int test_type3aster_localall(void)
{
	addr pos, pos1, pos2, pos3;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type3aster_localall(local, LISPDECL_STRING, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 3), "type3aster_localall1");
	test(GetStatusDynamic(pos), "type3aster_localall2");

	GetArrayType(pos, 0, &pos1);
	test(test_typecheck(pos1, LISPDECL_ASTERISK, 0), "type3aster_localall3");
	test(GetStatusDynamic(pos1), "type3aster_localall4");

	GetArrayType(pos, 1, &pos2);
	test(test_typecheck(pos2, LISPDECL_ASTERISK, 0), "type3aster_localall5");
	test(GetStatusDynamic(pos2), "type3aster_localall6");
	test(pos1 != pos2, "type3aster_localall7");

	GetArrayType(pos, 2, &pos3);
	test(test_typecheck(pos3, LISPDECL_ASTERISK, 0), "type3aster_localall8");
	test(GetStatusDynamic(pos3), "type3aster_localall9");
	test(pos1 != pos3, "type3aster_localall10");

	rollback_local(local, stack);

	RETURN;
}

static int test_type4aster_localall(void)
{
	addr pos, pos1, pos2, pos3, pos4;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type4aster_localall(local, LISPDECL_STRING, &pos);
	test(test_typecheck(pos, LISPDECL_STRING, 4), "type4aster_localall1");
	test(GetStatusDynamic(pos), "type4aster_localall2");

	GetArrayType(pos, 0, &pos1);
	test(test_typecheck(pos1, LISPDECL_ASTERISK, 0), "type4aster_localall3");
	test(GetStatusDynamic(pos1), "type4aster_localall4");

	GetArrayType(pos, 1, &pos2);
	test(test_typecheck(pos2, LISPDECL_ASTERISK, 0), "type4aster_localall5");
	test(GetStatusDynamic(pos2), "type4aster_localall6");
	test(pos1 != pos2, "type4aster_localall7");

	GetArrayType(pos, 2, &pos3);
	test(test_typecheck(pos3, LISPDECL_ASTERISK, 0), "type4aster_localall8");
	test(GetStatusDynamic(pos3), "type4aster_localall9");
	test(pos1 != pos3, "type4aster_localall10");

	GetArrayType(pos, 3, &pos4);
	test(test_typecheck(pos4, LISPDECL_ASTERISK, 0), "type4aster_localall11");
	test(GetStatusDynamic(pos4), "type4aster_localall12");
	test(pos1 != pos4, "type4aster_localall13");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  etc
 */
static int test_type_eql_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	type_eql_alloc(local, T, &pos);
	test(test_typecheck(pos, LISPDECL_EQL, 1), "type_eql_alloc1");
	test(GetStatusDynamic(pos), "type_eql_alloc2");
	test(RefArrayType(pos, 0) == T, "type_eql_alloc3");

	type_eql_alloc(NULL, T, &pos);
	test(test_typecheck(pos, LISPDECL_EQL, 1), "type_eql_alloc4");
	test(! GetStatusDynamic(pos), "type_eql_alloc5");
	test(RefArrayType(pos, 0) == T, "type_eql_alloc6");

	type_eql_local(local, T, &pos);
	test(test_typecheck(pos, LISPDECL_EQL, 1), "type_eql_local1");
	test(GetStatusDynamic(pos), "type_eql_local2");
	test(RefArrayType(pos, 0) == T, "type_eql_local3");

	type_eql_heap(T, &pos);
	test(test_typecheck(pos, LISPDECL_EQL, 1), "type_eql_heap1");
	test(! GetStatusDynamic(pos), "type_eql_heap2");
	test(RefArrayType(pos, 0) == T, "type_eql_heap3");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_member_heap(void)
{
	addr pos, array, pos1, pos2;

	type_member_heap(&pos, NULL);
	test(test_typecheck(pos, LISPDECL_MEMBER, 1), "type_member_heap1");
	test(! GetStatusDynamic(pos), "type_member_heap2");
	GetArrayType(pos, 0, &array);
	test(GetStatusSize(array) == LISPSIZE_ARRAY4, "type_member_heap3");
	test(lenarrayr(array) == 0, "type_member_heap4");

	type0_heap(LISPDECL_SYMBOL, &pos1);
	type0_heap(LISPDECL_CONS, &pos2);
	type_member_heap(&pos, pos1, pos2, NULL);
	test(test_typecheck(pos, LISPDECL_MEMBER, 1), "type_member_heap5");
	test(! GetStatusDynamic(pos), "type_member_heap6");
	GetArrayType(pos, 0, &array);
	test(GetStatusSize(array) == LISPSIZE_ARRAY4, "type_member_heap7");
	test(lenarrayr(array) == 2, "type_member_heap8");
	test(refarray(array, 0) == pos1, "type_member_heap9");
	test(refarray(array, 1) == pos2, "type_member_heap10");

	RETURN;
}

static int test_type_satisfies_heap(void)
{
	addr pos, call;

	symbol_heap(&call);
	type_satisfies_heap(call, &pos);
	test(test_typecheck(pos, LISPDECL_SATISFIES, 1), "type_satisfies_heap1");
	test(! GetStatusDynamic(pos), "type_satisfies_heap2");
	test(RefArrayType(pos, 0) == call, "type_satisfies_heap3");

	/* empty function */
	vector4_heap(&call, 0);
	code_heap(&call, call);
	function_heap(&call, Nil, call);
	type_satisfies_heap(call, &pos);
	test(test_typecheck(pos, LISPDECL_SATISFIES, 1), "type_satisfies_heap4");
	test(! GetStatusDynamic(pos), "type_satisfies_heap5");
	test(RefArrayType(pos, 0) == call, "type_satisfies_heap6");

	/* compiled-function */
	compiled_heap(&call, Nil);
	type_satisfies_heap(call, &pos);
	test(test_typecheck(pos, LISPDECL_SATISFIES, 1), "type_satisfies_heap7");
	test(! GetStatusDynamic(pos), "type_satisfies_heap8");
	test(RefArrayType(pos, 0) == call, "type_satisfies_heap9");

	RETURN;
}

static int test_type_values_heap(void)
{
	addr v1, v2, v3, v4, pos;

	v1 = T;
	v2 = Nil;
	v3 = Unbound;
	v4 = fixnumh(10);
	type_values_heap(v1, v2, v3, v4, &pos);
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_heap1");
	test(! GetStatusDynamic(pos), "type_values_heap2");
	test(RefArrayType(pos, 0) == v1, "type_values_heap3");
	test(RefArrayType(pos, 1) == v2, "type_values_heap4");
	test(RefArrayType(pos, 2) == v3, "type_values_heap5");
	test(RefArrayType(pos, 3) == v4, "type_values_heap6");

	RETURN;
}

static int test_type_signed_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	type_signed_alloc(local, 8, &pos);
	test(test_typecheck(pos, LISPDECL_SIGNED_BYTE, 1), "type_signed_alloc1");
	test(GetStatusDynamic(pos), "type_signed_alloc2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "type_signed_alloc3");

	type_signed_alloc(NULL, 16, &pos);
	test(test_typecheck(pos, LISPDECL_SIGNED_BYTE, 1), "type_signed_alloc4");
	test(! GetStatusDynamic(pos), "type_signed_alloc5");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "type_signed_alloc6");

	type_signed_local(local, 8, &pos);
	test(test_typecheck(pos, LISPDECL_SIGNED_BYTE, 1), "type_signed_local1");
	test(GetStatusDynamic(pos), "type_signed_local2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "type_signed_local3");

	type_signed_heap(16, &pos);
	test(test_typecheck(pos, LISPDECL_SIGNED_BYTE, 1), "type_signed_heap1");
	test(! GetStatusDynamic(pos), "type_signed_heap2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "type_signed_heap3");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_unsigned_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	type_unsigned_alloc(local, 8, &pos);
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "type_unsigned_alloc1");
	test(GetStatusDynamic(pos), "type_unsigned_alloc2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "type_unsigned_alloc3");

	type_unsigned_alloc(NULL, 16, &pos);
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "type_unsigned_alloc4");
	test(! GetStatusDynamic(pos), "type_unsigned_alloc5");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "type_unsigned_alloc6");

	type_unsigned_local(local, 8, &pos);
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "type_unsigned_local1");
	test(GetStatusDynamic(pos), "type_unsigned_local2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "type_unsigned_local3");

	type_unsigned_heap(16, &pos);
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "type_unsigned_heap1");
	test(! GetStatusDynamic(pos), "type_unsigned_heap2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "type_unsigned_heap3");

	rollback_local(local, stack);

	RETURN;
}

static int test_type_function_heap(void)
{
	addr pos, args, values;

	args = T;
	values = Nil;
	type_function_heap(args, values, &pos);
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "type_function_heap1");
	test(! GetStatusDynamic(pos), "type_function_heap2");
	test(RefArrayType(pos, 0) == args, "type_function_heap3");
	test(RefArrayType(pos, 1) == values, "type_function_heap4");
	test(RefArrayType(pos, 2) == Nil, "type_function_heap5");

	RETURN;
}

static int test_type_compiled_heap(void)
{
	addr pos, args, values;

	args = T;
	values = Nil;
	type_compiled_heap(args, values, &pos);
	test(test_typecheck(pos, LISPDECL_COMPILED_FUNCTION, 3), "type_compiled_heap1");
	test(! GetStatusDynamic(pos), "type_compiled_heap2");
	test(RefArrayType(pos, 0) == args, "type_compiled_heap3");
	test(RefArrayType(pos, 1) == values, "type_compiled_heap4");
	test(RefArrayType(pos, 2) == Nil, "type_compiled_heap5");

	RETURN;
}

static int test_type_clos_heap(void)
{
	addr pos, clos;

	GetConst(CLOS_CONDITION, &clos);
	type_clos_heap(clos, &pos);
	test(test_typecheck(pos, LISPDECL_CLOS, 1), "type_clos_heap1");
	test(! GetStatusDynamic(pos), "type_clos_heap2");
	test(RefArrayType(pos, 0) == clos, "type_clos_heap3");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type(void)
{
	/* allocate */
	TestBreak(test_type_heap);
	TestBreak(test_getlispdecl);
	TestBreak(test_getnotdecl);
	TestBreak(test_type_setnotdecl);
	TestBreak(test_type_revnotdecl);
	TestBreak(test_type_setnotobject);
	TestBreak(test_type_getarraytype);
	TestBreak(test_type_getvalues1);
	/* check */
	TestBreak(test_decl_character_p);
	TestBreak(test_decl_float_p);
	TestBreak(test_decl_range_p);
	TestBreak(test_decl_subtypep_real);
	TestBreak(test_type_function_p);
	TestBreak(test_type_astert_p);
	TestBreak(test_type_function_aster_p);
	TestBreak(test_type_asterisk_p);
	/* copy */
	TestBreak(test_type_copy_unsafe_alloc);
	TestBreak(test_type_copydecl_unsafe_alloc);
	/* object */
	TestBreak(test_type0_alloc);
	TestBreak(test_type1_alloc);
	TestBreak(test_type2_alloc);
	TestBreak(test_type3_alloc);
	TestBreak(test_type4_alloc);
	TestBreak(test_type0not_alloc);
	TestBreak(test_type1not_alloc);
	TestBreak(test_type2not_alloc);
	TestBreak(test_type3not_alloc);
	TestBreak(test_type4not_alloc);
	TestBreak(test_type_aster_local);
	TestBreak(test_type1aster_localall);
	TestBreak(test_type2aster_localall);
	TestBreak(test_type3aster_localall);
	TestBreak(test_type4aster_localall);
	/* etc */
	TestBreak(test_type_eql_alloc);
	TestBreak(test_type_member_heap);
	TestBreak(test_type_satisfies_heap);
	TestBreak(test_type_values_heap);
	TestBreak(test_type_signed_alloc);
	TestBreak(test_type_unsigned_alloc);
	TestBreak(test_type_function_heap);
	TestBreak(test_type_compiled_heap);
	TestBreak(test_type_clos_heap);

	return 0;
}

int test_type(void)
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
		build_type();
		build_common();
		lisp_initialize = 1;
		result = testbreak_type();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

