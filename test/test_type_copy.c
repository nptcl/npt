#include "type_copy.c"
#include "bigcons.h"
#include "bigdata.h"
#include "bignum.h"
#include "calltype.h"
#include "character.h"
#include "clos.h"
#include "clos_object.h"
#include "common.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "pathname.h"
#include "random_state.h"
#include "ratio.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type_parse.h"

static int testlispdecl(addr pos, enum LISPDECL decl)
{
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != decl) {
		decl = RefLispDecl(pos);
		degrade_printf("lispdecl error\n");
		return 0;
	}
	return 1;
}

static void parse_type_string(addr *ret, const char *code)
{
	readstring(ret, code);
	parse_type_heap(ret, *ret);
}

static int test_getset_arraytype(void)
{
	addr left, right;

	type_object4(NULL, LISPDECL_INTEGER, Nil, Nil, Nil, Nil, &left);
	parse_type_string(&right, "(integer 10 20)");
	getset_arraytype(NULL, left, right, 1);
	GetArrayType(left, 1, &left);
	test(RefFixnum(left) == 10, "getset_arraytype1");

	RETURN;
}

static int test_getsettype_arraytype(void)
{
	addr type, left, right;

	parse_type_string(&type, "real");
	type_object4(NULL, LISPDECL_INTEGER, Nil, Nil, Nil, Nil, &left);
	type_object4(NULL, LISPDECL_INTEGER, Nil, Nil, Nil, Nil, &right);
	SetArrayType(right, 1, type);
	getsettype_arraytype(NULL, left, right, 1);
	GetArrayType(left, 1, &left);
	test(testlispdecl(left, LISPDECL_REAL), "getsettype_arraytype1");
	test(left != type, "getsettype_arraytype2");

	RETURN;
}

static int test_getset_array4(void)
{
	addr left, right, value;

	vector4_heap(&left, 4);
	vector4_heap(&right, 4);
	fixnum_heap(&value, 10);
	SetArrayA4(right, 1, value);
	getset_array4(NULL, left, right, 1);
	GetArrayA4(left, 1, &left);
	test(RefFixnum(left) == 10, "getset_array4-1");

	RETURN;
}

static int test_getsettype_array4(void)
{
	addr type, left, right;

	parse_type_string(&type, "real");
	vector4_heap(&left, 4);
	vector4_heap(&right, 4);
	SetArrayA4(right, 1, type);
	getsettype_array4(NULL, left, right, 1);
	GetArrayA4(left, 1, &left);
	test(testlispdecl(left, LISPDECL_REAL), "getsettype_array4-1");
	test(left != type, "getsettype_array4-2");

	RETURN;
}

static int test_copy_object(void)
{
	addr left, right;
	size_t size1, size2;

	parse_type_string(&left, "(not integer)");
	right = copy_allobject(NULL, left);
	test(left != right, "copy_object1");
	test(GetType(right) == LISPTYPE_TYPE, "copy_object2");
	test(RefLispDecl(left) == RefLispDecl(right), "copy_object3");
	LenArrayType(left, &size1);
	LenArrayType(right, &size2);
	test(size1 == size2, "copy_object4");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "copy_object5");

	RETURN;
}

static int test_copy_alltype(void)
{
	addr left, right;
	size_t size1, size2;

	parse_type_string(&left, "(not integer)");
	right = copy_alltype(NULL, left);
	test(left != right, "copy_alltype1");
	test(GetType(right) == LISPTYPE_TYPE, "copy_alltype2");
	test(RefLispDecl(left) == RefLispDecl(right), "copy_alltype3");
	LenArrayType(left, &size1);
	LenArrayType(right, &size2);
	test(size1 == size2, "copy_alltype4");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "copy_alltype5");
	test(RefLispDecl(left) == RefLispDecl(right), "copy_alltype6");

	RETURN;
}

static int test_copy_clos(void)
{
	addr left, right, clos;

	interncommon("STANDARD-CLASS", &clos);
	clos = find_class(clos);
	type_object1(NULL, LISPDECL_CLOS, clos, &left);
	type_copy_heap(&right, left);
	test(left != right, "copy_clos1");
	test(testlispdecl(right, LISPDECL_CLOS), "copy_clos2");
	GetArrayType(right, 0, &right);
	test(right == clos, "copy_clos3");

	RETURN;
}

static int test_copy_asterisk(void)
{
	addr left, right;

	type_asterisk_heap(&left);
	type_copy_heap(&right, left);
	test(left != right, "copy_asterisk1");
	test(testlispdecl(right, LISPDECL_ASTERISK), "copy_asterisk2");

	RETURN;
}

static int test_copy_optimized(void)
{
	addr left, right, aster;

	type_asterisk_heap(&aster);
	type_object1(NULL, LISPDECL_OPTIMIZED, aster, &left);
	type_copy_heap(&right, left);
	test(left != right, "copy_optimized1");
	test(testlispdecl(right, LISPDECL_OPTIMIZED), "copy_optimized2");
	GetArrayType(right, 0, &right);
	test(testlispdecl(right, LISPDECL_ASTERISK), "copy_optimized3");
	test(right != aster, "copy_optimized4");

	RETURN;
}

static int test_copy_subtypep(void)
{
	addr left, right, aster;

	type_asterisk_heap(&aster);
	type_object1(NULL, LISPDECL_SUBTYPEP, aster, &left);
	type_copy_heap(&right, left);
	test(left != right, "copy_subtypep1");
	test(testlispdecl(right, LISPDECL_SUBTYPEP), "copy_subtypep2");
	GetArrayType(right, 0, &right);
	test(testlispdecl(right, LISPDECL_ASTERISK), "copy_subtypep3");
	test(right != aster, "copy_subtypep4");

	RETURN;
}

static int test_copy_and(void)
{
	addr left, right;

	parse_type_string(&left, "(and integer character)");
	type_copy_heap(&right, left);
	test(left != right, "copy_and1");
	test(testlispdecl(right, LISPDECL_AND), "copy_and2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "copy_and3");
	test(LenArrayA4r(left) == LenArrayA4r(right), "copy_and4");
	GetArrayA4(left, 0, &left);
	GetArrayA4(right, 0, &right);
	test(left != right, "copy_and5");
	test(testlispdecl(left, LISPDECL_INTEGER), "copy_and6");
	test(testlispdecl(right, LISPDECL_INTEGER), "copy_and7");

	RETURN;
}

static int test_copy_or(void)
{
	addr left, right;

	parse_type_string(&left, "(or integer character)");
	type_copy_heap(&right, left);
	test(left != right, "copy_or1");
	test(testlispdecl(right, LISPDECL_OR), "copy_or2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "copy_or3");
	test(LenArrayA4r(left) == LenArrayA4r(right), "copy_or4");
	GetArrayA4(left, 0, &left);
	GetArrayA4(right, 0, &right);
	test(left != right, "copy_or5");
	test(testlispdecl(left, LISPDECL_INTEGER), "copy_or6");
	test(testlispdecl(right, LISPDECL_INTEGER), "copy_or7");

	RETURN;
}

static int test_copy_eql(void)
{
	addr left, right;

	parse_type_string(&left, "(eql 100)");
	type_copy_heap(&right, left);
	test(left != right, "copy_eql1");
	test(testlispdecl(right, LISPDECL_EQL), "copy_eql2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "copy_eql3");

	RETURN;
}

static int test_copy_member(void)
{
	addr left, right, check1, check2;

	parse_type_string(&left, "(member 10 20 30)");
	type_copy_heap(&right, left);
	test(left != right, "copy_member1");
	test(testlispdecl(right, LISPDECL_MEMBER), "copy_member2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(LenArrayA4r(left) == LenArrayA4r(right), "copy_member3");
	GetArrayA4(left, 0, &check1);
	GetArrayA4(right, 0, &check2);
	test(check1 == check2, "copy_member4");
	GetArrayA4(left, 2, &check1);
	GetArrayA4(right, 2, &check2);
	test(check1 == check2, "copy_member5");

	RETURN;
}

static int test_copy_mod(void)
{
	addr left, right;

	parse_type_string(&left, "(mod 256)");
	type_copy_heap(&right, left);
	test(left != right, "copy_mod1");
	test(testlispdecl(right, LISPDECL_MOD), "copy_mod2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "copy_mod3");

	RETURN;
}

static int test_copy_not(void)
{
	addr left, right;

	parse_type_string(&left, "(not integer)");
	type_copy_heap(&right, left);
	test(left != right, "copy_not1");
	test(testlispdecl(right, LISPDECL_NOT), "copy_not2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "copy_not3");
	test(testlispdecl(left, LISPDECL_INTEGER), "copy_not4");
	test(testlispdecl(right, LISPDECL_INTEGER), "copy_not5");

	RETURN;
}

static int test_copy_satisfies(void)
{
	addr left, right;

	parse_type_string(&left, "(satisfies hello)");
	type_copy_heap(&right, left);
	test(left != right, "copy_satisfies1");
	test(testlispdecl(right, LISPDECL_SATISFIES), "copy_satisfies2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "copy_satisfies3");

	RETURN;
}

static int test_copy_values(void)
{
	addr left, right;

	readstring(&left, "(values integer)");
	parse_type_values_heap(&left, left);
	type_copy_heap(&right, left);
	test(left != right, "copy_values1");
	test(testlispdecl(right, LISPDECL_VALUES), "copy_values2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "copy_values3");
	GetCar(left, &left);
	GetCar(right, &right);
	test(left != right, "copy_values4");
	test(testlispdecl(left, LISPDECL_INTEGER), "copy_values5");
	test(testlispdecl(right, LISPDECL_INTEGER), "copy_values6");

	RETURN;
}

static int test_copy_atom(void)
{
	addr left, right;

	parse_type_string(&left, "atom");
	type_copy_heap(&right, left);
	test(left != right, "copy_atom1");
	test(testlispdecl(right, LISPDECL_ATOM), "copy_atom2");

	RETURN;
}

static int test_copy_list(void)
{
	addr left, right;

	parse_type_string(&left, "list");
	type_copy_heap(&right, left);
	test(left != right, "copy_list1");
	test(testlispdecl(right, LISPDECL_LIST), "copy_list2");

	RETURN;
}

static int test_copy_boolean(void)
{
	addr left, right;

	parse_type_string(&left, "boolean");
	type_copy_heap(&right, left);
	test(left != right, "copy_boolean1");
	test(testlispdecl(right, LISPDECL_BOOLEAN), "copy_boolean2");

	RETURN;
}

static int test_copy_vector(void)
{
	addr left, right;

	parse_type_string(&left, "(vector * *)");
	type_copy_heap(&right, left);
	test(left != right, "copy_vector1");
	test(testlispdecl(right, LISPDECL_VECTOR), "copy_vector2");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_copy(void)
{
	TestBreak(test_getset_arraytype);
	TestBreak(test_getsettype_arraytype);
	TestBreak(test_getset_array4);
	TestBreak(test_getsettype_array4);
	TestBreak(test_copy_object);
	TestBreak(test_copy_alltype);
	TestBreak(test_copy_clos);
	TestBreak(test_copy_asterisk);
	TestBreak(test_copy_optimized);
	TestBreak(test_copy_subtypep);
	TestBreak(test_copy_and);
	TestBreak(test_copy_or);
	TestBreak(test_copy_eql);
	TestBreak(test_copy_member);
	TestBreak(test_copy_mod);
	TestBreak(test_copy_not);
	TestBreak(test_copy_satisfies);
	TestBreak(test_copy_values);
	TestBreak(test_copy_atom);
	TestBreak(test_copy_list);
	TestBreak(test_copy_boolean);
	TestBreak(test_copy_vector);

	return 0;
}

int test_type_copy(void)
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
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_calltype();
		build_syscall();
		build_common();
		build_readtable();
		lisp_init = 1;
		result = testbreak_type_copy();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

