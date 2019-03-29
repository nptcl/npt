#include "clos_combination.c"
#include "degrade.h"
#include "execute.h"
#include "function.h"

#if 0
/*
 *  qualifiers-check
 */
static int test_qualifiers_equal_list(void)
{
	addr left, right, asterisk, value;

	/* nil nil (recursive) */
	test(qualifiers_equal_list(Nil, Nil), "qualifiers_equal_list1");
	consnil_heap(&left);
	test(! qualifiers_equal_list(left, Nil), "qualifiers_equal_list2");
	test(! qualifiers_equal_list(Nil, left), "qualifiers_equal_list3");

	/* (:around) (*) */
	GetConstant(CONSTANT_COMMON_ASTERISK, &asterisk);
	GetConstant(CONSTANT_KEYWORD_AROUND, &value);
	list_heap(&left, value, NULL);
	list_heap(&right, asterisk, NULL);
	test(qualifiers_equal_list(left, right), "qualifiers_equal_list4");
	test(! qualifiers_equal_list(right, left), "qualifiers_equal_list5");

	/* (:around) (:around) */
	list_heap(&left, value, NULL);
	list_heap(&right, value, NULL);
	test(qualifiers_equal_list(left, right), "qualifiers_equal_list6");

	/* (:around) (:around . *) */
	cons_heap(&right, value, asterisk);
	test(qualifiers_equal_list(left, right), "qualifiers_equal_list7");
	test(! qualifiers_equal_list(right, left), "qualifiers_equal_list8");

	/* (:around) (* . *) */
	cons_heap(&right, asterisk, asterisk);
	test(qualifiers_equal_list(left, right), "qualifiers_equal_list9");
	test(! qualifiers_equal_list(right, left), "qualifiers_equal_list10");

	RETURN;
}

static void test_symbolcall(Execute ptr, addr right)
{
	GetCar(right, &right);
	right = (GetType(right) == LISPTYPE_CONS)? T: Nil;
	setresult_control(ptr, right);
}

static int test_qualifiers_equal_symbol(void)
{
	addr call, args, symbol;
	const char *str = "TEST-SYMBOL-CALL";
	Execute ptr;

	ptr = Execute_Thread;
	internchar(LISP_PACKAGE, str, &symbol);
	compiled_heap(&call, symbol);
	setcompiled_rest(call, test_symbolcall);
	SetFunctionSymbol(symbol, call);
	test(! qualifiers_equal_symbol(ptr, Nil, symbol), "qualifiers_equal_symbol1");
	consnil_heap(&args);
	test(qualifiers_equal_symbol(ptr, args, symbol), "qualifiers_equal_symbol2");

	RETURN;
}

static int test_qualifiers_equal(void)
{
	addr left, right, call;
	const char *str = "TEST-SYMBOL-CALL";
	Execute ptr;

	ptr = Execute_Thread;
	GetConstant(CONSTANT_KEYWORD_AROUND, &left);
	list_heap(&left, left, NULL);
	GetConstant(CONSTANT_COMMON_ASTERISK, &right);
	test(qualifiers_equal(ptr, left, right), "qualifiers_equal1");

	list_heap(&right, right, NULL);
	test(qualifiers_equal(ptr, left, right), "qualifiers_equal2");

	internchar(LISP_PACKAGE, str, &right);
	compiled_heap(&call, right);
	setcompiled_rest(call, test_symbolcall);
	SetFunctionSymbol(right, call);
	test(qualifiers_equal(ptr, left, right), "qualifiers_equal3");

	test(! qualifiers_equal(ptr, left, Nil), "qualifiers_equal4");

	RETURN;
}

static int test_check_qualifiers_equal_long(void)
{
	addr qualifiers, combination, pos;
	Execute ptr;

	ptr = Execute_Thread;
	combination_standard_qualifiers(&qualifiers);
	make_instance_method_combination(&combination, Nil);
	setf_clos_elt(combination, Clos_combination_qualifiers, qualifiers);
	test(check_qualifiers_equal_long(ptr, combination, Nil),
			"check_qualifiers_equal_long1");
	test(! check_qualifiers_equal_long(ptr, combination, T),
			"check_qualifiers_equal_long2");
	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_long(ptr, combination, pos),
			"check_qualifiers_equal_long3");
	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, pos, NULL);
	test(! check_qualifiers_equal_long(ptr, combination, pos),
			"check_qualifiers_equal_long4");

	RETURN;
}

static int test_check_qualifiers_equal_short(void)
{
	addr combination, name, pos;

	make_instance_method_combination(&combination, Nil);
	internchar(LISP_PACKAGE, "HELLO", &name);
	setf_clos_elt(combination, Clos_combination_name, name);

	test(! check_qualifiers_equal_short(combination, Nil),
			"check_qualifiers_equal_short1");
	test(! check_qualifiers_equal_short(combination, T),
			"check_qualifiers_equal_short2");

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_short(combination, pos),
			"check_qualifiers_equal_short3");

	list_heap(&pos, name, NULL);
	test(check_qualifiers_equal_short(combination, pos),
			"check_qualifiers_equal_short4");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	test(! check_qualifiers_equal_short(combination, pos),
			"check_qualifiers_equal_short5");

	RETURN;
}

static int test_check_qualifiers_equal(void)
{
	addr qualifiers, combination, name;
	Execute ptr;

	ptr = Execute_Thread;
	combination_standard_qualifiers(&qualifiers);
	make_instance_method_combination(&combination, Nil);
	setf_clos_elt(combination, Clos_combination_qualifiers, qualifiers);
	internchar(LISP_PACKAGE, "HELLO", &name);
	setf_clos_elt(combination, Clos_combination_name, name);
	list_heap(&name, name, NULL);

	setf_clos_elt(combination, Clos_combination_long_p, T);
	test(check_qualifiers_equal(ptr, combination, Nil), "check_qualifiers_equal1");
	test(! check_qualifiers_equal(ptr, combination, name), "check_qualifiers_equal2");

	setf_clos_elt(combination, Clos_combination_long_p, Nil);
	test(! check_qualifiers_equal(ptr, combination, Nil), "check_qualifiers_equal3");
	test(check_qualifiers_equal(ptr, combination, name), "check_qualifiers_equa4");

	RETURN;
}


/*
 *  qualifiers-position
 */
static int test_method_combination_qualifiers_count(void)
{
	addr qualifiers, combination;
	size_t size;

	combination_standard_qualifiers(&qualifiers);
	make_instance_method_combination(&combination, Nil);
	setf_clos_elt(combination, Clos_combination_qualifiers, qualifiers);
	setf_clos_elt(combination, Clos_combination_long_p, T);
	method_combination_qualifiers_count(combination, &size);
	test(size == 4, "method_combination_qualifiers_count1");
	setf_clos_elt(combination, Clos_combination_long_p, Nil);
	method_combination_qualifiers_count(combination, &size);
	test(size == 2, "method_combination_qualifiers_count2");

	RETURN;
}

static int test_qualifiers_position_short_nil(void)
{
	addr combination, pos;
	size_t position;

	make_instance_method_combination(&combination, Nil);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	setf_clos_elt(combination, Clos_combination_name, pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_short_nil(pos, combination, &position),
			"qualifiers_position_short_nil1");
	test(position == 1, "qualifiers_position_short_nil2");

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_short_nil(pos, combination, &position),
			"qualifiers_position_short_nil3");
	test(position == 0, "qualifiers_position_short_nil4");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	test(qualifiers_position_short_nil(pos, combination, &position),
			"qualifiers_position_short_nil5");

	RETURN;
}

static int test_qualifiers_position_long_nil(void)
{
	addr qualifiers, combination, pos;
	Execute ptr;
	size_t position;

	ptr = Execute_Thread;
	combination_standard_qualifiers(&qualifiers);
	make_instance_method_combination(&combination, Nil);
	setf_clos_elt(combination, Clos_combination_qualifiers, qualifiers);

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_long_nil(ptr, pos, combination, &position),
			"qualifiers_position_long_nil1");
	test(position == 0, "qualifiers_position_long_nil2");

	test(! qualifiers_position_long_nil(ptr, Nil, combination, &position),
			"qualifiers_position_long_nil3");
	test(position == 2, "qualifiers_position_long_nil4");

	test(qualifiers_position_long_nil(ptr, T, combination, &position),
			"qualifiers_position_long_nil5");

	RETURN;
}

static int test_qualifiers_position_nil(void)
{
	addr qualifiers, combination;
	Execute ptr;
	size_t position;

	ptr = Execute_Thread;
	combination_standard_qualifiers(&qualifiers);
	make_instance_method_combination(&combination, Nil);
	setf_clos_elt(combination, Clos_combination_qualifiers, qualifiers);
	setf_clos_elt(combination, Clos_combination_long_p, T);
	test(! qualifiers_position_nil(ptr, Nil, combination, &position),
			"qualifiers_position_nil1");
	test(position == 2, "qualifiers_position_nil2");

	RETURN;
}

static int test_qualifiers_position(void)
{
	addr combination, pos;
	Execute ptr;
	size_t position;

	ptr = Execute_Thread;
	make_instance_method_combination(&combination, Nil);
	setf_clos_elt(combination, Clos_combination_long_p, Nil);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	setf_clos_elt(combination, Clos_combination_name, pos);
	list_heap(&pos, pos, NULL);
	qualifiers_position(ptr, pos, combination, &position);
	test(position == 1, "qualifiers_position1");

	RETURN;
}


/*
 *  standard method-combination
 */
static int test_make_instance_method_combination(void)
{
	addr instance, clos;

	make_instance_method_combination(&instance, Nil);
	GetConstant(CONSTANT_CLOS_METHOD_COMBINATION, &clos);
	test(std_subtype_p(instance, clos), "make_instance_method_combination1");

	RETURN;
}

static int test_define_method_combination_constant(void)
{
	addr name, pos, clos, check;

	define_method_combination_constant(CONSTANT_COMMON_PLUS, 1);
	GetConstant(CONSTANT_COMMON_PLUS, &name);
	pos = find_method_combination(name);
	GetConstant(CONSTANT_CLOS_METHOD_COMBINATION, &clos);
	test(std_subtype_p(pos, clos), "define_method_combination_constant1");

	clos_elt(pos, Clos_combination_name, &check);
	test(check == name, "define_method_combination_constant2");
	clos_elt(pos, Clos_combination_long_p, &check);
	test(check == Nil, "define_method_combination_constant3");
	clos_elt(pos, Clos_combination_identity, &check);
	test(check == T, "define_method_combination_constant4");

	RETURN;
}

static int test_qualifier_check(enum CONSTANT_INDEX index, addr one)
{
	addr left, right;

	GetCdr(one, &one);
	GetCar(one, &one);
	GetCons(one, &left, &one);
	GetConstant(index, &right);
	return left == right && one == Nil;
}

static int test_qualifier_nil(addr one)
{
	GetCdr(one, &one);
	GetCar(one, &one);
	return one == Nil;
}

static int test_combination_standard_qualifiers(void)
{
	addr left, right;

	combination_standard_qualifiers(&right);
	GetCons(right, &left, &right);
	test(test_qualifier_check(CONSTANT_KEYWORD_AROUND, left),
			"combination_standard_qualifiers1");
	GetCons(right, &left, &right);
	test(test_qualifier_check(CONSTANT_KEYWORD_BEFORE, left),
			"combination_standard_qualifiers2");
	GetCons(right, &left, &right);
	test(test_qualifier_nil(left), "combination_standard_qualifiers3");
	GetCons(right, &left, &right);
	test(test_qualifier_check(CONSTANT_KEYWORD_AFTER, left),
			"combination_standard_qualifiers4");
	test(right == Nil, "combination_standard_qualifiers5");

	RETURN;
}

static int test_define_method_combination_standard(void)
{
	addr name, pos, clos, check;

	define_method_combination_standard();
	GetConstant(CONSTANT_COMMON_STANDARD, &name);
	pos = find_method_combination(name);
	GetConstant(CONSTANT_CLOS_METHOD_COMBINATION, &clos);
	test(std_subtype_p(pos, clos), "define_method_combination_standard1");

	clos_elt(pos, Clos_combination_name, &check);
	test(check == name, "define_method_combination_standard2");
	clos_elt(pos, Clos_combination_long_p, &check);
	test(check == T, "define_method_combination_standard3");
	clos_elt(pos, Clos_combination_qualifiers, &check);
	test(GetType(check) == LISPTYPE_CONS, "define_method_combination_standard4");

	RETURN;
}
#endif


/*
 *  main
 */
static int testbreak_clos_combination(void)
{
#if 0
	/* qualifiers-check */
	TestBreak(test_qualifiers_equal_list);
	TestBreak(test_qualifiers_equal_symbol);
	TestBreak(test_qualifiers_equal);
	TestBreak(test_check_qualifiers_equal_long);
	TestBreak(test_check_qualifiers_equal_short);
	TestBreak(test_check_qualifiers_equal);
	/* qualifiers-position */
	TestBreak(test_method_combination_qualifiers_count);
	TestBreak(test_qualifiers_position_short_nil);
	TestBreak(test_qualifiers_position_long_nil);
	TestBreak(test_qualifiers_position_nil);
	TestBreak(test_qualifiers_position);
	/* standard method-combination */
	TestBreak(test_make_instance_method_combination);
	TestBreak(test_define_method_combination_constant);
	TestBreak(test_combination_standard_qualifiers);
	TestBreak(test_define_method_combination_standard);
#endif

	return 0;
}

int test_clos_combination(void)
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
		lisp_init = 1;
		result = testbreak_clos_combination();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

