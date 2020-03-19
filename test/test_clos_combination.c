#include "clos_combination.c"
#include "clos_class.h"
#include "degrade.h"
#include "execute.h"
#include "function.h"

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

static int test_symbolcall(Execute ptr, addr right)
{
	GetCar(right, &right);
	right = (GetType(right) == LISPTYPE_CONS)? T: Nil;
	setresult_control(ptr, right);
	return 0;
}

static int test_qualifiers_equal_symbol(void)
{
	addr call, args, symbol;
	const char *str = "TEST-SYMBOL-CALL";
	Execute ptr;

	ptr = Execute_Thread;
	internchar(LISP_PACKAGE, str, &symbol);
	compiled_heap(&call, symbol);
	SetPointer_rest(p_debug1, test_symbolcall);
	setcompiled_rest(call, p_debug1);
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
	SetPointer_rest(p_debug1, test_symbolcall);
	setcompiled_rest(call, p_debug1);
	SetFunctionSymbol(right, call);
	test(qualifiers_equal(ptr, left, right), "qualifiers_equal3");

	test(! qualifiers_equal(ptr, left, Nil), "qualifiers_equal4");

	RETURN;
}

static int test_check_qualifiers_equal_standard(void)
{
	addr pos;

	test(check_qualifiers_equal_standard(Nil), "check_qualifiers_equal_standard1");
	test(! check_qualifiers_equal_standard(T), "check_qualifiers_equal_standard2");

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_standard(pos), "check_qualifiers_equal_standard3");

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, pos, NULL);
	test(! check_qualifiers_equal_standard(pos), "check_qualifiers_equal_standard4");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_standard(pos), "check_qualifiers_equal_standard5");

	GetConstant(CONSTANT_KEYWORD_AFTER, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_standard(pos), "check_qualifiers_equal_standard6");

	GetConstant(CONSTANT_KEYWORD_TEST, &pos);
	list_heap(&pos, pos, NULL);
	test(! check_qualifiers_equal_standard(pos), "check_qualifiers_equal_standard7");

	RETURN;
}

static void test_standard_qualifiers(addr *ret)
{
	/* ((nil (:before))
	 *  (nil (:after))
	 *  (nil (:around))
	 *  (nil (:key))
	 *  (nil nil))
	 */
	addr list, pos;

	list = Nil;
	/* before */
	GetConst(KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	list_heap(&pos, Nil, pos, NULL);
	cons_heap(&list, pos, list);
	/* after */
	GetConst(KEYWORD_AFTER, &pos);
	list_heap(&pos, pos, NULL);
	list_heap(&pos, Nil, pos, NULL);
	cons_heap(&list, pos, list);
	/* around */
	GetConst(KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	list_heap(&pos, Nil, pos, NULL);
	cons_heap(&list, pos, list);
	/* key */
	GetConst(KEYWORD_KEY, &pos);
	list_heap(&pos, pos, NULL);
	list_heap(&pos, Nil, pos, NULL);
	cons_heap(&list, pos, list);
	/* nil */
	list_heap(&pos, Nil, Nil, NULL);
	cons_heap(&list, pos, list);
	/* result */
	nreverse_list_unsafe(ret, list);
}

static void test_make_instance_longcomb(addr *ret)
{
	addr pos, comb;

	test_standard_qualifiers(&pos);
	GetConst(CLOS_LONG_METHOD_COMBINATION, &comb);
	clos_instance_heap(comb, &comb);
	stdset_longcomb_qualifiers(comb, pos);
	*ret = comb;
}

static int test_check_qualifiers_equal_long(void)
{
	addr comb, pos;
	Execute ptr;

	ptr = Execute_Thread;
	test_make_instance_longcomb(&comb);
	test(check_qualifiers_equal_long(ptr, comb, Nil),
			"check_qualifiers_equal_long1");
	test(! check_qualifiers_equal_long(ptr, comb, T),
			"check_qualifiers_equal_long2");
	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_long(ptr, comb, pos),
			"check_qualifiers_equal_long3");
	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, pos, NULL);
	test(! check_qualifiers_equal_long(ptr, comb, pos),
			"check_qualifiers_equal_long4");

	RETURN;
}

static void test_make_instance_shortcomb(addr *ret)
{
	addr comb;

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &comb);
	clos_instance_heap(comb, &comb);
	*ret = comb;
}

static int test_check_qualifiers_equal_short(void)
{
	addr comb, name, pos;

	test_make_instance_shortcomb(&comb);
	internchar(LISP_PACKAGE, "HELLO", &name);
	stdset_shortcomb_name(comb, name);

	test(! check_qualifiers_equal_short(comb, Nil),
			"check_qualifiers_equal_short1");
	test(! check_qualifiers_equal_short(comb, T),
			"check_qualifiers_equal_short2");

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(check_qualifiers_equal_short(comb, pos),
			"check_qualifiers_equal_short3");

	list_heap(&pos, name, NULL);
	test(check_qualifiers_equal_short(comb, pos),
			"check_qualifiers_equal_short4");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	test(! check_qualifiers_equal_short(comb, pos),
			"check_qualifiers_equal_short5");

	RETURN;
}

static int test_check_qualifiers_equal(void)
{
	addr comb, name, hello;
	Execute ptr;

	ptr = Execute_Thread;
	internchar(LISP_PACKAGE, "HELLO", &hello);
	list_heap(&name, hello, NULL);

	/* standard */
	comb = Nil;
	test(check_qualifiers_equal(ptr, comb, Nil), "check_qualifiers_equal1");
	test(! check_qualifiers_equal(ptr, comb, name), "check_qualifiers_equal2");

	/* long */
	test_make_instance_longcomb(&comb);
	stdset_longcomb_name(comb, hello);
	test(check_qualifiers_equal(ptr, comb, Nil), "check_qualifiers_equal3");
	test(! check_qualifiers_equal(ptr, comb, name), "check_qualifiers_equal4");

	/* short */
	test_make_instance_shortcomb(&comb);
	stdset_shortcomb_name(comb, hello);
	test(! check_qualifiers_equal(ptr, comb, Nil), "check_qualifiers_equal5");
	test(check_qualifiers_equal(ptr, comb, name), "check_qualifiers_equal6");

	RETURN;
}


/*
 *  qualifiers-position
 */
static int test_method_combination_qualifiers_count(void)
{
	addr comb;
	size_t size;

	/* standard */
	method_combination_qualifiers_count(Nil, &size);
	test(size == Clos_standard_size, "method_combination_qualifiers_count1");

	/* long */
	test_make_instance_longcomb(&comb);
	method_combination_qualifiers_count(comb, &size);
	test(size == 5, "method_combination_qualifiers_count2");

	/* short */
	test_make_instance_shortcomb(&comb);
	method_combination_qualifiers_count(comb, &size);
	test(size == 2, "method_combination_qualifiers_count3");

	RETURN;
}

static int test_qualifiers_position_standard_nil(void)
{
	addr pos;
	size_t index;

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_standard_nil(pos, &index),
			"qualifiers_position_standard_nil1");
	test(index == Clos_standard_around, "qualifiers_position_standard_nil2");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_standard_nil(pos, &index),
			"qualifiers_position_standard_nil3");
	test(index == Clos_standard_before, "qualifiers_position_standard_nil4");

	GetConstant(CONSTANT_KEYWORD_AFTER, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_standard_nil(pos, &index),
			"qualifiers_position_standard_nil5");
	test(index == Clos_standard_after, "qualifiers_position_standard_nil6");

	test(! qualifiers_position_standard_nil(Nil, &index),
			"qualifiers_position_standard_nil7");
	test(index == Clos_standard_primary, "qualifiers_position_standard_nil8");

	test(qualifiers_position_standard_nil(T, &index),
			"qualifiers_position_standard_nil9");

	RETURN;
}

static int test_qualifiers_position_long_nil(void)
{
	addr comb, pos;
	Execute ptr;
	size_t index;

	ptr = Execute_Thread;
	test_make_instance_longcomb(&comb);

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_long_nil(ptr, pos, comb, &index),
			"qualifiers_position_long_nil1");
	test(index == 2, "qualifiers_position_long_nil2");

	test(! qualifiers_position_long_nil(ptr, Nil, comb, &index),
			"qualifiers_position_long_nil3");
	test(index == 4, "qualifiers_position_long_nil4");

	test(qualifiers_position_long_nil(ptr, T, comb, &index),
			"qualifiers_position_long_nil5");

	RETURN;
}

static int test_qualifiers_position_short_nil(void)
{
	addr comb, pos;
	size_t index;

	test_make_instance_shortcomb(&comb);

	internchar(LISP_PACKAGE, "HELLO", &pos);
	stdset_shortcomb_name(comb, pos);

	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_short_nil(pos, comb, &index),
			"qualifiers_position_short_nil1");
	test(index == 1, "qualifiers_position_short_nil2");

	GetConstant(CONSTANT_KEYWORD_AROUND, &pos);
	list_heap(&pos, pos, NULL);
	test(! qualifiers_position_short_nil(pos, comb, &index),
			"qualifiers_position_short_nil3");
	test(index == 0, "qualifiers_position_short_nil4");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &pos);
	list_heap(&pos, pos, NULL);
	test(qualifiers_position_short_nil(pos, comb, &index),
			"qualifiers_position_short_nil5");

	RETURN;
}

static int test_qualifiers_position_nil(void)
{
	addr comb;
	Execute ptr;
	size_t index;

	ptr = Execute_Thread;

	/* standard */
	test(! qualifiers_position_nil(ptr, Nil, Nil, &index),
			"qualifiers_position_nil1");
	test(index == Clos_standard_primary, "qualifiers_position_nil2");

	/* long */
	test_make_instance_longcomb(&comb);
	test(! qualifiers_position_nil(ptr, Nil, comb, &index),
			"qualifiers_position_nil3");
	test(index == 4, "qualifiers_position_nil4");

	RETURN;
}

static int test_qualifiers_position(void)
{
	addr comb, pos;
	Execute ptr;
	size_t index;

	ptr = Execute_Thread;
	test_make_instance_shortcomb(&comb);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	stdset_shortcomb_name(comb, pos);
	list_heap(&pos, pos, NULL);
	qualifiers_position(ptr, pos, comb, &index);
	test(index == 1, "qualifiers_position1");

	RETURN;
}


#if 0
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

static int test_qualifier_check(constindex index, addr one)
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

	test_standard_qualifiers(&right);
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
	/* qualifiers-check */
	TestBreak(test_qualifiers_equal_list);
	TestBreak(test_qualifiers_equal_symbol);
	TestBreak(test_qualifiers_equal);
	TestBreak(test_check_qualifiers_equal_standard);
	TestBreak(test_check_qualifiers_equal_long);
	TestBreak(test_check_qualifiers_equal_short);
	TestBreak(test_check_qualifiers_equal);
	/* qualifiers-position */
	TestBreak(test_method_combination_qualifiers_count);
	TestBreak(test_qualifiers_position_standard_nil);
	TestBreak(test_qualifiers_position_long_nil);
	TestBreak(test_qualifiers_position_short_nil);
	TestBreak(test_qualifiers_position_nil);
	TestBreak(test_qualifiers_position);
#if 0
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
		lisp_initialize = 1;
		result = testbreak_clos_combination();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

