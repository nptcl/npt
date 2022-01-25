#include "type_parse.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "control_operator.h"
#include "declare.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
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

static void test_parse_type_error(addr *ret, addr pos)
{
	if (parse_type_(Execute_Thread, ret, pos, Nil)) {
		Error(fmte_("parse-type error.", NULL));
	}
}

static void test_parse_char(addr *ret, const char *str)
{
	test_parse_type_error(ret, readr_debug(str));
}


/*
 *  Compound-type
 */
static int test_typelist_and(void)
{
	addr pos, check;

	test_parse_char(&pos, "(and)");
	test(test_typecheck(pos, LISPDECL_AND, 1), "typelist_and1");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "typelist_and2");
	test(lenarrayr(pos) == 0, "typelist_and3");

	test_parse_char(&pos, "(and string cons fixnum)");
	test(test_typecheck(pos, LISPDECL_AND, 1), "typelist_and4");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "typelist_and5");
	test(lenarrayr(pos) == 3, "typelist_and6");

	GetArrayA4(pos, 0, &check);
	test(test_typecheck(check, LISPDECL_STRING, 1), "typelist_and7");
	GetArrayA4(pos, 1, &check);
	test(test_typecheck(check, LISPDECL_CONS, 2), "typelist_and8");
	GetArrayA4(pos, 2, &check);
	test(test_typecheck(check, LISPDECL_FIXNUM, 0), "typelist_and9");

	RETURN;
}

static int test_typelist_or(void)
{
	addr pos, check;

	test_parse_char(&pos, "(or)");
	test(test_typecheck(pos, LISPDECL_OR, 1), "typelist_or1");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "typelist_or2");
	test(lenarrayr(pos) == 0, "typelist_or3");

	test_parse_char(&pos, "(or fixnum string)");
	test(test_typecheck(pos, LISPDECL_OR, 1), "typelist_or2");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "typelist_or3");
	GetArrayA4(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "typelist_or4");
	GetArrayA4(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_STRING), "typelist_or6");

	RETURN;
}

static int test_typelist_eql(void)
{
	addr pos;

	test_parse_char(&pos, "(eql t)");
	test(test_typecheck(pos, LISPDECL_EQL, 1), "typelist_eql1");
	GetArrayType(pos, 0, &pos);
	test(pos == T, "typelist_eql2");

	RETURN;
}

static int test_typelist_member(void)
{
	addr pos, check;

	test_parse_char(&pos, "(member 100 t)");
	test(test_typecheck(pos, LISPDECL_MEMBER, 1), "typelist_member1");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "typelist_member2");
	GetArrayA4(pos, 0, &check);
	test(RefFixnum(check) == 100, "typelist_member3");
	GetArrayA4(pos, 1, &check);
	test(check == T, "typelist_member4");

	RETURN;
}

static int test_typelist_mod(void)
{
	addr pos, check;

	test_parse_char(&pos, "(mod 100)");
	test(test_typecheck(pos, LISPDECL_MOD, 1), "typelist_mod1");
	GetArrayType(pos, 0, &check);
	test(RefFixnum(check) == 100, "typelist_mod2");

	RETURN;
}

static int test_typelist_not(void)
{
	addr pos;

	test_parse_char(&pos, "(not bignum)");
	test(test_typecheck(pos, LISPDECL_NOT, 1), "typelist_not1");
	GetArrayType(pos, 0, &pos);
	test(test_typeonly(pos, LISPDECL_BIGNUM), "typelist_not2");

	RETURN;
}

static int test_typelist_satisfies(void)
{
	addr pos;

	test_parse_char(&pos, "(satisfies hello)");
	test(test_typecheck(pos, LISPDECL_SATISFIES, 1), "typelist_satisfies1");
	GetArrayType(pos, 0, &pos);
	test(pos == readr_debug("hello"), "typelist_satisfies2");

	RETURN;
}


/*
 *  Atomic-type
 */
static int test_typelist_cons(void)
{
	addr pos, check;

	test_parse_char(&pos, "cons");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons1");
	test(lenarrayr(pos) == 2, "typelist_cons2");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_cons3");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_cons4");

	test_parse_char(&pos, "(cons)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons5");
	test(lenarrayr(pos) == 2, "typelist_cons6");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_cons7");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_cons8");

	test_parse_char(&pos, "(cons *)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons9");
	test(lenarrayr(pos) == 2, "typelist_cons10");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_cons11");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_cons12");

	test_parse_char(&pos, "(cons * *)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons13");
	test(lenarrayr(pos) == 2, "typelist_cons14");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_cons15");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_cons16");

	test_parse_char(&pos, "(cons fixnum)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons17");
	test(lenarrayr(pos) == 2, "typelist_cons18");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "typelist_cons19");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_cons20");

	test_parse_char(&pos, "(cons fixnum *)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons21");
	test(lenarrayr(pos) == 2, "typelist_cons22");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "typelist_cons23");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_cons24");

	test_parse_char(&pos, "(cons fixnum integer)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons25");
	test(lenarrayr(pos) == 2, "typelist_cons26");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_FIXNUM), "typelist_cons27");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_INTEGER), "typelist_cons28");

	test_parse_char(&pos, "(cons * integer)");
	test(test_typecheck(pos, LISPDECL_CONS, 2), "typelist_cons29");
	test(lenarrayr(pos) == 2, "typelist_cons30");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_cons31");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_INTEGER), "typelist_cons32");

	RETURN;
}

static int test_type_function_lambda_var(void)
{
	int check;
	addr pos, left, right;

	check = type_function_lambda(Execute_Thread, &pos, Nil, Nil);
	test(check == 0, "type_function_lambda_var1");
	GetArrayA2(pos, 0, &pos);
	test(pos == Nil, "type_function_lambda_var2");

	pos = readr_debug("(fixnum string)");
	check = type_function_lambda(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_lambda_var3");
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_var4");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_lambda_var5");
	test(right == Nil, "type_function_lambda_var6");

	RETURN;
}

static int test_type_function_lambda_opt(void)
{
	int check;
	addr pos, left, right;

	pos = readr_debug("(&optional fixnum string)");
	check = type_function_lambda(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_lambda_opt1");
	GetArrayA2(pos, 0, &right);
	test(right == Nil, "type_function_lambda_opt2");
	GetArrayA2(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_opt3");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_lambda_opt4");
	test(right == Nil, "type_function_lambda_opt5");

	pos = readr_debug("(fixnum &optional string)");
	check = type_function_lambda(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_lambda_opt6");
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_opt7");
	test(right == Nil, "type_function_lambda_opt8");
	GetArrayA2(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_lambda_opt9");
	test(right == Nil, "type_function_lambda_opt10");

	RETURN;
}

static int test_type_function_lambda_rest(void)
{
	int check;
	addr pos, left, right;

	pos = readr_debug("(fixnum &rest string)");
	check = type_function_lambda(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_lambda_rest1");
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_lambda_rest2");
	test(right == Nil, "type_function_lambda_rest3");
	GetArrayA2(pos, 1, &right);
	test(right == Nil, "type_function_lambda_rest4");
	GetArrayA2(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_STRING), "type_function_lambda_rest5");

	pos = readr_debug("(&rest nil)");
	check = type_function_lambda(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_lambda_rest6");
	GetArrayA2(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_NIL), "type_function_lambda_rest7");

	RETURN;
}

static int test_type_function_lambda_key(void)
{
	int check;
	addr pos, left, right;

	pos = readr_debug("(&key (hello fixnum))");
	check = type_function_lambda(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_lambda_key1");
	GetArrayA2(pos, 3, &right);
	GetCons(right, &left, &right);
	test(right == Nil, "type_function_lambda_key2");
	GetCons(left, &left, &right);
	test(left == readr_debug("hello"), "type_function_lambda_key3");
	test(test_typeonly(right, LISPDECL_FIXNUM), "type_function_lambda_key4");

	RETURN;
}

static int test_type_function_list(void)
{
	int check;
	addr pos, left, right;

	pos = readr_debug("*");
	check = type_function_list(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_list1");
	test(type_asterisk_p(pos), "type_function_list2");

	pos = readr_debug("(fixnum string)");
	check = type_function_list(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "type_function_list3");
	GetArrayA2(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_function_list4");
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_function_list5");
	test(right == Nil, "type_function_list6");

	RETURN;
}

static void test_parse_values(addr *ret, const char *str)
{
	if (type_function_values(Execute_Thread, ret, readr_debug(str), Nil)) {
		Error(fmte_("type_function_values error.", NULL));
	}
}

static int test_type_values_var(void)
{
	addr pos, check;


	test_parse_values(&pos, "(values)");
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_var1");
	GetArrayType(pos, 0, &pos);
	test(pos == Nil, "type_values_var2");

	test_parse_values(&pos, "(values fixnum string)");
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

static int test_type_values_opt(void)
{
	addr pos, left, right;

	test_parse_values(&pos, "(values fixnum &optional string)");
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_opt1");
	GetArrayType(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_values_opt2");
	test(right == Nil, "type_values_opt3");
	GetArrayType(pos, 1, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_STRING), "type_values_opt4");
	test(right == Nil, "type_values_opt5");

	test_parse_values(&pos, "(values &optional fixnum string)");
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

static int test_type_values_rest(void)
{
	addr pos, left, right;

	test_parse_values(&pos, "(values fixnum &rest list)");
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_rest1");
	GetArrayType(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_values_rest2");
	test(right == Nil, "type_values_rest3");
	GetArrayType(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_LIST), "type_values_rest4");

	test_parse_values(&pos, "(values &rest list)");
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_values_rest5");
	GetArrayType(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_LIST), "type_values_rest6");

	RETURN;
}

static int test_type_values(void)
{
	addr pos, left, right;

	test_parse_values(&pos, "(values fixnum)");
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "type_type_values1");
	GetArrayType(pos, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "type_type_values2");
	test(right == Nil, "type_type_values3");
	GetArrayType(pos, 2, &right);
	test(test_typeonly(right, LISPDECL_T), "type_type_values4");

	RETURN;
}

static int test_typelist_function(void)
{
	addr pos, check, left, right;

	test_parse_char(&pos, "(function)");
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "typelist_function1");
	test(lenarrayr(pos) == 3, "typelist_function2");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_function3");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_function4");

	test_parse_char(&pos, "(function *)");
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "typelist_function5");
	test(lenarrayr(pos) == 3, "typelist_function6");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_function7");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_function8");

	test_parse_char(&pos, "(function * *)");
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "typelist_function9");
	test(lenarrayr(pos) == 3, "typelist_function10");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_function11");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_function12");

	test_parse_char(&pos, "(function (fixnum))");
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "typelist_function13");
	test(lenarrayr(pos) == 3, "typelist_function14");
	GetArrayType(pos, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "typelist_function15");
	getarray(check, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "typelist_function16");
	test(right == Nil, "typelist_function17");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_function18");

	test_parse_char(&pos, "(function (fixnum) *)");
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "typelist_function19");
	test(lenarrayr(pos) == 3, "typelist_function20");
	GetArrayType(pos, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "typelist_function21");
	getarray(check, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "typelist_function22");
	test(right == Nil, "typelist_function23");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_function24");

	test_parse_char(&pos, "(function (fixnum) (values))");
	test(test_typecheck(pos, LISPDECL_FUNCTION, 3), "typelist_function25");
	test(lenarrayr(pos) == 3, "typelist_function26");
	GetArrayType(pos, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "typelist_function27");
	getarray(check, 0, &right);
	GetCons(right, &left, &right);
	test(test_typeonly(left, LISPDECL_FIXNUM), "typelist_function28");
	test(right == Nil, "typelist_function29");
	GetArrayType(pos, 1, &check);
	test(test_typeonly(check, LISPDECL_VALUES), "typelist_function30");

	RETURN;
}

static int test_parse_array_length(void)
{
	int check;
	addr list;
	size_t size;

	list = readr_debug("(*)");
	size = 0;
	test(! parse_array_length(list, &size, &check), "parse_array_length1");
	test(check, "parse_array_length2");
	test(size == 1, "parse_array_length3");

	list = readr_debug("(10)");
	test(! parse_array_length(list, &size, &check), "parse_array_length4");
	test(! check, "parse_array_length5");

	list = readr_debug("(* * * *)");
	test(! parse_array_length(list, &size, &check), "parse_array_length6");
	test(check, "parse_array_length7");
	test(size == 4, "parse_array_length8");

	list = readr_debug("(* * 10 * *)");
	test(! parse_array_length(list, &size, &check), "parse_array_length9");
	test(! check, "parse_array_length10");

	RETURN;
}

static int test_parse_array_fixnum_check(void)
{
	addr pos;

	fixnum_heap(&pos, 0);
	parse_array_fixnum_check(&pos, pos);
	test(fixnump(pos), "parse_array_fixnum_check1");

	fixnum_heap(&pos, 1);
	parse_array_fixnum_check(&pos, pos);
	test(fixnump(pos), "parse_array_fixnum_check2");

	fixnum_heap(&pos, 100);
	parse_array_fixnum_check(&pos, pos);
	test(fixnump(pos), "parse_array_fixnum_check3");

	RETURN;
}

static int test_parse_array_dimension(void)
{
	addr pos, check;

	pos = readr_debug("(* 10 20)");
	parse_array_dimension(&pos, pos);
	getarray(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_ASTERISK), "parse_array_dimension1");
	getarray(pos, 1, &check);
	test(RefFixnum(check) == 10, "parse_array_dimension2");
	getarray(pos, 2, &check);
	test(RefFixnum(check) == 20, "parse_array_dimension3");
	test(lenarrayr(pos) == 3, "parse_array_dimension4");

	RETURN;
}

static int test_parse_array_second(void)
{
	addr pos, check;

	parse_array_second(&pos, Nil);
	test(RefFixnum(pos) == 0, "parse_array_second1");

	pos = readr_debug("3");
	parse_array_second(&pos, pos);
	test(RefFixnum(pos) == 3, "parse_array_second2");

	pos = readr_debug("(* * * * *)");
	parse_array_second(&pos, pos);
	test(RefFixnum(pos) == 5, "parse_array_second3");

	pos = readr_debug("(12 34)");
	parse_array_second(&pos, pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "parse_array_second4");
	test(lenarrayr(pos) == 2, "parse_array_second5");
	getarray(pos, 0, &check);
	test(RefFixnum(check) == 12, "parse_array_second6");
	getarray(pos, 1, &check);
	test(RefFixnum(check) == 34, "parse_array_second7");

	RETURN;
}

static int test_typelist_array(void)
{
	addr pos, check;

	test_parse_char(&pos, "(array)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array1");
	test(lenarrayr(pos) == 2, "typelist_array2");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_array3");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_array4");

	test_parse_char(&pos, "(array *)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array5");
	test(lenarrayr(pos) == 2, "typelist_array6");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_array7");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_array8");

	test_parse_char(&pos, "(array * *)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array9");
	test(lenarrayr(pos) == 2, "typelist_array10");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_array11");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_array12");

	test_parse_char(&pos, "(array fixnum)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array13");
	test(lenarrayr(pos) == 2, "typelist_array14");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_SIGNED_BYTE), "typelist_array15");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_array16");

	test_parse_char(&pos, "(array fixnum *)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array17");
	test(lenarrayr(pos) == 2, "typelist_array18");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_SIGNED_BYTE), "typelist_array19");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_array20");

	test_parse_char(&pos, "(array fixnum 44)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array21");
	test(lenarrayr(pos) == 2, "typelist_array22");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_SIGNED_BYTE), "typelist_array23");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 44, "typelist_array24");

	test_parse_char(&pos, "(array * 44)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array25");
	test(lenarrayr(pos) == 2, "typelist_array26");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_array27");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 44, "typelist_array28");

	test_parse_char(&pos, "(array t 44)");
	test(test_typecheck(pos, LISPDECL_ARRAY, 2), "typelist_array29");
	test(lenarrayr(pos) == 2, "typelist_array30");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_T), "typelist_array31");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 44, "typelist_array32");

	RETURN;
}

static int test_typelist_simple_array(void)
{
	addr pos, check;

	test_parse_char(&pos, "(simple-array)");
	test(test_typecheck(pos, LISPDECL_SIMPLE_ARRAY, 2), "typelist_simple_array1");
	test(lenarrayr(pos) == 2, "typelist_simple_array2");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_simple_array3");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_simple_array4");

	RETURN;
}

static int test_typelist_vector(void)
{
	addr pos, check;

	test_parse_char(&pos, "(vector)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector1");
	test(lenarrayr(pos) == 2, "typelist_vector2");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_vector3");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_vector4");

	test_parse_char(&pos, "(vector *)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector5");
	test(lenarrayr(pos) == 2, "typelist_vector6");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_vector7");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_vector8");

	test_parse_char(&pos, "(vector * *)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector9");
	test(lenarrayr(pos) == 2, "typelist_vector10");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_vector11");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_vector12");

	test_parse_char(&pos, "(vector fixnum)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector13");
	test(lenarrayr(pos) == 2, "typelist_vector14");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_SIGNED_BYTE), "typelist_vector15");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_vector16");

	test_parse_char(&pos, "(vector fixnum *)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector17");
	test(lenarrayr(pos) == 2, "typelist_vector18");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_SIGNED_BYTE), "typelist_vector19");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "typelist_vector20");

	test_parse_char(&pos, "(vector fixnum 44)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector21");
	test(lenarrayr(pos) == 2, "typelist_vector22");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_SIGNED_BYTE), "typelist_vector23");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 44, "typelist_vector24");

	test_parse_char(&pos, "(vector * 44)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector25");
	test(lenarrayr(pos) == 2, "typelist_vector26");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_vector27");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 44, "typelist_vector28");

	test_parse_char(&pos, "(vector t 44)");
	test(test_typecheck(pos, LISPDECL_VECTOR, 2), "typelist_vector29");
	test(lenarrayr(pos) == 2, "typelist_vector30");
	GetArrayType(pos, 0, &check);
	test(test_typeonly(check, LISPDECL_T), "typelist_vector31");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 44, "typelist_vector32");

	RETURN;
}

static int test_typelist_simple_vector(void)
{
	addr pos, check;

	test_parse_char(&pos, "(simple-vector)");
	test(test_typecheck(pos, LISPDECL_SIMPLE_VECTOR, 1), "typelist_simple_vector1");
	test(lenarrayr(pos) == 1, "typelist_simple_vector2");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_simple_vector3");

	test_parse_char(&pos, "(simple-vector *)");
	test(test_typecheck(pos, LISPDECL_SIMPLE_VECTOR, 1), "typelist_simple_vector4");
	test(lenarrayr(pos) == 1, "typelist_simple_vector5");
	GetArrayType(pos, 0, &check);
	test(type_asterisk_p(check), "typelist_simple_vector6");

	test_parse_char(&pos, "(simple-vector 0)");
	test(test_typecheck(pos, LISPDECL_SIMPLE_VECTOR, 1), "typelist_simple_vector7");
	test(lenarrayr(pos) == 1, "typelist_simple_vector8");
	GetArrayType(pos, 0, &check);
	test(RefFixnum(check) == 0, "typelist_simple_vector9");

	RETURN;
}

static int typelist_size_check(const char *name, enum LISPDECL type)
{
	char data[256];
	addr pos, check;

	snprintf(data, 256, "(%s 10)", name);
	test_parse_char(&pos, data);
	if (! test_typecheck(pos, type, 1)) {
		degrade_printf("typecheck error.\n");
		return 0;
	}
	if (lenarrayr(pos) != 1) {
		degrade_printf("lenarray error.\n");
		return 0;
	}
	GetArrayType(pos, 0, &check);
	if (RefFixnum(check) != 10) {
		degrade_printf("fixnum error.\n");
		return 0;
	}

	return 1;
}

#define TypeListSize(a,b,c) test(typelist_size_check(a, b), "typelist-size." c)
static int test_typelist_size(void)
{
	TypeListSize("SIMPLE-VECTOR",
			LISPDECL_SIMPLE_VECTOR, "simple-vector");
	TypeListSize("BIT-VECTOR",
			LISPDECL_BIT_VECTOR, "bit-vector");
	TypeListSize("SIMPLE-BIT-VECTOR",
			LISPDECL_SIMPLE_BIT_VECTOR, "simple-bit-vector");
	TypeListSize("STRING",
			LISPDECL_STRING, "string");
	TypeListSize("BASE-STRING",
			LISPDECL_BASE_STRING, "base-string");
	TypeListSize("SIMPLE-STRING",
			LISPDECL_SIMPLE_STRING, "simple-string");
	TypeListSize("SIMPLE-BASE-STRING",
			LISPDECL_SIMPLE_BASE_STRING, "simple-base-string");

	RETURN;
}
#undef TypeListSize

static int test_type_range_element(void)
{
	addr a, b, x, y;

	a = readr_debug("hello");
	fixnum_heap(&b, 100);
	type_range_element(a, b, typelist_real_p, &x, &y);
	test(x == Nil, "type_range_element1");
	test(y == b, "type_range_element2");

	list_heap(&y, b, NULL);
	type_range_element(a, y, typelist_real_p, &x, &y);
	test(x == T, "type_range_element3");
	test(y == b, "type_range_element4");

	RETURN;
}

static int test_typelist_range(void)
{
	addr pos;

	test_parse_char(&pos, "(real)");
	test(test_typecheck(pos, LISPDECL_REAL, 4), "typelist_range1");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_range2");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_range3");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_range4");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_range5");

	test_parse_char(&pos, "(rational *)");
	test(test_typecheck(pos, LISPDECL_RATIONAL, 4), "typelist_range6");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_range7");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_range8");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_range9");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_range10");

	test_parse_char(&pos, "(integer * *)");
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "typelist_range11");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_range12");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_range13");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_range14");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_range15");

	test_parse_char(&pos, "(real 100)");
	test(test_typecheck(pos, LISPDECL_REAL, 4), "typelist_range16");
	test(RefArrayType(pos, 0) == Nil, "typelist_range17");
	test(RefFixnum(RefArrayType(pos, 1)) == 100, "typelist_range18");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_range19");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_range20");

	test_parse_char(&pos, "(real (100) *)");
	test(test_typecheck(pos, LISPDECL_REAL, 4), "typelist_range21");
	test(RefArrayType(pos, 0) == T, "typelist_range22");
	test(RefFixnum(RefArrayType(pos, 1)) == 100, "typelist_range23");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_range24");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_range25");

	test_parse_char(&pos, "(real (100) 200)");
	test(test_typecheck(pos, LISPDECL_REAL, 4), "typelist_range26");
	test(RefArrayType(pos, 0) == T, "typelist_range27");
	test(RefFixnum(RefArrayType(pos, 1)) == 100, "typelist_range28");
	test(RefArrayType(pos, 2) == Nil, "typelist_range29");
	test(RefFixnum(RefArrayType(pos, 3)) == 200, "typelist_range30");

	test_parse_char(&pos, "(real * (200))");
	test(test_typecheck(pos, LISPDECL_REAL, 4), "typelist_range31");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_range32");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_range33");
	test(RefArrayType(pos, 2) == T, "typelist_range34");
	test(RefFixnum(RefArrayType(pos, 3)) == 200, "typelist_range35");

	RETURN;
}

static int test_typelist_float(void)
{
	addr pos;

	test_parse_char(&pos, "(float)");
	test(test_typecheck(pos, LISPDECL_FLOAT, 4), "typelist_float1");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_float2");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_float3");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_float4");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_float5");

	test_parse_char(&pos, "(short-float *)");
	test(test_typecheck(pos, LISPDECL_SHORT_FLOAT, 4), "typelist_float6");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_float7");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_float8");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_float9");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_float10");

	test_parse_char(&pos, "(single-float * *)");
	test(test_typecheck(pos, LISPDECL_SINGLE_FLOAT, 4), "typelist_float11");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_float12");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_float13");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_float14");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_float15");

	test_parse_char(&pos, "(double-float 100.0d0 *)");
	test(test_typecheck(pos, LISPDECL_DOUBLE_FLOAT, 4), "typelist_float16");
	test(RefArrayType(pos, 0) == Nil, "typelist_float17");
	test(RefDoubleFloat(RefArrayType(pos, 1)) == 100.0, "typelist_float18");
	test(type_asterisk_p(RefArrayType(pos, 2)), "typelist_float19");
	test(type_asterisk_p(RefArrayType(pos, 3)), "typelist_float20");

	test_parse_char(&pos, "(long-float * (100.0L0))");
	test(test_typecheck(pos, LISPDECL_LONG_FLOAT, 4), "typelist_float21");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_float22");
	test(type_asterisk_p(RefArrayType(pos, 1)), "typelist_float23");
	test(RefArrayType(pos, 2) == T, "typelist_float24");
	test(RefLongFloat(RefArrayType(pos, 3)) == 100.0L, "typelist_float25");

	RETURN;
}

static int test_typelist_byte(void)
{
	addr pos;

	test_parse_char(&pos, "(signed-byte)");
	test(test_typecheck(pos, LISPDECL_SIGNED_BYTE, 1), "typelist_byte1");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_byte2");

	test_parse_char(&pos, "(unsigned-byte *)");
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "typelist_byte3");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_byte4");

	test_parse_char(&pos, "(unsigned-byte 111)");
	test(test_typecheck(pos, LISPDECL_UNSIGNED_BYTE, 1), "typelist_byte5");
	test(RefFixnum(RefArrayType(pos, 0)) == 111, "typelist_byte6");

	RETURN;
}

static int test_typelist_complex(void)
{
	addr pos;

	test_parse_char(&pos, "(complex)");
	test(test_typecheck(pos, LISPDECL_COMPLEX, 1), "typelist_complex1");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_complex2");

	test_parse_char(&pos, "(complex *)");
	test(test_typecheck(pos, LISPDECL_COMPLEX, 1), "typelist_complex3");
	test(type_asterisk_p(RefArrayType(pos, 0)), "typelist_complex4");

	test_parse_char(&pos, "(complex integer)");
	test(test_typecheck(pos, LISPDECL_COMPLEX, 1), "typelist_complex5");
	GetArrayType(pos, 0, &pos);
	test(test_typeonly(pos, LISPDECL_INTEGER), "typelist_complex6");

	RETURN;
}


/*
 *  parse-type
 */
static int test_parse_type_default(void)
{
	int check;
	addr symbol, args, pos;

	symbol = readr_debug("integer");
	args = readr_debug("(10 (20))");
	check = parse_type_default(Execute_Thread, &pos, symbol, args, Nil);
	test(check == 0, "parse_type_default1");
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "parse_type_default2");

	symbol = readr_debug("hello");
	args = readr_debug("(10 (20))");
	check = parse_type_default(Execute_Thread, &pos, symbol, args, Nil);
	test(check == 0, "parse_type_default3");
	test(pos == NULL, "parse_type_default4");

	RETURN;
}

static int test_parse_type_list_call(Execute ptr, addr args)
{
	GetConst(COMMON_KEYWORD, &args);
	setresult_control(ptr, args);
	return 0;
}

static int test_parse_type_list(void)
{
	int check;
	addr pos, symbol;

	/* list */
	pos = readr_debug("(integer 10 20)");
	check = parse_type_list(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_list1");
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "parse_type_list2");

	/* deftype */
	compiled_system(&pos, Nil);
	SetPointer(p_debug1, dynamic, test_parse_type_list_call);
	setcompiled_dynamic(pos, p_debug1);
	symbol = readr_debug("test-parse-type-list");
	setdeftype_symbol_(symbol, pos);
	pos = readr_debug("(test-parse-type-list)");
	check = parse_type_list(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_list3");
	test(test_typecheck(pos, LISPDECL_KEYWORD, 0), "parse_type_list4");

	/* error */
	pos = readr_debug("(no-such-lisp-type 10 20)");
	check = parse_type_list(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_list5");
	test(type_delay_p(pos), "parse_type_list6");

	RETURN;
}

static int test_parse_type_symbol(void)
{
	int check;
	addr pos;

	pos = readr_debug("symbol");
	check = parse_type_symbol(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_symbol1");
	test(test_typecheck(pos, LISPDECL_SYMBOL, 0), "parse_type_symbol2");

	pos = readr_debug("no-such-lisp-type");
	check = parse_type_symbol(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_symbol3");
	test(type_delay_p(pos), "parse_type_symbol4");

	RETURN;
}

static int test_parse_type_null(void)
{
	int check;
	addr pos;

	pos = readr_debug("symbol");
	check = parse_type_null(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_null1");
	test(test_typecheck(pos, LISPDECL_SYMBOL, 0), "parse_type_null2");

	pos = readr_debug("no-such-lisp-type");
	check = parse_type_null(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_null3");
	test(type_delay_p(pos), "parse_type_null4");

	pos = readr_debug("(integer 10 20)");
	check = parse_type_null(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_null5");
	test(test_typecheck(pos, LISPDECL_INTEGER, 4), "parse_type_null6");

	pos = readr_debug("(no-such-lisp-type 10 20)");
	check = parse_type_null(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_null7");
	test(type_delay_p(pos), "parse_type_null8");

	type0_heap(LISPDECL_KEYWORD, &pos);
	check = parse_type_null(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_null9");
	test(test_typecheck(pos, LISPDECL_KEYWORD, 0), "parse_type_null10");

	RETURN;
}

static int test_parse_type(void)
{
	int check;
	addr pos;

	pos = readr_debug("symbol");
	check = parse_type_(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type1");
	test(test_typecheck(pos, LISPDECL_SYMBOL, 0), "parse_type2");

	RETURN;
}

static int test_parse_type_not(void)
{
	int check;
	addr pos;

	pos = readr_debug("symbol");
	check = parse_type_not_(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_not1");
	test(GetType(pos) == LISPTYPE_TYPE, "parse_type_not2");
	test(RefLispDecl(pos) == LISPDECL_SYMBOL, "parse_type_not3");
	test(RefNotDecl(pos), "parse_type_not4");

	RETURN;
}

static int test_parse_type_noaster(void)
{
	int check;
	addr pos;

	pos = readr_debug("symbol");
	check = parse_type_noaster_(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_noaster1");
	test(test_typecheck(pos, LISPDECL_SYMBOL, 0), "parse_type_noaster2");

	RETURN;
}

static int test_parse_type_values(void)
{
	int check;
	addr pos;

	pos = readr_debug("(values fixnum)");
	check = parse_type_values_(Execute_Thread, &pos, pos, Nil);
	test(check == 0, "parse_type_values1");
	test(test_typecheck(pos, LISPDECL_VALUES, 4), "parse_type_values2");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_parse(void)
{
	/* Compound-type */
	TestBreak(test_typelist_and);
	TestBreak(test_typelist_or);
	TestBreak(test_typelist_eql);
	TestBreak(test_typelist_member);
	TestBreak(test_typelist_mod);
	TestBreak(test_typelist_not);
	TestBreak(test_typelist_satisfies);

	/* Atomic-type */
	TestBreak(test_typelist_cons);
	TestBreak(test_type_function_lambda_var);
	TestBreak(test_type_function_lambda_opt);
	TestBreak(test_type_function_lambda_rest);
	TestBreak(test_type_function_lambda_key);
	TestBreak(test_type_function_list);
	TestBreak(test_type_values_var);
	TestBreak(test_type_values_opt);
	TestBreak(test_type_values_rest);
	TestBreak(test_type_values);
	TestBreak(test_typelist_function);
	TestBreak(test_parse_array_length);
	TestBreak(test_parse_array_fixnum_check);
	TestBreak(test_parse_array_dimension);
	TestBreak(test_parse_array_second);
	TestBreak(test_typelist_array);
	TestBreak(test_typelist_simple_array);
	TestBreak(test_typelist_vector);
	TestBreak(test_typelist_simple_vector);
	TestBreak(test_typelist_size);
	TestBreak(test_type_range_element);
	TestBreak(test_typelist_range);
	TestBreak(test_typelist_float);
	TestBreak(test_typelist_byte);
	TestBreak(test_typelist_complex);

	/* parse-type */
	TestBreak(test_parse_type_default);
	TestBreak(test_parse_type_list);
	TestBreak(test_parse_type_symbol);
	TestBreak(test_parse_type_null);
	TestBreak(test_parse_type);
	TestBreak(test_parse_type_not);
	TestBreak(test_parse_type_noaster);
	TestBreak(test_parse_type_values);

	return 0;
}

static void testinit_type_parse(Execute ptr)
{
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
	build_syscall();
	build_common();
	build_reader();
	build_pathname();
	build_declare();
}

int test_type_parse(void)
{
	DegradeTitle;
	return DegradeCode(type_parse);
}

