#include "lambda.c"
#include "clos.h"
#include "character.h"
#include "code.h"
#include "condition.h"
#include "common.h"
#include "control.h"
#include "degrade.h"
#include "function.h"
#include "object.h"
#include "package_import.h"
#include "pathname.h"
#include "print.h"
#include "reader.h"
#include "stream.h"
#include "strtype.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_constant_eq(void)
{
	addr symbol;

	internchar_debug("COMMON-LISP", "&OPTIONAL", &symbol);
	test(constant_eq(CONSTANT_AMPERSAND_OPTIONAL, symbol), "constant_eq1");
	internchar_debug("KEYWORD", "TEST", &symbol);
	test(! constant_eq(CONSTANT_AMPERSAND_OPTIONAL, symbol), "constant_eq2");

	RETURN;
}

static int test_member_ampersand(void)
{
	addr symbol;

	internchar_debug("COMMON-LISP", "&OPTIONAL", &symbol);
	test(member_ampersand(symbol, AMPERSAND_GENERIC), "member_ampersand1");
	test(member_ampersand(symbol, AMPERSAND_ORDINARY), "member_ampersand2");
	internchar_debug("COMMON-LISP", "&ALLOW-OTHER-KEYS", &symbol);
	test(member_ampersand(symbol, AMPERSAND_GENERIC), "member_ampersand3");
	test(member_ampersand(symbol, AMPERSAND_ORDINARY), "member_ampersand4");
	internchar_debug("COMMON-LISP", "&AUX", &symbol);
	test(! member_ampersand(symbol, AMPERSAND_GENERIC), "member_ampersand5");
	test(member_ampersand(symbol, AMPERSAND_ORDINARY), "member_ampersand6");
	internchar_debug("COMMON-LISP", "&WHOLE", &symbol);
	test(! member_ampersand(symbol, AMPERSAND_ORDINARY), "member_ampersand7");
	test(member_ampersand(symbol, AMPERSAND_METHOD_COMBINATION),
			"member_ampersand8");
	internchar_debug("COMMON-LISP", "&BODY", &symbol);
	test(! member_ampersand(symbol, AMPERSAND_METHOD_COMBINATION),
			"member_ampersand9");
	test(member_ampersand(symbol, AMPERSAND_MACRO), "member_ampersand10");

	RETURN;
}

static int test_variable_check(void)
{
	addr symbol;
	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	Error(variable_check_(symbol, AMPERSAND_ORDINARY));
	test(1, "variable_check1");
	RETURN;
}

static int test_varcons_local(void)
{
	addr cons, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &cons);
	varcons_data(cons, &check);
	test(check == Nil, "varcons_local1");
	pushqueue_local(local, cons, T);
	varcons_data(cons, &check);
	test(GetType(check) == LISPTYPE_CONS, "varcons_local2");
	GetCons(check, &cons, &check);
	test(cons == T, "varcons_local3");
	test(check == Nil, "varcons_local4");
	rollback_local(local, stack);

	RETURN;
}

static int test_push_varcons(void)
{
	addr cons, symbol, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &cons);
	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	push_varcons_(local, cons, symbol, AMPERSAND_ORDINARY);

	varcons_data(cons, &check);
	test(GetType(check) == LISPTYPE_CONS, "push_varcons1");
	GetCons(check, &cons, &check);
	test(cons == symbol, "push_varcons2");
	test(check == Nil, "push_varcons3");
	rollback_local(local, stack);

	RETURN;
}

static int test_push_namecons(void)
{
	addr cons, symbol, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &cons);
	internchar_keyword_debug("HELLO", &symbol);
	push_namecons_(local, cons, symbol);

	varcons_data(cons, &check);
	test(GetType(check) == LISPTYPE_CONS, "push_namecons1");
	GetCons(check, &cons, &check);
	test(cons == symbol, "push_namecons2");
	test(check == Nil, "push_namecons3");
	rollback_local(local, stack);

	RETURN;
}

static int test_make_keyword_from_symbol(void)
{
	addr symbol, check;

	internchar_keyword_debug("HELLO", &symbol);
	make_keyword_from_symbol_(symbol, &check);
	test(symbol == check, "make_keyword_from_symbol1");
	internchar_debug(LISP_COMMON_USER, "HELLO", &check);
	make_keyword_from_symbol_(check, &check);
	test(symbol == check, "make_keyword_from_symbol2");

	RETURN;
}

static int test_list2_check(void)
{
	addr pos, pos1, pos2, value1, value2, value3;

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	pos1 = 0;
	pos2 = 0;
	test(list2_check(value1, &pos1, &pos2), "list2_check1");
	list_heap(&pos, value1, NULL);
	test(list2_check(pos, &pos1, &pos2), "list2_check2");
	list_heap(&pos, value1, value2, NULL);
	test(list2_check(pos, &pos1, &pos2) == 0, "list2_check3");
	test(pos1 == value1, "list2_check4");
	test(pos2 == value2, "list2_check5");
	list_heap(&pos, value1, value2, value3, NULL);
	test(list2_check(pos, &pos1, &pos2), "list2_check6");

	RETURN;
}

static int test_key_name_values(void)
{
	addr pos, pos2, symbol, name;

	internchar_debug(LISP_COMMON_USER, "AAA", &pos);
	key_name_values_(pos, &symbol, &name);
	test(pos == symbol, "key_name_values1");
	internchar_keyword_debug("AAA", &pos);
	test(pos == name, "key_name_values2");

	internchar_keyword_debug("AAA", &pos);
	internchar_debug(LISP_COMMON_USER, "AAA", &pos2);
	list_heap(&symbol, pos, pos2, NULL);  /* (name symbol) */
	key_name_values_(symbol, &symbol, &name);
	test(symbol == pos2, "key_name_values3");
	test(name == pos, "key_name_values4");

	RETURN;
}


/*
 *  lambda-macro
 */
static int test_push_varcons_macro(void)
{
	addr instance, symbol, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &instance);
	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	push_varcons_macro_(local, instance, symbol);
	varcons_data(instance, &cons);
	GetCons(cons, &instance, &cons);
	test(instance == symbol, "push_varcons_macro1");
	test(cons == Nil, "push_varcons_macro2");
	rollback_local(local, stack);

	RETURN;
}

static int test_ordinary_opt(void)
{
	addr pos, cons, value, check, var, init, sup;

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	ordinary_opt_(pos, &var, &init, &sup);
	test(var == pos, "ordinary_opt1");
	test(init == Nil, "ordinary_opt2");
	test(sup == Nil, "ordinary_opt3");

	list_heap(&cons, pos, NULL);
	ordinary_opt_(cons, &var, &init, &sup);
	test(var == pos, "ordinary_opt4");
	test(init == Nil, "ordinary_opt5");
	test(sup == Nil, "ordinary_opt6");

	fixnum_heap(&value, 100);
	list_heap(&cons, pos, value, NULL);
	ordinary_opt_(cons, &var, &init, &sup);
	test(var == pos, "ordinary_opt7");
	test(init == value, "ordinary_opt8");
	test(sup == Nil, "ordinary_opt9");

	internchar_debug(LISP_COMMON_USER, "CHECK", &check);
	list_heap(&cons, pos, value, check, NULL);
	ordinary_opt_(cons, &var, &init, &sup);
	test(var == pos, "ordinary_opt10");
	test(init == value, "ordinary_opt11");
	test(sup == check, "ordinary_opt12");

	RETURN;
}

static int test_ordinary_key(void)
{
	addr instance, pos, var, name, init, sup;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &instance);
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	ordinary_key_(local, instance, pos, &var, &name, &init, &sup);
	test(pos == var, "ordinary_key1");
	internchar_keyword_debug("HELLO", &pos);
	test(pos == name, "ordinary_key2");
	test(init == Nil, "ordinary_key3");
	test(sup == Nil, "ordinary_key4");
	varcons_data(instance, &instance);
	GetCons(instance, &pos, &instance);
	test(pos == name, "ordinary_key5");
	test(instance == Nil, "ordinary_key6");
	rollback_local(local, stack);

	RETURN;
}

static int test_ordinary_aux(void)
{
	addr pos, cons, value, var, init;

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	ordinary_aux_(pos, &var, &init);
	test(var == pos, "ordinary_aux1");
	test(init == Nil, "ordinary_aux2");

	list_heap(&cons, pos, NULL);
	ordinary_aux_(cons, &var, &init);
	test(var == pos, "ordinary_aux3");
	test(init == Nil, "ordinary_aux4");

	fixnum_heap(&value, 100);
	list_heap(&cons, pos, value, NULL);
	ordinary_aux_(cons, &var, &init);
	test(var == pos, "ordinary_aux5");
	test(init == value, "ordinary_aux6");

	RETURN;
}


/*
 *  lambda-ordinary
 */
static int test_push_varcons_ordinary(void)
{
	addr instance, symbol, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &instance);
	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	push_varcons_ordinary_(local, instance, symbol);
	varcons_data(instance, &cons);
	GetCons(cons, &instance, &cons);
	test(instance == symbol, "push_varcons_ordinary1");
	test(cons == Nil, "push_varcons_ordinary2");
	rollback_local(local, stack);

	RETURN;
}


/*
 *  lambda-generic-function
 */
static int test_generic_function_key_cons(void)
{
	addr pos, check, cons, symbol, name;

	/* symbol */
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	generic_function_key_cons_(pos, &symbol, &name);
	test(symbol == pos, "generic_function_key_cons1");
	internchar_keyword_debug("HELLO", &check);
	test(name == check, "generic_function_key_cons2");

	/* (symbol) */
	list_heap(&cons, pos, NULL);
	generic_function_key_cons_(cons, &symbol, &name);
	test(symbol == pos, "generic_function_key_cons3");
	test(name == check, "generic_function_key_cons4");

	/* ((name symbol)) */
	internchar_debug(LISP_COMMON_USER, "AAA", &check);
	list_heap(&cons, check, pos, NULL);
	list_heap(&cons, cons, NULL);
	generic_function_key_cons_(cons, &symbol, &name);
	test(symbol == pos, "generic_function_key_cons5");
	test(name == check, "generic_function_key_cons6");

	RETURN;
}

static int test_generic_function_key(void)
{
	addr pos, check, cons, symbol, name;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &cons);

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	generic_function_key_(local, cons, pos, &symbol, &name);
	test(symbol == pos, "generic_function_key1");
	internchar_keyword_debug("HELLO", &check);
	test(name == check, "generic_function_key2");
	varcons_data(cons, &cons);
	GetCons(cons, &check, &cons);
	test(name == check, "generic_function_key3");
	test(cons == Nil, "generic_function_key4");
	rollback_local(local, stack);

	RETURN;
}

static int test_push_varcons_generic_function(void)
{
	addr pos, cons, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	varcons_local(local, &cons);
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	push_varcons_generic_function_(local, cons, pos);
	varcons_data(cons, &cons);
	GetCons(cons, &check, &cons);
	test(check == pos, "push_varcons_generic_function1");
	test(cons == Nil, "push_varcons_generic_function2");
	rollback_local(local, stack);

	RETURN;
}

static int test_nextcons_finish(void)
{
	addr one, cons, base, value;

	base = Nil;
	fixnum_heap(&value, 100);
	list_heap(&cons, value, T, NULL);
	nextcons_finish(one, cons, base);
	test(one == T, "nextcons_finish1");
	nextcons_finish(one, cons, base);
	test(0, "nextcons_finish_error");
finish:

	RETURN;
}

static void import_constant_test(addr package, constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	Error(import_package_(package, symbol));
}

static void import_test(void)
{
	addr package;

	Error(find_char_package_(LISP_COMMON_USER, &package));
	Error(import_package_(package, Nil));
	Error(import_package_(package, T));
	import_constant_test(package, CONSTANT_COMMON_EQL);
	import_constant_test(package, CONSTANT_AMPERSAND_OPTIONAL);
	import_constant_test(package, CONSTANT_AMPERSAND_REST);
	import_constant_test(package, CONSTANT_AMPERSAND_KEY);
	import_constant_test(package, CONSTANT_AMPERSAND_ALLOW);
	import_constant_test(package, CONSTANT_AMPERSAND_AUX);
	import_constant_test(package, CONSTANT_AMPERSAND_WHOLE);
	import_constant_test(package, CONSTANT_AMPERSAND_BODY);
	import_constant_test(package, CONSTANT_AMPERSAND_ENVIRONMENT);
}

static void lambda_readtest(addr *ret, const char *str)
{
	if (readstring_debug(ret, str)) {
		Error(fmte_("error", NULL));
	}
}

static int eqtree(addr left, addr right)
{
	addr value1, value2;

	if (GetType(left) != LISPTYPE_CONS) return left == right;
	if (GetType(right) != LISPTYPE_CONS) return 0;
	GetCons(left, &value1, &left);
	GetCons(right, &value2, &right);
	return eqtree(value1, value2) && eqtree(left, right);
}

static int test_lambda_generic_function_var(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&check, "(nil nil nil nil nil)");
	lambda_generic_function_(local, &pos, Nil);
	test(eqtree(pos, check), "lambda_generic_function_var1");

	lambda_readtest(&pos, "(var)");
	lambda_readtest(&check, "((var) nil nil nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_var2");

	lambda_readtest(&pos, "(aaa bbb)");
	lambda_readtest(&check, "((aaa bbb) nil nil nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_var3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_generic_function_opt(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&pos, "(&optional)");
	lambda_readtest(&check, "(nil nil nil nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_opt1");

	lambda_readtest(&pos, "(&optional aaa)");
	lambda_readtest(&check, "(nil (aaa) nil nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_opt2");

	lambda_readtest(&pos, "(aaa &optional bbb (ccc))");
	lambda_readtest(&check, "((aaa) (bbb ccc) nil nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_opt3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_generic_function_rest(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&pos, "(&rest args)");
	lambda_readtest(&check, "(nil nil args nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_rest1");

	lambda_readtest(&pos, "(aaa &rest args)");
	lambda_readtest(&check, "((aaa) nil args nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_rest2");

	lambda_readtest(&pos, "(&optional aaa &rest args)");
	lambda_readtest(&check, "(nil (aaa) args nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_rest2");

	lambda_readtest(&pos, "(aaa &optional bbb (ccc) &rest ddd)");
	lambda_readtest(&check, "((aaa) (bbb ccc) ddd nil nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_rest3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_generic_function_key(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&pos, "(&key)");
	lambda_readtest(&check, "(nil nil nil t nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_key1");

	lambda_readtest(&pos, "(&key aaa)");
	lambda_readtest(&check, "(nil nil nil ((aaa :aaa)) nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_key2");

	lambda_readtest(&pos, "(aaa &key (bbb))");
	lambda_readtest(&check, "((aaa) nil nil ((bbb :bbb)) nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_key3");

	lambda_readtest(&pos, "(aaa &key ((ccc ddd)))");
	lambda_readtest(&check, "((aaa) nil nil ((ddd ccc)) nil)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_key4");

	lambda_readtest(&pos, "(&key &allow-other-keys)");
	lambda_readtest(&check, "(nil nil nil t t)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_key5");

	lambda_readtest(&pos, "(&key aaa &allow-other-keys)");
	lambda_readtest(&check, "(nil nil nil ((aaa :aaa)) t)");
	lambda_generic_function_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_generic_function_key6");

	rollback_local(local, stack);

	RETURN;
}

static int test_atleast_argument_count(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_generic_function_(local, &pos, Nil);
	atleast_argument_count(pos, &size);
	test(size == 0, "atleast_argument_count1");

	lambda_readtest(&pos, "(var)");
	lambda_generic_function_(local, &pos, pos);
	atleast_argument_count(pos, &size);
	test(size == 1, "atleast_argument_count2");

	lambda_readtest(&pos, "(aaa bbb)");
	lambda_generic_function_(local, &pos, pos);
	atleast_argument_count(pos, &size);
	test(size == 2, "atleast_argument_count3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  lambda-specialized
 */
static int test_specialized_var_cons(void)
{
	addr pos, pos2, var, spec;

	spec = NULL;

	fixnum_heap(&pos, 100);
	specialized_var_cons_(pos, &var, &spec);
	test(var == pos, "specialized_var_cons1");
	test(spec == T, "specialized_var_cons2");

	list_heap(&var, pos, NULL);
	specialized_var_cons_(var, &var, &spec);
	test(var == pos, "specialized_var_cons3");
	test(spec == T, "specialized_var_cons4");

	fixnum_heap(&pos2, 200);
	list_heap(&var, pos, pos2, NULL);
	specialized_var_cons_(var, &var, &spec);
	test(var == pos, "specialized_var_cons5");
	test(spec == pos2, "specialized_var_cons6");

	RETURN;
}

static int test_check_specializer_form(void)
{
	addr pos, eql;

	test(check_specializer_form(Nil), "check_specializer_form1");
	test(check_specializer_form(T), "check_specializer_form2");
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	test(check_specializer_form(pos), "check_specializer_form3");
	list_heap(&pos, T, NULL);
	test(! check_specializer_form(pos), "check_specializer_form4");
	list_heap(&pos, T, T, NULL);
	test(! check_specializer_form(pos), "check_specializer_form5");

	internchar_debug(LISP_COMMON, "EQL", &eql);
	list_heap(&pos, eql, T, NULL);
	test(check_specializer_form(pos), "check_specializer_form6");
	list_heap(&pos, eql, T, T, NULL);
	test(! check_specializer_form(pos), "check_specializer_form7");

	RETURN;
}

static int test_specialized_var(void)
{
	addr pos, symbol, eql, value, cons, var, spec;

	var = NULL;

	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	specialized_var_(symbol, &var, &spec);
	test(var == symbol, "specialized_var1");
	test(spec == T, "specialized_var2");

	internchar_debug(LISP_COMMON, "EQL", &eql);
	fixnum_heap(&value, 100);
	list_heap(&spec, eql, value, NULL);
	list_heap(&cons, symbol, spec, NULL);
	specialized_var_(cons, &var, &spec);
	test(var == symbol, "specialized_var3");
	test(GetType(spec) == LISPTYPE_CONS, "specialized_var4");
	GetCons(spec, &pos, &spec);
	test(pos == eql, "specialized_var5");
	GetCons(spec, &pos, &spec);
	test(pos == value, "specialized_var6");
	test(spec == Nil, "specialized_var7");

	RETURN;
}

static int test_lambda_specialized_var(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&check, "(nil nil nil nil nil nil)");
	lambda_specialized_(local, &pos, Nil);
	test(eqtree(pos, check), "lambda_specialized_var1");

	lambda_readtest(&pos, "(var)");
	lambda_readtest(&check, "(((var t)) nil nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_var2");

	lambda_readtest(&pos, "(aaa bbb)");
	lambda_readtest(&check, "(((aaa t) (bbb t)) nil nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_var3");

	lambda_readtest(&pos, "(aaa (bbb fixnum))");
	lambda_readtest(&check,
			"(((aaa t) (bbb fixnum)) nil nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_var4");

	lambda_readtest(&pos, "((aaa (eql hello)) bbb)");
	lambda_readtest(&check,
			"(((aaa (eql hello)) (bbb t)) nil nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_var5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_specialized_opt(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&check, "(nil nil nil nil nil nil)");
	lambda_specialized_(local, &pos, Nil);
	test(eqtree(pos, check), "lambda_specialized_opt1");

	lambda_readtest(&pos, "(&optional)");
	lambda_readtest(&check, "(nil nil nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_opt2");

	lambda_readtest(&pos, "(&optional aaa)");
	lambda_readtest(&check, "(nil ((aaa nil nil)) nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_opt3");

	lambda_readtest(&pos, "(aaa &optional (bbb))");
	lambda_readtest(&check,
			"(((aaa t)) ((bbb nil nil)) nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_opt4");

	lambda_readtest(&pos, "(&optional (bbb) ccc)");
	lambda_readtest(&check,
			"(nil ((bbb nil nil) (ccc nil nil)) nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_opt5");

	lambda_readtest(&pos, "(&optional (ccc t))");
	lambda_readtest(&check,
			"(nil ((ccc t nil)) nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_opt6");

	lambda_readtest(&pos, "(&optional (ddd t eee))");
	lambda_readtest(&check,
			"(nil ((ddd t eee)) nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_opt7");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_specialized_rest(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&pos, "(&rest args)");
	lambda_readtest(&check, "(nil nil args nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_rest1");

	lambda_readtest(&pos, "(&optional &rest args)");
	lambda_readtest(&check, "(nil nil args nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_rest2");

	lambda_readtest(&pos, "(&optional bbb &rest args)");
	lambda_readtest(&check,
			"(nil ((bbb nil nil)) args nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_rest3");

	lambda_readtest(&pos, "(bbb &rest ccc)");
	lambda_readtest(&check, "(((bbb t)) nil ccc nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_rest4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_specialized_key(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&pos, "(&key)");
	lambda_readtest(&check, "(nil nil nil t nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_key1");

	lambda_readtest(&pos, "(&rest args &key &allow-other-keys)");
	lambda_readtest(&check, "(nil nil args t t nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_key2");

	lambda_readtest(&pos, "(&optional &key aaa)");
	lambda_readtest(&check, "(nil nil nil ((aaa :aaa nil nil)) nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_key3");

	lambda_readtest(&pos, "(zzz &key (aaa) &allow-other-keys)");
	lambda_readtest(&check, "(((zzz t)) nil nil ((aaa :aaa nil nil)) t nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_key4");

	lambda_readtest(&pos, "(&key (aaa t) (bbb ccc ddd))");
	lambda_readtest(&check,
			"(nil nil nil ((aaa :aaa t nil) (bbb :bbb ccc ddd)) nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_key5");

	lambda_readtest(&pos, "(&key ((aa bb) cc dd) &allow-other-keys)");
	lambda_readtest(&check, "(nil nil nil ((bb aa cc dd)) t nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_key6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lambda_specialized_aux(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	import_test();
	local = Local_Thread;
	push_local(local, &stack);

	lambda_readtest(&pos, "(&aux)");
	lambda_readtest(&check, "(nil nil nil nil nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_aux1");

	lambda_readtest(&pos, "(&optional &key &aux)");
	lambda_readtest(&check, "(nil nil nil t nil nil)");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_aux2");

	lambda_readtest(&pos, "(aaa &aux bbb)");
	lambda_readtest(&check, "(((aaa t)) nil nil nil nil ((bbb nil)))");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_aux3");

	lambda_readtest(&pos, "(&key &allow-other-keys &aux (bbb))");
	lambda_readtest(&check, "(nil nil nil t t ((bbb nil)))");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_aux4");

	lambda_readtest(&pos, "(&aux (bbb ccc))");
	lambda_readtest(&check, "(nil nil nil nil nil ((bbb ccc)))");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_aux5");

	lambda_readtest(&pos, "(&aux (aaa) bbb)");
	lambda_readtest(&check, "(nil nil nil nil nil ((aaa nil) (bbb nil)))");
	lambda_specialized_(local, &pos, pos);
	test(eqtree(pos, check), "lambda_specialized_aux6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  main
 */
static int testcase_lambda(void)
{
	TestBreak(test_constant_eq);
	TestBreak(test_member_ampersand);
	TestBreak(test_variable_check);
	TestBreak(test_varcons_local);
	TestBreak(test_push_varcons);
	TestBreak(test_push_namecons);
	TestBreak(test_make_keyword_from_symbol);
	TestBreak(test_list2_check);
	TestBreak(test_key_name_values);
	/* lambda-macro */
	TestBreak(test_push_varcons_macro);
	TestBreak(test_ordinary_opt);
	TestBreak(test_ordinary_key);
	TestBreak(test_ordinary_aux);
	/* lambda-ordinary */
	TestBreak(test_push_varcons_ordinary);
	/* lambda-generic-function */
	TestBreak(test_generic_function_key_cons);
	TestBreak(test_generic_function_key);
	TestBreak(test_push_varcons_generic_function);
	TestBreak(test_nextcons_finish);
	TestBreak(test_lambda_generic_function_var);
	TestBreak(test_lambda_generic_function_opt);
	TestBreak(test_lambda_generic_function_rest);
	TestBreak(test_lambda_generic_function_key);
	TestBreak(test_atleast_argument_count);
	/* lambda-sepcialized */
	TestBreak(test_specialized_var_cons);
	TestBreak(test_check_specializer_form);
	TestBreak(test_specialized_var);
	TestBreak(test_lambda_specialized_var);
	TestBreak(test_lambda_specialized_opt);
	TestBreak(test_lambda_specialized_rest);
	TestBreak(test_lambda_specialized_key);
	TestBreak(test_lambda_specialized_aux);

	return 0;
}

static void testinit_lambda(Execute ptr)
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
	build_code();
}

int test_lambda(void)
{
	DegradeTitle;
	return DegradeCode(lambda);
}

