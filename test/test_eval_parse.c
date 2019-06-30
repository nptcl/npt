#include "eval_parse.c"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "ratio.h"
#include "readtable.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"

/*
 *  environment
 */
static int test_environment_symbol(void)
{
	addr symbol;

	environment_symbol(&symbol);
	test(symbolp(symbol), "environment_symbol1");

	RETURN;
}

static int test_envroot_heap(void)
{
	addr pos;

	envroot_heap(&pos);
	test(GetType(pos) == LISPSYSTEM_ENVROOT, "envroot_heap1");
	test(lenarrayr(pos) == 2, "envroot_heap2");

	RETURN;
}

static int test_envstack_heap(void)
{
	addr pos, v1, v2, v3, v4;

	v1 = fixnumh(10);
	v2 = fixnumh(20);
	v3 = fixnumh(30);
	v4 = fixnumh(40);
	envstack_heap(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPSYSTEM_ENVSTACK, "envstack_heap1");
	test(RefArrayA2(pos, 0) == v1, "envstack_heap2");
	test(RefArrayA2(pos, 1) == v2, "envstack_heap3");
	test(RefArrayA2(pos, 2) == v3, "envstack_heap4");
	test(RefArrayA2(pos, 3) == v4, "envstack_heap5");

	RETURN;
}

static int test_init_environment(void)
{
	Execute ptr;
	addr control, pos;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	test(GetType(pos) == LISPSYSTEM_ENVROOT, "init_environment1");
	test(lenarrayr(pos) == 2, "init_environment2");
	free_control(ptr, control);

	RETURN;
}

static int test_snapshot_envstack(void)
{
	Execute ptr;
	addr control, pos, value;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	snapshot_envstack(&pos);
	test(pos == Nil, "snapshot_envstack1");

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	fixnum_heap(&value, 10);
	SetArrayA2(pos, 1, value);

	snapshot_envstack(&pos);
	test(pos == value, "snapshot_envstack2");

	free_control(ptr, control);

	RETURN;
}

static int test_push_envstack(void)
{
	Execute ptr;
	addr control, pos, check, temp, v1, v2, v3;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	push_envstack(0, v1, v2, v3);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetArrayA2(pos, 1, &check);
	test(check == Nil, "push_envstack1");
	GetArrayA2(pos, 0, &check);
	test(check != Nil, "push_envstack2");
	GetArrayA2(check, 0, &temp);
	test(temp == Nil, "push_envstack3");
	GetArrayA2(check, 1, &temp);
	test(temp == v1, "push_envstack4");
	GetArrayA2(check, 2, &temp);
	test(temp == v2, "push_envstack5");
	GetArrayA2(check, 3, &temp);
	test(temp == v3, "push_envstack6");

	push_envstack(0, Nil, Nil, Nil);
	GetArrayA2(pos, 0, &temp);
	GetArrayA2(temp, 0, &temp);
	test(check == temp, "push_envstack7");

	push_envstack(1, v1, v2, v3);
	GetArrayA2(pos, 1, &temp);
	test(temp != Nil, "push_envstack9");

	free_control(ptr, control);

	RETURN;
}

static int test_rollback_envstack(void)
{
	Execute ptr;
	addr control, pos, left, right;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);

	push_envstack(1, T, Nil, T);
	push_envstack(1, T, Nil, T);
	push_envstack(1, T, Nil, T);
	snapshot_envstack(&left);
	push_envstack(1, T, Nil, T);
	push_envstack(1, T, Nil, T);
	push_envstack(1, T, Nil, T);
	rollback_envstack(left);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetArrayA2(pos, 1, &right);
	test(left == right, "rollback_envstack1");

	free_control(ptr, control);

	RETURN;
}

static int test_defmacro_envstack(void)
{
	Execute ptr;
	addr control, pos, left, right, v1, v2, v3;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);

	defmacro_envstack(v1, v2);
	GetArrayA2(pos, 0, &left);
	GetArrayA2(left, 1, &right);
	test(right == v1, "defmacro_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v2, "defmacro_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == T, "defmacro_envstack3");

	macrolet_envstack(v2, v3);
	GetArrayA2(pos, 1, &left);
	GetArrayA2(left, 1, &right);
	test(right == v2, "macrolet_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v3, "macrolet_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == T, "macrolet_envstack3");

	define_symbol_macro_envstack(v1, v2);
	GetArrayA2(pos, 0, &left);
	GetArrayA2(left, 1, &right);
	test(right == v1, "define_symbol_macro_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v2, "define_symbol_macro_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == Nil, "define_symbol_macro_envstack3");

	symbol_macrolet_envstack(v2, v3);
	GetArrayA2(pos, 1, &left);
	GetArrayA2(left, 1, &right);
	test(right == v2, "symbol_macrolet_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v3, "symbol_macrolet_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == Nil, "symbol_macrolet_envstack3");

	free_control(ptr, control);

	RETURN;
}

static int test_environment_heap(void)
{
	Execute ptr;
	addr control, pos, left, pos1, pos2;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	defmacro_envstack(Nil, T);
	macrolet_envstack(T, Nil);
	GetArrayA2(pos, 0, &pos1);
	GetArrayA2(pos, 1, &pos2);

	environment_heap(&pos);
	test(GetType(pos) == LISPTYPE_ENVIRONMENT, "environment_heap1");
	GetArrayA2(pos, 0, &left);
	test(left == pos1, "environment_heap2");
	GetArrayA2(pos, 1, &left);
	test(left == pos2, "environment_heap3");
	test(GetUser(pos), "environment_heap4");

	close_environment(pos);
	GetArrayA2(pos, 0, &left);
	test(left == Nil, "close_environment1");
	GetArrayA2(pos, 1, &left);
	test(left == Nil, "close_environment2");
	test(! GetUser(pos), "close_environment3");

	free_control(ptr, control);

	RETURN;
}


/*
 *  memory
 */
static int test_eval_parse_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	eval_parse_alloc(NULL, &pos, EVAL_PARSE_T, 10);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_parse_alloc1");
	test(! GetStatusDynamic(pos), "eval_parse_alloc2");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_parse_alloc3");
	test(lenarrayr(pos) == 10, "eval_parse_alloc4");

	eval_parse_local(local, &pos, EVAL_PARSE_T, 10);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_parse_alloc5");
	test(GetStatusDynamic(pos), "eval_parse_alloc6");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_parse_alloc7");
	test(lenarrayr(pos) == 10, "eval_parse_alloc8");

	eval_parse_heap(&pos, EVAL_PARSE_T, 10);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_parse_alloc9");
	test(! GetStatusDynamic(pos), "eval_parse_alloc10");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_parse_alloc11");
	test(lenarrayr(pos) == 10, "eval_parse_alloc12");

	rollback_local(local, stack);

	RETURN;
}

static int test_eval_single_parse_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	eval_single_parse_alloc(local, &pos, EVAL_PARSE_T, T);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_single_parse_alloc1");
	test(GetStatusDynamic(pos), "eval_single_parse_alloc2");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_single_parse_alloc3");
	test(lenarrayr(pos) == 1, "eval_single_parse_alloc4");
	test(RefEval(pos, 0) == T, "eval_single_parse_alloc5");

	eval_single_parse_local(local, &pos, EVAL_PARSE_T, T);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_single_parse_alloc6");
	test(GetStatusDynamic(pos), "eval_single_parse_alloc7");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_single_parse_alloc8");
	test(lenarrayr(pos) == 1, "eval_single_parse_alloc9");
	test(RefEval(pos, 0) == T, "eval_single_parse_alloc10");

	eval_single_parse_heap(&pos, EVAL_PARSE_T, T);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_single_parse_alloc11");
	test(! GetStatusDynamic(pos), "eval_single_parse_alloc12");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_single_parse_alloc13");
	test(lenarrayr(pos) == 1, "eval_single_parse_alloc14");
	test(RefEval(pos, 0) == T, "eval_single_parse_alloc15");

	rollback_local(local, stack);

	RETURN;
}

static int test_StructEvalParse(void)
{
	addr pos;
	struct eval_parse *str;

	eval_single_parse_heap(&pos, EVAL_PARSE_T, T);
	str = StructEvalParse(pos);
	test(str->type == EVAL_PARSE_T, "StructEvalParse1");

	RETURN;
}

static int test_RefEvalParse(void)
{
	addr pos, check;

	eval_parse_heap(&pos, EVAL_PARSE_T, 5);
	test(RefEvalParse(pos, 2) == Nil, "RefEvalParse1");
	SetEvalParse(pos, 2, T);
	test(RefEvalParse(pos, 2) == T, "RefEvalParse2");
	GetEvalParse(pos, 2, &check);
	test(check == T, "RefEvalParse3");

	RETURN;
}

static int test_RefEvalParseType(void)
{
	enum EVAL_PARSE check;
	addr pos;

	eval_parse_heap(&pos, EVAL_PARSE_T, 5);
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "RefEvalParseType1");
	SetEvalParseType(pos, EVAL_PARSE_NIL);
	test(RefEvalParseType(pos) == EVAL_PARSE_NIL, "RefEvalParseType2");
	GetEvalParseType(pos, &check);
	test(check == EVAL_PARSE_NIL, "RefEvalParseType3");

	RETURN;
}


/*
 *  eval_parse
 */
static int test_parse_eval_nil(void)
{
	addr pos;

	eval_parse(&pos, Nil);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_nil1");
	test(RefEvalParseType(pos) == EVAL_PARSE_NIL, "parse_eval_nil2");
	test(RefEval(pos, 0) == Nil, "parse_eval_nil3");

	RETURN;
}

static int test_parse_eval_t(void)
{
	addr pos;

	eval_parse(&pos, T);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_t1");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "parse_eval_t2");
	test(RefEval(pos, 0) == T, "parse_eval_t3");

	RETURN;
}

static int test_parse_eval_symbol(void)
{
	addr pos, value;

	internchar(LISP_PACKAGE, "HELLO", &value);
	eval_parse(&pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_symbol1");
	test(RefEvalParseType(pos) == EVAL_PARSE_SYMBOL, "parse_eval_symbol2");
	test(RefEval(pos, 0) == value, "parse_eval_symbol3");

	RETURN;
}

static int test_parse_eval_fixnum(void)
{
	addr pos, value;

	fixnum_heap(&value, 100);
	eval_parse(&pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_fixnum1");
	test(RefEvalParseType(pos) == EVAL_PARSE_INTEGER, "parse_eval_fixnum2");
	test(RefEval(pos, 0) == value, "parse_eval_fixnum3");

	RETURN;
}

static int test_parse_eval_bignum(void)
{
	addr pos, value;

	bignum_value_alloc(NULL, &value, signplus_bignum, 100);
	eval_parse(&pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_bignum1");
	test(RefEvalParseType(pos) == EVAL_PARSE_INTEGER, "parse_eval_bignum2");
	test(RefEval(pos, 0) == value, "parse_eval_bignum3");

	RETURN;
}

static int test_parse_eval_ratio(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_alloc(local, &pos, signplus_bignum, 4);
	bignum_value_alloc(local, &value, signplus_bignum, 5);
	ratio_reduction_heap(local, &value, signplus_bignum, pos, value);
	eval_parse(&pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_ratio1");
	test(RefEvalParseType(pos) == EVAL_PARSE_RATIONAL, "parse_eval_ratio2");
	test(RefEval(pos, 0) == value, "parse_eval_ratio3");
	rollback_local(local, stack);

	RETURN;
}

static int test_parse_eval_string(void)
{
	addr pos, value;

	strvect_char_heap(&value, "Hello");
	eval_parse(&pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_string1");
	test(RefEvalParseType(pos) == EVAL_PARSE_STRING, "parse_eval_string2");
	test(RefEval(pos, 0) == value, "parse_eval_string3");

	RETURN;
}

static int test_parse_eval_character(void)
{
	addr pos, value;

	character_heap(&value, 'A');
	eval_parse(&pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_character1");
	test(RefEvalParseType(pos) == EVAL_PARSE_CHARACTER, "parse_eval_character2");
	test(RefEval(pos, 0) == value, "parse_eval_character3");

	RETURN;
}


/*
 *  parse_cons
 */
static int test_checktype(addr eval, enum EVAL_PARSE type)
{
	if (GetType(eval) != LISPTYPE_EVAL) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefEvalType(eval) != EVAL_TYPE_PARSE) {
		degrade_printf("eval type error\n");
		return 0;
	}
	if (RefEvalParseType(eval) != type) {
		degrade_printf("eval-parse type error\n");
		return 0;
	}

	return 1;
}

static int test_evalsymbol(addr eval, const char *package, const char *name)
{
	if (test_checktype(eval, EVAL_PARSE_SYMBOL)) return 1;
	if (RefEval(eval, 0) == interncharr(package, name)) {
		degrade_printf("symbol name error\n");
		return 0;
	}

	return 1;
}

static int test_evalkeyword(addr symbol, const char *name)
{
	return test_evalsymbol(symbol, LISP_KEYWORD, name);
}

static int test_evalsymlocal(addr symbol, const char *name)
{
	return symbol == interncharr(LISP_PACKAGE, name);
}

#if 0
static int test_eqkeyword(addr symbol, const char *name)
{
	if (! IsSymbol(symbol)) {
		degrade_printf("symbol type error\n");
		return 0;
	}
	if (symbol != interncharr(LISP_KEYWORD, name)) {
		degrade_printf("symbol error\n");
		return 0;
	}

	return 1;
}
#endif

static int test_eqsymbol(addr symbol, const char *name)
{
	if (! IsSymbol(symbol)) {
		degrade_printf("symbol type error\n");
		return 0;
	}
	if (symbol != interncharr(LISP_PACKAGE, name)) {
		degrade_printf("symbol error\n");
		return 0;
	}

	return 1;
}

static int test_eqlfixnum(addr eval, fixnum value)
{
	if (test_checktype(eval, EVAL_PARSE_INTEGER)) return 1;
	if (RefFixnum(RefEval(eval, 0)) != value) {
		degrade_printf("fixnum value error\n");
		return 0;
	}

	return 1;
}

static int test_eqlstring(addr eval, const char *value)
{
	if (test_checktype(eval, EVAL_PARSE_STRING)) return 1;
	if (! string_equal_char(RefEval(eval, 0), value)) {
		degrade_printf("string value error\n");
		return 0;
	}

	return 1;
}

static int test_parse_allcons(void)
{
	addr cons, eval;

	readstring(&cons, "(10 \"Hello\" :aaa)");
	parse_allcons(&cons, cons);
	/* 10 */
	getcons(cons, &eval, &cons);
	test(test_eqlfixnum(eval, 10), "parse_allcons1");
	/* "Hello" */
	getcons(cons, &eval, &cons);
	test(test_eqlstring(eval, "Hello"), "parse_allcons2");
	/* :aaa */
	getcons(cons, &eval, &cons);
	test(test_evalkeyword(eval, "AAA"), "parse_allcons3");
	/* nil */
	test(cons == Nil, "parse_allcons4");

	RETURN;
}

static int test_parse_progn(void)
{
	addr cons, eval;

	readstring(&cons, "(progn 10 \"Hello\" :aaa)");
	eval_parse(&eval, cons);

	/* progn */
	test(test_checktype(eval, EVAL_PARSE_PROGN), "parse_progn1");
	GetEvalParse(eval, 0, &cons);

	/* 10 */
	getcons(cons, &eval, &cons);
	test(test_eqlfixnum(eval, 10), "parse_progn2");
	/* "Hello" */
	getcons(cons, &eval, &cons);
	test(test_eqlstring(eval, "Hello"), "parse_progn3");
	/* :aaa */
	getcons(cons, &eval, &cons);
	test(test_evalkeyword(eval, "AAA"), "parse_progn4");
	/* nil */
	test(cons == Nil, "parse_progn5");

	RETURN;
}

static int test_check_variable(void)
{
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	check_variable(pos);
	test(1, "check_variable1");

	RETURN;
}

static int test_check_function_variable(void)
{
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	check_function_variable(pos);
	test(1, "check_function_variable1");
	test(parse_callname_heap(&pos, pos) == 0, "check_function_variable2");
	check_function_variable(pos);
	test(1, "check_function_variable3");

	RETURN;
}

static int test_parse_letone(void)
{
	addr pos, cons, symbol, value;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	parse_letone(pos, &symbol, &value);
	test(symbol == pos, "parse_letone1");
	test(value == Nil, "parse_letone2");

	readstring(&cons, "(hello)");
	parse_letone(cons, &symbol, &value);
	test(symbol == pos, "parse_letone3");
	test(value == Nil, "parse_letone4");

	readstring(&cons, "(hello t)");
	parse_letone(cons, &symbol, &value);
	test(symbol == pos, "parse_letone5");
	test(value == T, "parse_letone6");

	RETURN;
}

static int test_parse_letarg(void)
{
	addr cons, symbol, value;

	parse_letarg(&cons, Nil);
	test(cons == Nil, "parse_letarg1");

	readstring(&cons, "(aaa)");
	parse_letarg(&cons, cons);
	GetCons(cons, &value, &cons);
	/* (aaa nil) */
	GetCons(value, &symbol, &value);
	test(symbolp(symbol), "parse_letarg2");
	GetNameSymbol(symbol, &symbol);
	test(string_equal_char(symbol, "AAA"), "parse_letarg3");
	test(test_checktype(value, EVAL_PARSE_NIL), "parse_letarg4");
	test(cons == Nil, "parse_letarg5");

	readstring(&cons, "((aaa) (bbb 100))");
	parse_letarg(&cons, cons);
	GetCons(cons, &value, &cons);
	/* (aaa nil) */
	GetCons(value, &symbol, &value);
	GetNameSymbol(symbol, &symbol);
	test(string_equal_char(symbol, "AAA"), "parse_letarg6");
	test(test_checktype(value, EVAL_PARSE_NIL), "parse_letarg7");

	/* (bbb 100) */
	GetCons(cons, &value, &cons);
	GetCons(value, &symbol, &value);
	GetNameSymbol(symbol, &symbol);
	test(string_equal_char(symbol, "BBB"), "parse_letarg8");
	test(test_eqlfixnum(value, 100), "parse_letarg9");
	test(cons == Nil, "parse_letarg10");

	RETURN;
}

static int test_parse_let(void)
{
	addr cons, left, right;

	readstring(&cons, "(let nil)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LET), "parse_let1");
	test(RefEval(cons, 0) == Nil, "parse_let2");
	test(RefEval(cons, 1) == Nil, "parse_let3");
	test(RefEval(cons, 2) == Nil, "parse_let4");

	readstring(&cons, "(let* (aaa) (declare (ignore aaa)) 10 20 30)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LETA), "parse_let5");

	GetEvalParse(cons, 0, &right);
	GetCons(right, &left, &right);
	test(right == Nil, "parse_let6");
	GetCons(left, &left, &right);
	GetNameSymbol(left, &left);
	test(string_equal_char(left, "AAA"), "parse_let7");
	test(test_checktype(right, EVAL_PARSE_NIL), "parse_let8");

	GetEvalParse(cons, 1, &left);
	test(GetType(left) == LISPTYPE_EVAL, "parse_let9");
	test(RefEvalType(left) == EVAL_TYPE_DECLARE, "parse_let10");

	GetEvalParse(cons, 2, &right);
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 10), "parse_let11");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 20), "parse_let12");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 30), "parse_let13");
	test(right == Nil, "parse_let14");

	RETURN;
}

static int test_parse_setq(void)
{
	addr cons, left, right;

	readstring(&cons, "(setq)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_SETQ), "parse_setq1");
	test(RefEval(cons, 0) == Nil, "parse_setq2");

	readstring(&cons, "(setq aaa 10 bbb 20)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_SETQ), "parse_setq3");
	GetEvalParse(cons, 0, &cons);
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	GetNameSymbol(left, &left);
	test(string_equal_char(left, "AAA"), "parse_setq4");
	test(test_eqlfixnum(right, 10), "parse_setq5");

	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	GetNameSymbol(left, &left);
	test(string_equal_char(left, "BBB"), "parse_setq6");
	test(test_eqlfixnum(right, 20), "parse_setq7");
	test(cons == Nil, "parse_setq8");

	RETURN;
}

static int test_check_variable_notnil(void)
{
	addr pos;

	pos = Nil;
	check_variable_notnil(&pos);
	test(pos == Nil, "check_variable_notnil1");
	internchar(LISP_PACKAGE, "AAA", &pos);
	check_variable_notnil(&pos);
	test(symbolp(pos), "check_variable_notnil2");
	test(test_eqsymbol(pos, "AAA"), "check_variable_notnil3");

	RETURN;
}

static int test_parse_var(void)
{
	addr cons, symbol, check;

	readstring(&cons, "(aaa bbb ccc)");
	parse_var(&cons, cons);
	test(length_list_unsafe(cons) == 3, "parse_var1");
	GetCons(cons, &symbol, &cons);
	readstring(&check, "aaa");
	test(symbol == check, "parse_var2");
	GetCons(cons, &symbol, &cons);
	readstring(&check, "bbb");
	test(symbol == check, "parse_var3");

	RETURN;
}

static int test_parse_optional(void)
{
	addr cons, left, right;

	parse_optional(&cons, Nil);
	test(cons == Nil, "parse_optional1");

	readstring(&cons, "((bbb 10 nil) (ccc nil ddd))");
	parse_optional(&cons, cons);
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "BBB"), "parse_optional2");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 10), "parse_optional3");
	GetCons(right, &left, &right);
	test(left == Nil, "parse_optional4");
	test(right == Nil, "parse_optional5");

	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "CCC"), "parse_optional6");
	GetCons(right, &left, &right);
	test(test_checktype(left, EVAL_PARSE_NIL), "parse_optional7");
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "DDD"), "parse_optional8");
	test(right == Nil, "parse_optional9");
	test(cons == Nil, "parse_optional10");

	RETURN;
}

static int test_parse_key(void)
{
	addr cons, left, right, check;

	parse_key(&cons, Nil);
	test(cons == Nil, "parse_key1");

	readstring(&cons, "((bbb :bbb 10 nil) (ccc hello nil ddd))");
	parse_key(&cons, cons);
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "BBB"), "parse_key2");
	GetCons(right, &left, &right);
	internchar_keyword("BBB", &check);
	test(left == check, "parse_key3");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 10), "parse_key4");
	GetCons(right, &left, &right);
	test(left == Nil, "parse_key5");
	test(right == Nil, "parse_key6");

	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "CCC"), "parse_key7");
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "HELLO"), "parse_key8");
	GetCons(right, &left, &right);
	test(test_checktype(left, EVAL_PARSE_NIL), "parse_key9");
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "DDD"), "parse_key10");
	test(right == Nil, "parse_key11");
	test(cons == Nil, "parse_key12");

	RETURN;
}

static int test_parse_aux(void)
{
	addr cons, left, right;

	parse_aux(&cons, Nil);
	test(cons == Nil, "parse_aux1");

	readstring(&cons, "((bbb 10) (ccc nil))");
	parse_aux(&cons, cons);
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "BBB"), "parse_aux2");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 10), "parse_aux3");
	test(right == Nil, "parse_aux4");

	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "CCC"), "parse_aux5");
	GetCons(right, &left, &right);
	test(test_checktype(left, EVAL_PARSE_NIL), "parse_aux6");
	test(right == Nil, "parse_aux7");
	test(cons == Nil, "parse_aux8");

	RETURN;
}

static int test_parse_ordinary(void)
{
	addr cons, left, right, check, keyword;

	readstring(&cons, "(a b &optional c &rest d &key e &allow-other-keys &aux f)");
	parse_ordinary(&cons, cons);
	/* var */
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "A"), "parse_ordinary1");
	GetCons(right, &left, &right);
	test(test_evalsymlocal(left, "B"), "parse_ordinary2");
	test(right == Nil, "parse_ordinary3");
	/* opt */
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	GetCons(left, &check, &left);
	test(test_evalsymlocal(check, "C"), "parse_ordinary4");
	GetCons(left, &check, &left);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_ordinary5");
	GetCons(left, &check, &left);
	test(check == Nil, "parse_ordinary6");
	test(left == Nil, "parse_ordinary7");
	test(right == Nil, "parse_ordinary8");
	/* rest */
	GetCons(cons, &right, &cons);
	test(test_evalsymlocal(right, "D"), "parse_ordinary9");
	/* key */
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	GetCons(left, &check, &left);
	test(test_evalsymlocal(check, "E"), "parse_ordinary10");
	GetCons(left, &check, &left);
	internchar_keyword("E", &keyword);
	test(check == keyword, "parse_ordinary11");
	GetCons(left, &check, &left);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_ordinary12");
	GetCons(left, &check, &left);
	test(check == Nil, "parse_ordinary13");
	test(left == Nil, "parse_ordinary14");
	test(right == Nil, "parse_ordinary15");
	/* allow-other-keys */
	GetCons(cons, &right, &cons);
	test(right == T, "parse_ordinary16");
	/* aux */
	GetCons(cons, &right, &cons);
	GetCons(right, &left, &right);
	GetCons(left, &check, &left);
	test(test_evalsymlocal(check, "F"), "parse_ordinary17");
	GetCons(left, &check, &left);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_ordinary18");
	test(left == Nil, "parse_ordinary19");
	test(right == Nil, "parse_ordinary20");
	/* nil */
	test(cons == Nil, "parse_ordinary21");

	RETURN;
}

static int test_parse_defun(void)
{
	addr cons, left, right, check;

	readstring(&cons, "(defun hello ())");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_DEFUN), "parse_defun1");
	/* name */
	GetEvalParse(cons, 0, &left);
	test(GetType(left) == LISPTYPE_CALLNAME, "parse_defun2");
	test(RefCallNameType(left) == CALLNAME_SYMBOL, "parse_defun3");
	GetCallName(left, &left);
	test(left == interncharr(LISP_PACKAGE, "HELLO"), "parse_defun4");
	/* lambda-list */
	GetEvalParse(cons, 1, &right);
	GetCons(right, &left, &right); /* var */
	test(left == Nil, "parse_defun5");
	GetCons(right, &left, &right); /* opt */
	test(left == Nil, "parse_defun6");
	GetCons(right, &left, &right); /* rest */
	test(left == Nil, "parse_defun7");
	GetCons(right, &left, &right); /* key */
	test(left == Nil, "parse_defun8");
	GetCons(right, &left, &right); /* allow */
	test(left == Nil, "parse_defun9");
	GetCons(right, &left, &right); /* aux */
	test(left == Nil, "parse_defun10");
	test(right == Nil, "parse_defun11");
	/* decl */
	GetEvalParse(cons, 2, &right);
	test(right == Nil, "parse_defun12");
	/* doc */
	GetEvalParse(cons, 3, &right);
	test(right == Nil, "parse_defun13");
	/* body */
	GetEvalParse(cons, 4, &right);
	test(right != Nil, "parse_defun14");

	readstring(&cons,
			"(defun (setf hello) (value)"
			"  (declare (ignore value))"
			"  \"HELLO\""
			"  :body1 :body2)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_DEFUN), "parse_defun15");
	/* name */
	GetEvalParse(cons, 0, &left);
	test(GetType(left) == LISPTYPE_CALLNAME, "parse_defun16");
	test(RefCallNameType(left) == CALLNAME_SETF, "parse_defun17");
	GetCallName(left, &left);
	test(left == interncharr(LISP_PACKAGE, "HELLO"), "parse_defun18");
	/* lambda-list */
	GetEvalParse(cons, 1, &right);
	GetCons(right, &left, &right); /* var */
	test(left != Nil, "parse_defun19");
	GetCons(left, &check, &left);
	test(test_evalsymlocal(check, "VALUE"), "parse_defun20");
	test(left == Nil, "parse_defun21");
	GetCons(right, &left, &right); /* opt */
	test(left == Nil, "parse_defun22");
	GetCons(right, &left, &right); /* rest */
	test(left == Nil, "parse_defun23");
	GetCons(right, &left, &right); /* key */
	test(left == Nil, "parse_defun24");
	GetCons(right, &left, &right); /* allow */
	test(left == Nil, "parse_defun25");
	GetCons(right, &left, &right); /* aux */
	test(left == Nil, "parse_defun26");
	test(right == Nil, "parse_defun27");
	/* decl */
	GetEvalParse(cons, 2, &right);
	test(GetType(right) == LISPTYPE_EVAL, "parse_defun28");
	test(RefEvalType(right) == EVAL_TYPE_DECLARE, "parse_defun29");
	/* doc */
	GetEvalParse(cons, 3, &right);
	test(GetType(right) == LISPTYPE_STRING, "parse_defun30");
	/* body */
	GetEvalParse(cons, 4, &right);
	test(right != Nil, "parse_defun31");
	GetCons(right, &left, &right);
	test(eval_parse_p(left), "parse_defun31");
	test(RefEvalParseType(left) == EVAL_PARSE_BLOCK, "parse_defun32");
	test(right == Nil, "parse_defun33");

	RETURN;
}

static int test_parse_macro_var(void)
{
	addr cons, pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	cons = readr("(x y)");
	lambda_macro(local, &cons, cons, Nil);
	GetCar(cons, &cons);
	test(length_list_unsafe(cons) == 2, "parse_macro_var1");
	GetCons(cons, &pos, &cons);
	test(pos == readr("x"), "parse_macro_var2");
	GetCons(cons, &pos, &cons);
	test(pos == readr("y"), "parse_macro_var3");

	cons = readr("(x (y z) w)");
	lambda_macro(local, &cons, cons, Nil);
	GetCar(cons, &cons);
	test(length_list_unsafe(cons) == 3, "parse_macro_var4");
	GetCons(cons, &pos, &cons);
	test(pos == readr("x"), "parse_macro_var5");
	GetCons(cons, &pos, &cons);
	test(consp(pos), "parse_macro_var6");
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 2, "parse_macro_var7");
	GetCons(pos, &check, &pos);
	test(check == readr("y"), "parse_macro_var8");
	GetCons(pos, &check, &pos);
	test(check == readr("z"), "parse_macro_var9");
	GetCons(cons, &pos, &cons);
	test(pos == readr("w"), "parse_macro_var10");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_macro_opt(void)
{
	addr cons, list, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	cons = readr("(&optional x y)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &cons, &pos, NULL);
	test(length_list_unsafe(cons) == 2, "parse_macro_opt1");

	cons = readr("(&optional (x nil s))");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &cons, &pos, NULL);
	test(length_list_unsafe(cons) == 1, "parse_macro_opt2");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_macro_rest(void)
{
	addr cons, list, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	cons = readr("(&rest x)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &cons, &pos, NULL);
	test(consp(cons), "parse_macro_rest1");
	GetCons(cons, &pos, &cons);
	test(pos == readr("x"), "parse_macro_rest2");
	test(cons == readr("&rest"), "parse_macro_rest3");

	cons = readr("(&body y)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &cons, &pos, NULL);
	test(consp(cons), "parse_macro_rest4");
	GetCons(cons, &pos, &cons);
	test(pos == readr("y"), "parse_macro_rest5");
	test(cons == readr("&body"), "parse_macro_rest6");

	cons = readr("(x . y)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &cons, &pos, NULL);
	test(consp(cons), "parse_macro_rest7");
	GetCons(cons, &pos, &cons);
	test(pos == readr("y"), "parse_macro_rest8");
	test(cons == Nil, "parse_macro_rest9");

	cons = readr("z");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &cons, &pos, NULL);
	test(consp(cons), "parse_macro_rest10");
	GetCons(cons, &pos, &cons);
	test(pos == readr("z"), "parse_macro_rest11");
	test(cons == Nil, "parse_macro_rest12");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_macro_key(void)
{
	addr cons, list, pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	cons = readr("(&key x y)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &pos, &cons, &check, &pos, NULL);
	test(length_list_unsafe(cons) == 2, "parse_macro_key1");
	test(check == Nil, "parse_macro_key2");

	cons = readr("(&key ((x name) nil s) &allow-other-keys)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &pos, &cons, &check, &pos, NULL);
	test(length_list_unsafe(cons) == 1, "parse_macro_key3");
	test(check != Nil, "parse_macro_key4");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_macro_aux(void)
{
	addr cons, list, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	cons = readr("(&aux x y)");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &pos, &pos, &pos, &cons, &pos, NULL);
	test(length_list_unsafe(cons) == 2, "parse_macro_aux1");

	cons = readr("(&aux (x t))");
	lambda_macro(local, &list, cons, Nil);
	Lista_bind(list, &pos, &pos, &pos, &pos, &pos, &cons, &pos, NULL);
	test(length_list_unsafe(cons) == 1, "parse_macro_aux2");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_macro_whole_env(void)
{
	addr cons, list, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	cons = readr("(&whole x &environment y)");
	lambda_macro(local, &list, cons, Nil);
	List_bind(list, &pos, &pos, &pos, &pos, &pos, &pos, &cons, &pos, NULL);
	test(cons == readr("x"), "parse_macro_whole1");
	test(pos == readr("y"), "parse_macro_whole2");

	cons = readr("(&whole whole b c d &optional e &environment env)");
	lambda_macro(local, &list, cons, Nil);
	List_bind(list, &pos, &pos, &pos, &pos, &pos, &pos, &cons, &pos, NULL);
	test(cons == readr("whole"), "parse_macro_environmente1");
	test(pos == readr("env"), "parse_macro_environmente2");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_macro_function(void)
{
	Execute ptr;
	addr control, args, cons, call;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	args = readr("(x y z)");
	lambda_macro(ptr->local, &args, args, Nil);
	cons = readr("(x y z :hello)");
	parse_allcons(&cons, cons);

	make_macro_function(&call, args, Nil, Nil, cons);
	test(functionp(call), "make_macro_function1");
	test(StructFunction(call)->macro, "make_macro_function2");

	list_heap(&args, T, Nil, Nil, Nil, NULL);
	callclang_funcall(ptr, &cons, call, args, Nil, NULL);
	test(cons == readr(":hello"),"make_macro_function3");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_defmacro(void)
{
	Execute ptr;
	addr control, eval, name, lambda, stack;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	eval = readr("(defmacro aaa () :hello)");
	lambda = readr("defmacro");
	getmacro_symbol(lambda, &lambda);
	callclang_funcall(ptr, &eval, lambda, eval, Nil, NULL);
	GetCdr(eval, &eval);
	parse_defmacro(&eval, eval);
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &lambda);
	test(name == readr("aaa"), "parse_defmacro1");
	test(functionp(lambda), "parse_defmacro2");

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 0, &stack); /* global */
	GetArrayA2(stack, 1, &stack); /* call */
	test(stack == name, "parse_defmacro3");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_macrolet_args(void)
{
	Execute ptr;
	addr control, one, stack;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(aaa (a) a ''a)");
	parse_macrolet_one(one);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 1, &stack); /* local */
	GetArrayA2(stack, 1, &stack); /* call */
	test(stack == readr("aaa"), "parse_macrolet_args1");

	one = readr("((bbb (a) a ''b) (ccc (a) a ''c))");
	parse_macrolet_args(one);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 1, &stack); /* local */
	GetArrayA2(stack, 1, &stack); /* call */
	test(stack == readr("ccc"), "parse_macrolet_args2");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_macrolet(void)
{
	Execute ptr;
	addr control, one, stack;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(((aaa () :hello)) :bbb)");
	parse_macrolet(&one, one);
	test(RefEvalParseType(one) == EVAL_PARSE_LOCALLY, "parse_macrolet1");

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 1, &stack); /* local */
	test(stack == Nil, "parse_macrolet2");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_define_symbol_macro(void)
{
	Execute ptr;
	addr control, one;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(aaa ''Hello)");
	parse_define_symbol_macro(&one, one);
	test(RefEvalParseType(one) == EVAL_PARSE_DEFINE_SYMBOL_MACRO,
			"parse_define_symbol_macro1");
	GetEvalParse(one, 0, &one);
	test(one == readr("aaa"), "parse_define_symbol_macro2");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_symbol_macrolet_args(void)
{
	Execute ptr;
	addr control, one, name, form, env;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("((aaa ''Hello) (bbb ''zzz))");
	parse_symbol_macrolet_args(&one, one);
	test(consp(one), "parse_symbol_macrolet_args1");
	test(length_list_unsafe(one) == 2, "parse_symbol_macrolet_args2");
	GetCar(one, &one);
	List_bind(one, &name, &form, &env, NULL);
	test(name == readr("aaa"), "parse_symbol_macrolet_args3");
	test(eval_parse_p(form), "parse_symbol_macrolet_args4");
	test(GetType(env) == LISPTYPE_ENVIRONMENT, "parse_symbol_macrolet_args5");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_symbol_macrolet(void)
{
	Execute ptr;
	addr control, one;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(((aaa ''Hello) (bbb ''zzz)) :hello)");
	parse_symbol_macrolet(&one, one);
	test(RefEvalParseType(one) == EVAL_PARSE_SYMBOL_MACROLET,
			"parse_symbol_macrolet1");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_quote(void)
{
	addr cons;

	readstring(&cons, "(quote hello)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_QUOTE), "parse_quote1");
	GetEvalParse(cons, 0, &cons);
	test(cons == interncharr(LISP_PACKAGE, "HELLO"), "parse_quote2");

	RETURN;
}

static int test_parse_lambda(void)
{
	addr cons, left, right, control;
	Execute ptr;
	
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(Execute_Thread);

	readstring(&cons, "(lambda ())");
	parse_lambda(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LAMBDA), "parse_lambda1");
	/* lambda-list */
	GetEvalParse(cons, 0, &right);
	GetCons(right, &left, &right); /* var */
	test(left == Nil, "parse_lambda2");
	GetCons(right, &left, &right); /* opt */
	test(left == Nil, "parse_lambda3");
	GetCons(right, &left, &right); /* rest */
	test(left == Nil, "parse_lambda4");
	GetCons(right, &left, &right); /* key */
	test(left == Nil, "parse_lambda5");
	GetCons(right, &left, &right); /* allow */
	test(left == Nil, "parse_lambda6");
	GetCons(right, &left, &right); /* aux */
	test(left == Nil, "parse_lambda7");
	test(right == Nil, "parse_lambda8");
	/* decl */
	GetEvalParse(cons, 1, &right);
	test(right == Nil, "parse_lambda9");
	/* doc */
	GetEvalParse(cons, 2, &right);
	test(right == Nil, "parse_lambda10");
	/* body */
	GetEvalParse(cons, 3, &right);
	test(right == Nil, "parse_lambda11");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_function_argument(void)
{
	addr cons, left, right, check, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(Execute_Thread);

	internchar(LISP_PACKAGE, "HELLO", &cons);
	parse_function_argument(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function_argument1");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CALLNAME, "parse_function_argument2");
	test(RefCallNameType(cons) == CALLNAME_SYMBOL, "parse_function_argument3");
	GetCallName(cons, &cons);
	test(cons == interncharr(LISP_PACKAGE, "HELLO"), "parse_function_argument4");

	readstring(&cons, "(setf hello)");
	parse_function_argument(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function_argument5");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CALLNAME, "parse_function_argument6");
	test(RefCallNameType(cons) == CALLNAME_SETF, "parse_function_argument7");
	GetCallName(cons, &cons);
	test(cons == interncharr(LISP_PACKAGE, "HELLO"), "parse_function_argument8");

	readstring(&cons,
			"(lambda (value)"
			"  (declare (ignore value))"
			"  \"HELLO\""
			"  :body1 :body2)");
	parse_function_argument(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LAMBDA), "parse_function_argument9");
	/* lambda-list */
	GetEvalParse(cons, 0, &right);
	GetCons(right, &left, &right); /* var */
	test(left != Nil, "parse_function_argument10");
	GetCons(left, &check, &left);
	test(test_evalsymlocal(check, "VALUE"), "parse_function_argument11");
	test(left == Nil, "parse_function_argument12");
	GetCons(right, &left, &right); /* opt */
	test(left == Nil, "parse_function_argument13");
	GetCons(right, &left, &right); /* rest */
	test(left == Nil, "parse_function_argument14");
	GetCons(right, &left, &right); /* key */
	test(left == Nil, "parse_function_argument15");
	GetCons(right, &left, &right); /* allow */
	test(left == Nil, "parse_function_argument16");
	GetCons(right, &left, &right); /* aux */
	test(left == Nil, "parse_function_argument17");
	test(right == Nil, "parse_function_argument18");
	/* decl */
	GetEvalParse(cons, 1, &right);
	test(GetType(right) == LISPTYPE_EVAL, "parse_function_argument19");
	test(RefEvalType(right) == EVAL_TYPE_DECLARE, "parse_function_argument20");
	/* doc */
	GetEvalParse(cons, 2, &right);
	test(GetType(right) == LISPTYPE_STRING, "parse_function_argument21");
	/* body */
	GetEvalParse(cons, 3, &right);
	test(right != Nil, "parse_function_argument22");
	GetCons(right, &left, &right);
	test(test_evalkeyword(left, "BODY1"), "parse_function_argument23");
	GetCons(right, &left, &right);
	test(test_evalkeyword(left, "BODY2"), "parse_function_argument24");
	test(right == Nil, "parse_function_argument25");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_function(void)
{
	addr cons;

	readstring(&cons, "(function hello)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function1");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CALLNAME, "parse_function2");
	test(RefCallNameType(cons) == CALLNAME_SYMBOL, "parse_function3");
	GetCallName(cons, &cons);
	test(cons == interncharr(LISP_PACKAGE, "HELLO"), "parse_function4");

	readstring(&cons, "(function (lambda () :hello))");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LAMBDA), "parse_function5");

	readstring(&cons, "(function (setf hello))");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function6");

	RETURN;
}

static int test_parse_if(void)
{
	addr cons, check;

	readstring(&cons, "(if 100 (progn 200))");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_IF), "parse_if1");
	GetEvalParse(cons, 0, &check);
	test(test_eqlfixnum(check, 100), "parse_if2");
	GetEvalParse(cons, 1, &check);
	test(test_checktype(check, EVAL_PARSE_PROGN), "parse_if3");
	GetEvalParse(cons, 2, &check);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_if4");

	readstring(&cons, "(if 100 (progn 200) 300)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_IF), "parse_if5");
	GetEvalParse(cons, 0, &check);
	test(test_eqlfixnum(check, 100), "parse_if6");
	GetEvalParse(cons, 1, &check);
	test(test_checktype(check, EVAL_PARSE_PROGN), "parse_if7");
	GetEvalParse(cons, 2, &check);
	test(test_eqlfixnum(check, 300), "parse_if8");

	RETURN;
}

static int test_parse_unwind_protect(void)
{
	addr cons, check, code;

	readstring(&cons, "(unwind-protect 100)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_UNWIND_PROTECT), "parse_unwind_protect1");
	GetEvalParse(cons, 0, &check);
	test(test_eqlfixnum(check, 100), "parse_unwind_protect2");
	GetEvalParse(cons, 1, &check);
	test(check == Nil, "parse_unwind_protect2");

	readstring(&cons, "(unwind-protect 100 :hello :body)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_UNWIND_PROTECT), "parse_unwind_protect3");
	GetEvalParse(cons, 0, &check);
	test(test_eqlfixnum(check, 100), "parse_unwind_protect4");
	GetEvalParse(cons, 1, &check);
	test(check != Nil, "parse_unwind_protect5");
	GetCons(check, &code, &check);
	test(test_evalkeyword(code, "HELLO"), "parse_unwind_protect6");
	GetCons(check, &code, &check);
	test(test_evalkeyword(code, "BODY"), "parse_unwind_protect7");
	test(check == Nil, "parse_unwind_protect8");

	RETURN;
}

static int test_tagbody_tag_p(void)
{
	addr tag;

	internchar(LISP_PACKAGE, "HELLO", &tag);
	test(tagbody_tag_p(tag), "tagbody_tag_p1");
	internchar(LISP_KEYWORD, "HELLO", &tag);
	test(tagbody_tag_p(tag), "tagbody_tag_p2");
	fixnum_heap(&tag, 100);
	test(tagbody_tag_p(tag), "tagbody_tag_p3");
	strvect_char_heap(&tag, "HELLO");
	test(! tagbody_tag_p(tag), "tagbody_tag_p4");
	bignum_value_alloc(NULL, &tag, signminus_bignum, 100);
	test(tagbody_tag_p(tag), "tagbody_tag_p5");

	RETURN;
}

static int test_tagsymbol(addr eval, const char *name)
{
	addr pos;

	if (test_checktype(eval, EVAL_PARSE_TAG)) return 1;
	GetEvalParse(eval, 0, &pos);
	if (GetType(pos) != LISPTYPE_SYMBOL) {
		degrade_printf("symbol type error\n");
		return 0;
	}
	if (pos == interncharr(LISP_PACKAGE, name)) {
		degrade_printf("symbol name error\n");
		return 0;
	}

	return 1;
}

static int test_tagfixnum(addr eval, fixnum value)
{
	addr pos;

	if (test_checktype(eval, EVAL_PARSE_TAG)) return 1;
	GetEvalParse(eval, 0, &pos);
	if (GetType(pos) != LISPTYPE_FIXNUM) {
		degrade_printf("fixnum type error\n");
		return 0;
	}
	if (RefFixnum(pos) == value) {
		degrade_printf("fixnum value error\n");
		return 0;
	}

	return 1;
}

static int test_check_tagbody(void)
{
	addr cons, tag, body, check;

	readstring(&cons, "(100 (progn) aaa (quote bbb) (function ccc))");
	check_tagbody(cons, &tag, &body);
	/* tag */
	GetCons(tag, &check, &tag);
	test(test_tagfixnum(check, 100), "check_tagbody1");
	GetCons(tag, &check, &tag);
	test(test_tagsymbol(check, "AAA"), "check_tagbody2");
	test(tag == Nil, "check_tagbody3");
	/* body */
	GetCons(body, &check, &body);
	test(test_tagfixnum(check, 100), "check_tagbody4");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_PROGN), "check_tagbody5");
	GetCons(body, &check, &body);
	test(test_tagsymbol(check, "AAA"), "check_tagbody6");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_QUOTE), "check_tagbody7");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_FUNCTION), "check_tagbody8");
	test(body == Nil, "check_tagbody9");

	RETURN;
}

static int test_parse_tagbody(void)
{
	addr cons, tag, body, check;

	readstring(&cons, "(tagbody (progn) 100)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_TAGBODY), "parse_tagbody1");
	GetEvalParse(cons, 0, &tag);
	GetEvalParse(cons, 1, &body);

	GetCons(tag, &check, &tag);
	test(test_tagfixnum(check, 100), "parse_tagbody2");
	test(tag == Nil, "parse_tagbody3");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_PROGN), "parse_tagbody4");
	GetCons(body, &check, &body);
	test(test_tagfixnum(check, 100), "parse_tagbody5");
	test(body == Nil, "parse_tagbody6");

	RETURN;
}

static int test_parse_go(void)
{
	addr cons;

	readstring(&cons, "(go hello)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_GO), "parse_go1");
	GetEvalParse(cons, 0, &cons);
	test(symbolp(cons), "parse_go2");
	GetNameSymbol(cons, &cons);
	test(string_equal_char(cons, "HELLO"), "parse_go3");

	RETURN;
}

static int test_parse_block(void)
{
	addr cons, check;

	readstring(&cons, "(block nil)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_BLOCK), "parse_block1");
	test(RefEval(cons, 0) == Nil, "parse_block2");
	test(RefEval(cons, 1) == Nil, "parse_block3");

	readstring(&cons, "(block hello 10 20 30)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_BLOCK), "parse_block4");
	GetEvalParse(cons, 0, &check);
	test(check == interncharr(LISP_PACKAGE, "HELLO"), "parse_block5");
	GetEvalParse(cons, 1, &cons);
	GetCons(cons, &check, &cons);
	test(test_eqlfixnum(check, 10), "parse_block6");
	GetCons(cons, &check, &cons);
	test(test_eqlfixnum(check, 20), "parse_block7");
	GetCons(cons, &check, &cons);
	test(test_eqlfixnum(check, 30), "parse_block8");
	test(cons == Nil, "parse_block9");

	RETURN;
}

static int test_parse_return_from(void)
{
	addr cons, check;

	readstring(&cons, "(return-from nil)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_RETURN_FROM), "parse_return_from1");
	test(RefEval(cons, 0) == Nil, "parse_return_from2");
	GetEvalParse(cons, 1, &cons);
	test(test_checktype(cons, EVAL_PARSE_NIL), "parse_return_from3");

	readstring(&cons, "(return-from hello 100)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_RETURN_FROM), "parse_return_from4");
	GetEvalParse(cons, 0, &check);
	test(check == interncharr(LISP_PACKAGE, "HELLO"), "parse_return_from5");
	GetEvalParse(cons, 1, &cons);
	test(test_eqlfixnum(cons, 100), "parse_return_from6");

	RETURN;
}

static int test_parse_catch(void)
{
	addr cons, check;

	readstring(&cons, "(catch nil)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_CATCH), "parse_catch1");
	GetEvalParse(cons, 0, &check);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_catch2");
	test(RefEval(cons, 1) == Nil, "parse_catch3");

	readstring(&cons, "(catch hello 10 20 30)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_CATCH), "parse_catch4");
	GetEvalParse(cons, 0, &check);
	test(RefEvalParseType(check) == EVAL_PARSE_SYMBOL, "parse_catch5");
	GetEvalParse(check, 0, &check);
	GetNameSymbol(check, &check);
	test(string_equal_char(check, "HELLO"), "parse_catch6");
	GetEvalParse(cons, 1, &cons);
	GetCons(cons, &check, &cons);
	test(test_eqlfixnum(check, 10), "parse_catch7");
	GetCons(cons, &check, &cons);
	test(test_eqlfixnum(check, 20), "parse_catch8");
	GetCons(cons, &check, &cons);
	test(test_eqlfixnum(check, 30), "parse_catch9");
	test(cons == Nil, "parse_catch10");

	RETURN;
}

static int test_parse_throw(void)
{
	addr cons, check;

	readstring(&cons, "(throw nil 100)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_THROW), "parse_throw1");
	GetEvalParse(cons, 0, &check);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_throw2");
	GetEvalParse(cons, 1, &check);
	test(test_eqlfixnum(check, 100), "parse_throw6");

	readstring(&cons, "(throw hello 200)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_THROW), "parse_throw4");
	GetEvalParse(cons, 0, &check);
	test(RefEvalParseType(check) == EVAL_PARSE_SYMBOL, "parse_throw5");
	GetEvalParse(check, 0, &check);
	GetNameSymbol(check, &check);
	test(string_equal_char(check, "HELLO"), "parse_throw6");
	GetEvalParse(cons, 1, &cons);
	test(test_eqlfixnum(cons, 200), "parse_throw7");

	RETURN;
}

static int test_parse_flet_one(void)
{
	addr cons, check, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(Execute_Thread);

	readstring(&cons, "(hello ())");
	parse_flet_one(&cons, cons);
	GetCons(cons, &check, &cons); /* name */
	test(GetType(check) == LISPTYPE_CALLNAME, "parse_flet_one1");
	GetCallName(check, &check);
	test(check == interncharr(LISP_PACKAGE, "HELLO"), "parse_flet_one2");
	GetCons(cons, &check, &cons); /* args */
	GetCar(check, &check);
	test(check == Nil, "parse_flet_one3");
	GetCons(cons, &check, &cons); /* decl */
	test(check == Nil, "parse_flet_one4");
	GetCons(cons, &check, &cons); /* doc */
	test(check == Nil, "parse_flet_one5");
	GetCons(cons, &check, &cons); /* body */
	test(check != Nil, "parse_flet_one6");
	test(cons == Nil, "parse_flet_one7");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_flet_args(void)
{
	addr cons, check, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(Execute_Thread);

	readstring(&cons, "((aaa (a) :aaa) (bbb (b) :bbb))");
	parse_flet_args(&cons, cons);
	/* (aaa (a) :aaa) */
	GetCons(cons, &check, &cons);
	GetCar(check, &check);
	test(GetType(check) == LISPTYPE_CALLNAME, "parse_flet_args1");
	GetCallName(check, &check);
	test(test_eqsymbol(check, "AAA"), "parse_flet_args2");
	/* (bbb (b) :bbb) */
	GetCons(cons, &check, &cons);
	GetCar(check, &check);
	test(GetType(check) == LISPTYPE_CALLNAME, "parse_flet_args3");
	GetCallName(check, &check);
	test(test_eqsymbol(check, "BBB"), "parse_flet_args4");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_flet_labels(void)
{
	addr cons, left, right;

	readstring(&cons, "(flet ((aaa (a) :aaa)) :hello)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FLET), "parse_flet_labels1");

	GetEvalParse(cons, 0, &right);
	GetCons(right, &left, &right);
	GetCar(left, &left);
	test(GetType(left) == LISPTYPE_CALLNAME, "parse_flet_labels2");
	test(right == Nil, "parse_flet_labels3");
	GetCallName(left, &left);
	test(test_eqsymbol(left, "AAA"), "parse_flet_labels4");

	GetEvalParse(cons, 1, &left);
	test(left == Nil, "parse_flet_labels5");

	GetEvalParse(cons, 2, &right);
	GetCons(right, &left, &right);
	test(test_evalkeyword(left, "HELLO"), "parse_flet_labels6");
	test(right == Nil, "parse_flet_labels7");

	RETURN;
}

static int test_parse_the(void)
{
	addr cons, check;

	readstring(&cons, "(the integer (progn :hello))");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_THE), "parse_the1");
	GetEvalParse(cons, 0, &check);
	test(GetType(check) == LISPTYPE_TYPE, "parse_the2");
	GetEvalParse(cons, 1, &check);
	test(test_checktype(check, EVAL_PARSE_PROGN), "parse_the3");

	RETURN;
}

static int test_parse_eval_when(void)
{
	addr cons, check;

	readstring(&cons, "(eval-when (:compile-toplevel) :hello)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_EVAL_WHEN), "parse_eval_when1");
	GetEvalParse(cons, 0, &check);
	getcar(check, &check);
	test(test_evalkeyword(check, "HELLO"), "parse_eval_when2");
	test(RefEval(cons, 1) == T, "parse_eval_when3");
	test(RefEval(cons, 2) == Nil, "parse_eval_when4");
	test(RefEval(cons, 3) == Nil, "parse_eval_when5");

	readstring(&cons, "(eval-when (load) :hello)");
	eval_parse(&cons, cons);
	test(RefEval(cons, 1) == Nil, "parse_eval_when6");
	test(RefEval(cons, 2) == T, "parse_eval_when7");
	test(RefEval(cons, 3) == Nil, "parse_eval_when8");

	readstring(&cons, "(eval-when (:execute) :hello)");
	eval_parse(&cons, cons);
	test(RefEval(cons, 1) == Nil, "parse_eval_when9");
	test(RefEval(cons, 2) == Nil, "parse_eval_when10");
	test(RefEval(cons, 3) == T, "parse_eval_when11");

	readstring(&cons, "(eval-when (compile :load-toplevel eval) :hello)");
	eval_parse(&cons, cons);
	test(RefEval(cons, 1) == T, "parse_eval_when12");
	test(RefEval(cons, 2) == T, "parse_eval_when13");
	test(RefEval(cons, 3) == T, "parse_eval_when14");

	RETURN;
}

static int test_parse_values(void)
{
	addr cons, pos;

	readstring(&cons, "(values)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_VALUES), "parse_values1");
	GetEvalParse(cons, 0, &cons);
	test(cons == Nil, "parse_values2");

	readstring(&cons, "(values t)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_VALUES), "parse_values3");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CONS, "parse_values4");
	GetCons(cons, &pos, &cons);
	test(test_checktype(pos, EVAL_PARSE_T), "parse_values5");
	test(cons == Nil, "parse_values6");

	readstring(&cons, "(values 100 t)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_VALUES), "parse_values7");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CONS, "parse_values8");
	GetCons(cons, &pos, &cons);
	test(test_checktype(pos, EVAL_PARSE_INTEGER), "parse_values9");
	GetCons(cons, &pos, &cons);
	test(test_checktype(pos, EVAL_PARSE_T), "parse_values10");
	test(cons == Nil, "parse_values11");

	RETURN;
}

static int test_parse_call(void)
{
	addr cons, left, right;

	readstring(&cons, "(unbounded-function 10 20 30)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_CALL), "parse_call1");
	GetEvalParse(cons, 0, &left);
	test(test_checktype(left, EVAL_PARSE_FUNCTION), "parse_call2");
	GetEvalParse(left, 0, &left);
	test(GetType(left) == LISPTYPE_CALLNAME, "parse_call3");
	test(RefCallNameType(left) == CALLNAME_SYMBOL, "parse_call4");
	GetCallName(left, &left);
	test(test_eqsymbol(left, "UNBOUNDED-FUNCTION"), "parse_call5");
	GetEvalParse(cons, 1, &right);
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 10), "parse_call6");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 20), "parse_call7");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 30), "parse_call8");
	test(right == Nil, "parse_call9");

	RETURN;
}

static int test_parse_multiple_value_bind(void)
{
	addr pos, left, right;

	readstring(&pos, "(multiple-value-bind (a b c) (call) 10 20 30)");
	eval_parse(&pos, pos);
	test(test_checktype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND),
			"parse_multiple_value_bind1");
	GetEvalParse(pos, 0, &right);
	test(consp(right), "parse_multiple_value_bind2");
	test(length_list_unsafe(right) == 3, "parse_multiple_value_bind3");
	GetCons(right, &left, &right);
	test(left == readr("a"), "parse_multiple_value_bind4");
	GetEvalParse(pos, 1, &left);
	test(eval_parse_p(left), "parse_multiple_value_bind5");

	RETURN;
}

static int test_parse_multiple_value_call(void)
{
	addr cons, left, right;

	readstring(&cons, "(multiple-value-call #'hello 10 20 30)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_MULTIPLE_VALUE_CALL),
			"parse_multiple_value_call1");
	GetEvalParse(cons, 0, &left);
	test(test_checktype(left, EVAL_PARSE_FUNCTION), "parse_multiple_value_call2");
	GetEvalParse(left, 0, &left);
	test(GetType(left) == LISPTYPE_CALLNAME, "parse_multiple_value_call3");
	test(RefCallNameType(left) == CALLNAME_SYMBOL, "parse_multiple_value_call4");
	GetCallName(left, &left);
	test(test_eqsymbol(left, "HELLO"), "parse_multiple_value_call5");
	GetEvalParse(cons, 1, &right);
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 10), "parse_multiple_value_call6");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 20), "parse_multiple_value_call7");
	GetCons(right, &left, &right);
	test(test_eqlfixnum(left, 30), "parse_multiple_value_call8");
	test(right == Nil, "parse_multiple_value_call9");

	RETURN;
}


/*
 *  macro
 */
static int test_find_envstack(void)
{
	Execute ptr;
	addr control, name, v1, v2, stack, check;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);

	fixnum_heap(&v1, 10);
	name = readr("aaa");
	defmacro_envstack(name, v1);
	fixnum_heap(&v2, 20);
	name = readr("bbb");
	defmacro_envstack(name, v2);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 0, &stack); /* root */
	check = NULL;
	test(find_envstack(readr("aaa"), stack, T, &check), "find_envstack1");
	test(check == v1, "find_envstack2");
	check = NULL;
	test(find_envstack(readr("bbb"), stack, T, &check), "find_envstack3");
	test(check == v2, "find_envstack4");
	check = NULL;
	test(! find_envstack(readr("ccc"), stack, T, &check), "find_envstack5");

	free_control(ptr, control);

	RETURN;
}

static int test_check_macro_function(void)
{
	Execute ptr;
	addr control, v1, v2, v3, sym1, sym2, sym3, sym4, check;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);

	sym1 = readr("aaa");
	sym2 = readr("bbb");
	sym3 = readr("ccc");
	sym4 = readr("ddd");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);

	defmacro_envstack(sym1, v1);
	macrolet_envstack(sym2, v2);
	setmacro_symbol(sym3, v3);
	remmacro_symbol(sym4);

	test(check_macro_function(sym1, &check), "check_macro_function1");
	test(check == v1, "check_macro_function2");
	test(check_macro_function(sym2, &check), "check_macro_function3");
	test(check == v2, "check_macro_function4");
	test(check_macro_function(sym3, &check), "check_macro_function5");
	test(check == v3, "check_macro_function6");
	test(! check_macro_function(sym4, &check), "check_macro_function7");

	remmacro_symbol(sym3);

	free_control(ptr, control);

	RETURN;
}

static void test_call_macroexpand_hook_function(Execute ptr,
		addr call, addr form, addr env)
{
	fixnum_heap(&form, RefFixnum(form) + 1);
	setresult_control(ptr, form);
}

static int test_call_macroexpand_hook(void)
{
	addr control, call, hook;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&call, Nil);
	SetPointer(p_debug1, var3, test_call_macroexpand_hook_function);
	setcompiled_var3(call, p_debug1);
	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	pushspecial_control(ptr, hook, call);
	call_macroexpand_hook(&call, T, fixnumh(10), Nil);
	test(RefFixnum(call) == 11, "call_macroexpand_hook1");

	free_control(ptr, control);

	RETURN;
}

static int test_parse_macro(void)
{
	addr control, cons;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_environment(ptr);

	/*
	 *  common_eval.c:
	 *    (defmacro lambda (&whole whole &rest args)
	 *      (declare (ignore args))
	 *      `(function ,whole))
	 */
	readstring(&cons, "(lambda () :hello)");
	eval_parse(&cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LAMBDA), "parse_macro1");

	free_control(ptr, control);

	RETURN;
}


/*
 *  copy eval-parse
 */
static int test_copy_single(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	readstring(&pos, "t");
	eval_parse(&pos, pos);
	copy_single(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_single1");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "copy_single2");
	test(RefEvalParse(pos, 0) == T, "copy_single3");

	copy_single(local, &pos, pos);
	test(GetStatusDynamic(pos), "copy_single4");
	fixnum_local(local, &value, 100);
	test(GetStatusDynamic(value), "copy_single5");
	SetEvalParse(pos, 0, value);

	copy_single(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_single6");
	GetEvalParse(pos, 0, &value);
	test(! GetStatusDynamic(value), "copy_single7");
	test(RefFixnum(value) == 100, "copy_single8");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_nil(void)
{
	addr pos;

	readstring(&pos, "nil");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_nil1");
	test(RefEvalParseType(pos) == EVAL_PARSE_NIL, "copy_nil2");
	test(RefEvalParse(pos, 0) == Nil, "copy_nil3");

	RETURN;
}

static int test_copy_t(void)
{
	addr pos;

	readstring(&pos, "t");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_t1");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "copy_t2");
	test(RefEvalParse(pos, 0) == T, "copy_t3");

	RETURN;
}

static int test_copy_integer(void)
{
	addr pos;

	fixnum_heap(&pos, 100);
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_integer1");
	test(RefEvalParseType(pos) == EVAL_PARSE_INTEGER, "copy_integer2");
	GetEvalParse(pos, 0, &pos);
	test(RefFixnum(pos) == 100, "copy_integer3");

	RETURN;
}

static int test_copy_string(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_string1");
	test(RefEvalParseType(pos) == EVAL_PARSE_STRING, "copy_string2");
	GetEvalParse(pos, 0, &pos);
	test(string_equal_char(pos, "Hello"), "copy_string3");

	RETURN;
}

static int test_copy_symbol(void)
{
	addr pos;

	readstring(&pos, "hello");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_symbol1");
	test(RefEvalParseType(pos) == EVAL_PARSE_SYMBOL, "copy_symbol2");
	GetEvalParse(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "copy_symbol3");
	GetNameSymbol(pos, &pos);
	test(string_equal_char(pos, "HELLO"), "copy_symbol4");

	RETURN;
}

static int test_copy_float(void)
{
	addr pos;

	readstring(&pos, "10.5s0");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_float1");
	test(RefEvalParseType(pos) == EVAL_PARSE_FLOAT, "copy_float2");
	GetEvalParse(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "copy_float3");
	test(RefSingleFloat(pos) == 10.5f, "copy_float4");

	RETURN;
}

static int test_copy_function(void)
{
	addr pos;

	readstring(&pos, "#'hello");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_function1");
	test(RefEvalParseType(pos) == EVAL_PARSE_FUNCTION, "copy_function2");
	GetEvalParse(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_CALLNAME, "copy_function3");
	GetCallName(pos, &pos);
	GetNameSymbol(pos, &pos);
	test(string_equal_char(pos, "HELLO"), "copy_function4");

	RETURN;
}

static int test_copy_quote(void)
{
	addr pos;

	readstring(&pos, "'hello");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_quote1");
	test(RefEvalParseType(pos) == EVAL_PARSE_QUOTE, "copy_quote2");
	GetEvalParse(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "copy_quote3");
	GetNameSymbol(pos, &pos);
	test(string_equal_char(pos, "HELLO"), "copy_quote4");

	RETURN;
}

static int test_copy_go(void)
{
	addr pos;

	readstring(&pos, "(go hello)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_go1");
	test(RefEvalParseType(pos) == EVAL_PARSE_GO, "copy_go2");
	GetEvalParse(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "copy_go3");
	GetNameSymbol(pos, &pos);
	test(string_equal_char(pos, "HELLO"), "copy_go4");

	RETURN;
}

static int test_copy_declaim(void)
{
	addr pos;

	readstring(&pos, "(declaim (special aaa))");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_declaim1");
	test(RefEvalParseType(pos) == EVAL_PARSE_DECLAIM, "copy_declaim2");
	GetEvalParse(pos, 0, &pos);
	test(eval_declare_p(pos), "copy_declaim3");

	RETURN;
}

static int test_copy_allcons(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	readstring(&pos, "(progn 10 20 30)");
	eval_parse(&pos, pos);
	GetEvalParse(pos, 0, &pos);

	copy_allcons(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_allcons1");
	test(length_list_unsafe(pos) == 3, "copy_allcons2");

	GetCons(pos, &check, &pos);
	test(RefEvalParseType(check) == EVAL_PARSE_INTEGER, "copy_allcons3");
	GetEvalParse(check, 0, &check);
	test(RefFixnum(check) == 10, "copy_allcons4");

	GetCons(pos, &check, &pos);
	test(RefEvalParseType(check) == EVAL_PARSE_INTEGER, "copy_allcons5");
	GetEvalParse(check, 0, &check);
	test(RefFixnum(check) == 20, "copy_allcons6");

	GetCons(pos, &check, &pos);
	test(RefEvalParseType(check) == EVAL_PARSE_INTEGER, "copy_allcons7");
	GetEvalParse(check, 0, &check);
	test(RefFixnum(check) == 30, "copy_allcons8");
	test(pos == Nil, "copy_allcons9");

	readstring(&pos, "(progn 10 20 30)");
	eval_parse(&pos, pos);
	GetEvalParse(pos, 0, &pos);

	copy_allcons(local, &pos, pos);
	test(GetStatusDynamic(pos), "copy_allcons10");
	test(length_list_unsafe(pos) == 3, "copy_allcons11");

	copy_allcons(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_allcons12");
	test(length_list_unsafe(pos) == 3, "copy_allcons13");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_progn(void)
{
	addr pos;

	readstring(&pos, "(progn 10 20 30)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_progn1");
	test(RefEvalParseType(pos) == EVAL_PARSE_PROGN, "copy_progn2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 3, "copy_progn3");

	RETURN;
}

static int test_copy_let(void)
{
	addr pos, check, var, init, sym;

	readstring(&pos, "(let (a (b 100)) (declare (special a)) :hello)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_let1");
	test(RefEvalParseType(pos) == EVAL_PARSE_LET, "copy_let2");

	GetEvalParse(pos, 0, &check);
	test(length_list_unsafe(check) == 2, "copy_let3");
	GetCons(check, &var, &check);
	GetCons(var, &var, &init);
	readstring(&sym, "a");
	test(var == sym, "copy_let4");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_let5");
	GetCons(check, &var, &check);
	GetCons(var, &var, &init);
	readstring(&sym, "b");
	test(var == sym, "copy_let6");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_let7");

	GetEvalParse(pos, 1, &check);
	test(eval_declare_p(check), "copy_let8");

	GetEvalParse(pos, 2, &check);
	test(length_list_unsafe(check) == 1, "copy_let9");
	GetCar(check, &check);
	test(RefEvalParseType(check) == EVAL_PARSE_SYMBOL, "copy_let10");

	readstring(&pos, "(let* (a (b 100)) (declare (special a)) :hello)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(RefEvalParseType(pos) == EVAL_PARSE_LETA, "copy_let11");

	RETURN;
}

static int test_copy_setq(void)
{
	addr pos, symbol, value, check;

	readstring(&pos, "(setq aaa 10 bbb 20)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_setq1");
	test(RefEvalParseType(pos) == EVAL_PARSE_SETQ, "copy_setq2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "copy_setq3");

	GetCons(pos, &symbol, &pos);
	GetCons(symbol, &symbol, &value);
	readstring(&check, "aaa");
	test(symbol == check, "copy_setq4");
	test(RefEvalParseType(value) == EVAL_PARSE_INTEGER, "copy_setq5");
	GetEvalParse(value, 0, &value);
	test(RefFixnum(value) == 10, "copy_setq6");

	GetCons(pos, &symbol, &pos);
	GetCons(symbol, &symbol, &value);
	readstring(&check, "bbb");
	test(symbol == check, "copy_setq7");
	test(RefEvalParseType(value) == EVAL_PARSE_INTEGER, "copy_setq8");
	GetEvalParse(value, 0, &value);
	test(RefFixnum(value) == 20, "copy_setq9");

	RETURN;
}

static int test_copy_ordinary_optional(void)
{
	addr pos, var, init, svar, check;

	readstring(&pos, "(&optional aaa (bbb 10))");
	parse_ordinary(&pos, pos);
	/* (var opt rest key allow aux) */
	getnth(pos, 1, &pos);
	copy_ordinary_optional(NULL, &pos, pos);
	test(length_list_unsafe(pos) == 2, "copy_ordinary_optional1");
	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "aaa");
	test(var == check, "copy_ordinary_optional2");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_ordinary_optional3");
	test(svar == Nil, "copy_ordinary_optional4");

	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "bbb");
	test(var == check, "copy_ordinary_optional5");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_ordinary_optional6");

	RETURN;
}

static int test_copy_ordinary_key(void)
{
	addr pos, var, name, init, svar, check;

	readstring(&pos, "(&key aaa ((name bbb) 10 ddd))");
	parse_ordinary(&pos, pos);
	/* (var opt rest key allow aux) */
	getnth(pos, 3, &pos);
	copy_ordinary_key(NULL, &pos, pos);
	test(length_list_unsafe(pos) == 2, "copy_ordinary_key1");
	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &name, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "aaa");
	test(var == check, "copy_ordinary_key2");
	readstring(&check, ":aaa");
	test(name == check, "copy_ordinary_key3");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_ordinary_key4");
	test(svar == Nil, "copy_ordinary_key5");

	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &name, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "bbb");
	test(var == check, "copy_ordinary_key6");
	readstring(&check, "name");
	test(name == check, "copy_ordinary_key7");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_ordinary_key8");
	readstring(&check, "ddd");
	test(svar == check, "copy_ordinary_key9");

	RETURN;
}

static int test_copy_ordinary_aux(void)
{
	addr pos, var, init, check;

	readstring(&pos, "(&aux aaa (bbb 10))");
	parse_ordinary(&pos, pos);
	/* (var opt rest key allow aux) */
	getnth(pos, 5, &pos);
	copy_ordinary_aux(NULL, &pos, pos);
	test(length_list_unsafe(pos) == 2, "copy_ordinary_aux1");
	GetCons(pos, &init, &pos);
	GetCons(init, &var, &init);
	GetCar(init, &init);
	readstring(&check, "aaa");
	test(var == check, "copy_ordinary_aux2");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_ordinary_aux3");

	GetCons(pos, &init, &pos);
	GetCons(init, &var, &init);
	GetCar(init, &init);
	readstring(&check, "bbb");
	test(var == check, "copy_ordinary_aux4");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_ordinary_aux5");

	RETURN;
}

static int test_copy_ordinary(void)
{
	addr pos, var, check;

	readstring(&pos, "(aa &optional bb &rest cc &key dd &allow-other-keys &aux ee)");
	parse_ordinary(&pos, pos);
	copy_ordinary(NULL, &pos, pos);
	/* var */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_ordinary1");
	GetCar(var, &var);
	readstring(&check, "aa");
	test(var == check, "copy_ordinary2");
	/* optional */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_ordinary3");
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "bb");
	/* rest */
	GetCons(pos, &var, &pos);
	readstring(&check, "cc");
	test(var == check, "copy_ordinary5");
	/* key */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_ordinary6");
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "dd");
	test(var == check, "copy_ordinary7");
	/* allow-other-keys */
	GetCons(pos, &var, &pos);
	test(var == T, "copy_ordinary8");
	/* aux */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_ordinary9");
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "ee");
	test(var == check, "copy_ordinary10");

	RETURN;
}

static int test_copy_defun(void)
{
	addr pos, symbol, check;

	readstring(&pos, "(defun aa (bb) (declare (special cc)) \"DOC\" 10 20)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_defun1");
	test(RefEvalParseType(pos) == EVAL_PARSE_DEFUN, "copy_defun2");
	/* name */
	GetEvalParse(pos, 0, &symbol);
	test(GetType(symbol) == LISPTYPE_CALLNAME, "copy_defun3");
	GetCallName(symbol, &symbol);
	readstring(&check, "aa");
	test(symbol == check, "copy_defun4");
	/* args */
	GetEvalParse(pos, 1, &symbol);
	GetCar(symbol, &symbol);
	GetCar(symbol, &symbol);
	readstring(&check, "bb");
	test(symbol == check, "copy_defun5");
	/* decl */
	GetEvalParse(pos, 2, &symbol);
	test(eval_declare_p(symbol), "copy_defun6");
	/* doc */
	GetEvalParse(pos, 3, &symbol);
	test(stringp(symbol), "copy_defun7");
	/* cons */
	GetEvalParse(pos, 4, &symbol);
	getcar(symbol, &symbol);
	test(RefEvalParseType(symbol) == EVAL_PARSE_BLOCK, "copy_defun8");

	RETURN;
}

static int test_copy_lambda(void)
{
	addr pos, symbol, check;

	readstring(&pos, "#'(lambda (bb) (declare (special cc)) \"DOC\" 10 20)");
	eval_parse(&pos, pos);
	copy_eval_parse(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_lambda1");
	test(RefEvalParseType(pos) == EVAL_PARSE_LAMBDA, "copy_lambda2");
	/* args */
	GetEvalParse(pos, 0, &symbol);
	GetCar(symbol, &symbol);
	GetCar(symbol, &symbol);
	readstring(&check, "bb");
	test(symbol == check, "copy_lambda3");
	/* decl */
	GetEvalParse(pos, 1, &symbol);
	test(eval_declare_p(symbol), "copy_lambda4");
	/* doc */
	GetEvalParse(pos, 2, &symbol);
	test(stringp(symbol), "copy_lambda5");
	/* cons */
	GetEvalParse(pos, 3, &symbol);
	getcar(symbol, &symbol);
	test(RefEvalParseType(symbol) == EVAL_PARSE_INTEGER, "copy_lambda6");
	GetEvalParse(symbol, 0, &symbol);
	test(RefFixnum(symbol) == 10, "copy_lambda7");

	RETURN;
}

static int test_copy_if(void)
{
	addr pos, var, check;

	readstring(&check, "(if aa bb cc)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_if1");
	test(! GetStatusDynamic(pos), "copy_if2");
	test(RefEvalParseType(pos) == EVAL_PARSE_IF, "copy_if3");
	/* expr */
	GetEvalParse(pos, 0, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_if4");
	GetEvalParse(var, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_if5");
	/* then */
	GetEvalParse(pos, 1, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_if6");
	GetEvalParse(var, 0, &var);
	readstring(&check, "bb");
	test(var == check, "copy_if7");
	/* last  */
	GetEvalParse(pos, 2, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_if8");
	GetEvalParse(var, 0, &var);
	readstring(&check, "cc");
	test(var == check, "copy_if9");

	RETURN;
}

static int test_copy_unwind_protect(void)
{
	addr pos, var, check;

	readstring(&check, "(unwind-protect aa bb cc)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_unwind_protect1");
	test(! GetStatusDynamic(pos), "copy_unwind_protect2");
	test(RefEvalParseType(pos) == EVAL_PARSE_UNWIND_PROTECT, "copy_unwind_protect3");
	/* form */
	GetEvalParse(pos, 0, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_unwind_protect4");
	GetEvalParse(var, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_unwind_protect5");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 2, "copy_unwind_protect6");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_unwind_protect7");
	GetEvalParse(var, 0, &var);
	readstring(&check, "bb");
	test(var == check, "copy_unwind_protect8");

	RETURN;
}

static int test_copy_tagbody(void)
{
	addr pos, var, check;

	readstring(&check, "(tagbody aa (progn) (hello) cc)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_tagbody1");
	test(! GetStatusDynamic(pos), "copy_tagbody2");
	test(RefEvalParseType(pos) == EVAL_PARSE_TAGBODY, "copy_tagbody3");
	/* tag */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 2, "copy_tagbody4");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_TAG, "copy_tagbody5");
	GetEvalParse(var, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_tagbody7");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 4, "copy_tagbody8");
	GetCdr(var, &var); /* aa */
	GetCar(var, &var); /* (progn) */
	test(RefEvalParseType(var) == EVAL_PARSE_PROGN, "copy_tagbody9");

	RETURN;
}

static int test_copy_tag(void)
{
	addr pos, var, check;

	readstring(&check, "(tagbody aa bb cc)");
	eval_parse(&check, check);
	GetEvalParse(check, 0, &check);
	GetCar(check, &check); /* tag aa */
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_tag1");
	test(! GetStatusDynamic(pos), "copy_tag2");
	test(RefEvalParseType(pos) == EVAL_PARSE_TAG, "copy_tag3");
	/* tag */
	GetEvalParse(pos, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_tag4");
	/* value */
	GetEvalParse(pos, 1, &var);
	test(GetType(var) == LISPTYPE_FIXNUM, "copy_tag5");

	RETURN
}

static int test_copy_block(void)
{
	addr pos, var, check;

	readstring(&check, "(block aa bb cc)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_block1");
	test(! GetStatusDynamic(pos), "copy_block2");
	test(RefEvalParseType(pos) == EVAL_PARSE_BLOCK, "copy_block3");
	/* name */
	GetEvalParse(pos, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_block4");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 2, "copy_block5");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_block6");
	GetEvalParse(var, 0, &var);
	readstring(&check, "bb");
	test(var == check, "copy_block7");

	RETURN;
}

static int test_copy_return_from(void)
{
	addr pos, var, check;

	readstring(&check, "(return-from aa bb)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_return_from1");
	test(! GetStatusDynamic(pos), "copy_return_from2");
	test(RefEvalParseType(pos) == EVAL_PARSE_RETURN_FROM, "copy_return_from3");
	/* name */
	GetEvalParse(pos, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_return_from4");
	/* value */
	GetEvalParse(pos, 1, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_return_from5");
	GetEvalParse(var, 0, &var);
	readstring(&check, "bb");
	test(var == check, "copy_return_from6");

	RETURN;
}

static int test_copy_catch(void)
{
	addr pos, var, check;

	readstring(&check, "(catch aa bb cc)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_catch1");
	test(! GetStatusDynamic(pos), "copy_catch2");
	test(RefEvalParseType(pos) == EVAL_PARSE_CATCH, "copy_catch3");
	/* name */
	GetEvalParse(pos, 0, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_catch4");
	GetEvalParse(var, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_catch5");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 2, "copy_catch6");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_catch7");
	GetEvalParse(var, 0, &var);
	readstring(&check, "bb");
	test(var == check, "copy_catch8");

	RETURN;
}

static int test_copy_throw(void)
{
	addr pos, var, check;

	readstring(&check, "(throw aa bb)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_throw1");
	test(! GetStatusDynamic(pos), "copy_throw2");
	test(RefEvalParseType(pos) == EVAL_PARSE_THROW, "copy_throw3");
	/* name */
	GetEvalParse(pos, 0, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_throw4");
	GetEvalParse(var, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_throw5");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_throw6");
	GetEvalParse(var, 0, &var);
	readstring(&check, "bb");
	test(var == check, "copy_throw7");

	RETURN;
}

static int test_copy_flet_one(void)
{
	addr pos, var, check;

	readstring(&check, "(flet ((aa (bb) (declare (special bb)) \"HELLO\" :hello)))");
	eval_parse(&check, check);
	GetEvalParse(check, 0, &check); /* args */
	GetCar(check, &check);
	copy_flet_one(NULL, &pos, check);
	test(check != pos, "copy_flet_one1");
	/* name */
	GetCons(pos, &var, &pos);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_flet_one2");
	GetCallName(var, &var);
	readstring(&check, "aa");
	test(var == check, "copy_flet_one3");
	/* args */
	GetCons(pos, &var, &pos);
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "bb");
	test(var == check, "copy_flet_one4");
	/* decl */
	GetCons(pos, &var, &pos);
	test(eval_declare_p(var), "copy_flet_one5");
	/* doc */
	GetCons(pos, &var, &pos);
	test(stringp(var), "copy_flet_one6");
	/* cons */
	GetCar(pos, &var);
	test(length_list_unsafe(var) == 1, "copy_flet_one7");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_BLOCK, "copy_flet_one8");

	RETURN;
}

static int test_copy_flet_args(void)
{
	addr pos, var, check;

	readstring(&check, "(flet ((aa () :aa) (bb (b) :bb)) :hello)");
	eval_parse(&check, check);
	GetEvalParse(check, 0, &check); /* args */
	copy_flet_args(NULL, &pos, check);
	test(check != pos, "copy_flet_args1");
	test(length_list_unsafe(pos) == 2, "copy_flet_args2");
	/* aa */
	GetCons(pos, &var, &pos);
	GetCar(var, &var);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_flet_args3");
	GetCallName(var, &var);
	readstring(&check, "aa");
	test(var == check, "copy_flet_args4");
	/* bb */
	GetCons(pos, &var, &pos);
	GetCar(var, &var);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_flet_args5");
	GetCallName(var, &var);
	readstring(&check, "bb");
	test(var == check, "copy_flet_args6");

	RETURN;
}

static int test_copy_flet(void)
{
	addr pos, var, check;

	/* flet */
	readstring(&check, "(flet ((aa () :aa)) (declare (special bb)) :hello)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_flet1");
	test(! GetStatusDynamic(pos), "copy_flet2");
	test(RefEvalParseType(pos) == EVAL_PARSE_FLET, "copy_flet3");
	/* args */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 1, "copy_flet4");
	/* decl */
	GetEvalParse(pos, 1, &var);
	test(eval_declare_p(var), "copy_flet5");
	/* cons */
	GetEvalParse(pos, 2, &var);
	test(length_list_unsafe(var) == 1, "copy_flet6");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_flet7");
	GetEvalParse(var, 0, &var);
	readstring(&check, ":hello");
	test(var == check, "copy_flet8");

	/* labels */
	readstring(&check, "(labels ((aa () :aa) (bb (cc) dd)) :hello)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_flet9");
	test(! GetStatusDynamic(pos), "copy_flet10");
	test(RefEvalParseType(pos) == EVAL_PARSE_LABELS, "copy_flet11");
	/* args */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 2, "copy_flet12");

	RETURN;
}

static int test_copy_the(void)
{
	addr pos, var, check;

	readstring(&check, "(the integer (progn aa bb cc))");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_the1");
	test(! GetStatusDynamic(pos), "copy_the2");
	test(RefEvalParseType(pos) == EVAL_PARSE_THE, "copy_the3");
	/* type */
	GetEvalParse(pos, 0, &var);
	test(GetType(var) == LISPTYPE_TYPE, "copy_the4");
	test(RefLispDecl(var) == LISPDECL_INTEGER, "copy_the5");
	/* expr */
	GetEvalParse(pos, 1, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_PROGN, "copy_the6");

	RETURN;
}

static int test_copy_eval_when(void)
{
	addr pos, var, check;

	readstring(&check, "(eval-when (eval compile) :hello1 :hello2)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_eval_when1");
	test(! GetStatusDynamic(pos), "copy_eval_when2");
	test(RefEvalParseType(pos) == EVAL_PARSE_EVAL_WHEN, "copy_eval_when3");
	/* cons */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 2, "copy_eval_when4");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_eval_when5");
	GetEvalParse(var, 0, &var);
	readstring(&check, ":hello1");
	test(var == check, "copy_eval_when6");
	/* compilep */
	GetEvalParse(pos, 1, &var);
	test(var == T, "copy_eval_when7");
	/* loadp */
	GetEvalParse(pos, 2, &var);
	test(var == Nil, "copy_eval_when8");
	/* evalp */
	GetEvalParse(pos, 3, &var);
	test(var == T, "copy_eval_when9");

	RETURN;
}

static int test_copy_values(void)
{
	addr pos, var, check;

	readstring(&check, "(values 10 20 30)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_values1");
	test(! GetStatusDynamic(pos), "copy_values2");
	test(RefEvalParseType(pos) == EVAL_PARSE_VALUES, "copy_values3");
	/* cons */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 3, "copy_values4");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_INTEGER, "copy_values5");
	GetEvalParse(var, 0, &var);
	test(RefFixnum(var) == 10, "copy_values6");

	RETURN;
}

static int test_copy_locally(void)
{
	addr pos, var, check;

	readstring(&check, "(locally (declare (special a)) 10 20)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_locally1");
	test(! GetStatusDynamic(pos), "copy_locally2");
	test(RefEvalParseType(pos) == EVAL_PARSE_LOCALLY, "copy_locally3");
	/* type */
	GetEvalParse(pos, 0, &var);
	test(eval_declare_p(var), "copy_locally4");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 2, "copy_locally5");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_INTEGER, "copy_locally6");
	GetEvalParse(var, 0, &var);
	test(RefFixnum(var) == 10, "copy_locally7");

	RETURN;
}

static int test_copy_call(void)
{
	addr pos, var, check;

	readstring(&check, "(hello 10 20)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_call1");
	test(! GetStatusDynamic(pos), "copy_call2");
	test(RefEvalParseType(pos) == EVAL_PARSE_CALL, "copy_call3");
	/* type */
	GetEvalParse(pos, 0, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_FUNCTION, "copy_call4");
	GetEvalParse(var, 0, &var);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_call5");
	GetCallName(var, &var);
	readstring(&check, "hello");
	test(var == check, "copy_call6");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 2, "copy_call7");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_INTEGER, "copy_call8");
	GetEvalParse(var, 0, &var);
	test(RefFixnum(var) == 10, "copy_call9");

	RETURN;
}

static int test_copy_multiple_value_bind(void)
{
	addr check, pos;

	readstring(&check, "(multiple-value-bind (a b c) (call) 10 20 30)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_multiple_value_bind1");
	test(! GetStatusDynamic(pos), "copy_multiple_value_bind2");
	test(RefEvalParseType(pos) == EVAL_PARSE_MULTIPLE_VALUE_BIND,
			"copy_multiple_value_bind3");

	RETURN;
}

static int test_copy_multiple_value_call(void)
{
	addr pos, var, check;

	readstring(&check, "(multiple-value-call #'hello 10 20)");
	eval_parse(&check, check);
	copy_eval_parse(NULL, &pos, check);
	test(check != pos, "copy_multiple_value_call1");
	test(! GetStatusDynamic(pos), "copy_multiple_value_call2");
	test(RefEvalParseType(pos) == EVAL_PARSE_MULTIPLE_VALUE_CALL,
			"copy_multiple_value_call3");
	/* type */
	GetEvalParse(pos, 0, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_FUNCTION, "copy_multiple_value_call4");
	GetEvalParse(var, 0, &var);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_multiple_value_call5");
	GetCallName(var, &var);
	readstring(&check, "hello");
	test(var == check, "copy_multiple_value_call6");
	/* cons */
	GetEvalParse(pos, 1, &var);
	test(length_list_unsafe(var) == 2, "copy_multiple_value_call7");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_INTEGER, "copy_multiple_value_call8");
	GetEvalParse(var, 0, &var);
	test(RefFixnum(var) == 10, "copy_multiple_value_call9");

	RETURN;
}


/*
 *  Main
 */
static int testbreak_eval_parse(void)
{
	in_package_lisp_package();
	/* environment */
	TestBreak(test_environment_symbol);
	TestBreak(test_envroot_heap);
	TestBreak(test_envstack_heap);
	TestBreak(test_init_environment);
	TestBreak(test_snapshot_envstack);
	TestBreak(test_push_envstack);
	TestBreak(test_rollback_envstack);
	TestBreak(test_defmacro_envstack);
	TestBreak(test_environment_heap);
	/* memory */
	TestBreak(test_eval_parse_alloc);
	TestBreak(test_eval_single_parse_alloc);
	TestBreak(test_StructEvalParse);
	TestBreak(test_RefEvalParse);
	TestBreak(test_RefEvalParseType);
	/* eval_parse */
	TestBreak(test_parse_eval_nil);
	TestBreak(test_parse_eval_t);
	TestBreak(test_parse_eval_symbol);
	TestBreak(test_parse_eval_fixnum);
	TestBreak(test_parse_eval_bignum);
	TestBreak(test_parse_eval_ratio);
	TestBreak(test_parse_eval_string);
	TestBreak(test_parse_eval_character);
	/* parse_cons */
	TestBreak(test_parse_allcons);
	TestBreak(test_parse_progn);
	TestBreak(test_check_variable);
	TestBreak(test_check_function_variable);
	TestBreak(test_parse_letone);
	TestBreak(test_parse_letarg);
	TestBreak(test_parse_let);
	TestBreak(test_parse_setq);
	TestBreak(test_check_variable_notnil);
	TestBreak(test_parse_var);
	TestBreak(test_parse_optional);
	TestBreak(test_parse_key);
	TestBreak(test_parse_aux);
	TestBreak(test_parse_ordinary);
	TestBreak(test_parse_defun);
	TestBreak(test_parse_macro_var);
	TestBreak(test_parse_macro_opt);
	TestBreak(test_parse_macro_rest);
	TestBreak(test_parse_macro_key);
	TestBreak(test_parse_macro_aux);
	TestBreak(test_parse_macro_whole_env);
	TestBreak(test_make_macro_function);
	TestBreak(test_parse_defmacro);
	TestBreak(test_parse_macrolet_args);
	TestBreak(test_parse_macrolet);
	TestBreak(test_parse_define_symbol_macro);
	TestBreak(test_parse_symbol_macrolet_args);
	TestBreak(test_parse_symbol_macrolet);
	TestBreak(test_parse_quote);
	TestBreak(test_parse_lambda);
	TestBreak(test_parse_function_argument);
	TestBreak(test_parse_function);
	TestBreak(test_parse_if);
	TestBreak(test_parse_unwind_protect);
	TestBreak(test_tagbody_tag_p);
	TestBreak(test_check_tagbody);
	TestBreak(test_parse_tagbody);
	TestBreak(test_parse_go);
	TestBreak(test_parse_block);
	TestBreak(test_parse_return_from);
	TestBreak(test_parse_catch);
	TestBreak(test_parse_throw);
	TestBreak(test_parse_flet_one);
	TestBreak(test_parse_flet_args);
	TestBreak(test_parse_flet_labels);
	TestBreak(test_parse_the);
	TestBreak(test_parse_eval_when);
	TestBreak(test_parse_values);
	TestBreak(test_parse_call);
	TestBreak(test_parse_multiple_value_bind);
	TestBreak(test_parse_multiple_value_call);
	/* macro */
	TestBreak(test_find_envstack);
	TestBreak(test_check_macro_function);
	TestBreak(test_call_macroexpand_hook);
	TestBreak(test_parse_macro);
	/* copy eval-parse */
	TestBreak(test_copy_single);
	TestBreak(test_copy_nil);
	TestBreak(test_copy_t);
	TestBreak(test_copy_integer);
	TestBreak(test_copy_string);
	TestBreak(test_copy_symbol);
	TestBreak(test_copy_float);
	TestBreak(test_copy_function);
	TestBreak(test_copy_quote);
	TestBreak(test_copy_go);
	TestBreak(test_copy_declaim);
	TestBreak(test_copy_allcons);
	TestBreak(test_copy_progn);
	TestBreak(test_copy_let);
	TestBreak(test_copy_setq);
	TestBreak(test_copy_ordinary_optional);
	TestBreak(test_copy_ordinary_key);
	TestBreak(test_copy_ordinary_aux);
	TestBreak(test_copy_ordinary);
	TestBreak(test_copy_defun);
	TestBreak(test_copy_lambda);
	TestBreak(test_copy_if);
	TestBreak(test_copy_unwind_protect);
	TestBreak(test_copy_tagbody);
	TestBreak(test_copy_tag);
	TestBreak(test_copy_block);
	TestBreak(test_copy_return_from);
	TestBreak(test_copy_catch);
	TestBreak(test_copy_throw);
	TestBreak(test_copy_flet_one);
	TestBreak(test_copy_flet_args);
	TestBreak(test_copy_flet);
	TestBreak(test_copy_the);
	TestBreak(test_copy_eval_when);
	TestBreak(test_copy_values);
	TestBreak(test_copy_locally);
	TestBreak(test_copy_call);
	TestBreak(test_copy_multiple_value_bind);
	TestBreak(test_copy_multiple_value_call);

	return 0;
}

int test_eval_parse(void)
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
		build_syscall();
		build_common();
		build_readtable();
		build_pathname();
		build_eval_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_eval_parse();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

