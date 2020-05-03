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
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
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

static int test_init_parse_environment(void)
{
	Execute ptr;
	addr control, pos;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	test(GetType(pos) == LISPSYSTEM_ENVROOT, "init_parse_environment1");
	test(lenarrayr(pos) == 2, "init_parse_environment2");
	free_control_(ptr, control);

	RETURN;
}

static int test_snapshot_envstack(void)
{
	Execute ptr;
	addr control, pos, value;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	snapshot_envstack(ptr, &pos);
	test(pos == Nil, "snapshot_envstack1");

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	fixnum_heap(&value, 10);
	SetArrayA2(pos, 1, value);

	snapshot_envstack(ptr, &pos);
	test(pos == value, "snapshot_envstack2");

	free_control_(ptr, control);

	RETURN;
}

static int test_push_envstack(void)
{
	Execute ptr;
	addr control, pos, check, temp, v1, v2, v3;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);
	push_envstack(ptr, 0, v1, v2, v3);

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

	push_envstack(ptr, 0, Nil, Nil, Nil);
	GetArrayA2(pos, 0, &temp);
	GetArrayA2(temp, 0, &temp);
	test(check == temp, "push_envstack7");

	push_envstack(ptr, 1, v1, v2, v3);
	GetArrayA2(pos, 1, &temp);
	test(temp != Nil, "push_envstack9");

	free_control_(ptr, control);

	RETURN;
}

static int test_rollback_envstack(void)
{
	Execute ptr;
	addr control, pos, left, right;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	push_envstack(ptr, 1, T, Nil, T);
	push_envstack(ptr, 1, T, Nil, T);
	push_envstack(ptr, 1, T, Nil, T);
	snapshot_envstack(ptr, &left);
	push_envstack(ptr, 1, T, Nil, T);
	push_envstack(ptr, 1, T, Nil, T);
	push_envstack(ptr, 1, T, Nil, T);
	rollback_envstack(ptr, left);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetArrayA2(pos, 1, &right);
	test(left == right, "rollback_envstack1");

	free_control_(ptr, control);

	RETURN;
}

static int test_defmacro_envstack(void)
{
	Execute ptr;
	addr control, pos, left, right, v1, v2, v3;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);

	defmacro_envstack(ptr, v1, v2);
	GetArrayA2(pos, 0, &left);
	GetArrayA2(left, 1, &right);
	test(right == v1, "defmacro_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v2, "defmacro_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == T, "defmacro_envstack3");

	macrolet_envstack(ptr, v2, v3);
	GetArrayA2(pos, 1, &left);
	GetArrayA2(left, 1, &right);
	test(right == v2, "macrolet_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v3, "macrolet_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == T, "macrolet_envstack3");

	define_symbol_macro_envstack(ptr, v1, v2);
	GetArrayA2(pos, 0, &left);
	GetArrayA2(left, 1, &right);
	test(right == v1, "define_symbol_macro_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v2, "define_symbol_macro_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == Nil, "define_symbol_macro_envstack3");

	symbol_macrolet_envstack(ptr, v2, v3);
	GetArrayA2(pos, 1, &left);
	GetArrayA2(left, 1, &right);
	test(right == v2, "symbol_macrolet_envstack1");
	GetArrayA2(left, 2, &right);
	test(right == v3, "symbol_macrolet_envstack2");
	GetArrayA2(left, 3, &right);
	test(right == Nil, "symbol_macrolet_envstack3");

	free_control_(ptr, control);

	RETURN;
}

static int test_environment_heap(void)
{
	Execute ptr;
	addr control, pos, left, pos1, pos2;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	defmacro_envstack(ptr, Nil, T);
	macrolet_envstack(ptr, T, Nil);
	GetArrayA2(pos, 0, &pos1);
	GetArrayA2(pos, 1, &pos2);

	environment_heap(ptr, &pos);
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

	free_control_(ptr, control);

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

	eval_parse(Execute_Thread, &pos, Nil);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_nil1");
	test(RefEvalParseType(pos) == EVAL_PARSE_NIL, "parse_eval_nil2");
	test(RefEval(pos, 0) == Nil, "parse_eval_nil3");

	RETURN;
}

static int test_parse_eval_t(void)
{
	addr pos;

	eval_parse(Execute_Thread, &pos, T);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_t1");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "parse_eval_t2");
	test(RefEval(pos, 0) == T, "parse_eval_t3");

	RETURN;
}

static int test_parse_eval_symbol(void)
{
	addr pos, value;

	internchar(LISP_PACKAGE, "HELLO", &value);
	eval_parse(Execute_Thread, &pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_symbol1");
	test(RefEvalParseType(pos) == EVAL_PARSE_SYMBOL, "parse_eval_symbol2");
	test(RefEval(pos, 0) == value, "parse_eval_symbol3");

	RETURN;
}

static int test_parse_eval_fixnum(void)
{
	addr pos, value;

	fixnum_heap(&value, 100);
	eval_parse(Execute_Thread, &pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_fixnum1");
	test(RefEvalParseType(pos) == EVAL_PARSE_INTEGER, "parse_eval_fixnum2");
	test(RefEval(pos, 0) == value, "parse_eval_fixnum3");

	RETURN;
}

static int test_parse_eval_bignum(void)
{
	addr pos, value;

	bignum_value_alloc(NULL, &value, signplus_bignum, 100);
	eval_parse(Execute_Thread, &pos, value);
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
	eval_parse(Execute_Thread, &pos, value);
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
	eval_parse(Execute_Thread, &pos, value);
	test(GetType(pos) == LISPTYPE_EVAL, "parse_eval_string1");
	test(RefEvalParseType(pos) == EVAL_PARSE_STRING, "parse_eval_string2");
	test(RefEval(pos, 0) == value, "parse_eval_string3");

	RETURN;
}

static int test_parse_eval_character(void)
{
	addr pos, value;

	character_heap(&value, 'A');
	eval_parse(Execute_Thread, &pos, value);
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
	parse_allcons(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &eval, cons);

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
	Execute ptr;
	addr cons, symbol, value;

	ptr = Execute_Thread;
	parse_letarg(ptr, &cons, Nil);
	test(cons == Nil, "parse_letarg1");

	readstring(&cons, "(aaa)");
	parse_letarg(ptr, &cons, cons);
	GetCons(cons, &value, &cons);
	/* (aaa nil) */
	GetCons(value, &symbol, &value);
	test(symbolp(symbol), "parse_letarg2");
	GetNameSymbol(symbol, &symbol);
	test(string_equal_char(symbol, "AAA"), "parse_letarg3");
	test(test_checktype(value, EVAL_PARSE_NIL), "parse_letarg4");
	test(cons == Nil, "parse_letarg5");

	readstring(&cons, "((aaa) (bbb 100))");
	parse_letarg(ptr, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LET), "parse_let1");
	test(RefEval(cons, 0) == Nil, "parse_let2");
	test(RefEval(cons, 1) == Nil, "parse_let3");
	test(RefEval(cons, 2) == Nil, "parse_let4");

	readstring(&cons, "(let* (aaa) (declare (ignore aaa)) 10 20 30)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_SETQ), "parse_setq1");
	test(RefEval(cons, 0) == Nil, "parse_setq2");

	readstring(&cons, "(setq aaa 10 bbb 20)");
	eval_parse(Execute_Thread, &cons, cons);
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
	Execute ptr;
	addr cons, left, right;

	ptr = Execute_Thread;
	parse_optional(ptr, &cons, Nil);
	test(cons == Nil, "parse_optional1");

	readstring(&cons, "((bbb 10 nil) (ccc nil ddd))");
	parse_optional(ptr, &cons, cons);
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
	Execute ptr;
	addr cons, left, right, check;

	ptr = Execute_Thread;
	parse_key(ptr, &cons, Nil);
	test(cons == Nil, "parse_key1");

	readstring(&cons, "((bbb :bbb 10 nil) (ccc hello nil ddd))");
	parse_key(ptr, &cons, cons);
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
	Execute ptr;
	addr cons, left, right;

	ptr = Execute_Thread;
	parse_aux(ptr, &cons, Nil);
	test(cons == Nil, "parse_aux1");

	readstring(&cons, "((bbb 10) (ccc nil))");
	parse_aux(ptr, &cons, cons);
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
	parse_ordinary(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
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
	parse_allcons(ptr, &cons, cons);

	make_macro_function(ptr, &call, args, Nil, Nil, cons);
	test(functionp(call), "make_macro_function1");
	test(StructFunction(call)->macro, "make_macro_function2");

	list_heap(&args, T, Nil, Nil, Nil, NULL);
	callclang_funcall(ptr, &cons, call, args, Nil, NULL);
	test(cons == readr(":hello"),"make_macro_function3");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_defmacro(void)
{
	Execute ptr;
	addr control, eval, name, lambda, stack;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	eval = readr("(defmacro aaa () :hello)");
	lambda = readr("defmacro");
	getmacro_symbol(lambda, &lambda);
	callclang_funcall(ptr, &eval, lambda, eval, Nil, NULL);
	GetCdr(eval, &eval);
	parse_defmacro(ptr, &eval, eval);
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &lambda);
	test(name == readr("aaa"), "parse_defmacro1");
	test(functionp(lambda), "parse_defmacro2");

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 0, &stack); /* global */
	GetArrayA2(stack, 1, &stack); /* call */
	test(stack == name, "parse_defmacro3");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_macrolet_args(void)
{
	Execute ptr;
	addr control, one, stack;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(aaa (a) a ''a)");
	parse_macrolet_one(ptr, one);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 1, &stack); /* local */
	GetArrayA2(stack, 1, &stack); /* call */
	test(stack == readr("aaa"), "parse_macrolet_args1");

	one = readr("((bbb (a) a ''b) (ccc (a) a ''c))");
	parse_macrolet_args(ptr, one);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 1, &stack); /* local */
	GetArrayA2(stack, 1, &stack); /* call */
	test(stack == readr("ccc"), "parse_macrolet_args2");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_macrolet(void)
{
	Execute ptr;
	addr control, one, stack;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(((aaa () :hello)) :bbb)");
	parse_macrolet(ptr, &one, one);
	test(RefEvalParseType(one) == EVAL_PARSE_LOCALLY, "parse_macrolet1");

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 1, &stack); /* local */
	test(stack == Nil, "parse_macrolet2");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_define_symbol_macro(void)
{
	Execute ptr;
	addr control, one;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(aaa ''Hello)");
	parse_define_symbol_macro(ptr, &one, one);
	test(RefEvalParseType(one) == EVAL_PARSE_DEFINE_SYMBOL_MACRO,
			"parse_define_symbol_macro1");
	GetEvalParse(one, 0, &one);
	test(one == readr("aaa"), "parse_define_symbol_macro2");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_symbol_macrolet_args(void)
{
	Execute ptr;
	addr control, one, name, form, env;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("((aaa ''Hello) (bbb ''zzz))");
	parse_symbol_macrolet_args(ptr, &one, one);
	test(consp(one), "parse_symbol_macrolet_args1");
	test(length_list_unsafe(one) == 2, "parse_symbol_macrolet_args2");
	GetCar(one, &one);
	List_bind(one, &name, &form, &env, NULL);
	test(name == readr("aaa"), "parse_symbol_macrolet_args3");
	test(eval_parse_p(form), "parse_symbol_macrolet_args4");
	test(GetType(env) == LISPTYPE_ENVIRONMENT, "parse_symbol_macrolet_args5");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_symbol_macrolet(void)
{
	Execute ptr;
	addr control, one;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);

	one = readr("(((aaa ''Hello) (bbb ''zzz)) :hello)");
	parse_symbol_macrolet(ptr, &one, one);
	test(RefEvalParseType(one) == EVAL_PARSE_SYMBOL_MACROLET,
			"parse_symbol_macrolet1");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_quote(void)
{
	addr cons;

	readstring(&cons, "(quote hello)");
	eval_parse(Execute_Thread, &cons, cons);
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
	init_parse_environment(ptr);

	readstring(&cons, "(lambda ())");
	parse_lambda(ptr, &cons, cons);
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

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_function_argument(void)
{
	addr cons, left, right, check, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	internchar(LISP_PACKAGE, "HELLO", &cons);
	parse_function_argument(ptr, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function_argument1");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CALLNAME, "parse_function_argument2");
	test(RefCallNameType(cons) == CALLNAME_SYMBOL, "parse_function_argument3");
	GetCallName(cons, &cons);
	test(cons == interncharr(LISP_PACKAGE, "HELLO"), "parse_function_argument4");

	readstring(&cons, "(setf hello)");
	parse_function_argument(ptr, &cons, cons);
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
	parse_function_argument(ptr, &cons, cons);
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

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_function(void)
{
	addr cons;

	readstring(&cons, "(function hello)");
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function1");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CALLNAME, "parse_function2");
	test(RefCallNameType(cons) == CALLNAME_SYMBOL, "parse_function3");
	GetCallName(cons, &cons);
	test(cons == interncharr(LISP_PACKAGE, "HELLO"), "parse_function4");

	readstring(&cons, "(function (lambda () :hello))");
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LAMBDA), "parse_function5");

	readstring(&cons, "(function (setf hello))");
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_FUNCTION), "parse_function6");

	RETURN;
}

static int test_parse_if(void)
{
	addr cons, check;

	readstring(&cons, "(if 100 (progn 200))");
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_IF), "parse_if1");
	GetEvalParse(cons, 0, &check);
	test(test_eqlfixnum(check, 100), "parse_if2");
	GetEvalParse(cons, 1, &check);
	test(test_checktype(check, EVAL_PARSE_PROGN), "parse_if3");
	GetEvalParse(cons, 2, &check);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_if4");

	readstring(&cons, "(if 100 (progn 200) 300)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_UNWIND_PROTECT), "parse_unwind_protect1");
	GetEvalParse(cons, 0, &check);
	test(test_eqlfixnum(check, 100), "parse_unwind_protect2");
	GetEvalParse(cons, 1, &check);
	test(check == Nil, "parse_unwind_protect2");

	readstring(&cons, "(unwind-protect 100 :hello :body)");
	eval_parse(Execute_Thread, &cons, cons);
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

static int test_parse_tagbody_check(void)
{
	Execute ptr;
	addr cons, tag, body, check;

	ptr = Execute_Thread;
	readstring(&cons, "(100 (progn) aaa (quote bbb) (function ccc))");
	parse_tagbody_check(ptr, cons, &tag, &body);
	/* tag */
	GetCons(tag, &check, &tag);
	test(test_tagfixnum(check, 100), "parse_tagbody_check1");
	GetCons(tag, &check, &tag);
	test(test_tagsymbol(check, "AAA"), "parse_tagbody_check2");
	test(tag == Nil, "parse_tagbody_check3");
	/* body */
	GetCons(body, &check, &body);
	test(test_tagfixnum(check, 100), "parse_tagbody_check4");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_PROGN), "parse_tagbody_check5");
	GetCons(body, &check, &body);
	test(test_tagsymbol(check, "AAA"), "parse_tagbody_check6");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_QUOTE), "parse_tagbody_check7");
	GetCons(body, &check, &body);
	test(test_checktype(check, EVAL_PARSE_FUNCTION), "parse_tagbody_check8");
	test(body == Nil, "parse_tagbody_check9");

	RETURN;
}

static int test_parse_tagbody(void)
{
	addr cons, tag, body, check;

	readstring(&cons, "(tagbody (progn) 100)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_BLOCK), "parse_block1");
	test(RefEval(cons, 0) == Nil, "parse_block2");
	test(RefEval(cons, 1) == Nil, "parse_block3");

	readstring(&cons, "(block hello 10 20 30)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_RETURN_FROM), "parse_return_from1");
	test(RefEval(cons, 0) == Nil, "parse_return_from2");
	GetEvalParse(cons, 1, &cons);
	test(test_checktype(cons, EVAL_PARSE_NIL), "parse_return_from3");

	readstring(&cons, "(return-from hello 100)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_CATCH), "parse_catch1");
	GetEvalParse(cons, 0, &check);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_catch2");
	test(RefEval(cons, 1) == Nil, "parse_catch3");

	readstring(&cons, "(catch hello 10 20 30)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_THROW), "parse_throw1");
	GetEvalParse(cons, 0, &check);
	test(test_checktype(check, EVAL_PARSE_NIL), "parse_throw2");
	GetEvalParse(cons, 1, &check);
	test(test_eqlfixnum(check, 100), "parse_throw6");

	readstring(&cons, "(throw hello 200)");
	eval_parse(Execute_Thread, &cons, cons);
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
	init_parse_environment(ptr);

	readstring(&cons, "(hello ())");
	parse_flet_one(ptr, &cons, cons);
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

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_flet_args(void)
{
	addr cons, check, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	readstring(&cons, "((aaa (a) :aaa) (bbb (b) :bbb))");
	parse_flet_args(ptr, &cons, cons);
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

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_flet_labels(void)
{
	addr cons, left, right;

	readstring(&cons, "(flet ((aaa (a) :aaa)) :hello)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_EVAL_WHEN), "parse_eval_when1");
	GetEvalParse(cons, 0, &check);
	getcar(check, &check);
	test(test_evalkeyword(check, "HELLO"), "parse_eval_when2");
	test(RefEval(cons, 1) == T, "parse_eval_when3");
	test(RefEval(cons, 2) == Nil, "parse_eval_when4");
	test(RefEval(cons, 3) == Nil, "parse_eval_when5");

	readstring(&cons, "(eval-when (load) :hello)");
	eval_parse(Execute_Thread, &cons, cons);
	test(RefEval(cons, 1) == Nil, "parse_eval_when6");
	test(RefEval(cons, 2) == T, "parse_eval_when7");
	test(RefEval(cons, 3) == Nil, "parse_eval_when8");

	readstring(&cons, "(eval-when (:execute) :hello)");
	eval_parse(Execute_Thread, &cons, cons);
	test(RefEval(cons, 1) == Nil, "parse_eval_when9");
	test(RefEval(cons, 2) == Nil, "parse_eval_when10");
	test(RefEval(cons, 3) == T, "parse_eval_when11");

	readstring(&cons, "(eval-when (compile :load-toplevel eval) :hello)");
	eval_parse(Execute_Thread, &cons, cons);
	test(RefEval(cons, 1) == T, "parse_eval_when12");
	test(RefEval(cons, 2) == T, "parse_eval_when13");
	test(RefEval(cons, 3) == T, "parse_eval_when14");

	RETURN;
}

static int test_parse_values(void)
{
	addr cons, pos;

	readstring(&cons, "(values)");
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_VALUES), "parse_values1");
	GetEvalParse(cons, 0, &cons);
	test(cons == Nil, "parse_values2");

	readstring(&cons, "(values t)");
	eval_parse(Execute_Thread, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_VALUES), "parse_values3");
	GetEvalParse(cons, 0, &cons);
	test(GetType(cons) == LISPTYPE_CONS, "parse_values4");
	GetCons(cons, &pos, &cons);
	test(test_checktype(pos, EVAL_PARSE_T), "parse_values5");
	test(cons == Nil, "parse_values6");

	readstring(&cons, "(values 100 t)");
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &cons, cons);
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
	eval_parse(Execute_Thread, &pos, pos);
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
	eval_parse(Execute_Thread, &cons, cons);
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
static int test_findstack_environment(void)
{
	Execute ptr;
	addr control, name, v1, v2, stack, check;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	fixnum_heap(&v1, 10);
	name = readr("aaa");
	defmacro_envstack(ptr, name, v1);
	fixnum_heap(&v2, 20);
	name = readr("bbb");
	defmacro_envstack(ptr, name, v2);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 0, &stack); /* root */
	check = NULL;
	test(findstack_environment(readr("aaa"), stack, T, &check), "findstack_environment1");
	test(check == v1, "findstack_environment2");
	check = NULL;
	test(findstack_environment(readr("bbb"), stack, T, &check), "findstack_environment3");
	test(check == v2, "findstack_environment4");
	check = NULL;
	test(! findstack_environment(readr("ccc"), stack, T, &check), "findstack_environment5");

	free_control_(ptr, control);

	RETURN;
}

static int test_check_macro_function(void)
{
	Execute ptr;
	addr control, v1, v2, v3, sym1, sym2, sym3, sym4, check;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	sym1 = readr("aaa");
	sym2 = readr("bbb");
	sym3 = readr("ccc");
	sym4 = readr("ddd");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);

	defmacro_envstack(ptr, sym1, v1);
	macrolet_envstack(ptr, sym2, v2);
	setmacro_symbol(sym3, v3);
	remmacro_symbol(sym4);

	test(parse_cons_check_macro(ptr, sym1, &check), "parse_cons_check_macro1");
	test(check == v1, "parse_cons_check_macro2");
	test(parse_cons_check_macro(ptr, sym2, &check), "parse_cons_check_macro3");
	test(check == v2, "parse_cons_check_macro4");
	test(parse_cons_check_macro(ptr, sym3, &check), "parse_cons_check_macro5");
	test(check == v3, "parse_cons_check_macro6");
	test(! parse_cons_check_macro(ptr, sym4, &check), "parse_cons_check_macro7");

	remmacro_symbol(sym3);

	free_control_(ptr, control);

	RETURN;
}

static int test_call_macroexpand_hook_function(Execute ptr,
		addr call, addr form, addr env)
{
	fixnum_heap(&form, RefFixnum(form) + 1);
	setresult_control(ptr, form);
	return 0;
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
	call_macroexpand_hook(ptr, &call, T, fixnumh(10), Nil);
	test(RefFixnum(call) == 11, "call_macroexpand_hook1");

	free_control_(ptr, control);

	RETURN;
}

static int test_parse_macro(void)
{
	addr control, cons;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_parse_environment(ptr);

	/*
	 *  common_eval.c:
	 *    (defmacro lambda (&whole whole &rest args)
	 *      (declare (ignore args))
	 *      `(function ,whole))
	 */
	readstring(&cons, "(lambda () :hello)");
	eval_parse(ptr, &cons, cons);
	test(test_checktype(cons, EVAL_PARSE_LAMBDA), "parse_macro1");

	free_control_(ptr, control);

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
	TestBreak(test_init_parse_environment);
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
	TestBreak(test_parse_tagbody_check);
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
	TestBreak(test_findstack_environment);
	TestBreak(test_check_macro_function);
	TestBreak(test_call_macroexpand_hook);
	TestBreak(test_parse_macro);

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
		build_reader();
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

