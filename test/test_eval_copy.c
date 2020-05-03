#include "eval_copy.c"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "eval_parse.h"
#include "function.h"
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
 *  copy eval-parse
 */
static int test_copy_eval_single(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	readstring(&pos, "t");
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_single(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_eval_single1");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "copy_eval_single2");
	test(RefEvalParse(pos, 0) == T, "copy_eval_single3");

	copy_eval_single(local, &pos, pos);
	test(GetStatusDynamic(pos), "copy_eval_single4");
	fixnum_heap(&value, 100);
	test(! GetStatusDynamic(value), "copy_eval_single5");
	SetEvalParse(pos, 0, value);

	copy_eval_single(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_eval_single6");
	GetEvalParse(pos, 0, &value);
	test(! GetStatusDynamic(value), "copy_eval_single7");
	test(RefFixnum(value) == 100, "copy_eval_single8");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_nil(void)
{
	addr pos;

	readstring(&pos, "nil");
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
	test(! GetStatusDynamic(pos), "copy_nil1");
	test(RefEvalParseType(pos) == EVAL_PARSE_NIL, "copy_nil2");
	test(RefEvalParse(pos, 0) == Nil, "copy_nil3");

	RETURN;
}

static int test_copy_t(void)
{
	addr pos;

	readstring(&pos, "t");
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
	test(! GetStatusDynamic(pos), "copy_t1");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "copy_t2");
	test(RefEvalParse(pos, 0) == T, "copy_t3");

	RETURN;
}

static int test_copy_integer(void)
{
	addr pos;

	fixnum_heap(&pos, 100);
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	GetEvalParse(pos, 0, &pos);

	copy_eval_allcons(NULL, &pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	GetEvalParse(pos, 0, &pos);

	copy_eval_allcons(local, &pos, pos);
	test(GetStatusDynamic(pos), "copy_allcons10");
	test(length_list_unsafe(pos) == 3, "copy_allcons11");

	copy_eval_allcons(NULL, &pos, pos);
	test(! GetStatusDynamic(pos), "copy_allcons12");
	test(length_list_unsafe(pos) == 3, "copy_allcons13");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_progn(void)
{
	addr pos;

	readstring(&pos, "(progn 10 20 30)");
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
	test(RefEvalParseType(pos) == EVAL_PARSE_LETA, "copy_let11");

	RETURN;
}

static int test_copy_setq(void)
{
	addr pos, symbol, value, check;

	readstring(&pos, "(setq aaa 10 bbb 20)");
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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

static int test_copy_eval_ordinary_optional(void)
{
	addr pos, var, init, svar, check;

	readstring(&pos, "(&optional aaa (bbb 10))");
	parse_ordinary(Execute_Thread, &pos, pos);
	/* (var opt rest key allow aux) */
	getnth(pos, 1, &pos);
	copy_eval_ordinary_optional(NULL, &pos, pos);
	test(length_list_unsafe(pos) == 2, "copy_eval_ordinary_optional1");
	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "aaa");
	test(var == check, "copy_eval_ordinary_optional2");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_eval_ordinary_optional3");
	test(svar == Nil, "copy_eval_ordinary_optional4");

	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "bbb");
	test(var == check, "copy_eval_ordinary_optional5");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_eval_ordinary_optional6");

	RETURN;
}

static int test_copy_eval_ordinary_key(void)
{
	addr pos, var, name, init, svar, check;

	readstring(&pos, "(&key aaa ((name bbb) 10 ddd))");
	parse_ordinary(Execute_Thread, &pos, pos);
	/* (var opt rest key allow aux) */
	getnth(pos, 3, &pos);
	copy_eval_ordinary_key(NULL, &pos, pos);
	test(length_list_unsafe(pos) == 2, "copy_eval_ordinary_key1");
	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &name, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "aaa");
	test(var == check, "copy_eval_ordinary_key2");
	readstring(&check, ":aaa");
	test(name == check, "copy_eval_ordinary_key3");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_eval_ordinary_key4");
	test(svar == Nil, "copy_eval_ordinary_key5");

	GetCons(pos, &svar, &pos);
	GetCons(svar, &var, &svar);
	GetCons(svar, &name, &svar);
	GetCons(svar, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "bbb");
	test(var == check, "copy_eval_ordinary_key6");
	readstring(&check, "name");
	test(name == check, "copy_eval_ordinary_key7");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_eval_ordinary_key8");
	readstring(&check, "ddd");
	test(svar == check, "copy_eval_ordinary_key9");

	RETURN;
}

static int test_copy_eval_ordinary_aux(void)
{
	addr pos, var, init, check;

	readstring(&pos, "(&aux aaa (bbb 10))");
	parse_ordinary(Execute_Thread, &pos, pos);
	/* (var opt rest key allow aux) */
	getnth(pos, 5, &pos);
	copy_eval_ordinary_aux(NULL, &pos, pos);
	test(length_list_unsafe(pos) == 2, "copy_eval_ordinary_aux1");
	GetCons(pos, &init, &pos);
	GetCons(init, &var, &init);
	GetCar(init, &init);
	readstring(&check, "aaa");
	test(var == check, "copy_eval_ordinary_aux2");
	test(RefEvalParseType(init) == EVAL_PARSE_NIL, "copy_eval_ordinary_aux3");

	GetCons(pos, &init, &pos);
	GetCons(init, &var, &init);
	GetCar(init, &init);
	readstring(&check, "bbb");
	test(var == check, "copy_eval_ordinary_aux4");
	test(RefEvalParseType(init) == EVAL_PARSE_INTEGER, "copy_eval_ordinary_aux5");

	RETURN;
}

static int test_copy_eval_ordinary(void)
{
	addr pos, var, check;

	readstring(&pos, "(aa &optional bb &rest cc &key dd &allow-other-keys &aux ee)");
	parse_ordinary(Execute_Thread, &pos, pos);
	copy_eval_ordinary(NULL, &pos, pos);
	/* var */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_eval_ordinary1");
	GetCar(var, &var);
	readstring(&check, "aa");
	test(var == check, "copy_eval_ordinary2");
	/* optional */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_eval_ordinary3");
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "bb");
	/* rest */
	GetCons(pos, &var, &pos);
	readstring(&check, "cc");
	test(var == check, "copy_eval_ordinary5");
	/* key */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_eval_ordinary6");
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "dd");
	test(var == check, "copy_eval_ordinary7");
	/* allow-other-keys */
	GetCons(pos, &var, &pos);
	test(var == T, "copy_eval_ordinary8");
	/* aux */
	GetCons(pos, &var, &pos);
	test(length_list_unsafe(var) == 1, "copy_eval_ordinary9");
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "ee");
	test(var == check, "copy_eval_ordinary10");

	RETURN;
}

static int test_copy_defun(void)
{
	addr pos, symbol, check;

	readstring(&pos, "(defun aa (bb) (declare (special cc)) \"DOC\" 10 20)");
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &pos, pos);
	copy_eval_parse_heap(&pos, pos);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	GetEvalParse(check, 0, &check);
	GetCar(check, &check); /* tag aa */
	copy_eval_parse_heap(&pos, check);
	test(check != pos, "copy_tag1");
	test(! GetStatusDynamic(pos), "copy_tag2");
	test(RefEvalParseType(pos) == EVAL_PARSE_TAG, "copy_tag3");
	/* tag */
	GetEvalParse(pos, 0, &var);
	readstring(&check, "aa");
	test(var == check, "copy_tag4");

	RETURN
}

static int test_copy_block(void)
{
	addr pos, var, check;

	readstring(&check, "(block aa bb cc)");
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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

static int test_copy_eval_flet_one(void)
{
	addr pos, var, check;

	readstring(&check, "(flet ((aa (bb) (declare (special bb)) \"HELLO\" :hello)))");
	eval_parse(Execute_Thread, &check, check);
	GetEvalParse(check, 0, &check); /* args */
	GetCar(check, &check);
	copy_eval_flet_one(NULL, &pos, check);
	test(check != pos, "copy_eval_flet_one1");
	/* name */
	GetCons(pos, &var, &pos);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_eval_flet_one2");
	GetCallName(var, &var);
	readstring(&check, "aa");
	test(var == check, "copy_eval_flet_one3");
	/* args */
	GetCons(pos, &var, &pos);
	GetCar(var, &var);
	GetCar(var, &var);
	readstring(&check, "bb");
	test(var == check, "copy_eval_flet_one4");
	/* decl */
	GetCons(pos, &var, &pos);
	test(eval_declare_p(var), "copy_eval_flet_one5");
	/* doc */
	GetCons(pos, &var, &pos);
	test(stringp(var), "copy_eval_flet_one6");
	/* cons */
	GetCar(pos, &var);
	test(length_list_unsafe(var) == 1, "copy_eval_flet_one7");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_BLOCK, "copy_eval_flet_one8");

	RETURN;
}

static int test_copy_eval_flet_args(void)
{
	addr pos, var, check;

	readstring(&check, "(flet ((aa () :aa) (bb (b) :bb)) :hello)");
	eval_parse(Execute_Thread, &check, check);
	GetEvalParse(check, 0, &check); /* args */
	copy_eval_flet_args(NULL, &pos, check);
	test(check != pos, "copy_eval_flet_args1");
	test(length_list_unsafe(pos) == 2, "copy_eval_flet_args2");
	/* aa */
	GetCons(pos, &var, &pos);
	GetCar(var, &var);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_eval_flet_args3");
	GetCallName(var, &var);
	readstring(&check, "aa");
	test(var == check, "copy_eval_flet_args4");
	/* bb */
	GetCons(pos, &var, &pos);
	GetCar(var, &var);
	test(GetType(var) == LISPTYPE_CALLNAME, "copy_eval_flet_args5");
	GetCallName(var, &var);
	readstring(&check, "bb");
	test(var == check, "copy_eval_flet_args6");

	RETURN;
}

static int test_copy_eval_flet(void)
{
	addr pos, var, check;

	/* flet */
	readstring(&check, "(flet ((aa () :aa)) (declare (special bb)) :hello)");
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
	test(check != pos, "copy_eval_flet1");
	test(! GetStatusDynamic(pos), "copy_eval_flet2");
	test(RefEvalParseType(pos) == EVAL_PARSE_FLET, "copy_eval_flet3");
	/* args */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 1, "copy_eval_flet4");
	/* decl */
	GetEvalParse(pos, 1, &var);
	test(eval_declare_p(var), "copy_eval_flet5");
	/* cons */
	GetEvalParse(pos, 2, &var);
	test(length_list_unsafe(var) == 1, "copy_eval_flet6");
	GetCar(var, &var);
	test(RefEvalParseType(var) == EVAL_PARSE_SYMBOL, "copy_eval_flet7");
	GetEvalParse(var, 0, &var);
	readstring(&check, ":hello");
	test(var == check, "copy_eval_flet8");

	/* labels */
	readstring(&check, "(labels ((aa () :aa) (bb (cc) dd)) :hello)");
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
	test(check != pos, "copy_eval_flet9");
	test(! GetStatusDynamic(pos), "copy_eval_flet10");
	test(RefEvalParseType(pos) == EVAL_PARSE_LABELS, "copy_eval_flet11");
	/* args */
	GetEvalParse(pos, 0, &var);
	test(length_list_unsafe(var) == 2, "copy_eval_flet12");

	RETURN;
}

static int test_copy_the(void)
{
	addr pos, var, check;

	readstring(&check, "(the integer (progn aa bb cc))");
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
	eval_parse(Execute_Thread, &check, check);
	copy_eval_parse_heap(&pos, check);
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
static int testbreak_eval_copy(void)
{
	in_package_lisp_package();
	/* copy eval-parse */
	TestBreak(test_copy_eval_single);
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
	TestBreak(test_copy_eval_ordinary_optional);
	TestBreak(test_copy_eval_ordinary_key);
	TestBreak(test_copy_eval_ordinary_aux);
	TestBreak(test_copy_eval_ordinary);
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
	TestBreak(test_copy_eval_flet_one);
	TestBreak(test_copy_eval_flet_args);
	TestBreak(test_copy_eval_flet);
	TestBreak(test_copy_the);
	TestBreak(test_copy_eval_when);
	TestBreak(test_copy_values);
	TestBreak(test_copy_locally);
	TestBreak(test_copy_call);
	TestBreak(test_copy_multiple_value_bind);
	TestBreak(test_copy_multiple_value_call);

	return 0;
}

int test_eval_copy(void)
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
		result = testbreak_eval_copy();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

