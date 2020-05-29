#include "optimize.c"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "declare.h"
#include "degrade.h"
#include "function.h"
#include "ratio.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#if 0
/*
 *  optstruct
 */
static int test_initialize_optstruct(void)
{
	struct optstruct opt;
	addr pos;

	initialize_optstruct(&opt);
	test(opt.local[0] < 0, "initialize_optstruct1");
	getroot_declare(&pos);
	set_optimize_debug_declare(pos, 3);
	setroot_declare(pos);

	initialize_optstruct(&opt);
	test(opt.declaim[EVAL_OPTIMIZE_DEBUG] == 3, "initialize_optstruct2");
	test(opt.local[EVAL_OPTIMIZE_DEBUG] < 0, "initialize_optstruct3");
	build_declare();

	RETURN;
}

static int test_save_optstruct(void)
{
	struct optstruct opt, save;

	initialize_optstruct(&opt);
	memset(&save, 0xAA, sizeoft(save));
	opt.declaim[0] = -2;
	opt.declaim[1] = 3;
	opt.local[0] = -3;
	opt.local[1] = 2;
	save_optstruct(&opt, &save);
	test(save.declaim[0] == -2, "save_optstruct1");
	test(save.declaim[1] == 3, "save_optstruct2");
	test(save.local[0] == -3, "save_optstruct3");
	test(save.local[1] == 2, "save_optstruct4");

	memset(&opt, 0xAA, sizeoft(opt));
	rollback_optstruct(&opt, &save);
	test(opt.declaim[0] != -2, "save_optstruct5");
	test(opt.declaim[1] != 3, "save_optstruct6");
	test(opt.local[0] == -3, "save_optstruct7");
	test(opt.local[1] == 2, "save_optstruct8");

	RETURN;
}

static int test_optimize_value(void)
{
	struct optstruct opt;

	initialize_optstruct(&opt);
	opt.declaim[0] = opt.local[0] = -1;
	test(optimize_value(&opt, 0) == -1, "optimize_value1");

	opt.declaim[0] = 2;
	opt.local[0] = -1;
	test(optimize_value(&opt, 0) == 2, "optimize_value2");

	opt.declaim[0] = -1;
	opt.local[0] = 1;
	test(optimize_value(&opt, 0) == 1, "optimize_value3");

	opt.declaim[0] = 2;
	opt.local[0] = 3;
	test(optimize_value(&opt, 0) == 3, "optimize_value4");

	RETURN;
}

static int test_speed_on(void)
{
	struct optstruct opt;

	initialize_optstruct(&opt);
	opt.declaim[EVAL_OPTIMIZE_SPEED] = -1;
	opt.local[EVAL_OPTIMIZE_SPEED] = -1;
	test(speed_on(&opt), "speed_on1");

	opt.declaim[EVAL_OPTIMIZE_SPEED] = 0;
	opt.local[EVAL_OPTIMIZE_SPEED] = -1;
	test(! speed_on(&opt), "speed_on2");

	opt.declaim[EVAL_OPTIMIZE_SPEED] = -1;
	opt.local[EVAL_OPTIMIZE_SPEED] = 0;
	test(! speed_on(&opt), "speed_on3");

	opt.declaim[EVAL_OPTIMIZE_SPEED] = 2;
	opt.local[EVAL_OPTIMIZE_SPEED] = 0;
	test(! speed_on(&opt), "speed_on4");

	opt.declaim[EVAL_OPTIMIZE_SPEED] = 0;
	opt.local[EVAL_OPTIMIZE_SPEED] = 2;
	test(speed_on(&opt), "speed_on5");

	opt.declaim[EVAL_OPTIMIZE_SPEED] = 2;
	opt.local[EVAL_OPTIMIZE_SPEED] = -1;
	test(speed_on(&opt), "speed_on6");

	opt.declaim[EVAL_OPTIMIZE_SPEED] = -1;
	opt.local[EVAL_OPTIMIZE_SPEED] = 2;
	test(speed_on(&opt), "speed_on7");

	RETURN;
}


/*
 *  optparse
 */
static void parse_eval_string(addr *ret, const char *str)
{
	readstring(ret, str);
	eval_parse(Execute_Thread, ret, *ret);
}

static int test_check_evaltype(void)
{
	addr pos;

	parse_eval_string(&pos, "10");
	test(check_evaltype(pos, EVAL_PARSE_INTEGER), "check_evaltype1");
	test(! check_evaltype(pos, EVAL_PARSE_STRING), "check_evaltype2");
	test(! check_evaltype(Nil, EVAL_PARSE_INTEGER), "check_evaltype3");

	RETURN;
}

static void allminus_optstruct(struct optstruct *ptr)
{
	int i;
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		ptr->declaim[i] = ptr->local[i] = -1;
}

static int test_check_evaltype_on(void)
{
	addr pos;
	struct optstruct opt;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "10");
	opt.local[EVAL_OPTIMIZE_SPEED] = 3;
	test(check_evaltype_on(&opt, pos, EVAL_PARSE_INTEGER), "check_evaltype_on1");
	test(! check_evaltype_on(&opt, pos, EVAL_PARSE_STRING), "check_evaltype_on2");
	test(! check_evaltype_on(&opt, Nil, EVAL_PARSE_INTEGER), "check_evaltype_on3");

	opt.local[EVAL_OPTIMIZE_SPEED] = 0;
	test(! check_evaltype_on(&opt, pos, EVAL_PARSE_INTEGER), "check_evaltype_on4");
	test(! check_evaltype_on(&opt, pos, EVAL_PARSE_STRING), "check_evaltype_on5");
	test(! check_evaltype_on(&opt, Nil, EVAL_PARSE_INTEGER), "check_evaltype_on6");

	RETURN;
}

static int test_check_listtype_on(void)
{
	addr pos;
	struct optstruct opt;

	allminus_optstruct(&opt);
	readstring(&pos, "10");
	opt.local[EVAL_OPTIMIZE_SPEED] = 3;
	test(check_lisptype_on(&opt, pos, LISPTYPE_FIXNUM), "check_lisptype_on1");
	test(! check_lisptype_on(&opt, pos, LISPTYPE_STRING), "check_lisptype_on2");

	opt.local[EVAL_OPTIMIZE_SPEED] = 0;
	test(! check_lisptype_on(&opt, pos, LISPTYPE_FIXNUM), "check_lisptype_on3");
	test(! check_lisptype_on(&opt, pos, LISPTYPE_STRING), "check_lisptype_on4");

	RETURN;
}

static int check_evalinteger(addr pos, fixnum value)
{
	fixnum check;
	if (! check_evaltype(pos, EVAL_PARSE_INTEGER)) {
		degrade_printf("parse.integer error.\n");
		infobit(pos);
		return 0;
	}
	GetEvalParse(pos, 0, &pos);
	GetFixnum(pos, &check);
	if (check != value) {
		degrade_printf("parse.integer value error: %d /= %d.\n",
				(int)check, (int)value);
		return 0;
	}

	return 1;
}

static int test_checkvalue(void)
{
	addr pos;

	parse_eval_string(&pos, "nil");
	test(checkvalue(pos), "checkvalue1");
	parse_eval_string(&pos, "100");
	test(checkvalue(pos), "checkvalue2");
	parse_eval_string(&pos, "(call 10 20 30)");
	test(! checkvalue(pos), "checkvalue3");
	fixnum_heap(&pos, 100);
	test(! checkvalue(pos), "checkvalue4");

	parse_eval_string(&pos, "(values)");
	test(checkvalue(pos), "checkvalues1");
	parse_eval_string(&pos, "(values 10 20 30)");
	test(checkvalue(pos), "checkvalues2");
	parse_eval_string(&pos, "(values 10 20 (call))");
	test(! checkvalue(pos), "checkvalues3");

	parse_eval_string(&pos, "(the t 100)");
	test(checkvalue(pos), "checkvalue_the1");
	parse_eval_string(&pos, "(the t (call 10))");
	test(! checkvalue(pos), "checkvalue_the2");

	RETURN;
}


/*
 *  implicit
 */
static void implicit_string(addr *ret, const char *str)
{
	addr cons, pos, root;

	readstring(&cons, str);
	Check(! listp(cons), "type error");
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		eval_parse(Execute_Thread, &pos, pos);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

static void zerospeed(struct optstruct *ptr)
{
	ptr->local[EVAL_OPTIMIZE_SPEED] = 0;
}
static void remspeed(struct optstruct *ptr)
{
	ptr->local[EVAL_OPTIMIZE_SPEED] = -1;
}

static int test_optparse_implicit3(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	implicit_string(&pos, "(10 20 30)");
	test(checkparse_implicit3(&opt, pos), "checkparse_implicit3-1");
	zerospeed(&opt);
	implicit_string(&pos, "(10 20 30)");
	test(! checkparse_implicit3(&opt, pos), "checkparse_implicit3-2");
	remspeed(&opt);
	test(! checkparse_implicit3(&opt, Nil), "checkparse_implicit3-3");
	implicit_string(&pos, "(10)");
	test(! checkparse_implicit3(&opt, pos), "checkparse_implicit3-4");
	implicit_string(&pos, "(10 20)");
	test(checkparse_implicit3(&opt, pos), "checkparse_implicit3-5");
	implicit_string(&pos, "(10 (hello) 30)");
	test(! checkparse_implicit3(&opt, pos), "checkparse_implicit3-6");
	implicit_string(&pos, "(10 20 (hello))");
	test(! checkparse_implicit3(&opt, pos), "checkparse_implicit3-7");

	implicit_string(&pos, "(10 20 30 40)");
	test(optparse_implicit3(local, &pos, &opt, pos), "optparse_implicit3-1");
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_INTEGER), "optparse_implicit3-2");
	GetEvalParse(pos, 0, &pos);
	test(RefFixnum(pos) == 40, "optparse_implicit3-3");
	implicit_string(&pos, "(10 20 (call))");
	test(! optparse_implicit3(local, &pos, &opt, pos), "optparse_implicit3-4");

	RETURN;
}

static int test_optparse_implicit4(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	implicit_string(&pos, "(10 (call) 20 30 (call2))");
	test(checkparse_implicit4(&opt, pos), "checkparse_implicit4-1");
	zerospeed(&opt);
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-2");
	remspeed(&opt);
	implicit_string(&pos, "((call) (call2))");
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-3");
	implicit_string(&pos, "((call2))");
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-4");
	implicit_string(&pos, "(10 (call2))");
	test(checkparse_implicit4(&opt, pos), "checkparse_implicit4-5");
	implicit_string(&pos, "(10)");
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-6");
	implicit_string(&pos, "()");
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-7");
	implicit_string(&pos, "((call) 20)");
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-8");
	implicit_string(&pos, "((call1) (call2) (call3))");
	test(! checkparse_implicit4(&opt, pos), "checkparse_implicit4-9");
	implicit_string(&pos, "(10 (call1) (call2) (call3))");
	test(checkparse_implicit4(&opt, pos), "checkparse_implicit4-10");

	implicit_string(&pos, "(10 (call) 20 30 (call2))");
	test(optparse_implicit4(local, &pos, &opt, pos), "optparse_implicit4-1");
	test(consp(pos), "optparse_implicit4-2");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_implicit4-3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_implicit4-4");
	test(pos == Nil, "optparse_implicit4-5");
	test(! optparse_implicit4(local, &pos, &opt, Nil), "optparse_implicit4-6");
	implicit_string(&pos, "(10 20 30 (call2))");
	test(optparse_implicit4(local, &pos, &opt, pos), "optparse_implicit4-7");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_implicit4-8");
	test(pos == Nil, "optparse_implicit4-9");

	RETURN;
}

static int test_optparse_implicit5(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	implicit_string(&pos, "(10 (call1) 20 (call2) 30 40)");
	test(checkparse_implicit5(&opt, pos), "checkparse_implicit5-1");
	zerospeed(&opt);
	test(! checkparse_implicit5(&opt, pos), "checkparse_implicit5-2");
	remspeed(&opt);
	implicit_string(&pos, "(10 (call1) 20 (call2) 30 (call3))");
	test(! checkparse_implicit5(&opt, pos), "checkparse_implicit5-3");
	implicit_string(&pos, "((call1) (call2) 30)");
	test(! checkparse_implicit5(&opt, pos), "checkparse_implicit5-4");
	implicit_string(&pos, "(10 20 30)");
	test(! checkparse_implicit5(&opt, pos), "checkparse_implicit5-5");
	implicit_string(&pos, "(10 (call) 30)");
	test(checkparse_implicit5(&opt, pos), "checkparse_implicit5-6");
	implicit_string(&pos, "(10 20 (call))");
	test(! checkparse_implicit5(&opt, pos), "checkparse_implicit5-7");

	implicit_string(&pos, "(10 (call1) 20 (call2) 30 40)");
	test(optparse_implicit5(local, &pos, &opt, pos), "optparse_implicit5-1");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_implicit5-2");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_implicit5-3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_INTEGER), "optparse_implicit5-4");
	test(pos == Nil, "optparse_implicit5-5");

	fixnum_heap(&pos, 100);
	test(! optparse_implicit5(local, &pos, &opt, pos), "optparse_implicit5-6");
	implicit_string(&pos, "((call1) (call2) 40)");
	test(! optparse_implicit5(local, &pos, &opt, pos), "optparse_implicit5-7");

	RETURN;
}

static int test_optparse_implicit6(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	implicit_string(&pos, "(10 (progn 20) (progn (progn 30)) (progn))");
	test(checkparse_implicit6(&opt, pos), "checkparse_implicit6-1");
	zerospeed(&opt);
	test(! checkparse_implicit6(&opt, pos), "checkparse_implicit6-2");
	remspeed(&opt);
	implicit_string(&pos, "(10 20)");
	test(! checkparse_implicit6(&opt, pos), "checkparse_implicit6-3");
	implicit_string(&pos, "((progn))");
	test(checkparse_implicit6(&opt, pos), "checkparse_implicit6-4");

	implicit_string(&pos, "(10 (progn 20) (progn (progn 30)) (progn))");
	test(optparse_implicit6(local, &pos, &opt, pos), "optparse_implicit6-1");
	test(consp(pos), "optparse_implicit6-2");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 10), "optparse_implicit6-3");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 20), "optparse_implicit6-4");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 30), "optparse_implicit6-5");
	test(pos == Nil, "optparse_implicit6-6");

	RETURN;
}

static int test_optparse_implicit_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	implicit_string(&pos, "((progn 10) 20)");
	test(checkparse_implicit_all(&opt, pos), "checkparse_implicit_all1");
	zerospeed(&opt);
	test(! checkparse_implicit_all(&opt, pos), "checkparse_implicit_all2");
	remspeed(&opt);
	implicit_string(&pos, "(10 20 30)");
	test(! checkparse_implicit_all(&opt, pos), "checkparse_implicit_all3");

	implicit_string(&pos, "((progn 10) 20)");
	test(optparse_implicit_all(local, &pos, &opt, pos), "optparse_implicit_all1");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 10), "optparse_implicit_all2");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 20), "optparse_implicit_all3");
	test(pos == Nil, "optparse_implicit_all4");
	implicit_string(&pos, "(10 20 30)");
	test(! optparse_implicit_all(local, &pos, &opt, pos), "optparse_implicit_all5");

	RETURN;
}


/*
 *  progn
 */
static int test_optparse_progn1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn)");
	test(checkparse_progn1(&opt, pos), "checkparse_progn1-1");
	zerospeed(&opt);
	test(! checkparse_implicit6(&opt, pos), "checkparse_progn1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(progn 100)");
	test(! checkparse_progn1(&opt, pos), "checkparse_progn1-3");

	parse_eval_string(&pos, "(progn)");
	test(optparse_progn1(local, &pos, &opt, pos), "optparse_progn1-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_progn1-2");
	parse_eval_string(&pos, "100");
	test(! optparse_progn1(local, &pos, &opt, pos), "optparse_progn1-3");
	parse_eval_string(&pos, "(progn 100)");
	test(! optparse_progn1(local, &pos, &opt, pos), "optparse_progn1-4");

	RETURN;
}

static int test_optparse_progn2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn 100)");
	test(checkparse_progn2(&opt, pos), "checkparse_progn2-1");
	zerospeed(&opt);
	test(! checkparse_progn2(&opt, pos), "checkparse_progn2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "100");
	test(! checkparse_progn2(&opt, pos), "checkparse_progn2-3");

	parse_eval_string(&pos, "(progn 100)");
	test(optparse_progn2(local, &pos, &opt, pos), "optparse_progn2-1");
	test(check_evaltype(pos, EVAL_PARSE_INTEGER), "optparse_progn2-2");
	parse_eval_string(&pos, "100");
	test(! optparse_progn2(local, &pos, &opt, pos), "optparse_progn2-3");
	parse_eval_string(&pos, "(progn)");
	test(! optparse_progn2(local, &pos, &opt, pos), "optparse_progn2-4");
	parse_eval_string(&pos, "(progn 10 20)");
	test(! optparse_progn2(local, &pos, &opt, pos), "optparse_progn2-5");

	RETURN;
}

static int test_optparse_progn3(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn 10 20 30)");
	test(checkparse_progn3(&opt, pos), "checkparse_progn3-1");
	zerospeed(&opt);
	test(! checkparse_progn3(&opt, pos), "checkparse_progn3-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(progn 10 20 (call))");
	test(! checkparse_progn3(&opt, pos), "checkparse_progn3-3");

	parse_eval_string(&pos, "(progn 10 20 30)");
	test(optparse_progn3(local, &pos, &opt, pos), "optparse_progn3-1");
	test(check_evaltype(pos, EVAL_PARSE_INTEGER), "optparse_progn3-2");
	GetEvalParse(pos, 0, &pos);
	test(RefFixnum(pos) == 30, "optparse_progn3-3");
	parse_eval_string(&pos, "(progn 10 20 (call))");
	test(! optparse_progn3(local, &pos, &opt, pos), "optparse_progn3-4");

	RETURN;
}

static int test_optparse_progn4(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn 10 (call) 20 30 (call2))");
	test(checkparse_progn4(&opt, pos), "checkparse_progn4-1");
	zerospeed(&opt);
	test(! checkparse_progn4(&opt, pos), "checkparse_progn4-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(progn (call) (call2))");
	test(! checkparse_progn4(&opt, pos), "checkparse_progn4-3");

	parse_eval_string(&pos, "(progn 10 (call) 20 30 (call2))");
	test(optparse_progn4(local, &pos, &opt, pos), "optparse_progn4-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_progn4-2");
	GetEvalParse(pos, 0, &pos);
	test(consp(pos), "optparse_progn4-3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_progn4-4");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_progn4-5");
	test(pos == Nil, "optparse_progn4-6");
	parse_eval_string(&pos, "(progn (call) (call2))");
	test(! optparse_progn4(local, &pos, &opt, pos), "optparse_progn4-7");

	RETURN;
}

static int test_optparse_progn5(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn 10 (call1) 20 (call2) 30 40)");
	test(checkparse_progn5(&opt, pos), "checkparse_progn5-1");
	zerospeed(&opt);
	test(! checkparse_progn5(&opt, pos), "checkparse_progn5-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(progn (call1) (call2) 40)");
	test(! checkparse_progn5(&opt, pos), "checkparse_progn5-3");

	parse_eval_string(&pos, "(progn 10 (call1) 20 (call2) 30 40)");
	test(optparse_progn5(local, &pos, &opt, pos), "optparse_progn5-1");
	GetEvalParse(pos, 0, &pos);
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_progn5-2");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_progn5-3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_INTEGER), "optparse_progn5-4");
	test(pos == Nil, "optparse_progn5-5");
	parse_eval_string(&pos, "100");
	test(! optparse_progn5(local, &pos, &opt, pos), "optparse_progn5-6");
	parse_eval_string(&pos, "(progn (call1) (call2) 40)");
	GetEvalParse(pos, 0, &pos);
	test(! optparse_progn5(local, &pos, &opt, pos), "optparse_progn5-7");

	RETURN;
}

static int test_optparse_progn6(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn 10 (progn 20) (progn (progn 30)) (progn))");
	test(checkparse_progn6(&opt, pos), "checkparse_progn6-1");
	zerospeed(&opt);
	test(! checkparse_progn6(&opt, pos), "checkparse_progn6-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(progn 10 20 30)");
	test(! checkparse_progn6(&opt, pos), "checkparse_progn6-3");

	parse_eval_string(&pos, "(progn 10 (progn 20) (progn (progn 30)) (progn))");
	test(optparse_progn6(local, &pos, &opt, pos), "optparse_progn6-1");
	GetEvalParse(pos, 0, &pos);
	test(consp(pos), "optparse_progn6-2");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 10), "optparse_progn6-3");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 20), "optparse_progn6-4");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 30), "optparse_progn6-5");
	test(pos == Nil, "optparse_progn6-6");
	parse_eval_string(&pos, "(progn 10 20 30)");
	test(! optparse_progn6(local, &pos, &opt, pos), "optparse_progn6-7");

	RETURN;
}

static int test_optparse_progn_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn (progn 10) 20)");
	test(checkparse_progn_all(&opt, pos), "checkparse_progn_all1");
	zerospeed(&opt);
	test(! checkparse_progn_all(&opt, pos), "checkparse_progn_all2");
	remspeed(&opt);
	parse_eval_string(&pos, "(progn 10 20)");
	test(! checkparse_progn_all(&opt, pos), "checkparse_progn_all2");

	parse_eval_string(&pos, "(progn (progn 10) 20)");
	test(optparse_progn_all(local, &pos, &opt, pos), "optparse_progn_all1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_progn_all2");
	GetEvalParse(pos, 0, &pos);
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 10), "optparse_progn_all3");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 20), "optparse_progn_all4");
	test(pos == Nil, "optparse_progn_all5");
	parse_eval_string(&pos, "(progn 10 20 30)");
	test(! optparse_progn_all(local, &pos, &opt, pos), "optparse_progn_all6");

	RETURN;
}

static int test_optparse_progn(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(progn (progn 10) 20)");
	test(checkparse_progn(&opt, pos), "checkparse_progn1");
	parse_eval_string(&pos, "(and (progn 10) 20)");
	test(! checkparse_progn(&opt, pos), "checkparse_progn2");

	parse_eval_string(&pos, "(progn (progn 10) 20)");
	test(optparse_progn(local, &pos, &opt, pos), "optparse_progn1");
	parse_eval_string(&pos, "(and (progn 10) 20)");
	test(! optparse_progn(local, &pos, &opt, pos), "optparse_progn2");

	RETURN;
}


/*
 *  let
 */
static int test_check_lettype(void)
{
	addr pos;
	struct optstruct opt;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let nil (progn))");
	test(check_lettype(pos), "check_lettype1");
	parse_eval_string(&pos, "(let* nil (progn))");
	test(check_lettype(pos), "check_lettype2");
	parse_eval_string(&pos, "(progn)");
	test(! check_lettype(pos), "check_lettype3");
	parse_eval_string(&pos, "(let nil (progn))");
	test(check_lettype_on(&opt, pos), "check_lettype4");
	parse_eval_string(&pos, "(let* nil (progn))");
	test(check_lettype_on(&opt, pos), "check_lettype5");
	parse_eval_string(&pos, "(progn)");
	test(! check_lettype_on(&opt, pos), "check_lettype6");

	zerospeed(&opt);
	parse_eval_string(&pos, "(let nil (progn))");
	test(check_lettype(pos), "check_lettype7");
	parse_eval_string(&pos, "(let* nil (progn))");
	test(check_lettype(pos), "check_lettype8");
	parse_eval_string(&pos, "(progn)");
	test(! check_lettype(pos), "check_lettype9");
	parse_eval_string(&pos, "(let nil (progn))");
	test(! check_lettype_on(&opt, pos), "check_lettype10");
	parse_eval_string(&pos, "(let* nil (progn))");
	test(! check_lettype_on(&opt, pos), "check_lettype11");
	parse_eval_string(&pos, "(progn)");
	test(! check_lettype_on(&opt, pos), "check_lettype12");
	remspeed(&opt);

	RETURN;
}

static int test_optparse_let1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let nil 10 20)");
	test(checkparse_let1(&opt, pos), "checkparse_let1-1");
	zerospeed(&opt);
	test(! checkparse_let1(&opt, pos), "checkparse_let1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(let nil)");
	test(! checkparse_let1(&opt, pos), "checkparse_let1-3");
	parse_eval_string(&pos, "(let (a) 10 20)");
	test(! checkparse_let1(&opt, pos), "checkparse_let1-4");
	parse_eval_string(&pos, "(let nil (declare (special a)) 10 20)");
	test(! checkparse_let1(&opt, pos), "checkparse_let1-5");
	parse_eval_string(&pos, "(let nil (declare) 10 20)");
	test(checkparse_let1(&opt, pos), "checkparse_let1-6");

	parse_eval_string(&pos, "(let nil 10 20)");
	test(optparse_let1(local, &pos, &opt, pos), "optparse_let1-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_let1-2");
	GetEvalParse(pos, 0, &pos);
	GetCar(pos, &pos);
	test(check_evalinteger(pos, 10), "optparse_let1-3");
	parse_eval_string(&pos, "(let (a) 10 20)");
	test(! optparse_let1(local, &pos, &opt, pos), "optparse_let1-4");

	RETURN;
}

static int test_optparse_let2(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let nil (declare (special a)) 10 20)");
	test(checkparse_let2(&opt, pos), "checkparse_let2-1");
	zerospeed(&opt);
	test(! checkparse_let2(&opt, pos), "checkparse_let2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(let nil (declare (special a)))");
	test(! checkparse_let2(&opt, pos), "checkparse_let2-3");
	parse_eval_string(&pos, "(let (a) (declare (special a)) 10 20)");
	test(! checkparse_let2(&opt, pos), "checkparse_let2-4");
	parse_eval_string(&pos, "(let nil 10 20)");
	test(! checkparse_let2(&opt, pos), "checkparse_let2-5");
	parse_eval_string(&pos, "(let (a) (declare (special a)) 10 20)");
	test(! checkparse_let2(&opt, pos), "checkparse_let2-6");
	parse_eval_string(&pos, "(let nil (declare) 10 20)");
	test(! checkparse_let2(&opt, pos), "checkparse_let2-7");

	parse_eval_string(&pos, "(let nil (declare (special a)) 10 20)");
	test(optparse_let2(local, &pos, &opt, pos), "optparse_let2-1");
	test(check_evaltype(pos, EVAL_PARSE_LOCALLY), "optparse_let2-2");
	GetEvalParse(pos, 0, &check);
	test(eval_declare_p(check), "optparse_let2-3");
	GetEvalParse(pos, 1, &check);
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optparse_let2-4");
	parse_eval_string(&pos, "(let (a) 10 20)");
	test(! optparse_let2(local, &pos, &opt, pos), "optparse_let2-5");

	RETURN;
}

static int test_optparse_let3(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let nil)");
	test(checkparse_let3(&opt, pos), "checkparse_let3-1");
	zerospeed(&opt);
	test(! checkparse_let3(&opt, pos), "checkparse_let3-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(let nil (declare (special a)))");
	test(checkparse_let3(&opt, pos), "checkparse_let3-3");
	parse_eval_string(&pos, "(let (a) (declare (special a)))");
	test(! checkparse_let3(&opt, pos), "checkparse_let3-4");
	parse_eval_string(&pos, "(let nil 10 20)");
	test(! checkparse_let3(&opt, pos), "checkparse_let3-5");
	parse_eval_string(&pos, "(let (a) (declare (special a)) 10 20)");
	test(! checkparse_let3(&opt, pos), "checkparse_let3-6");

	parse_eval_string(&pos, "(let nil)");
	test(optparse_let3(local, &pos, &opt, pos), "optparse_let3-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_let3-2");
	parse_eval_string(&pos, "(let (a) 10 20)");
	test(! optparse_let3(local, &pos, &opt, pos), "optparse_let3-3");

	RETURN;
}

static int test_optparse_let4(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let (a b c) (declare (special a)))");
	test(checkparse_let4(&opt, pos), "checkparse_let4-1");
	zerospeed(&opt);
	test(! checkparse_let4(&opt, pos), "checkparse_let4-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(let nil)");
	test(! checkparse_let4(&opt, pos), "checkparse_let4-3");
	parse_eval_string(&pos, "(let (a (b 100) c))");
	test(! checkparse_let4(&opt, pos), "checkparse_let4-4");
	parse_eval_string(&pos, "(let (a b) 100)");
	test(! checkparse_let4(&opt, pos), "checkparse_let4-5");
	parse_eval_string(&pos, "(let (a b))");
	test(checkparse_let4(&opt, pos), "checkparse_let4-6");

	parse_eval_string(&pos, "(let (a b c) (declare (special a)))");
	test(optparse_let4(local, &pos, &opt, pos), "optparse_let4-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_let4-2");
	parse_eval_string(&pos, "(let (a) 10 20)");
	test(! optparse_let4(local, &pos, &opt, pos), "optparse_let4-3");

	RETURN;
}

static int test_optparse_let_args(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let (a (b (progn)) c) :hello)");
	test(checkparse_let_args(&opt, pos), "checkparse_let_args1");
	zerospeed(&opt);
	test(! checkparse_let_args(&opt, pos), "checkparse_let_args2");
	remspeed(&opt);
	parse_eval_string(&pos, "(let nil :hello)");
	test(! checkparse_let_args(&opt, pos), "checkparse_let_args3");
	parse_eval_string(&pos, "(let (a (b 100) c))");
	test(! checkparse_let_args(&opt, pos), "checkparse_let_args4");

	parse_eval_string(&pos, "(let ((a (progn)) b (c 100)) :hello)");
	test(optparse_let_args(local, &pos, &opt, pos), "optparse_let_args1");
	GetEvalParse(pos, 0, &pos);
	GetCar(pos, &pos);
	GetCdr(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_let_args2");
	parse_eval_string(&pos, "(let (a) 10 20)");
	test(! optparse_let_args(local, &pos, &opt, pos), "optparse_let_args3");

	RETURN;
}

static int test_optparse_let_body(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let nil (progn 10) 20 30)");
	test(checkparse_let_body(&opt, pos), "checkparse_let_body1");
	zerospeed(&opt);
	test(! checkparse_let_body(&opt, pos), "checkparse_let_body2");
	remspeed(&opt);
	parse_eval_string(&pos, "(let nil :hello)");
	test(! checkparse_let_body(&opt, pos), "checkparse_let_body3");
	parse_eval_string(&pos, "(let (a (b (progn)) c))");
	test(! checkparse_let_body(&opt, pos), "checkparse_let_body4");

	parse_eval_string(&pos, "(let (a b) (progn 10))");
	test(optparse_let_body(local, &pos, &opt, pos), "optparse_let_body1");
	GetEvalParse(pos, 2, &pos);
	GetCar(pos, &pos);
	test(check_evalinteger(pos, 10), "optparse_let_body2");
	parse_eval_string(&pos, "(let (a) (call) 20)");
	test(! optparse_let_body(local, &pos, &opt, pos), "optparse_let_body3");

	RETURN;
}

static int test_optparse_let(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(let nil 100)");
	test(checkparse_let(&opt, pos), "checkparse_let1");
	parse_eval_string(&pos, "(let* nil 100)");
	test(checkparse_let(&opt, pos), "checkparse_let2");
	parse_eval_string(&pos, "(let nil 100)");
	test(optparse_let(local, &pos, &opt, pos), "optparse_let1");
	parse_eval_string(&pos, "(let* nil 100)");
	test(optparse_let(local, &pos, &opt, pos), "optparse_let2");

	RETURN;
}


/*
 *  setq
 */
static int test_optparse_setq1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(setq)");
	test(checkparse_setq1(&opt, pos), "checkparse_setq1-1");
	zerospeed(&opt);
	test(! checkparse_setq1(&opt, pos), "checkparse_setq1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(setq aa 10 bb 20)");
	test(! checkparse_setq1(&opt, pos), "checkparse_setq1-3");

	parse_eval_string(&pos, "(setq)");
	test(optparse_setq1(local, &pos, &opt, pos), "optparse_setq1-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_setq1-2");
	parse_eval_string(&pos, "(setq aa 10)");
	test(! optparse_setq1(local, &pos, &opt, pos), "optparse_setq1-3");

	RETURN;
}

static int test_optparse_setq_all(void)
{
	addr pos, var, value;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(setq aaa (progn) bbb 100)");
	test(checkparse_setq_all(&opt, pos), "checkparse_setq_all1");
	zerospeed(&opt);
	test(! checkparse_setq_all(&opt, pos), "checkparse_setq_all2");
	remspeed(&opt);
	parse_eval_string(&pos, "(setq)");
	test(! checkparse_setq_all(&opt, pos), "checkparse_setq_all3");
	parse_eval_string(&pos, "(setq aaa 100)");
	test(! checkparse_setq_all(&opt, pos), "checkparse_setq_all4");

	parse_eval_string(&pos, "(setq aaa (progn) bbb 100)");
	test(optparse_setq_all(local, &pos, &opt, pos), "optparse_setq_all1");
	test(check_evaltype(pos, EVAL_PARSE_SETQ), "optparse_setq_all2");
	GetEvalParse(pos, 0, &pos);
	GetCons(pos, &var, &pos);
	GetCons(var, &var, &value);
	test(symbolp(var), "optparse_setq_all3");
	GetNameSymbol(var, &var);
	test(string_equal_char(var, "AAA"), "optparse_setq_all4");
	test(check_evaltype(value, EVAL_PARSE_NIL), "optparse_setq_all5");
	GetCons(pos, &var, &pos);
	GetCons(var, &var, &value);
	test(symbolp(var), "optparse_setq_all6");
	GetNameSymbol(var, &var);
	test(string_equal_char(var, "BBB"), "optparse_setq_all7");
	test(check_evalinteger(value, 100), "optparse_setq_all8");
	test(pos == Nil, "optparse_setq_all9");

	parse_eval_string(&pos, "(setq aaa 200 bbb 100)");
	test(! optparse_setq_all(local, &pos, &opt, pos), "optparse_setq_all10");

	RETURN;
}

static int test_optparse_setq(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(setq)");
	test(checkparse_setq(&opt, pos), "checkparse_setq1");
	parse_eval_string(&pos, "(setq aaa (progn) bbb 100)");
	test(checkparse_setq(&opt, pos), "checkparse_setq2");
	parse_eval_string(&pos, "(setq aaa 100 bbb 100)");
	test(! checkparse_setq(&opt, pos), "checkparse_setq3");

	parse_eval_string(&pos, "(setq)");
	test(optparse_setq(local, &pos, &opt, pos), "optparse_setq1");
	parse_eval_string(&pos, "(setq aaa (progn) bbb 100)");
	test(optparse_setq(local, &pos, &opt, pos), "optparse_setq2");
	parse_eval_string(&pos, "(setq aaa 100 bbb 100)");
	test(! optparse_setq(local, &pos, &opt, pos), "optparse_setq3");

	RETURN;
}


/*
 *  lambda-ordinary
 */
static int test_optparse_opt(void)
{
	addr pos, check, var, init, svar;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda (&optional (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 1, &pos); /* opt */
	test(checkparse_opt(&opt, pos), "checkparse_opt1");

	parse_eval_string(&pos, "#'(lambda (&optional (aa 100) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 1, &pos); /* opt */
	test(! checkparse_opt(&opt, pos), "checkparse_opt2");

	parse_eval_string(&pos, "#'(lambda (&optional (aa (progn) cc) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 1, &pos); /* opt */
	optparse_opt(local, &pos, &opt, pos);
	GetCons(pos, &var, &pos);
	GetCons(var, &var, &init);
	GetCons(init, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "aa");
	test(var == check, "optparse_opt1");
	test(check_evaltype(init, EVAL_PARSE_NIL), "optparse_opt2");
	readstring(&check, "cc");
	test(svar == check, "optparse_opt3");

	GetCons(pos, &var, &pos);
	GetCons(var, &var, &init);
	GetCar(init, &init);
	readstring(&check, "bb");
	test(var == check, "optparse_opt4");
	test(check_evaltype(init, EVAL_PARSE_NIL), "optparse_opt5");

	GetCons(pos, &var, &pos);
	test(pos == Nil, "optparse_opt6");

	RETURN;
}

static int test_optparse_key(void)
{
	addr pos, check, var, name, init, svar;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda (&key (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 3, &pos); /* key */
	test(checkparse_key(&opt, pos), "checkparse_key1");

	parse_eval_string(&pos, "#'(lambda (&key (aa 100) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 3, &pos); /* key */
	test(! checkparse_key(&opt, pos), "checkparse_key2");

	parse_eval_string(&pos, "#'(lambda (&key (aa (progn) cc) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 3, &pos); /* key */
	optparse_key(local, &pos, &opt, pos);
	GetCons(pos, &var, &pos);
	GetCons(var, &var, &name);
	GetCons(name, &name, &init);
	GetCons(init, &init, &svar);
	GetCar(svar, &svar);
	readstring(&check, "aa");
	test(var == check, "optparse_key1");
	readstring(&check, ":aa");
	test(name == check, "optparse_key2");
	test(check_evaltype(init, EVAL_PARSE_NIL), "optparse_key3");
	readstring(&check, "cc");
	test(svar == check, "optparse_key4");

	GetCons(pos, &var, &pos);
	GetCons(var, &var, &name);
	GetCons(name, &name, &init);
	GetCar(init, &init);
	readstring(&check, "bb");
	test(var == check, "optparse_key5");
	readstring(&check, ":bb");
	test(name == check, "optparse_key6");
	test(check_evaltype(init, EVAL_PARSE_NIL), "optparse_key7");

	GetCons(pos, &var, &pos);
	test(pos == Nil, "optparse_key8");

	RETURN;
}

static int test_optparse_aux(void)
{
	addr pos, check, var, init;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda (&aux (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	getnth(pos, 5, &pos); /* aux */
	optparse_aux(local, &pos, &opt, pos);
	GetCons(pos, &var, &pos);
	GetCons(var, &var, &init);
	GetCar(init, &init);
	readstring(&check, "aa");
	test(var == check, "optparse_aux1");
	test(check_evaltype(init, EVAL_PARSE_NIL), "optparse_aux2");

	GetCons(pos, &var, &pos);
	GetCons(var, &var, &init);
	GetCar(init, &init);
	readstring(&check, "bb");
	test(var == check, "optparse_aux3");
	test(check_evaltype(init, EVAL_PARSE_NIL), "optparse_aux4");

	GetCons(pos, &var, &pos);
	test(pos == Nil, "optparse_aux5");

	RETURN;
}

static int test_optparse_lambda_ordinary(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda (&optional (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	test(checkparse_lambda_ordinary(&opt, pos), "checkparse_lambda_ordinary1");
	parse_eval_string(&pos, "#'(lambda (&key (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	test(checkparse_lambda_ordinary(&opt, pos), "checkparse_lambda_ordinary2");
	parse_eval_string(&pos, "#'(lambda (&aux (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	test(checkparse_lambda_ordinary(&opt, pos), "checkparse_lambda_ordinary3");
	parse_eval_string(&pos, "#'(lambda (&optional (aa 100) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	test(! checkparse_lambda_ordinary(&opt, pos), "checkparse_lambda_ordinary4");

	parse_eval_string(&pos, "#'(lambda (&optional (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	optparse_lambda_ordinary(local, &pos, &opt, pos);
	getnth(pos, 1, &pos); /* opt */
	GetCar(pos, &pos); /* aa -> (var init svar) */
	GetCar(pos, &pos); /* var */
	readstring(&check, "aa");
	test(pos == check, "optparse_lambda_ordinary1");

	parse_eval_string(&pos, "#'(lambda (&key (aa (progn)) bb) :body)");
	GetEvalParse(pos, 0, &pos);
	optparse_lambda_ordinary(local, &pos, &opt, pos);
	getnth(pos, 3, &pos); /* key */
	GetCar(pos, &pos); /* aa -> (var name init svar) */
	GetCar(pos, &pos); /* var */
	readstring(&check, "aa");
	test(pos == check, "optparse_lambda_ordinary2");

	RETURN;
}


/*
 *  defun
 */
static int test_optparse_defun_args(void)
{
	addr pos, var, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(defun name (&optional (aa (progn)) bb) :body)");
	test(checkparse_defun_args(&opt, pos), "checkparse_defun_args1");
	parse_eval_string(&pos, "(defun name (&optional (aa 100) bb) :body)");
	test(! checkparse_defun_args(&opt, pos), "checkparse_defun_args2");

	parse_eval_string(&pos, "(defun name (&optional (aa (progn)) bb) :body)");
	test(optparse_defun_args(local, &pos, &opt, pos), "optparse_defun_args1");
	test(check_evaltype(pos, EVAL_PARSE_DEFUN), "optparse_defun_args2");
	GetEvalParse(pos, 1, &pos); /* args */
	getnth(pos, 1, &pos); /* opt */
	GetCar(pos, &pos); /* aa */
	GetCons(pos, &var, &pos);
	readstring(&check, "aa");
	test(var == check, "optparse_defun_args3");
	GetCar(pos, &var);
	test(check_evaltype(var, EVAL_PARSE_NIL), "optparse_defun_args4");

	parse_eval_string(&pos, "(defun name (&optional (aa 100) bb) :body)");
	test(! optparse_defun_args(local, &pos, &opt, pos), "optparse_defun_args5");
	parse_eval_string(&pos, "100");
	test(! optparse_defun_args(local, &pos, &opt, pos), "optparse_defun_args6");

	RETURN;
}

static int test_optparse_defun_body(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(defun name () (progn))");
	test(checkparse_defun_body(&opt, pos), "checkparse_defun_body1");
	parse_eval_string(&pos, "(defun name () 10 20 (progn))");
	test(checkparse_defun_body(&opt, pos), "checkparse_defun_body2");
	parse_eval_string(&pos, "(defun name () (call) 100)");
	test(! checkparse_defun_body(&opt, pos), "checkparse_defun_body3");

	parse_eval_string(&pos, "(defun name () (progn 100))");
	test(optparse_defun_body(local, &pos, &opt, pos), "optparse_defun_body1");
	test(check_evaltype(pos, EVAL_PARSE_DEFUN), "optparse_defun_body2");
	GetEvalParse(pos, 4, &pos); /* body */
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_INTEGER), "optparse_defun_body3");
	parse_eval_string(&pos, "(defun name () 100)");
	//test(! optparse_defun_body(local, &pos, &opt, pos), "optparse_defun_body4");
	//parse_eval_string(&pos, "100");
	//test(! optparse_defun_body(local, &pos, &opt, pos), "optparse_defun_body5");

	RETURN;
}

static int test_optparse_defun(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(defun name (&optional (aaa (progn))) :hello)");
	test(checkparse_defun(&opt, pos), "checkparse_defun1");
	parse_eval_string(&pos, "(defun name () (progn))");
	test(checkparse_defun(&opt, pos), "checkparse_defun3");
	//parse_eval_string(&pos, "(defun name (&key aaa) 200)");
	//test(! checkparse_defun(&opt, pos), "checkparse_defun4");

	parse_eval_string(&pos, "(defun name (&optional (aaa (progn))) :hello)");
	test(optparse_defun(local, &pos, &opt, pos), "optparse_defun1");
	test(check_evaltype(pos, EVAL_PARSE_DEFUN), "optparse_defun2");
	parse_eval_string(&pos, "(defun name () (progn))");
	test(optparse_defun(local, &pos, &opt, pos), "optparse_defun3");
	test(check_evaltype(pos, EVAL_PARSE_DEFUN), "optparse_defun2");
	parse_eval_string(&pos, "(defun name (&key aaa) 200)");
	//test(! optparse_defun(local, &pos, &opt, pos), "optparse_defun4");

	RETURN;
}


/*
 *  lambda
 */
static int test_optparse_lambda_args(void)
{
	addr pos, var, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda (&optional (aa (progn)) bb) :body)");
	test(checkparse_lambda_args(&opt, pos), "checkparse_lambda_args1");
	parse_eval_string(&pos, "#'(lambda (&optional (aa 100) bb) :body)");
	test(! checkparse_lambda_args(&opt, pos), "checkparse_lambda_args2");

	parse_eval_string(&pos, "#'(lambda (&optional (aa (progn)) bb) :body)");
	test(optparse_lambda_args(local, &pos, &opt, pos), "optparse_lambda_args1");
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optparse_lambda_args2");
	GetEvalParse(pos, 0, &pos); /* args */
	getnth(pos, 1, &pos); /* opt */
	GetCar(pos, &pos); /* aa */
	GetCons(pos, &var, &pos);
	readstring(&check, "aa");
	test(var == check, "optparse_lambda_args3");
	GetCar(pos, &var);
	test(check_evaltype(var, EVAL_PARSE_NIL), "optparse_lambda_args4");

	parse_eval_string(&pos, "#'(lambda (&optional (aa 100) bb) :body)");
	test(! optparse_lambda_args(local, &pos, &opt, pos), "optparse_lambda_args5");
	parse_eval_string(&pos, "100");
	test(! optparse_lambda_args(local, &pos, &opt, pos), "optparse_lambda_args6");

	RETURN;
}

static int test_optparse_lambda_body(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda () (progn))");
	test(checkparse_lambda_body(&opt, pos), "checkparse_lambda_body1");
	parse_eval_string(&pos, "#'(lambda () 10 20 (progn))");
	test(checkparse_lambda_body(&opt, pos), "checkparse_lambda_body2");
	parse_eval_string(&pos, "#'(lambda () (call) 100)");
	test(! checkparse_lambda_body(&opt, pos), "checkparse_lambda_body3");

	parse_eval_string(&pos, "#'(lambda () (progn 100))");
	test(optparse_lambda_body(local, &pos, &opt, pos), "optparse_lambda_body1");
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optparse_lambda_body2");
	GetEvalParse(pos, 3, &pos); /* body */
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_INTEGER), "optparse_lambda_body3");
	parse_eval_string(&pos, "#'(lambda () 100)");
	test(! optparse_lambda_body(local, &pos, &opt, pos), "optparse_lambda_body4");
	parse_eval_string(&pos, "100");
	test(! optparse_lambda_body(local, &pos, &opt, pos), "optparse_lambda_body5");

	RETURN;
}

static int test_optparse_lambda(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "#'(lambda (&optional (aaa (progn))) :hello)");
	test(checkparse_lambda(&opt, pos), "checkparse_lambda1");
	parse_eval_string(&pos, "#'(lambda () (progn))");
	test(checkparse_lambda(&opt, pos), "checkparse_lambda3");
	parse_eval_string(&pos, "#'(lambda (&key aaa) 200)");
	test(! checkparse_lambda(&opt, pos), "checkparse_lambda4");

	parse_eval_string(&pos, "#'(lambda (&optional (aaa (progn))) :hello)");
	test(optparse_lambda(local, &pos, &opt, pos), "optparse_lambda1");
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optparse_lambda2");
	parse_eval_string(&pos, "#'(lambda () (progn))");
	test(optparse_lambda(local, &pos, &opt, pos), "optparse_lambda3");
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optparse_lambda2");
	parse_eval_string(&pos, "#'(lambda (&key aaa) 200)");
	test(! optparse_lambda(local, &pos, &opt, pos), "optparse_lambda4");

	RETURN;
}


/*
 *  if
 */
static int test_optparse_if1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(if nil 100 200)");
	test(checkparse_if1(&opt, pos), "checkparse_if1-1");
	zerospeed(&opt);
	test(! checkparse_if1(&opt, pos), "checkparse_if1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(if t 100 200)");
	test(! checkparse_if1(&opt, pos), "checkparse_if1-3");

	parse_eval_string(&pos, "(if nil 100 200)");
	test(optparse_if1(local, &pos, &opt, pos), "optparse_if1-1");
	test(check_evalinteger(pos, 200), "optparse_if1-2");
	parse_eval_string(&pos, "(if :Hello 100 200)");
	test(! optparse_if1(local, &pos, &opt, pos), "optparse_if1-3");
	parse_eval_string(&pos, "100");
	test(! optparse_if1(local, &pos, &opt, pos), "optparse_if1-4");

	RETURN;
}

static int test_optparse_if2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(if #\\a 100 200)");
	test(checkparse_if2(&opt, pos), "checkparse_if2-1");
	zerospeed(&opt);
	test(! checkparse_if2(&opt, pos), "checkparse_if2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(if nil 100 200)");
	test(! checkparse_if2(&opt, pos), "checkparse_if2-3");
	parse_eval_string(&pos, "(if (call) 100 200)");
	test(! checkparse_if2(&opt, pos), "checkparse_if2-4");
	parse_eval_string(&pos, "(if :Hello 100 200)");
	test(checkparse_if2(&opt, pos), "checkparse_if2-5");

	parse_eval_string(&pos, "(if #\\a 100 200)");
	test(optparse_if2(local, &pos, &opt, pos), "optparse_if2-1");
	test(check_evalinteger(pos, 100), "optparse_if2-2");
	parse_eval_string(&pos, "(if nil 100 200)");
	test(! optparse_if2(local, &pos, &opt, pos), "optparse_if2-3");
	parse_eval_string(&pos, "100");
	test(! optparse_if2(local, &pos, &opt, pos), "optparse_if2-4");

	RETURN;
}

static int test_optparse_if_all(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(if t (progn) 200)");
	test(checkparse_if_all(&opt, pos), "checkparse_if_all1");
	zerospeed(&opt);
	test(! checkparse_if_all(&opt, pos), "checkparse_if_all-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(if (progn) 100 200)");
	test(checkparse_if_all(&opt, pos), "checkparse_if_all3");
	parse_eval_string(&pos, "(if 100 200 (progn))");
	test(checkparse_if_all(&opt, pos), "checkparse_if_all4");
	parse_eval_string(&pos, "(if 100 200 300)");
	test(! checkparse_if_all(&opt, pos), "checkparse_if_all5");

	parse_eval_string(&pos, "(if (progn) 100 200)");
	test(optparse_if_all(local, &pos, &opt, pos), "optparse_if_all1");
	test(check_evaltype(pos, EVAL_PARSE_IF), "optparse_if_all2");
	GetEvalParse(pos, 0, &pos);
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_if_all3");

	parse_eval_string(&pos, "(if 100 (progn) 200)");
	test(optparse_if_all(local, &pos, &opt, pos), "optparse_if_all4");
	GetEvalParse(pos, 1, &pos);
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_if_all5");

	parse_eval_string(&pos, "(if t 100 (progn 200))");
	test(optparse_if_all(local, &pos, &opt, pos), "optparse_if_all6");
	test(check_evaltype(pos, EVAL_PARSE_IF), "optparse_if_all7");
	GetEvalParse(pos, 2, &pos);
	test(check_evalinteger(pos, 200), "optparse_if_all8");

	parse_eval_string(&pos, "(if t 100 200)");
	test(! optparse_if_all(local, &pos, &opt, pos), "optparse_if_all9");
	parse_eval_string(&pos, "100");
	test(! optparse_if_all(local, &pos, &opt, pos), "optparse_if_all10");

	RETURN;
}

static int test_optparse_if(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(if nil 100 200)");
	test(checkparse_if(&opt, pos), "checkparse_if1");
	parse_eval_string(&pos, "(if t 100 200)");
	test(checkparse_if(&opt, pos), "checkparse_if2");
	parse_eval_string(&pos, "(if (call) (progn) 200)");
	test(checkparse_if(&opt, pos), "checkparse_if3");
	parse_eval_string(&pos, "(if (call) 20 30)");
	test(! checkparse_if(&opt, pos), "checkparse_if4");

	parse_eval_string(&pos, "(if nil 100 200)");
	test(optparse_if(local, &pos, &opt, pos), "optparse_if1");
	test(check_evalinteger(pos, 200), "optparse_if2");
	parse_eval_string(&pos, "(if t 100 200)");
	test(optparse_if(local, &pos, &opt, pos), "optparse_if3");
	test(check_evalinteger(pos, 100), "optparse_if4");
	parse_eval_string(&pos, "(if (call) (progn) 200)");
	test(optparse_if(local, &pos, &opt, pos), "optparse_if5");
	parse_eval_string(&pos, "(if (call) 20 30)");
	test(! optparse_if(local, &pos, &opt, pos), "optparse_if6");

	RETURN;
}


/*
 *  unwind-protect
 */
static int test_optparse_unwind_protect1(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(unwind-protect 10 20 30)");
	test(checkparse_unwind_protect1(&opt, pos), "checkparse_unwind_protect1-1");
	zerospeed(&opt);
	test(! checkparse_unwind_protect1(&opt, pos), "checkparse_unwind_protect1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(unwind-protect (call) 10 20 30)");
	test(! checkparse_unwind_protect1(&opt, pos), "checkparse_unwind_protect1-3");

	parse_eval_string(&pos, "(unwind-protect 10 20 30)");
	test(optparse_unwind_protect1(local, &pos, &opt, pos),
			"optparse_unwind_protect1-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_unwind_protect1-2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 3, "optparse_unwind_protect1-3");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 20), "optparse_unwind_protect1-4");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 30), "optparse_unwind_protect1-5");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 10), "optparse_unwind_protect1-6");

	parse_eval_string(&pos, "(unwind-protect (call) 10 20 30)");
	test(! optparse_unwind_protect1(local, &pos, &opt, pos),
			"optparse_unwind_protect1-7");

	RETURN;
}

static int test_optparse_unwind_protect2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(unwind-protect 10)");
	test(checkparse_unwind_protect2(&opt, pos), "checkparse_unwind_protect2-1");
	zerospeed(&opt);
	test(! checkparse_unwind_protect2(&opt, pos), "checkparse_unwind_protect2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(unwind-protect hello 10 20 30)");
	test(! checkparse_unwind_protect2(&opt, pos), "checkparse_unwind_protect2-3");

	parse_eval_string(&pos, "(unwind-protect 10)");
	test(optparse_unwind_protect2(local, &pos, &opt, pos),
			"optparse_unwind_protect2-1");
	test(check_evalinteger(pos, 10), "optparse_unwind_protect2-2");
	parse_eval_string(&pos, "(unwind-protect hello 10 20 30)");
	test(! optparse_unwind_protect2(local, &pos, &opt, pos),
			"optparse_unwind_protect2-3");

	RETURN;
}

static int test_optparse_unwind_protect_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(unwind-protect 10 (progn (call)) 10)");
	test(checkparse_unwind_protect_all(&opt, pos),
			"checkparse_unwind_protect_all1");
	zerospeed(&opt);
	test(! checkparse_unwind_protect_all(&opt, pos),
			"checkparse_unwind_protect_all2");
	remspeed(&opt);
	parse_eval_string(&pos, "(unwind-protect hello (call) 10)");
	test(! checkparse_unwind_protect_all(&opt, pos),
			"checkparse_unwind_protect_all3");

	parse_eval_string(&pos, "(unwind-protect 10 (progn (call)) 20)");
	test(optparse_unwind_protect_all(local, &pos, &opt, pos),
			"optparse_unwind_protect_all1");
	test(check_evaltype(pos, EVAL_PARSE_UNWIND_PROTECT),
			"optparse_unwind_protect_all2");
	GetEvalParse(pos, 0, &check);
	test(check_evalinteger(check, 10), "optparse_unwind_protect_all3");
	GetEvalParse(pos, 1, &check);
	GetCar(check, &check);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_unwind_protect_all4");
	parse_eval_string(&pos, "(unwind-protect hello (call) 10)");
	test(! optparse_unwind_protect_all(local, &pos, &opt, pos),
			"optparse_unwind_protect_all5");

	RETURN;
}

static int test_optparse_unwind_protect(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(unwind-protect 10 20 30)");
	test(checkparse_unwind_protect(&opt, pos), "checkparse_unwind_protect1");
	parse_eval_string(&pos, "(unwind-protect (call) 10)");
	test(! checkparse_unwind_protect(&opt, pos), "checkparse_unwind_protect2");

	parse_eval_string(&pos, "(unwind-protect 10 20 30)");
	test(optparse_unwind_protect(local, &pos, &opt, pos), "optparse_unwind_protect1");
	parse_eval_string(&pos, "(unwind-protect (call) 10)");
	test(! optparse_unwind_protect(local, &pos, &opt, pos), "optparse_unwind_protect2");

	RETURN;
}


/*
 *  tagbody
 */
static int test_optparse_tagbody1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(tagbody)");
	test(checkparse_tagbody1(&opt, pos), "checkparse_tagbody1-1");
	zerospeed(&opt);
	test(! checkparse_tagbody1(&opt, pos), "checkparse_tagbody1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(tagbody 10 20 30)");
	test(checkparse_tagbody1(&opt, pos), "checkparse_tagbody1-3");
	parse_eval_string(&pos, "(tagbody 10 (call) 30)");
	test(! checkparse_tagbody1(&opt, pos), "checkparse_tagbody1-4");

	parse_eval_string(&pos, "(tagbody)");
	test(optparse_tagbody1(local, &pos, &opt, pos), "optparse_tagbody1-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_tagbody1-2");
	parse_eval_string(&pos, "(tagbody (call))");
	test(! optparse_tagbody1(local, &pos, &opt, pos), "optparse_tagbody1-3");
	parse_eval_string(&pos, "100");
	test(! optparse_tagbody1(local, &pos, &opt, pos), "optparse_tagbody1-4");

	RETURN;
}

static int test_optparse_tagbody2(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(tagbody (call) (call2))");
	test(checkparse_tagbody2(&opt, pos), "checkparse_tagbody2-1");
	zerospeed(&opt);
	test(! checkparse_tagbody2(&opt, pos), "checkparse_tagbody2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(tagbody (call) hello (call2))");
	test(! checkparse_tagbody2(&opt, pos), "checkparse_tagbody2-3");

	parse_eval_string(&pos, "(tagbody (call) (call2))");
	test(optparse_tagbody2(local, &pos, &opt, pos), "optparse_tagbody2-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_tagbody2-2");
	GetEvalParse(pos, 0, &pos);
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_tagbody2-3");
	GetEvalParse(check, 0, &check); /* function */
	GetEvalParse(check, 0, &check); /* callname */
	GetCallName(check, &check); /* symbol */
	GetNameSymbol(check, &check);
	test(string_equal_char(check, "CALL"), "optparse_tagbody2-4");

	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_tagbody2-5");
	GetEvalParse(check, 0, &check); /* function */
	GetEvalParse(check, 0, &check); /* callname */
	GetCallName(check, &check); /* symbol */
	GetNameSymbol(check, &check);
	test(string_equal_char(check, "CALL2"), "optparse_tagbody2-6");

	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_NIL), "optparse_tagbody2-7");
	test(pos == Nil, "optparse_tagbody2-8");

	parse_eval_string(&pos, "(tagbody hello (call2))");
	test(! optparse_tagbody2(local, &pos, &opt, pos), "optparse_tagbody2-9");
	parse_eval_string(&pos, "100");
	test(! optparse_tagbody2(local, &pos, &opt, pos), "optparse_tagbody2-10");

	RETURN;
}

static int test_optparse_tagbody_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(tagbody 100 (progn))");
	test(checkparse_tagbody_all(&opt, pos), "checkparse_tagbody_all1");
	parse_eval_string(&pos, "(tagbody 100 (call) (call2) 200 300 400)");
	test(! checkparse_tagbody_all(&opt, pos), "checkparse_tagbody_all2");

	parse_eval_string(&pos, "(tagbody 10 (progn) 20)");
	test(optparse_tagbody_all(local, &pos, &opt, pos), "optparse_tagbody_all1");
	test(check_evaltype(pos, EVAL_PARSE_TAGBODY), "optparse_tagbody_all2");
	GetEvalParse(pos, 0, &check);
	test(length_list_unsafe(check) == 2, "optparse_tagbody_all3");
	GetEvalParse(pos, 1, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_tagbody_all4");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_TAG), "optparse_tagbody_all5");
	GetEvalParse(check, 0, &check);
	test(RefFixnum(check) == 10, "optparse_tagbody_all6");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_TAG), "optparse_tagbody_all7");
	GetEvalParse(check, 0, &check);
	test(RefFixnum(check) == 20, "optparse_tagbody_all8");
	GetCons(pos, &check, &pos);
	test(pos == Nil, "optparse_tagbody_all9");

	parse_eval_string(&pos, "(tagbody 100 (call))");
	test(! optparse_tagbody_all(local, &pos, &opt, pos), "optparse_tagbody_all10");
	parse_eval_string(&pos, "100");
	test(! optparse_tagbody_all(local, &pos, &opt, pos), "optparse_tagbody_all11");

	RETURN;
}

static int test_optparse_tagbody(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(tagbody)");
	test(checkparse_tagbody(&opt, pos), "checkparse_tagbody1");
	parse_eval_string(&pos, "(tagbody (call) (call2))");
	test(checkparse_tagbody(&opt, pos), "checkparse_tagbody2");
	parse_eval_string(&pos, "(tagbody 100 (progn) (call))");
	test(checkparse_tagbody(&opt, pos), "checkparse_tagbody3");
	parse_eval_string(&pos, "(tagbody 100 (call) (call))");
	test(! checkparse_tagbody(&opt, pos), "checkparse_tagbody4");

	parse_eval_string(&pos, "(tagbody)");
	test(optparse_tagbody(local, &pos, &opt, pos), "optparse_tagbody1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_tagbody2");
	parse_eval_string(&pos, "100");
	test(! optparse_tagbody(local, &pos, &opt, pos), "optparse_tagbody3");

	RETURN;
}


/*
 *  block
 */
static int test_optparse_block1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(block hello)");
	test(checkparse_block1(&opt, pos), "checkparse_block1-1");
	zerospeed(&opt);
	test(! checkparse_block1(&opt, pos), "checkparse_block1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(block hello 10)");
	test(! checkparse_block1(&opt, pos), "checkparse_block1-3");

	parse_eval_string(&pos, "(block hello)");
	test(optparse_block1(local, &pos, &opt, pos), "optparse_block1-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_block1-2");
	parse_eval_string(&pos, "(block hello 10 20)");
	test(! optparse_block1(local, &pos, &opt, pos), "optparse_block1-3");

	RETURN;
}

static int test_optparse_block2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(block hello 10 20)");
	test(checkparse_block2(&opt, pos), "checkparse_block2-1");
	zerospeed(&opt);
	test(! checkparse_block2(&opt, pos), "checkparse_block2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(block hello (call))");
	test(! checkparse_block2(&opt, pos), "checkparse_block2-3");
	parse_eval_string(&pos, "(block hello)");
	test(! checkparse_block2(&opt, pos), "checkparse_block2-4");

	parse_eval_string(&pos, "(block hello 10 20)");
	test(optparse_block2(local, &pos, &opt, pos), "optparse_block2-1");
	test(check_evalinteger(pos, 20), "optparse_block2-2");
	parse_eval_string(&pos, "(block hello (call))");
	test(! optparse_block2(local, &pos, &opt, pos), "optparse_block2-3");

	RETURN;
}

static int test_optparse_block_all(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(block hello (progn (call)) 20)");
	test(checkparse_block_all(&opt, pos), "checkparse_block_all1");
	parse_eval_string(&pos, "(block hello (call) 20)");
	test(! checkparse_block_all(&opt, pos), "checkparse_block_all2");

	parse_eval_string(&pos, "(block hello (progn (call)) 20)");
	test(optparse_block_all(local, &pos, &opt, pos), "optparse_block_all1");
	test(check_evaltype(pos, EVAL_PARSE_BLOCK), "optparse_block_all2");
	GetEvalParse(pos, 1, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_block_all3");
	parse_eval_string(&pos, "(block hello (call) 10)");
	test(! optparse_block_all(local, &pos, &opt, pos), "optparse_block_all4");

	RETURN;
}

static int test_optparse_return_from(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(return-from hello (progn (call)))");
	test(checkparse_return_from(&opt, pos), "checkparse_return_from1");
	parse_eval_string(&pos, "(return-from hello (call))");
	test(! checkparse_return_from(&opt, pos), "checkparse_return_from2");

	parse_eval_string(&pos, "(return-from hello (progn (call)))");
	test(optparse_return_from(local, &pos, &opt, pos), "optparse_return_from1");
	test(check_evaltype(pos, EVAL_PARSE_RETURN_FROM), "optparse_return_from2");
	GetEvalParse(pos, 1, &pos);
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_return_from3");
	parse_eval_string(&pos, "(return-from hello (call))");
	test(! optparse_return_from(local, &pos, &opt, pos), "optparse_return_from4");

	RETURN;
}

static int test_optparse_block(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(block hello (progn (call)) 20)");
	test(checkparse_block(&opt, pos), "checkparse_block1");
	parse_eval_string(&pos, "(block hello (call) 20)");
	test(! checkparse_block(&opt, pos), "checkparse_block2");
	parse_eval_string(&pos, "(return-from hello (progn (call)))");
	test(checkparse_block(&opt, pos), "checkparse_block3");
	parse_eval_string(&pos, "(return-from hello (call))");
	test(! checkparse_block(&opt, pos), "checkparse_block4");

	parse_eval_string(&pos, "(block hello (progn (call)) 20)");
	test(optparse_block(local, &pos, &opt, pos), "optparse_block1");
	test(check_evaltype(pos, EVAL_PARSE_BLOCK), "optparse_block2");
	GetEvalParse(pos, 1, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_block3");
	parse_eval_string(&pos, "(block hello (call) 10)");
	test(! optparse_block(local, &pos, &opt, pos), "optparse_block4");

	RETURN;
}


/*
 *  catch
 */
static int test_optparse_catch1(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(catch name)");
	test(checkparse_catch1(&opt, pos), "checkparse_catch1-1");
	zerospeed(&opt);
	test(! checkparse_catch1(&opt, pos), "checkparse_catch1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(catch name 10)");
	test(! checkparse_catch1(&opt, pos), "checkparse_catch1-3");

	parse_eval_string(&pos, "(catch name)");
	test(optparse_catch1(local, &pos, &opt, pos), "optparse_catch1-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_catch1-2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_catch1-3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_SYMBOL), "optparse_catch1-4");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_NIL), "optparse_catch1-5");
	parse_eval_string(&pos, "(catch name 10 20 30)");
	test(! optparse_catch1(local, &pos, &opt, pos), "optparse_catch1-6");

	RETURN;
}

static int test_optparse_catch2(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(catch name 10 20 30)");
	test(checkparse_catch2(&opt, pos), "checkparse_catch2-1");
	zerospeed(&opt);
	test(! checkparse_catch2(&opt, pos), "checkparse_catch2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(catch name)");
	test(! checkparse_catch2(&opt, pos), "checkparse_catch2-3");

	parse_eval_string(&pos, "(catch name 10 20 30)");
	test(optparse_catch2(local, &pos, &opt, pos), "optparse_catch2-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_catch2-2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_catch2-3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_SYMBOL), "optparse_catch2-4");
	GetCons(pos, &check, &pos);
	test(check_evalinteger(check, 30), "optparse_catch2-5");
	parse_eval_string(&pos, "(catch name)");
	test(! optparse_catch2(local, &pos, &opt, pos), "optparse_catch2-6");

	RETURN;
}

static int test_optparse_catch_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(catch hello (progn (call)) 20)");
	test(checkparse_catch_all(&opt, pos), "checkparse_catch_all1");
	parse_eval_string(&pos, "(catch (progn hello) (call) 20)");
	test(checkparse_catch_all(&opt, pos), "checkparse_catch_all2");
	parse_eval_string(&pos, "(catch hello (call) 20)");
	test(! checkparse_catch_all(&opt, pos), "checkparse_catch_all3");

	parse_eval_string(&pos, "(catch (progn) (progn (call)) 20)");
	test(optparse_catch_all(local, &pos, &opt, pos), "optparse_catch_all1");
	test(check_evaltype(pos, EVAL_PARSE_CATCH), "optparse_catch_all2");
	GetEvalParse(pos, 0, &check);
	test(check_evaltype(check, EVAL_PARSE_NIL), "optparse_catch_all3");
	GetEvalParse(pos, 1, &check);
	test(length_list_unsafe(check) == 2, "optparse_catch_all4");
	GetCar(check, &check);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_catch_all5");
	parse_eval_string(&pos, "(catch hello (call) 10)");
	test(! optparse_catch_all(local, &pos, &opt, pos), "optparse_catch_all6");

	RETURN;
}

static int test_optparse_throw(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(throw hello (progn (call)))");
	test(checkparse_throw(&opt, pos), "checkparse_throw1");
	parse_eval_string(&pos, "(throw (progn hello) (call))");
	test(checkparse_throw(&opt, pos), "checkparse_throw2");
	parse_eval_string(&pos, "(throw hello (call))");
	test(! checkparse_throw(&opt, pos), "checkparse_throw3");

	parse_eval_string(&pos, "(throw (progn) (progn (call)))");
	test(optparse_throw(local, &pos, &opt, pos), "optparse_throw1");
	test(check_evaltype(pos, EVAL_PARSE_THROW), "optparse_throw2");
	GetEvalParse(pos, 0, &check);
	test(check_evaltype(check, EVAL_PARSE_NIL), "optparse_throw3");
	GetEvalParse(pos, 1, &check);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_throw4");
	parse_eval_string(&pos, "(throw hello (call))");
	test(! optparse_throw(local, &pos, &opt, pos), "optparse_throw5");

	RETURN;
}

static int test_optparse_catch(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(catch hello)");
	test(checkparse_catch(&opt, pos), "checkparse_catch1");
	parse_eval_string(&pos, "(throw (progn) 10)");
	test(checkparse_catch(&opt, pos), "checkparse_catch2");
	parse_eval_string(&pos, "(catch hello (call))");
	test(! checkparse_catch(&opt, pos), "checkparse_catch3");

	parse_eval_string(&pos, "(catch hello)");
	test(optparse_catch(local, &pos, &opt, pos), "optparse_catch1");
	parse_eval_string(&pos, "(throw (progn) 10)");
	test(optparse_catch(local, &pos, &opt, pos), "optparse_catch2");
	parse_eval_string(&pos, "(catch hello (call))");
	test(! optparse_catch(local, &pos, &opt, pos), "optparse_catch3");

	RETURN;
}


/*
 *  flet / labels
 */
static int test_check_fletlabels(void)
{
	addr pos;
	struct optstruct opt;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((a ())) :hello)");
	test(check_fletlabels(pos), "check_fletlabels1");
	parse_eval_string(&pos, "(labels ((a ())) :hello)");
	test(check_fletlabels(pos), "check_fletlabels2");
	parse_eval_string(&pos, "(progn)");
	test(! check_fletlabels(pos), "check_fletlabels3");

	RETURN;
}

static int test_check_fletlabels_on(void)
{
	addr pos;
	struct optstruct opt;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((a ())) :hello)");
	test(check_fletlabels_on(&opt, pos), "check_fletlabels_on1");
	parse_eval_string(&pos, "(labels ((a ())) :hello)");
	test(check_fletlabels_on(&opt, pos), "check_fletlabels_on2");
	parse_eval_string(&pos, "(progn)");
	test(! check_fletlabels_on(&opt, pos), "check_fletlabels_on3");

	zerospeed(&opt);
	parse_eval_string(&pos, "(flet ((a ())) :hello)");
	test(! check_fletlabels_on(&opt, pos), "check_fletlabels_on4");
	parse_eval_string(&pos, "(labels ((a ())) :hello)");
	test(! check_fletlabels_on(&opt, pos), "check_fletlabels_on5");
	parse_eval_string(&pos, "(progn)");
	test(! check_fletlabels_on(&opt, pos), "check_fletlabels_on6");
	remspeed(&opt);

	RETURN;
}

static int test_optparse_flet1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((a (b) :hello)))");
	test(checkparse_flet1(&opt, pos), "checkparse_flet1-1");
	zerospeed(&opt);
	test(! checkparse_flet1(&opt, pos), "checkparse_flet1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(flet ((a (b) :hello)) :aaa)");
	test(! checkparse_flet1(&opt, pos), "checkparse_flet1-3");

	parse_eval_string(&pos, "(flet ((a (b) :hello)))");
	test(optparse_flet1(local, &pos, &opt, pos), "optparse_flet1-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_flet1-2");
	parse_eval_string(&pos, "(flet nil :hello)");
	test(! optparse_flet1(local, &pos, &opt, pos), "optparse_flet1-3");

	RETURN;
}

static int test_optparse_flet2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((a ())) 10 20 30)");
	test(checkparse_flet2(&opt, pos), "checkparse_flet2-1");
	zerospeed(&opt);
	test(! checkparse_flet2(&opt, pos), "checkparse_flet2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(flet ((a ())) (declare (special a)) 10 20 30)");
	test(! checkparse_flet2(&opt, pos), "checkparse_flet2-3");
	parse_eval_string(&pos, "(flet ((a ())))");
	test(! checkparse_flet2(&opt, pos), "checkparse_flet2-4");
	parse_eval_string(&pos, "(flet ((a ())) 10 (call) 30)");
	test(! checkparse_flet2(&opt, pos), "checkparse_flet2-5");

	parse_eval_string(&pos, "(flet nil 10 20 30)");
	test(optparse_flet2(local, &pos, &opt, pos), "optparse_flet2-1");
	test(check_evalinteger(pos, 30), "optparse_flet2-2");
	parse_eval_string(&pos, "(flet nil (call))");
	test(! optparse_flet2(local, &pos, &opt, pos), "optparse_flet2-3");

	RETURN;
}

static int test_optparse_flet3(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet nil 10 20 30)");
	test(checkparse_flet3(&opt, pos), "checkparse_flet3-1");
	zerospeed(&opt);
	test(! checkparse_flet3(&opt, pos), "checkparse_flet3-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(flet nil)");
	test(! checkparse_flet3(&opt, pos), "checkparse_flet3-3");
	parse_eval_string(&pos, "(flet ((a ())) 10 20 30)");
	test(! checkparse_flet3(&opt, pos), "checkparse_flet3-4");
	parse_eval_string(&pos, "(flet nil (declare (special a)) 10 20)");
	test(! checkparse_flet3(&opt, pos), "checkparse_flet3-5");

	parse_eval_string(&pos, "(flet nil 10 20 30)");
	test(optparse_flet3(local, &pos, &opt, pos), "optparse_flet3-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_flet3-2");
	GetEvalParse(pos, 0, &pos);
	GetCar(pos, &pos);
	test(check_evalinteger(pos, 10), "optparse_flet3-3");
	parse_eval_string(&pos, "(flet nil)");
	test(! optparse_flet3(local, &pos, &opt, pos), "optparse_flet3-4");

	RETURN;
}

static int test_optparse_flet4(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet nil (declare (special a)) 10)");
	test(checkparse_flet4(&opt, pos), "checkparse_flet4-1");
	zerospeed(&opt);
	test(! checkparse_flet4(&opt, pos), "checkparse_flet4-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(flet ((a ())) (declare (special a)) 10)");
	test(! checkparse_flet4(&opt, pos), "checkparse_flet4-3");
	parse_eval_string(&pos, "(flet nil 10)");
	test(! checkparse_flet4(&opt, pos), "checkparse_flet4-4");
	parse_eval_string(&pos, "(flet nil (declare (special a)))");
	test(! checkparse_flet4(&opt, pos), "checkparse_flet4-5");

	parse_eval_string(&pos, "(flet nil (declare (special a)) 10)");
	test(optparse_flet4(local, &pos, &opt, pos), "optparse_flet4-1");
	test(check_evaltype(pos, EVAL_PARSE_LOCALLY), "optparse_flet4-2");
	GetEvalParse(pos, 0, &check);
	test(eval_declare_p(check), "optparse_flet4-3");
	GetEvalParse(pos, 1, &pos);
	GetCar(pos, &pos);
	test(check_evalinteger(pos, 10), "optparse_flet4-4");
	parse_eval_string(&pos, "(flet nil)");
	test(! optparse_flet4(local, &pos, &opt, pos), "optparse_flet4-5");

	RETURN;
}

static int test_checkparse_flet_args(void)
{
	addr pos;
	struct optstruct opt;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((aa (&optional (bb (progn))))) :hello)");
	test(checkparse_flet_args(&opt, pos), "checkparse_flet_args1");
	//parse_eval_string(&pos, "(flet ((aa (&optional (bb 100)))) :hello)");
	//test(! checkparse_flet_args(&opt, pos), "checkparse_flet_args2");
	parse_eval_string(&pos, "(flet ((aa () (progn))) :hello)");
	test(checkparse_flet_args(&opt, pos), "checkparse_flet_args3");
	//parse_eval_string(&pos, "(flet ((aa () 100)) :hello)");
	//test(! checkparse_flet_args(&opt, pos), "checkparse_flet_args4");
	//parse_eval_string(&pos, "(flet ((aa () 100)) (progn))");
	//test(! checkparse_flet_args(&opt, pos), "checkparse_flet_args5");

	RETURN;
}

static int test_optparse_flet_args(void)
{
	addr pos, var, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((aa (&optional (bb (progn 10))))) :hello)");
	test(optparse_flet_args(local, &pos, &opt, pos), "optparse_flet_args1");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optparse_flet_args2");
	GetEvalParse(pos, 0, &pos); /* flet-args */
	GetCar(pos, &pos); /* (aa ...) */
	GetCons(pos, &var, &pos); /* name */
	test(GetType(var) == LISPTYPE_CALLNAME, "optparse_flet_args3");
	GetCallName(var, &var);
	readstring(&check, "aa");
	test(var == check, "optparse_flet_args4");
	GetCons(pos, &var, &pos); /* lambda-list */
	getnth(var, 1, &var); /* optional */
	GetCar(var, &var); /* first argument */
	GetCdr(var, &var); /* var */
	GetCar(var, &var); /* init */
	test(check_evalinteger(var, 10), "optparse_flet_args5");

	parse_eval_string(&pos, "(flet ((aa () (progn 20))) :hello)");
	test(optparse_flet_args(local, &pos, &opt, pos), "optparse_flet_args6");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optparse_flet_args7");
	GetEvalParse(pos, 0, &pos); /* flet-args */
	GetCar(pos, &pos); /* (aa ...) */
	GetCdr(pos, &pos); /* name */
	GetCdr(pos, &pos); /* args */
	GetCdr(pos, &pos); /* decl */
	GetCdr(pos, &pos); /* doc */
	GetCar(pos, &pos); /* cons */
	GetCar(pos, &pos); /* (10) */
	test(check_evalinteger(pos, 20), "optparse_flet_args8");

	RETURN;
}

static int test_optparse_flet_body(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ((a ())) (progn (call)) 10)");
	test(checkparse_flet_body(&opt, pos), "checkparse_flet_body1");
	parse_eval_string(&pos, "(flet ((a ())) (call) 10)");
	test(! checkparse_flet_body(&opt, pos), "checkparse_flet_body2");
	parse_eval_string(&pos, "(flet ((a () (progn))))");
	test(! checkparse_flet_body(&opt, pos), "checkparse_flet_body3");

	parse_eval_string(&pos, "(flet ((a ())) (progn (call)) 10)");
	test(optparse_flet_body(local, &pos, &opt, pos), "optparse_flet_body1");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optparse_flet_body2");
	GetEvalParse(pos, 2, &pos); /* cons */
	test(length_list_unsafe(pos) == 2, "optparse_flet_body3");
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_flet_body4");
	parse_eval_string(&pos, "(flet ((a () (progn))))");
	test(! optparse_flet_body(local, &pos, &opt, pos), "optparse_flet_body5");

	RETURN;
}

static int test_optparse_flet(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(flet ())");
	test(checkparse_flet(&opt, pos), "checkparse_flet1");
	parse_eval_string(&pos, "(flet () 10 20 30)");
	test(checkparse_flet(&opt, pos), "checkparse_flet2");
	parse_eval_string(&pos, "(flet () (call) 20 30)");
	test(checkparse_flet(&opt, pos), "checkparse_flet3");
	parse_eval_string(&pos, "(flet () (declare (special a)) (call) 30)");
	test(checkparse_flet(&opt, pos), "checkparse_flet4");
	parse_eval_string(&pos, "(flet ((a (&optional (b (progn))))) (call))");
	test(checkparse_flet(&opt, pos), "checkparse_flet5");
	parse_eval_string(&pos, "(flet ((a ())) (progn))");
	test(checkparse_flet(&opt, pos), "checkparse_flet6");
	//parse_eval_string(&pos, "(flet ((a ())) (call) (call2))");
	//test(! checkparse_flet(&opt, pos), "checkparse_flet7");

	parse_eval_string(&pos, "(flet ())");
	test(optparse_flet(local, &pos, &opt, pos), "optparse_flet1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_flet2");
	//parse_eval_string(&pos, "(flet ((a ())) (call) (call2))");
	//test(! optparse_flet(local, &pos, &opt, pos), "optparse_flet3");

	RETURN;
}

static int test_optparse_flet_error(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(flet ((z () (declare (optimize speed)) (progn 10)) "
			"       (x () (progn))) "
			"  :body)");
	test(checkparse(&opt, pos), "optparse_flet_error1");
	test(optparse(local, &pos, &opt, pos), "optparse_flet_error2");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optparse_flet_error3");
	GetEvalParse(pos, 0, &pos); /* flet-args */
	getnth(pos, 0, &check); /* z */
	getnth(check, 4, &check); /* body */
	test(length_list_unsafe(check) == 1, "optparse_flet_error4");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optparse_flet_error5");
	getnth(pos, 1, &check); /* x */
	getnth(check, 4, &check); /* body */
	GetCar(check, &check); /* progn */
	//test(check_evaltype(check, EVAL_PARSE_PROGN), "optparse_flet_error6");

	RETURN;
}



/*
 *  the
 */
static int test_optparse_the1(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(the integer (call))");
	test(checkparse_the1(&opt, pos), "checkparse_the1-1");
	zerospeed(&opt);
	test(! checkparse_the1(&opt, pos), "checkparse_the1-2");
	remspeed(&opt);
	GetEvalParse(pos, 0, &check);
	type_optimize_heap(Local_Thread, &check, check);
	SetEvalParse(pos, 0, check);
	test(! checkparse_the1(&opt, pos), "checkparse_the1-3");

	parse_eval_string(&pos, "(the integer (call))");
	test(optparse_the1(local, &pos, &opt, pos), "optparse_the1-1");
	test(! optparse_the1(local, &pos, &opt, pos), "optparse_the1-2");

	RETURN;
}

static int test_optparse_the2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(the integer (progn))");
	test(checkparse_the2(&opt, pos), "checkparse_the2-1");
	parse_eval_string(&pos, "(the integer (call))");
	test(! checkparse_the2(&opt, pos), "checkparse_the2-2");

	parse_eval_string(&pos, "(the integer (progn))");
	test(optparse_the2(local, &pos, &opt, pos), "optparse_the2-1");
	GetEvalParse(pos, 1, &pos);
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_the2-2");
	parse_eval_string(&pos, "(the integer (call))");
	test(! optparse_the2(local, &pos, &opt, pos), "optparse_the2-3");

	RETURN;
}

static int test_optparse_the(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(the integer (progn))");
	test(checkparse_the(&opt, pos), "checkparse_the-1");

	parse_eval_string(&pos, "(the integer (progn))");
	test(optparse_the(local, &pos, &opt, pos), "optparse_the-1");
	GetEvalParse(pos, 1, &pos);
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_the-2");

	RETURN;
}


/*
 *  eval-when
 */
static int test_optparse_eval_when1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(eval-when (compile))");
	test(checkparse_eval_when1(&opt, pos), "checkparse_eval_when1-1");
	zerospeed(&opt);
	test(! checkparse_eval_when1(&opt, pos), "checkparse_eval_when1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(eval-when (compile) 10)");
	test(! checkparse_eval_when1(&opt, pos), "checkparse_eval_when1-3");

	parse_eval_string(&pos, "(eval-when (compile))");
	test(optparse_eval_when1(local, &pos, &opt, pos), "optparse_eval_when1-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_eval_when1-2");

	RETURN;
}

static int test_optparse_eval_when_all(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(eval-when (compile) (progn (call)) 10)");
	test(checkparse_eval_when_all(&opt, pos), "checkparse_eval_when_all1");
	parse_eval_string(&pos, "(eval-when (compile) 10)");
	test(! checkparse_eval_when_all(&opt, pos), "checkparse_eval_when_all3");

	parse_eval_string(&pos, "(eval-when (compile) (progn (call)) 10)");
	test(optparse_eval_when_all(local, &pos, &opt, pos), "optparse_eval_when_all1");
	test(check_evaltype(pos, EVAL_PARSE_EVAL_WHEN), "optparse_eval_when_all2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_eval_when_all3");
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_eval_when_all4");

	RETURN;
}

static int test_optparse_eval_when(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(eval-when (compile))");
	test(checkparse_eval_when(&opt, pos), "checkparse_eval_when1");
	parse_eval_string(&pos, "(eval-when (compile) (progn (call)) 10)");
	test(checkparse_eval_when(&opt, pos), "checkparse_eval_when1");
	parse_eval_string(&pos, "(eval-when (compile) 10)");
	test(! checkparse_eval_when(&opt, pos), "checkparse_eval_when3");

	parse_eval_string(&pos, "(eval-when (compile) (progn (call)) 10)");
	test(optparse_eval_when(local, &pos, &opt, pos), "optparse_eval_when1");
	test(check_evaltype(pos, EVAL_PARSE_EVAL_WHEN), "optparse_eval_when2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_eval_when3");
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_eval_when4");

	RETURN;
}


/*
 *  values
 */
static int test_optparse_values(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(values (progn))");
	test(checkparse_values(&opt, pos), "checkparse_values1");
	parse_eval_string(&pos, "(values)");
	test(! checkparse_values(&opt, pos), "checkparse_values2");
	parse_eval_string(&pos, "(values 10 20 30)");
	test(! checkparse_values(&opt, pos), "checkparse_values3");

	parse_eval_string(&pos, "(values (progn))");
	test(optparse_values(local, &pos, &opt, pos), "optparse_values1");
	test(check_evaltype(pos, EVAL_PARSE_VALUES), "optparse_values2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 1, "optparse_values3");
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_values4");
	parse_eval_string(&pos, "(values)");
	test(! optparse_values(local, &pos, &opt, pos), "optparse_values5");

	RETURN;
}


/*
 *  locally
 */
static int test_optparse_locally1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(locally (call) 10)");
	test(checkparse_locally1(&opt, pos), "checkparse_locally1-1");
	zerospeed(&opt);
	test(! checkparse_locally1(&opt, pos), "checkparse_locally1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! checkparse_locally1(&opt, pos), "checkparse_locally1-3");

	parse_eval_string(&pos, "(locally (call) 10)");
	test(optparse_locally1(local, &pos, &opt, pos), "optparse_locally1-1");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optparse_locally1-2");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "optparse_locally1-3");
	GetCar(pos, &pos);
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_locally1-4");
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! optparse_locally1(local, &pos, &opt, pos), "optparse_locally1-5");

	RETURN;
}

static int test_optparse_locally2(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(locally (declare (special a)))");
	test(checkparse_locally2(&opt, pos), "checkparse_locally2-1");
	zerospeed(&opt);
	test(! checkparse_locally2(&opt, pos), "checkparse_locally2-2");
	remspeed(&opt);
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! checkparse_locally2(&opt, pos), "checkparse_locally2-3");

	parse_eval_string(&pos, "(locally (declare (special a)))");
	test(optparse_locally2(local, &pos, &opt, pos), "optparse_locally2-1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_locally2-2");
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! optparse_locally2(local, &pos, &opt, pos), "optparse_locally2-3");

	RETURN;
}

static int test_optparse_locally_all(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(locally (declare (special a)) (progn) 10)");
	test(checkparse_locally_all(&opt, pos), "checkparse_locally_all1");
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! checkparse_locally_all(&opt, pos), "checkparse_locally_all2");

	parse_eval_string(&pos, "(locally (declare (special a)) (progn) 10)");
	test(optparse_locally_all(local, &pos, &opt, pos), "optparse_locally_all1");
	test(check_evaltype(pos, EVAL_PARSE_LOCALLY), "optparse_locally_all2");
	GetEvalParse(pos, 1, &pos);
	test(length_list_unsafe(pos) == 1, "optparse_locally_all3");
	GetCar(pos, &pos);
	test(check_evalinteger(pos, 10), "optparse_locally_all4");
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! optparse_locally_all(local, &pos, &opt, pos), "optparse_locally_all5");

	RETURN;
}

static int test_optparse_locally(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(locally)");
	test(checkparse_locally(&opt, pos), "checkparse_locally1");
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! checkparse_locally(&opt, pos), "checkparse_locally2");

	parse_eval_string(&pos, "(locally)");
	test(optparse_locally(local, &pos, &opt, pos), "optparse_locally1");
	test(check_evaltype(pos, EVAL_PARSE_NIL), "optparse_locally2");
	parse_eval_string(&pos, "(locally (declare (special a)) :hello)");
	test(! optparse_locally(local, &pos, &opt, pos), "optparse_locally3");

	RETURN;
}


/*
 *  call
 */
static int test_optparse_call1(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "((lambda () (progn)) 10 20 30)");
	test(checkparse_call1(&opt, pos), "checkparse_call1-1");
	zerospeed(&opt);
	test(! checkparse_call1(&opt, pos), "checkparse_call1-2");
	remspeed(&opt);
	parse_eval_string(&pos, "((lambda () 100) 10 20 30)");
	test(! checkparse_call1(&opt, pos), "checkparse_call1-3");
	parse_eval_string(&pos, "(hello 10 20 30)");
	test(! checkparse_call1(&opt, pos), "checkparse_call1-4");

	parse_eval_string(&pos, "((lambda () (progn)) 10 20 30)");
	test(optparse_call1(local, &pos, &opt, pos), "optparse_call1-1");
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_call1-2");
	GetEvalParse(pos, 0, &pos);
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optparse_call1-3");
	GetEvalParse(pos, 3, &pos); /* body */
	test(pos == Nil, "optparse_call1-4");

	parse_eval_string(&pos, "((lambda () (progn :hello)) 10 20 30)");
	test(optparse_call1(local, &pos, &opt, pos), "optparse_call1-4");
	GetEvalParse(pos, 0, &pos);
	GetEvalParse(pos, 3, &pos); /* body */
	test(length_list_unsafe(pos) == 1, "optparse_call1-5");

	parse_eval_string(&pos, "((lambda () 100) 10 20 30)");
	test(! optparse_call1(local, &pos, &opt, pos), "optparse_call1-6");

	RETURN;
}

static int test_optparse_call_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(call (progn) 20 30)");
	test(checkparse_call_all(&opt, pos), "checkparse_call_all1");
	parse_eval_string(&pos, "((lambda () (progn)) (aa) 10)");
	test(! checkparse_call_all(&opt, pos), "checkparse_call_all2");
	parse_eval_string(&pos, "(call (aa) 10)");
	test(! checkparse_call_all(&opt, pos), "checkparse_call_all3");
	parse_eval_string(&pos, "(call)");
	test(! checkparse_call_all(&opt, pos), "checkparse_call_all4");

	parse_eval_string(&pos, "(call (progn (aaa)) 10)");
	test(optparse_call_all(local, &pos, &opt, pos), "optparse_call_all1");
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optparse_call_all2");
	GetEvalParse(pos, 0, &check);
	test(check_evaltype(check, EVAL_PARSE_FUNCTION), "optparse_call_all3");
	GetEvalParse(pos, 1, &check);
	test(length_list_unsafe(check) == 2, "optparse_call_all4");
	GetCar(check, &check);
	test(check_evaltype(check, EVAL_PARSE_CALL), "optparse_call_all5");
	parse_eval_string(&pos, "(call)");
	test(! optparse_call_all(local, &pos, &opt, pos), "optparse_call_all6");

	RETURN;
}


/*
 *  multiple-value-call
 */
static int test_optparse_multiple_value_call_expr(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(multiple-value-call (lambda () (progn)) 10 20 30)");
	test(checkparse_multiple_value_call_expr(&opt, pos),
			"checkparse_multiple_value_call_expr1");
	zerospeed(&opt);
	test(! checkparse_multiple_value_call_expr(&opt, pos),
			"checkparse_multiple_value_call_expr2");
	remspeed(&opt);
	parse_eval_string(&pos, "(multiple-value-call (lambda () 100) 10 20 30)");
	test(! checkparse_multiple_value_call_expr(&opt, pos),
			"checkparse_multiple_value_call_expr3");
	parse_eval_string(&pos, "(multiple-value-call #'hello 10 20 30)");
	test(! checkparse_multiple_value_call_expr(&opt, pos),
			"checkparse_multiple_value_call_expr4");

	parse_eval_string(&pos, "(multiple-value-call (lambda () (progn)) 10 20 30)");
	test(optparse_multiple_value_call_expr(local, &pos, &opt, pos),
			"optparse_multiple_value_call_expr1");
	test(check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL),
			"optparse_multiple_value_call_expr2");
	GetEvalParse(pos, 0, &pos);
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA),
			"optparse_multiple_value_call_expr3");
	GetEvalParse(pos, 3, &pos); /* body */
	test(pos == Nil, "optparse_multiple_value_call_expr4");

	parse_eval_string(&pos, "(multiple-value-call (lambda () (progn :hello)) 10 20 30)");
	test(optparse_multiple_value_call_expr(local, &pos, &opt, pos),
			"optparse_multiple_value_call_expr4");
	GetEvalParse(pos, 0, &pos);
	GetEvalParse(pos, 3, &pos); /* body */
	test(length_list_unsafe(pos) == 1, "optparse_multiple_value_call_expr5");

	parse_eval_string(&pos, "(multiple-value-call (lambda () 100) 10 20 30)");
	test(! optparse_multiple_value_call_expr(local, &pos, &opt, pos),
			"optparse_multiple_value_call_expr6");

	RETURN;
}

static int test_optparse_multiple_value_call_all(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	parse_eval_string(&pos, "(multiple-value-call #'call (progn) 20 30)");
	test(checkparse_multiple_value_call_all(&opt, pos),
			"checkparse_multiple_value_call_all1");
	parse_eval_string(&pos, "(multiple-value-call (lambda () (progn)) (aa) 10)");
	test(! checkparse_multiple_value_call_all(&opt, pos),
			"checkparse_multiple_value_call_all2");
	parse_eval_string(&pos, "(multiple-value-call #'call (aa) 10)");
	test(! checkparse_multiple_value_call_all(&opt, pos),
			"checkparse_multiple_value_call_all3");
	parse_eval_string(&pos, "(multiple-value-call #'call)");
	test(! checkparse_multiple_value_call_all(&opt, pos),
			"checkparse_multiple_value_call_all4");

	parse_eval_string(&pos, "(multiple-value-call #'call (progn (aaa)) 10)");
	test(optparse_multiple_value_call_all(local, &pos, &opt, pos),
			"optparse_multiple_value_call_all1");
	test(check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL),
			"optparse_multiple_value_call_all2");
	GetEvalParse(pos, 0, &check);
	test(check_evaltype(check, EVAL_PARSE_FUNCTION),
			"optparse_multiple_value_call_all3");
	GetEvalParse(pos, 1, &check);
	test(length_list_unsafe(check) == 2,
			"optparse_multiple_value_call_all4");
	GetCar(check, &check);
	test(check_evaltype(check, EVAL_PARSE_CALL),
			"optparse_multiple_value_call_all5");
	parse_eval_string(&pos, "(multiple-value-call #'call)");
	test(! optparse_multiple_value_call_all(local, &pos, &opt, pos),
			"optparse_multiple_value_call_all6");

	RETURN;
}


/*
 *  all-optimize
 */
static int test_optimize_implicit(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	implicit_string(&pos, "((locally (declare (optimize speed)) (progn 10)) (progn))");
	test(checkparse_implicit(&opt, pos), "optimize_implicit1");
	test(optparse_implicit(local, &pos, &opt, pos), "optimize_implicit2");
	test(length_list_unsafe(pos) == 2, "optimize_implicit3");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_implicit4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_implicit5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_implicit6");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_implicit7");

	RETURN;
}

static int test_optimize_progn(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn (locally (declare (optimize speed)) (progn 10)) (progn))");
	test(checkparse(&opt, pos), "optimize_progn1");
	test(optparse(local, &pos, &opt, pos), "optimize_progn2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_progn3");
	GetEvalParse(pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "optimize_progn4");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_progn5");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_progn6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_progn7");
	GetCons(pos, &check, &pos);
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_progn8");

	RETURN;
}

static int test_optimize_let_init(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(let ((a (locally (declare (optimize speed)) (progn 10))) "
			"      (b (progn))) "
			"  :hello)");
	test(checkparse(&opt, pos), "optimize_let_init1");
	test(optparse(local, &pos, &opt, pos), "optimize_let_init2");
	test(check_evaltype(pos, EVAL_PARSE_LET), "optimize_let_init3");
	GetEvalParse(pos, 0, &pos); /* args */
	test(length_list_unsafe(pos) == 2, "optimize_let_init4");
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check);
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_let_init5");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_let_init6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_let_init7");
	GetCons(pos, &check, &pos); /* b */
	GetCdr(check, &check);
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_let_init8");

	RETURN;
}

static int test_optimize_let_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(let nil "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_let_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_let_body2");
	test(check_evaltype(pos, EVAL_PARSE_LET), "optimize_let_body3");
	GetEvalParse(pos, 2, &pos); /* body */
	test(length_list_unsafe(pos) == 2, "optimize_let_body4");
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_let_body5");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_let_body6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_let_body7");
	GetCons(pos, &check, &pos); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_let_body8");

	RETURN;
}

static int test_optimize_let_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  (let nil (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_let_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_let_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_let_decl3");
	GetEvalParse(pos, 0, &pos); /* cons */
	test(length_list_unsafe(pos) == 2, "optimize_let_decl4");
	GetCons(pos, &check, &pos); /* let */
	test(check_evaltype(check, EVAL_PARSE_LET), "optimize_let_decl5");
	GetEvalParse(check, 2, &check); /* cons */
	test(length_list_unsafe(check) == 1, "optimize_let_decl6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_let_decl7");
	GetCons(pos, &check, &pos); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_let_decl8");

	RETURN;
}

static int test_optimize_leta_init(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(let* ((a (locally (declare (optimize speed)) (progn 10))) "
			"       (b (progn))) "
			"  :hello)");
	test(checkparse(&opt, pos), "optimize_leta_init1");
	test(optparse(local, &pos, &opt, pos), "optimize_leta_init2");
	test(check_evaltype(pos, EVAL_PARSE_LETA), "optimize_leta_init3");
	GetEvalParse(pos, 0, &pos); /* args */
	test(length_list_unsafe(pos) == 2, "optimize_leta_init4");
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check);
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_leta_init5");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_leta_init6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_leta_init7");
	GetCons(pos, &check, &pos); /* b */
	GetCdr(check, &check);
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_leta_init8");

	RETURN;
}

static int test_optimize_leta_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(let* nil "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_leta_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_leta_body2");
	test(check_evaltype(pos, EVAL_PARSE_LETA), "optimize_leta_body3");
	GetEvalParse(pos, 2, &pos); /* body */
	test(length_list_unsafe(pos) == 2, "optimize_leta_body4");
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_leta_body5");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_leta_body6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_leta_body7");
	GetCons(pos, &check, &pos); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_leta_body8");

	RETURN;
}

static int test_optimize_leta_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  (let* nil (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_leta_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_leta_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_leta_decl3");
	GetEvalParse(pos, 0, &pos); /* cons */
	test(length_list_unsafe(pos) == 2, "optimize_leta_decl4");
	GetCons(pos, &check, &pos); /* let* */
	test(check_evaltype(check, EVAL_PARSE_LETA), "optimize_leta_decl5");
	GetEvalParse(check, 2, &check); /* cons */
	test(length_list_unsafe(check) == 1, "optimize_leta_decl6");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_leta_decl7");
	GetCons(pos, &check, &pos); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_leta_decl8");

	RETURN;
}

static int test_optimize_setq(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(setq a (locally (declare (optimize speed)) (progn 10)) b (progn))");
	test(checkparse(&opt, pos), "optimize_setq1");
	test(optparse(local, &pos, &opt, pos), "optimize_setq2");
	test(check_evaltype(pos, EVAL_PARSE_SETQ), "optimize_setq3");
	GetEvalParse(pos, 0, &pos);
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check);
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_setq4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_setq5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_setq6");
	GetCar(pos, &check); /* b */
	GetCdr(check, &check);
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_setq7");

	RETURN;
}

static int test_optimize_defun_args(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(defun name (&optional "
			"    (a (locally (declare (optimize speed)) (progn 10))) "
			"    (b (progn))) "
			"  :hello)");
	test(checkparse(&opt, pos), "optimize_defun_args1");
	test(optparse(local, &pos, &opt, pos), "optimize_defun_args2");
	test(check_evaltype(pos, EVAL_PARSE_DEFUN), "optimize_defun_args3");
	GetEvalParse(pos, 1, &pos); /* args */
	getnth(pos, 1, &pos); /* &optional */
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_defun_args4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_defun_args5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_defun_args6");
	GetCar(pos, &check); /* b */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_defun_args7");

	RETURN;
}

static int test_optimize_defun_body(void)
{
	addr pos;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(defun name () "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_defun_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_defun_body2");
	test(check_evaltype(pos, EVAL_PARSE_DEFUN), "optimize_defun_body3");
	//GetEvalParse(pos, 4, &pos); /* body */
	//GetCons(pos, &check, &pos); /* locally */
	//test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_defun_body4");
	//GetEvalParse(check, 1, &check);
	//test(length_list_unsafe(check) == 1, "optimize_defun_body5");
	//GetCar(check, &check);
	//test(check_evalinteger(check, 10), "optimize_defun_body6");
	//GetCar(pos, &check); /* progn */
	//test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_defun_body7");

	RETURN;
}

static int test_optimize_defun_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  (defun name () "
			"    (declare (optimize speed)) "
			"    (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_defun_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_defun_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_defun_decl3");
	GetEvalParse(pos, 0, &pos); /* cons */
	GetCons(pos, &check, &pos); /* defun */
	test(check_evaltype(check, EVAL_PARSE_DEFUN), "optimize_defun_decl4");
	GetEvalParse(check, 4, &check); /* body */
	test(length_list_unsafe(check) == 1, "optimize_defun_decl5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_defun_decl6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_defun_decl7");

	RETURN;
}

static int test_optimize_lambda_args(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"#'(lambda (&optional "
			"    (a (locally (declare (optimize speed)) (progn 10))) "
			"    (b (progn))) "
			"  :hello)");
	test(checkparse(&opt, pos), "optimize_lambda_args1");
	test(optparse(local, &pos, &opt, pos), "optimize_lambda_args2");
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optimize_lambda_args3");
	GetEvalParse(pos, 0, &pos); /* args */
	getnth(pos, 1, &pos); /* &optional */
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_lambda_args4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_lambda_args5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_lambda_args6");
	GetCar(pos, &check); /* b */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_lambda_args7");

	RETURN;
}

static int test_optimize_lambda_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"#'(lambda () "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_lambda_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_lambda_body2");
	test(check_evaltype(pos, EVAL_PARSE_LAMBDA), "optimize_lambda_body3");
	GetEvalParse(pos, 3, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_lambda_body4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_lambda_body5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_lambda_body6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_lambda_body7");

	RETURN;
}

static int test_optimize_lambda_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  #'(lambda () "
			"    (declare (optimize speed)) "
			"    (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_lambda_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_lambda_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_lambda_decl3");
	GetEvalParse(pos, 0, &pos); /* cons */
	GetCons(pos, &check, &pos); /* lambda */
	test(check_evaltype(check, EVAL_PARSE_LAMBDA), "optimize_lambda_decl4");
	GetEvalParse(check, 3, &check); /* body */
	test(length_list_unsafe(check) == 1, "optimize_lambda_decl5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_lambda_decl6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_lambda_decl7");

	RETURN;
}

static int test_optimize_if(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(if (call) "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_if1");
	test(optparse(local, &pos, &opt, pos), "optimize_if2");
	test(check_evaltype(pos, EVAL_PARSE_IF), "optimize_if3");
	GetEvalParse(pos, 1, &check); /* then */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_if4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_if5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_if6");
	GetEvalParse(pos, 2, &check); /* else */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_if7");

	RETURN;
}

static int test_optimize_unwind_protect(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(unwind-protect (call) "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_unwind_protect1");
	test(optparse(local, &pos, &opt, pos), "optimize_unwind_protect2");
	test(check_evaltype(pos, EVAL_PARSE_UNWIND_PROTECT), "optimize_unwind_protect3");
	GetEvalParse(pos, 1, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_unwind_protect4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_unwind_protect5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_unwind_protect6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_unwind_protect7");

	RETURN;
}

static int test_optimize_tagbody(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(tagbody "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_tagbody1");
	test(optparse(local, &pos, &opt, pos), "optimize_tagbody2");
	test(check_evaltype(pos, EVAL_PARSE_TAGBODY), "optimize_tagbody3");
	GetEvalParse(pos, 1, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_tagbody4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_tagbody5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_tagbody6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_tagbody7");

	RETURN;
}

static int test_optimize_block(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(block name "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_block1");
	test(optparse(local, &pos, &opt, pos), "optimize_block2");
	test(check_evaltype(pos, EVAL_PARSE_BLOCK), "optimize_block3");
	GetEvalParse(pos, 1, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_block4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_block5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_block6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_block7");

	RETURN;
}

static int test_optimize_return_from(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(return-from name "
			"  (locally (declare (optimize speed)) (progn 10)))");
	test(checkparse(&opt, pos), "optimize_return_from1");
	test(optparse(local, &pos, &opt, pos), "optimize_return_from2");
	test(check_evaltype(pos, EVAL_PARSE_RETURN_FROM), "optimize_return_from3");
	GetEvalParse(pos, 1, &check); /* expr */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_return_from4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_return_from5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_return_from6");

	RETURN;
}

static int test_optimize_catch(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(catch name "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_catch1");
	test(optparse(local, &pos, &opt, pos), "optimize_catch2");
	test(check_evaltype(pos, EVAL_PARSE_CATCH), "optimize_catch3");
	GetEvalParse(pos, 1, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_catch4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_catch5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_catch6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_catch7");

	RETURN;
}

static int test_optimize_throw(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(throw name "
			"  (locally (declare (optimize speed)) (progn 10)))");
	test(checkparse(&opt, pos), "optimize_throw1");
	test(optparse(local, &pos, &opt, pos), "optimize_throw2");
	test(check_evaltype(pos, EVAL_PARSE_THROW), "optimize_throw3");
	GetEvalParse(pos, 1, &check); /* expr */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_throw4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_throw5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_throw6");

	RETURN;
}

static int test_optimize_flet_args_init(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(flet ((z (&optional "
			"    (a (locally (declare (optimize speed)) (progn 10))) "
			"    (b (progn))) "
			"    :hello)) "
			"  :body)");
	test(checkparse(&opt, pos), "optimize_flet_args_init1");
	test(optparse(local, &pos, &opt, pos), "optimize_flet_args_init2");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optimize_flet_args_init3");
	GetEvalParse(pos, 0, &pos); /* flet-args */
	GetCar(pos, &pos); /* (z args ...) */
	getnth(pos, 1, &pos); /* args */
	getnth(pos, 1, &pos); /* &optional */
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_flet_args_init4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_flet_args_init5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_flet_args_init6");
	GetCar(pos, &check); /* b */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_flet_args_init7");

	RETURN;
}

static int test_optimize_flet_args_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(flet ((z () "
			"    (locally (declare (optimize speed)) (progn 10)) "
			"    (progn))) "
			"  :body)");
	test(checkparse(&opt, pos), "optimize_flet_args_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_flet_args_body2");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optimize_flet_args_body3");
	GetEvalParse(pos, 0, &pos); /* flet-args */
	getnth(pos, 0, &pos); /* z */
	getnth(pos, 4, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	//test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_flet_args_body4");
	//GetEvalParse(check, 1, &check);
	//test(length_list_unsafe(check) == 1, "optimize_flet_args_body5");
	//GetCar(check, &check);
	//test(check_evalinteger(check, 10), "optimize_flet_args_body6");
	//GetCar(pos, &check); /* progn */
	//test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_flet_args_body7");

	RETURN;
}

static int test_optimize_flet_args_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(flet ((z () (declare (optimize speed)) (progn 10)) "
			"       (x () (progn))) "
			"  :body)");
	test(checkparse(&opt, pos), "optimize_flet_args_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_flet_args_decl2");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optimize_flet_args_decl3");
	GetEvalParse(pos, 0, &pos); /* flet-args */
	getnth(pos, 0, &check); /* z */
	getnth(check, 4, &check); /* body */
	test(length_list_unsafe(check) == 1, "optimize_flet_args_decl4");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_flet_args_decl5");
	getnth(pos, 1, &check); /* x */
	getnth(check, 4, &check); /* body */
	GetCar(check, &check); /* progn */
	//test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_flet_args_decl6");

	RETURN;
}

static int test_optimize_flet_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(flet () "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_flet_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_flet_body2");
	test(check_evaltype(pos, EVAL_PARSE_FLET), "optimize_flet_body3");
	GetEvalParse(pos, 2, &pos); /* flet-body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_flet_body4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_flet_body5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_flet_body6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_flet_body7");

	RETURN;
}

static int test_optimize_flet_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  (flet () (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_flet_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_flet_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_flet_decl3");
	GetEvalParse(pos, 0, &pos); /* cons */
	GetCons(pos, &check, &pos); /* flet */
	test(check_evaltype(check, EVAL_PARSE_FLET), "optimize_flet_decl4");
	GetEvalParse(check, 2, &check); /* flet-body */
	test(length_list_unsafe(check) == 1, "optimize_flet_decl5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_flet_decl6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_flet_decl7");

	RETURN;
}

static int test_optimize_labels_args_init(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(labels ((z (&optional "
			"    (a (locally (declare (optimize speed)) (progn 10))) "
			"    (b (progn))) "
			"    :hello)) "
			"  :body)");
	test(checkparse(&opt, pos), "optimize_labels_args_init1");
	test(optparse(local, &pos, &opt, pos), "optimize_labels_args_init2");
	test(check_evaltype(pos, EVAL_PARSE_LABELS), "optimize_labels_args_init3");
	GetEvalParse(pos, 0, &pos); /* labels-args */
	GetCar(pos, &pos); /* (z args ...) */
	getnth(pos, 1, &pos); /* args */
	getnth(pos, 1, &pos); /* &optional */
	GetCons(pos, &check, &pos); /* a */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_labels_args_init4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_labels_args_init5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_labels_args_init6");
	GetCar(pos, &check); /* b */
	GetCdr(check, &check); /* var */
	GetCar(check, &check); /* init */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_labels_args_init7");

	RETURN;
}

static int test_optimize_labels_args_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(labels ((z () "
			"    (locally (declare (optimize speed)) (progn 10)) "
			"    (progn))) "
			"  :body)");
	test(checkparse(&opt, pos), "optimize_labels_args_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_labels_args_body2");
	test(check_evaltype(pos, EVAL_PARSE_LABELS), "optimize_labels_args_body3");
	GetEvalParse(pos, 0, &pos); /* labels-args */
	getnth(pos, 0, &pos); /* z */
	getnth(pos, 4, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	//test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_labels_args_body4");
	//GetEvalParse(check, 1, &check);
	//test(length_list_unsafe(check) == 1, "optimize_labels_args_body5");
	//GetCar(check, &check);
	//test(check_evalinteger(check, 10), "optimize_labels_args_body6");
	//GetCar(pos, &check); /* progn */
	//test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_labels_args_body7");

	RETURN;
}

static int test_optimize_labels_args_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(labels ((z () (declare (optimize speed)) (progn 10)) "
			"       (x () (progn))) "
			"  :body)");
	test(checkparse(&opt, pos), "optimize_labels_args_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_labels_args_decl2");
	test(check_evaltype(pos, EVAL_PARSE_LABELS), "optimize_labels_args_decl3");
	GetEvalParse(pos, 0, &pos); /* labels-args */
	getnth(pos, 0, &check); /* z */
	getnth(check, 4, &check); /* body */
	test(length_list_unsafe(check) == 1, "optimize_labels_args_decl4");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_labels_args_decl5");
	getnth(pos, 1, &check); /* x */
	getnth(check, 4, &check); /* body */
	GetCar(check, &check); /* progn */
	//test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_labels_args_decl6");

	RETURN;
}

static int test_optimize_labels_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(labels () "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_labels_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_labels_body2");
	test(check_evaltype(pos, EVAL_PARSE_LABELS), "optimize_labels_body3");
	GetEvalParse(pos, 2, &pos); /* labels-body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_labels_body4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_labels_body5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_labels_body6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_labels_body7");

	RETURN;
}

static int test_optimize_labels_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  (labels () (declare (optimize speed)) (progn 10)) "
			"  (progn))");
	test(checkparse(&opt, pos), "optimize_labels_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_labels_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_labels_decl3");
	GetEvalParse(pos, 0, &pos); /* cons */
	GetCons(pos, &check, &pos); /* labels */
	test(check_evaltype(check, EVAL_PARSE_LABELS), "optimize_labels_decl4");
	GetEvalParse(check, 2, &check); /* labels-body */
	test(length_list_unsafe(check) == 1, "optimize_labels_decl5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_labels_decl6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_labels_decl7");

	RETURN;
}

static int test_optimize_the(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(the integer "
			"  (locally (declare (optimize speed)) (progn 10)))");
	test(checkparse(&opt, pos), "optimize_the1");
	test(optparse(local, &pos, &opt, pos), "optimize_the2");
	test(check_evaltype(pos, EVAL_PARSE_THE), "optimize_the3");
	GetEvalParse(pos, 1, &check); /* expr */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_the4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_the5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_the6");

	RETURN;
}

static int test_optimize_eval_when(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(eval-when (load) "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_eval_when1");
	test(optparse(local, &pos, &opt, pos), "optimize_eval_when2");
	test(check_evaltype(pos, EVAL_PARSE_EVAL_WHEN), "optimize_eval_when3");
	GetEvalParse(pos, 0, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_eval_when4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_eval_when5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_eval_when6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_eval_when7");

	RETURN;
}

static int test_optimize_values(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(values "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_values1");
	test(optparse(local, &pos, &opt, pos), "optimize_values2");
	test(check_evaltype(pos, EVAL_PARSE_VALUES), "optimize_values3");
	GetEvalParse(pos, 0, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_values4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_values5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_values6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_values7");

	RETURN;
}

static int test_optimize_locally_body(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(locally "
			"  (let nil (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_locally_body1");
	test(optparse(local, &pos, &opt, pos), "optimize_locally_body2");
	test(check_evaltype(pos, EVAL_PARSE_LOCALLY), "optimize_locally_body3");
	GetEvalParse(pos, 1, &pos); /* body */
	GetCons(pos, &check, &pos); /* let */
	test(check_evaltype(check, EVAL_PARSE_LET), "optimize_locally_body4");
	GetEvalParse(check, 2, &check);
	test(length_list_unsafe(check) == 1, "optimize_locally_body5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_locally_body6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_locally_body7");

	RETURN;
}

static int test_optimize_locally_decl(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(progn "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_locally_decl1");
	test(optparse(local, &pos, &opt, pos), "optimize_locally_decl2");
	test(check_evaltype(pos, EVAL_PARSE_PROGN), "optimize_locally_decl3");
	GetEvalParse(pos, 0, &pos); /* body */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_locally_decl4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_locally_decl5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_locally_decl6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_locally_decl7");

	RETURN;
}

static int test_optimize_multiple_value_call(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(multiple-value-call #'call "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_multiple_value_call1");
	test(optparse(local, &pos, &opt, pos), "optimize_multiple_value_call2");
	test(check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL),
			"optimize_multiple_value_call3");
	GetEvalParse(pos, 1, &pos); /* args */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_multiple_value_call4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_multiple_value_call5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_multiple_value_call6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_multiple_value_call7");

	RETURN;
}

static int test_optimize_call(void)
{
	addr pos, check;
	struct optstruct opt;
	LocalRoot local = Local_Thread;

	allminus_optstruct(&opt);
	zerospeed(&opt);
	parse_eval_string(&pos,
			"(call "
			"  (locally (declare (optimize speed)) (progn 10)) "
			"  (progn)) ");
	test(checkparse(&opt, pos), "optimize_call1");
	test(optparse(local, &pos, &opt, pos), "optimize_call2");
	test(check_evaltype(pos, EVAL_PARSE_CALL), "optimize_call3");
	GetEvalParse(pos, 1, &pos); /* args */
	GetCons(pos, &check, &pos); /* locally */
	test(check_evaltype(check, EVAL_PARSE_LOCALLY), "optimize_call4");
	GetEvalParse(check, 1, &check);
	test(length_list_unsafe(check) == 1, "optimize_call5");
	GetCar(check, &check);
	test(check_evalinteger(check, 10), "optimize_call6");
	GetCar(pos, &check); /* progn */
	test(check_evaltype(check, EVAL_PARSE_PROGN), "optimize_call7");

	RETURN;
}
#endif


/*
 *  Main
 */
static int testbreak_optimize(void)
{
#if 0
	/* optstruct */
	TestBreak(test_initialize_optstruct);
	TestBreak(test_save_optstruct);
	TestBreak(test_optimize_value);
	TestBreak(test_speed_on);
	/* optparse */
	TestBreak(test_check_evaltype);
	TestBreak(test_check_evaltype_on);
	TestBreak(test_check_listtype_on);
	TestBreak(test_checkvalue);
	/* implicit */
	TestBreak(test_optparse_implicit3);
	TestBreak(test_optparse_implicit4);
	TestBreak(test_optparse_implicit5);
	TestBreak(test_optparse_implicit6);
	TestBreak(test_optparse_implicit_all);
	/* progn */
	TestBreak(test_optparse_progn1);
	TestBreak(test_optparse_progn2);
	TestBreak(test_optparse_progn3);
	TestBreak(test_optparse_progn4);
	TestBreak(test_optparse_progn5);
	TestBreak(test_optparse_progn6);
	TestBreak(test_optparse_progn_all);
	TestBreak(test_optparse_progn);
	/* let */
	TestBreak(test_check_lettype);
	TestBreak(test_optparse_let1);
	TestBreak(test_optparse_let2);
	TestBreak(test_optparse_let3);
	TestBreak(test_optparse_let4);
	TestBreak(test_optparse_let_args);
	TestBreak(test_optparse_let_body);
	TestBreak(test_optparse_let);
	/* setq */
	TestBreak(test_optparse_setq1);
	TestBreak(test_optparse_setq_all);
	TestBreak(test_optparse_setq);
	/* lambda-ordinary */
	TestBreak(test_optparse_opt);
	TestBreak(test_optparse_key);
	TestBreak(test_optparse_aux);
	TestBreak(test_optparse_lambda_ordinary);
	/* defun */
	TestBreak(test_optparse_defun_args);
	TestBreak(test_optparse_defun_body);
	TestBreak(test_optparse_defun);
	/* lambda */
	TestBreak(test_optparse_lambda_args);
	TestBreak(test_optparse_lambda_body);
	TestBreak(test_optparse_lambda);
	/* if */
	TestBreak(test_optparse_if1);
	TestBreak(test_optparse_if2);
	TestBreak(test_optparse_if_all);
	TestBreak(test_optparse_if);
	/* unwind-protect */
	TestBreak(test_optparse_unwind_protect1);
	TestBreak(test_optparse_unwind_protect2);
	TestBreak(test_optparse_unwind_protect_all);
	TestBreak(test_optparse_unwind_protect);
	/* tagbody */
	TestBreak(test_optparse_tagbody1);
	TestBreak(test_optparse_tagbody2);
	TestBreak(test_optparse_tagbody_all);
	TestBreak(test_optparse_tagbody);
	/* block */
	TestBreak(test_optparse_block1);
	TestBreak(test_optparse_block2);
	TestBreak(test_optparse_block_all);
	TestBreak(test_optparse_return_from);
	TestBreak(test_optparse_block);
	/* catch */
	TestBreak(test_optparse_catch1);
	TestBreak(test_optparse_catch2);
	TestBreak(test_optparse_catch_all);
	TestBreak(test_optparse_throw);
	TestBreak(test_optparse_catch);
	/* flet / labels */
	TestBreak(test_check_fletlabels);
	TestBreak(test_check_fletlabels_on);
	TestBreak(test_optparse_flet1);
	TestBreak(test_optparse_flet2);
	TestBreak(test_optparse_flet3);
	TestBreak(test_optparse_flet4);
	TestBreak(test_checkparse_flet_args);
	TestBreak(test_optparse_flet_args);
	TestBreak(test_optparse_flet_body);
	TestBreak(test_optparse_flet);
	TestBreak(test_optparse_flet_error);
	/* the */
	TestBreak(test_optparse_the1);
	TestBreak(test_optparse_the2);
	TestBreak(test_optparse_the);
	/* eval-when */
	TestBreak(test_optparse_eval_when1);
	TestBreak(test_optparse_eval_when_all);
	TestBreak(test_optparse_eval_when);
	/* values */
	TestBreak(test_optparse_values);
	/* locally */
	TestBreak(test_optparse_locally1);
	TestBreak(test_optparse_locally2);
	TestBreak(test_optparse_locally_all);
	TestBreak(test_optparse_locally);
	/* call */
	TestBreak(test_optparse_call1);
	TestBreak(test_optparse_call_all);
	/* multiple-value-call */
	TestBreak(test_optparse_multiple_value_call_expr);
	TestBreak(test_optparse_multiple_value_call_all);
	/* all-optimize */
	TestBreak(test_optimize_implicit);
	TestBreak(test_optimize_progn);
	TestBreak(test_optimize_let_init);
	TestBreak(test_optimize_let_body);
	TestBreak(test_optimize_let_decl);
	TestBreak(test_optimize_leta_init);
	TestBreak(test_optimize_leta_body);
	TestBreak(test_optimize_leta_decl);
	TestBreak(test_optimize_setq);
	TestBreak(test_optimize_defun_args);
	TestBreak(test_optimize_defun_body);
	TestBreak(test_optimize_defun_decl);
	TestBreak(test_optimize_lambda_args);
	TestBreak(test_optimize_lambda_body);
	TestBreak(test_optimize_lambda_decl);
	TestBreak(test_optimize_if);
	TestBreak(test_optimize_unwind_protect);
	TestBreak(test_optimize_tagbody);
	TestBreak(test_optimize_block);
	TestBreak(test_optimize_return_from);
	TestBreak(test_optimize_catch);
	TestBreak(test_optimize_throw);
	TestBreak(test_optimize_flet_args_init);
	TestBreak(test_optimize_flet_args_body);
	TestBreak(test_optimize_flet_args_decl);
	TestBreak(test_optimize_flet_body);
	TestBreak(test_optimize_flet_decl);
	TestBreak(test_optimize_labels_args_init);
	TestBreak(test_optimize_labels_args_body);
	TestBreak(test_optimize_labels_args_decl);
	TestBreak(test_optimize_labels_body);
	TestBreak(test_optimize_labels_decl);
	TestBreak(test_optimize_the);
	TestBreak(test_optimize_eval_when);
	TestBreak(test_optimize_values);
	TestBreak(test_optimize_locally_body);
	TestBreak(test_optimize_locally_decl);
	TestBreak(test_optimize_call);
	TestBreak(test_optimize_multiple_value_call);
#endif

	return 0;
}

int test_optimize(void)
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
		build_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_optimize();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

