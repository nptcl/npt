#include "callname.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "eval_copy.h"
#include "eval_object.h"
#include "function.h"
#include "optimize.h"
#include "optimize_parse.h"
#include "parse.h"
#include "parse_object.h"
#include "strtype.h"
#include "strvect.h"
#include "subtypep_optimize.h"
#include "type_error.h"

static int checkparse_all_(OptimizeInfo *str, int *ret);
static int optparse_all_(OptimizeInfo *str, int *ret);

#define Return_or_optparse(call, str, ret) { \
	Return(call(str, ret)); \
	if (*ret) { \
		return 0;\
	} \
}
#define Return_check_optparse(call, str, ret) { \
	Return(call(str, ret)); \
	if (*ret == 0) { \
		return 0; \
	} \
};

static int checkparse_inplace_(OptimizeInfo *str, addr pos, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	str->pos = pos;
	Return(checkparse_all_(str, &check));
	*str = save;

	return Result(ret, check);
}

static int optparse_inplace_(OptimizeInfo *str, addr pos, addr *value, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	str->pos = pos;
	Return(optparse_all_(str, &check));
	*value = check? str->pos: pos;
	*str = save;

	return Result(ret, check);
}


/*
 *  optimize-check
 */
/* (lisp-system::optimize-check parse) -> 0 / 1 */
static int checkparse_optimize_check_(OptimizeInfo *str, addr *value, int *ret)
{
	int check;
	addr call, left, right;

	/* call */
	call = str->pos;
	if (! optimize_evaltype(call, EVAL_PARSE_CALL))
		goto skip;
	GetEvalParse(call, 0, &left);
	/* function */
	if (! optimize_evaltype(left, EVAL_PARSE_FUNCTION))
		goto skip;
	/* function name */
	GetEvalParse(left, 0, &left);
	GetCallName(left, &left);
	GetConst(SYSTEM_OPTIMIZE_CHECK, &right);
	if (left != right)
		goto skip;
	/* argument */
	GetEvalParse(call, 1, &right);
	if (! consp_getcons(right, &left, &right))
		goto skip;
	/* PARSE */
	if (! optimize_evaltype(left, EVAL_PARSE_SYMBOL))
		goto skip;
	GetEvalParse(left, 0, &left);
	Return(string_designer_equalp_char_(left, "PARSE", &check));
	if (! check)
		goto skip;
	/* result */
	*value = right;
	return Result(ret, 1);

skip:
	return Result(ret, 0);
}

static int checkparse_check1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list;

	Return(checkparse_optimize_check_(str, &list, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, list == Nil);
}

static int optparse_check1_(OptimizeInfo *str, int *ret)
{
	addr x;

	Return_check_optparse(checkparse_check1_, str, ret);
	fixnum_heap(&x, optimize_speed_on(str)? 1: 0);
	eval_single_parse_local(str->local, &str->pos, EVAL_PARSE_INTEGER, x);

	return Result(ret, 1);
}

/* (lisp-system::optimize-check parse list) -> (...) */
static int checkparse_check2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list, pos;

	Return(checkparse_optimize_check_(str, &list, &check));
	if (! check)
		return Result(ret, 0);
	if (list == Nil)
		return Result(ret, 0);
	if (! consp_getcons(list, &pos, &list))
		return Result(ret, 0);
	if (list != Nil)
		return Result(ret, 0);
	if (! optimize_evaltype(pos, EVAL_PARSE_SYMBOL))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos);
	return string_designer_equalp_char_(pos, "LIST", ret);
}

static int optparse_check2_(OptimizeInfo *str, int *ret)
{
	addr symbol, x, list;

	Return_check_optparse(checkparse_check2_, str, ret);
	list = Nil;
	/* compilation-speed */
	GetConst(COMMON_COMPILATION_SPEED, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_COMPILATION));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* debug */
	GetConst(COMMON_DEBUG, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_DEBUG));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* safety */
	GetConst(COMMON_SAFETY, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SAFETY));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* space */
	GetConst(COMMON_SPACE, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SPACE));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* speed */
	GetConst(COMMON_SPEED, &symbol);
	fixnum_heap(&x, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SPEED));
	cons_heap(&x, symbol, x);
	cons_heap(&list, x, list);
	/* quote */
	eval_single_parse_heap(&list, EVAL_PARSE_QUOTE, list);
	str->pos = list;

	return Result(ret, 1);
}

/* optparse-check */
static int checkparse_check_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_check1_, str, ret);
	Return_or_optparse(checkparse_check2_, str, ret);

	return Result(ret, 0);
}

static int optparse_run_(OptimizeInfo *str, int *ret, int (*call)(OptimizeInfo *))
{
	int update, result;
	addr pos;

	update = str->update;
	pos = str->pos;
	for (result = 0; ; result |= str->update) {
		str->update = 0;
		Return((*call)(str));

		if (str->update == 0)
			break;
	}

	if (result) {
		str->update = 1;
		return Result(ret, 1);
	}
	else {
		str->pos = pos;
		str->update = update;
		return Result(ret, 0);
	}
}

static int optparse_check_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_check1_));
	Return(optimize_extract_(str, optparse_check2_));

	return 0;
}
static int optparse_check_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_check_run_);
}


/*
 *  optimize_value
 */
static int optimize_value_function(addr pos, int functionp);
static int optimize_value_values(addr list, int functionp)
{
	addr pos;

	GetEvalParse(list, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! optimize_value_function(pos, functionp))
			return 0;
	}

	return 1;
}

static int optmize_value_the(addr pos, int functionp)
{
	GetEvalParse(pos, 1, &pos); /* expr */
	return optimize_value_function(pos, functionp);
}

static int optimize_value_function(addr pos, int functionp)
{
	if (! eval_parse_p(pos))
		return 0;
	switch (RefEvalParseType(pos)) {
		case EVAL_PARSE_NIL:
		case EVAL_PARSE_T:
		case EVAL_PARSE_CLOS:
		case EVAL_PARSE_INTEGER:
		case EVAL_PARSE_RATIONAL:
		case EVAL_PARSE_COMPLEX:
		case EVAL_PARSE_CHARACTER:
		case EVAL_PARSE_ARRAY:
		case EVAL_PARSE_VECTOR:
		case EVAL_PARSE_BITVECTOR:
		case EVAL_PARSE_STRING:
		case EVAL_PARSE_FLOAT:
		case EVAL_PARSE_PATHNAME:
		case EVAL_PARSE_QUOTE:
		case EVAL_PARSE_LAMBDA:
			return 1;

		case EVAL_PARSE_FUNCTION:
			return functionp;

		case EVAL_PARSE_THE:
			return optmize_value_the(pos, functionp);

		case EVAL_PARSE_VALUES:
			return optimize_value_values(pos, functionp);

		default:
			return 0;
	}
}

static int optimize_value(addr pos)
{
	return optimize_value_function(pos, 1);
}


/*
 *  implicit
 */
#define Return_or_checkparse_implicit(call, str, pos, ret) { \
	Return(call(str, pos, ret)); \
	if (*ret) { \
		return 0;  \
	} \
}

#define Return_check_checkparse_implicit(call, str, pos, ret) { \
	Return(call(str, pos, ret)); \
	if (*ret == 0) { \
		return 0;  \
	} \
}

static int optimize_lisptype_on(OptimizeInfo *str, addr pos, enum LISPTYPE type)
{
	return optimize_speed_on(str) && GetType(pos) == type;
}

/* (10 20 30) -> (30) */
static int checkparse_implicit3_(OptimizeInfo *str, addr list, int *ret)
{
	addr value;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	GetCdr(list, &value);
	if (value == Nil)
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &value, &list);
		if (! optimize_value(value))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_implicit3_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr x;

	Return_check_checkparse_implicit(checkparse_implicit3_, str, list, ret);
	if (list == Nil)
		return Result(ret, 0);
	for (x = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
	}
	conscar_local(str->local, value, x);

	return Result(ret, 1);
}

/* (10 (call1) 20 30 (call2)) -> ((call1) (call2)) */
static int checkparse_implicit4_(OptimizeInfo *str, addr list, int *ret)
{
	int update1, update2, valuep;
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	update1 = update2 = 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		valuep = optimize_value(check);
		if (list == Nil && ! valuep)
			update1 = 1;
		if (list != Nil && valuep)
			update2 = 1;
	}

	return Result(ret, update1 && update2);
}

static int optparse_implicit4_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr root, check;
	LocalRoot local;

	Return_check_checkparse_implicit(checkparse_implicit4_, str, list, ret);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &check, &list);
		if (! optimize_value(check))
			cons_local(local, &root, check, root);
	}
	nreverse(value, root);

	return Result(ret, 1);
}

/* (10 (call1) 20 (call2) 30 40) -> ((call1) (call2) 40) */
static int checkparse_implicit5_(OptimizeInfo *str, addr list, int *ret)
{
	int update1, update2, update3, valuep;
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	update1 = update2 = update3 = 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		valuep = optimize_value(check);
		if (list == Nil && valuep)
			update1 = 1;
		if (list != Nil && valuep)
			update2 = 1;
		if (list != Nil && ! valuep)
			update3 = 1;
	}

	return Result(ret, update1 && update2 && update3);
}

static int optparse_implicit5_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr root, pos;
	LocalRoot local;

	Return_check_checkparse_implicit(checkparse_implicit5_, str, list, ret);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (list == Nil || ! optimize_value(pos))
			cons_local(local, &root, pos, root);
	}
	nreverse(value, root);

	return Result(ret, 1);
}

/* (x (progn y) z) -> (x y z) */
static int checkparse_implicit6_(OptimizeInfo *str, addr list, int *ret)
{
	addr pos;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (optimize_evaltype(pos, EVAL_PARSE_PROGN))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static void optparse_implicit6_next(LocalRoot local, addr list, addr *value)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (optimize_evaltype(pos, EVAL_PARSE_PROGN)) {
			GetEvalParse(pos, 0, &pos);
			optparse_implicit6_next(local, pos, value);
			continue;
		}
		cons_local(local, value, pos, *value);
	}
}

static int optparse_implicit6_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	addr root;

	Return_check_checkparse_implicit(checkparse_implicit6_, str, list, ret);
	root = Nil;
	optparse_implicit6_next(str->local, list, &root);
	nreverse(value, root);

	return Result(ret, 1);
}

/* (...) */
static int checkparse_implicit_all_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	/* Don't check optimize. */
	if (! consp(list))
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(checkparse_inplace_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_implicit_all_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, pos;
	LocalRoot local;

	Return_check_checkparse_implicit(checkparse_implicit_all_, str, list, ret);
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(optparse_inplace_(str, pos, &pos, &check));
		update |= check;
		cons_local(local, &root, pos, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* implicit */
static int checkparse_implicit_(OptimizeInfo *str, addr pos, int *ret)
{
	Return_or_checkparse_implicit(checkparse_implicit3_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit4_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit5_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit6_, str, pos, ret);
	Return_or_checkparse_implicit(checkparse_implicit_all_, str, pos, ret);

	return Result(ret, 0);
}

#define Return_or_optparse_implicit(call, str, var, value, check) { \
	Return(call(str, var, &var, ret)); \
	if (*ret) { \
		*value = var; \
		return 0;  \
	} \
}
static int optparse_implicit_call_(OptimizeInfo *str, addr var, addr *value, int *ret)
{
	Return_or_optparse_implicit(optparse_implicit3_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit4_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit5_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit6_, str, var, value, check);
	Return_or_optparse_implicit(optparse_implicit_all_, str, var, value, check);

	return Result(ret, 0);
}

static int optparse_implicit_(OptimizeInfo *str, addr pos, addr *value, int *ret)
{
	int check, result;
	addr var;

	Return(checkparse_implicit_(str, pos, &check));
	if (! check) {
		*value = pos;
		return Result(ret, 0);
	}

	result = 0;
	var = pos;
	for (;;) {
		Return(optparse_implicit_call_(str, var, &var, &check));
		if (! check)
			break;
		result = 1;
	}
	if (result) {
		*value = var;
		return Result(ret, 1);
	}
	else {
		*value = pos;
		return Result(ret, 0);
	}
}


/*
 *  progn
 */
/* (progn) -> nil */
static int checkparse_progn1_(OptimizeInfo *str, int *ret)
{
	*ret = optimize_evaltype_on(str, EVAL_PARSE_PROGN)
		&& RefEvalParse(str->pos, 0) == Nil;
	return 0;
}

static int optparse_progn1_(OptimizeInfo *str, int *ret)
{
	Return_check_optparse(checkparse_progn1_, str, ret);
	eval_single_parse_local(str->local, &str->pos, EVAL_PARSE_NIL, Nil);
	return Result(ret, 1);
}

/* (progn x) -> x */
static int checkparse_progn2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);

	return Result(ret, singlep(pos));
}

static int optparse_progn2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_progn2_, str, ret);
	GetEvalParse(str->pos, 0, &pos);
	GetCar(pos, &(str->pos));

	return Result(ret, 1);
}

/* (progn 10 20 30) -> 30 */
static int checkparse_progn3_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit3_(str, pos, ret);
}

static int optparse_progn3_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	Return_check_optparse(checkparse_progn3_, str, ret);
	GetEvalParse(str->pos, 0, &pos);
	Return(optparse_implicit3_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	GetCar(pos, &pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (progn 10 (call1) 20 30 (call2)) -> (progn (call1) (call2)) */
static int checkparse_progn4_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit4_(str, pos, ret);
}

static int optparse_progn4_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	Return_check_optparse(checkparse_progn4_, str, ret);
	GetEvalParse(str->pos, 0, &pos);
	Return(optparse_implicit4_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (progn 10 (call1) 20 (call2) 30 40) -> (progn (call1) (call2) 40) */
static int checkparse_progn5_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit5_(str, pos, ret);
}

static int optparse_progn5_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	Return_check_optparse(checkparse_progn5_, str, ret);
	GetEvalParse(str->pos, 0, &pos);
	Return(optparse_implicit5_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (progn x (progn y) z) -> (progn x y z) */
static int checkparse_progn6_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit6_(str, pos, ret);
}

static int optparse_progn6_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	Return_check_optparse(checkparse_progn6_, str, ret);
	GetEvalParse(str->pos, 0, &pos);
	Return(optparse_implicit6_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (progn ...) */
static int checkparse_progn_all_(OptimizeInfo *str, int *ret)
{
	addr list;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(list, 0, &list);
	return checkparse_implicit_all_(str, list, ret);
}

static int optparse_progn_all_(OptimizeInfo *str, int *ret)
{
	int ignore;
	addr list;

	Return_check_optparse(checkparse_progn_all_, str, ret);
	GetEvalParse(str->pos, 0, &list);
	Return(optparse_implicit_all_(str, list, &list, &ignore));
	eval_single_parse_local(str->local, &list, EVAL_PARSE_PROGN, list);
	str->pos = list;

	return Result(ret, 1);
}


/* optparse-progn */
static int checkparse_progn_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_progn1_, str, ret);
	Return_or_optparse(checkparse_progn2_, str, ret);
	Return_or_optparse(checkparse_progn3_, str, ret);
	Return_or_optparse(checkparse_progn4_, str, ret);
	Return_or_optparse(checkparse_progn5_, str, ret);
	Return_or_optparse(checkparse_progn6_, str, ret);
	Return_or_optparse(checkparse_progn_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_progn_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_progn1_));
	Return(optimize_extract_(str, optparse_progn2_));
	Return(optimize_extract_(str, optparse_progn3_));
	Return(optimize_extract_(str, optparse_progn4_));
	Return(optimize_extract_(str, optparse_progn5_));
	Return(optimize_extract_(str, optparse_progn6_));
	Return(optimize_extract_(str, optparse_progn_all_));

	return 0;
}
static int optparse_progn_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_progn_run_);
}


/*
 *  let
 */
static int optimize_lettype(addr pos)
{
	EvalParse type;

	if (! eval_parse_p(pos))
		return 0;
	GetEvalParseType(pos, &type);
	return type == EVAL_PARSE_LET || type == EVAL_PARSE_LETA;
}

static int optimize_lettype_on(OptimizeInfo *str)
{
	return optimize_speed_on(str) && optimize_lettype(str->pos);
}

/* (let nil . nil) -> nil */
static int checkparse_let1_(OptimizeInfo *str, int *ret)
{
	addr pos, args, body;

	if (! optimize_lettype_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 2, &body); /* body */

	return Result(ret, args == Nil && body == Nil);
}

static int optparse_let1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_let1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (let (aaa bbb (ccc)) . nil) -> nil */
static int checkparse_let2_(OptimizeInfo *str, int *ret)
{
	addr pos, args, body, x;

	if (! optimize_lettype_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 0, &args); /* args */
	if (args == Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 2, &body); /* body */
	if (body != Nil)
		return Result(ret, 0);
	while (args != Nil) {
		GetCons(args, &x, &args);
		GetCdr(x, &x); /* (var . init) */
		if (RefEvalParseType(x) != EVAL_PARSE_NIL)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_let2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_let2_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* let-args */
static int checkparse_let_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos); /* args */
	while (pos != Nil) {
		GetCons(pos, &value, &pos);
		GetCdr(value, &value); /* (var . init) */
		Return(checkparse_inplace_(str, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_let_args_(OptimizeInfo *str, int *ret)
{
	int update, check;
	EvalParse type;
	addr pos, args, decl, body, var, init, root;
	LocalRoot local;

	Return_check_optparse(checkparse_let_args_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	local = str->local;
	update = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		cons_local(local, &var, var, init);
		cons_local(local, &root, var, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(&args, root);

	eval_parse_local(local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* let-body */
static int checkparse_implicit_declare_(
		OptimizeInfo *str, addr decl, addr cons, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	if (decl != Nil)
		apply_array_declare(str->value.local, decl);
	Return(checkparse_implicit_(str, cons, &check));
	*str = save;

	return Result(ret, check);
}

static int optparse_implicit_declare_(OptimizeInfo *str,
		addr decl, addr cons, addr *value, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	if (decl != Nil)
		apply_array_declare(str->value.local, decl);
	Return(optparse_implicit_(str, cons, value, &check));
	*str = save;

	return Result(ret, check);
}

static int checkparse_let_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_let_body_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, args, decl, body;

	Return_check_optparse(checkparse_let_body_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-let */
static int checkparse_let_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_let1_, str, ret);
	Return_or_optparse(checkparse_let2_, str, ret);
	Return_or_optparse(checkparse_let_args_, str, ret);
	Return_or_optparse(checkparse_let_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_let_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_let1_));
	Return(optimize_extract_(str, optparse_let2_));
	Return(optimize_extract_(str, optparse_let_args_));
	Return(optimize_extract_(str, optparse_let_body_));

	return 0;
}
static int optparse_let_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_let_run_);
}


/*
 *  setq
 */
/* (setq) -> nil */
static int checkparse_setq1_(OptimizeInfo *str, int *ret)
{
	if (! optimize_evaltype_on(str, EVAL_PARSE_SETQ))
		return Result(ret, 0);

	return Result(ret, RefEvalParse(str->pos, 0) == Nil);
}

static int optparse_setq1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_setq1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* setq-all */
static int checkparse_setq_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list, x;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_SETQ))
		return Result(ret, 0);
	GetEvalParse(list, 0, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCdr(x, &x); /* (var . expr) */
		Return(checkparse_inplace_(str, x, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_setq_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr list, root, var, expr;
	LocalRoot local;

	Return_check_optparse(checkparse_setq_all_, str, ret);
	GetEvalParse(str->pos, 0, &list);
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		GetCons(var, &var, &expr);
		Return(optparse_inplace_(str, expr, &expr, &check));
		update |= check;
		cons_local(local, &var, var, expr);
		cons_local(local, &root, var, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(&list, root);
	eval_single_parse_local(local, &list, EVAL_PARSE_SETQ, list);
	str->pos = list;

	return Result(ret, 1);
}

/* optparse-setq */
static int checkparse_setq_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_setq1_, str, ret);
	Return_or_optparse(checkparse_setq_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_setq_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_setq1_));
	Return(optimize_extract_(str, optparse_setq_all_));

	return 0;
}
static int optparse_setq_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_setq_run_);
}


/*
 *  lambda-ordinary
 */
/* &optional */
static int checkparse_opt_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos); /* var */
		GetCar(pos, &pos); /* init */
		Return(checkparse_inplace_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_opt_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, x, var, init, svar;
	LocalRoot local;

	/* opt -> (var init svar) */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, &svar, NULL);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		list_local(local, &x, var, init, svar, NULL);
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* &key */
static int checkparse_key_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos); /* var */
		GetCdr(pos, &pos); /* name */
		GetCar(pos, &pos); /* init */
		Return(checkparse_inplace_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_key_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, x, var, name, init, svar;
	LocalRoot local;

	/* key -> (var name init svar) */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &name, &init, &svar, NULL);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		list_local(local, &x, var, name, init, svar, NULL);
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* &aux */
static int checkparse_aux_(OptimizeInfo *str, addr list, int *ret)
{
	return checkparse_opt_(str, list, ret);
}

static int optparse_aux_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, x, var, init;
	LocalRoot local;

	/* aux -> (var init) */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, NULL);
		Return(optparse_inplace_(str, init, &init, &check));
		update |= check;
		list_local(local, &x, var, init, NULL);
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

/* interface */
static int checkparse_lambda_ordinary_(OptimizeInfo *str, addr args, int *ret)
{
	int check;
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);

	Return(checkparse_opt_(str, opt, &check));
	if (check)
		return Result(ret, 1);
	Return(checkparse_key_(str, key, &check));
	if (check)
		return Result(ret, 1);

	return checkparse_aux_(str, aux, ret);
}

static int optparse_lambda_ordinary_(
		OptimizeInfo *str, addr args, addr *value, int *ret)
{
	int update, check;
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	update = 0;
	/* opt */
	Return(checkparse_opt_(str, opt, &check));
	if (check) {
		Return(optparse_opt_(str, opt, &opt, &check));
		update |= check;
	}
	/* key */
	Return(checkparse_key_(str, key, &check));
	if (check) {
		Return(optparse_key_(str, key, &key, &check));
		update |= check;
	}
	/* aux */
	Return(checkparse_aux_(str, aux, &check));
	if (check) {
		Return(optparse_aux_(str, aux, &aux, &check));
		update |= check;
	}
	/* result */
	if (! update)
		return Result(ret, 0);
	list_local(str->local, value, var, opt, rest, key, allow, aux, NULL);

	return Result(ret, 1);
}


/*
 *  defun
 */
/* args */
static int checkparse_defun_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFUN))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos);
	return checkparse_lambda_ordinary_(str, pos, ret);
}

static int optparse_defun_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body, form;

	Return_check_optparse(checkparse_defun_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);
	GetEvalParse(pos, 5, &form);

	Return(optparse_lambda_ordinary_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	SetEvalParse(pos, 5, form);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_defun_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFUN))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_defun_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body, form;

	Return_check_optparse(checkparse_defun_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);
	GetEvalParse(pos, 5, &form);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	SetEvalParse(pos, 5, form);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-defun */
static int checkparse_defun_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_defun_args_, str, ret);
	Return_or_optparse(checkparse_defun_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_defun_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_defun_args_));
	Return(optimize_extract_(str, optparse_defun_body_));

	return 0;
}
static int optparse_defun_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_defun_run_);
}


/*
 *  macro-lambda
 */
static int checkparse_lambda_macro_(OptimizeInfo *str, addr args, int *ret);
static int checkparse_macro_var_(OptimizeInfo *str, addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! consp(pos))
			continue;
		Return(checkparse_lambda_macro_(str, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_lambda_macro_(OptimizeInfo *str, addr pos, addr *value, int *ret);
static int optparse_macro_var_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check;
	addr root, var;
	LocalRoot local;

	/* var */
	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var)) {
			Return(optparse_lambda_macro_(str, var, &var, &check));
			update |= check;
		}
		cons_local(local, &root, var, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

static int checkparse_lambda_macro_(OptimizeInfo *str, addr args, int *ret)
{
	int check;
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	/* var */
	Return(checkparse_macro_var_(str, var, &check));
	if (check)
		return Result(ret, 1);
	/* opt */
	Return(checkparse_opt_(str, opt, &check));
	if (check)
		return Result(ret, 1);
	/* key */
	Return(checkparse_key_(str, key, &check));
	if (check)
		return Result(ret, 1);
	/* aux */
	return checkparse_aux_(str, aux, ret);
}

static int optparse_lambda_macro_(OptimizeInfo *str, addr args, addr *value, int *ret)
{
	int update, check;
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	update = 0;
	/* var */
	Return(checkparse_macro_var_(str, var, &check));
	if (check) {
		Return(optparse_macro_var_(str, var, &var, &check));
		update |= check;
	}
	/* opt */
	Return(checkparse_opt_(str, opt, &check));
	if (check) {
		Return(optparse_opt_(str, opt, &opt, &check));
		update |= check;
	}
	/* key */
	Return(checkparse_key_(str, key, &check));
	if (check) {
		Return(optparse_key_(str, key, &key, &check));
		update |= check;
	}
	/* aux */
	Return(checkparse_opt_(str, aux, &check));
	if (check) {
		Return(optparse_aux_(str, aux, &aux, &check));
		update |= check;
	}
	/* result */
	if (! update)
		return Result(ret, 0);
	list_local(str->local, value, var, opt, rest, key, allow, aux, whole, env, NULL);

	return Result(ret, 1);
}


/*
 *  defmacro
 */
/* args */
static int checkparse_defmacro_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MACRO_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos); /* args */
	return checkparse_lambda_macro_(str, pos, ret);
}

static int optparse_defmacro_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, call;

	Return_check_optparse(checkparse_defmacro_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &call);

	Return(optparse_lambda_macro_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, call);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_defmacro_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MACRO_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_defmacro_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, call;

	Return_check_optparse(checkparse_defmacro_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &call);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, call);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-defmacro */
static int checkparse_defmacro_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_defmacro_args_, str, ret);
	Return_or_optparse(checkparse_defmacro_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_defmacro_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_defmacro_args_));
	Return(optimize_extract_(str, optparse_defmacro_body_));

	return 0;
}
static int optparse_defmacro_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_defmacro_run_);
}


/*
 *  deftype
 */
/* args */
static int checkparse_deftype_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFTYPE))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* args */
	return checkparse_lambda_macro_(str, pos, ret);
}

static int optparse_deftype_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_deftype_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_lambda_macro_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_deftype_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFTYPE))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_deftype_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_deftype_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-deftype */
static int checkparse_deftype_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_deftype_args_, str, ret);
	Return_or_optparse(checkparse_deftype_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_deftype_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_deftype_args_));
	Return(optimize_extract_(str, optparse_deftype_body_));

	return 0;
}
static int optparse_deftype_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_deftype_run_);
}


/*
 *  define-compiler-macro
 */
/* args */
static int checkparse_define_compiler_macro_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_COMPILER_MACRO))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* args */
	return checkparse_lambda_macro_(str, pos, ret);
}

static int optparse_define_compiler_macro_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_define_compiler_macro_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_lambda_macro_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_define_compiler_macro_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_COMPILER_MACRO))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_define_compiler_macro_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body;

	Return_check_optparse(checkparse_define_compiler_macro_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-define-compiler-macro */
static int checkparse_define_compiler_macro_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_define_compiler_macro_args_, str, ret);
	Return_or_optparse(checkparse_define_compiler_macro_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_define_compiler_macro_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_define_compiler_macro_args_));
	Return(optimize_extract_(str, optparse_define_compiler_macro_body_));

	return 0;
}
static int optparse_define_compiler_macro_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_define_compiler_macro_run_);
}


/*
 *  destructuring-bind
 */
/* args */
static int checkparse_destructuring_bind_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, expr, lambda;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DESTRUCTURING_BIND))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &lambda);

	Return(checkparse_inplace_(str, expr, &check));
	if (check)
		return Result(ret, 1);

	return checkparse_inplace_(str, lambda, ret);
}

static int optparse_destructuring_bind_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, expr, lambda;

	Return_check_optparse(checkparse_destructuring_bind_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &lambda);

	update = 0;
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	Return(optparse_inplace_(str, lambda, &lambda, &check));
	update |= check;
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DESTRUCTURING_BIND, 2);
	SetEvalParse(pos, 0, expr);
	SetEvalParse(pos, 1, lambda);
	str->pos = pos;

	return Result(ret, 1);
}


/*
 *  lambda
 */
/* args */
static int checkparse_lambda_args_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos);
	return checkparse_lambda_ordinary_(str, pos, ret);
}

static int optparse_lambda_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, form;

	Return_check_optparse(checkparse_lambda_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &form);

	Return(optparse_lambda_ordinary_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, form);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_lambda_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LAMBDA))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_lambda_body_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, form;

	Return_check_optparse(checkparse_lambda_body_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &form);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, form);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-lambda */
static int checkparse_lambda_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_lambda_args_, str, ret);
	Return_or_optparse(checkparse_lambda_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_lambda_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_lambda_args_));
	Return(optimize_extract_(str, optparse_lambda_body_));

	return 0;
}
static int optparse_lambda_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_lambda_run_);
}


/*
 *  if
 */
/* (if nil a b) -> b */
static int checkparse_if1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return Result(ret, optimize_evaltype(pos, EVAL_PARSE_NIL));
}

static int optparse_if1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_if1_, str, ret);
	GetEvalParse(str->pos, 2, &pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (if x a b) -> a */
static int checkparse_if2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	if (optimize_evaltype(pos, EVAL_PARSE_NIL))
		return Result(ret, 0);

	return Result(ret, optimize_value(pos));
}

static int optparse_if2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_if2_, str, ret);
	GetEvalParse(str->pos, 1, &pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_if_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int optparse_if_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, expr, ifthen, ifelse;

	Return_check_optparse(checkparse_if_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &ifthen);
	GetEvalParse(pos, 2, &ifelse);

	update = 0;
	/* expr */
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	/* then */
	Return(optparse_inplace_(str, ifthen, &ifthen, &check));
	update |= check;
	/* else */
	Return(optparse_inplace_(str, ifelse, &ifelse, &check));
	update |= check;
	/* result */
	if (! update)
		return Result(ret, 0);

	eval_parse_local(str->local, &pos, EVAL_PARSE_IF, 3);
	SetEvalParse(pos, 0, expr);
	SetEvalParse(pos, 1, ifthen);
	SetEvalParse(pos, 2, ifelse);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-if */
static int checkparse_if_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_if1_, str, ret);
	Return_or_optparse(checkparse_if2_, str, ret);
	Return_or_optparse(checkparse_if_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_if_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_if1_));
	Return(optimize_extract_(str, optparse_if2_));
	Return(optimize_extract_(str, optparse_if_all_));

	return 0;
}
static int optparse_if_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_if_run_);
}


/*
 *  unwind-protect
 */
/* (unwind-protect value . tail) -> (progn ,@tail value) */
static int checkparse_unwind_protect1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return Result(ret, optimize_value(pos));
}

static int optparse_unwind_protect1_(OptimizeInfo *str, int *ret)
{
	addr pos, form, list, root;
	LocalRoot local;

	Return_check_optparse(checkparse_unwind_protect1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	cons_local(local, &root, form, root);
	nreverse(&list, root);
	/* progn */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* (unwind-protect form . all-value) -> form */
static int checkparse_unwind_protect2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (! optimize_value(x))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_unwind_protect2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_unwind_protect2_, str, ret);
	GetEvalParse(str->pos, 0, &pos); /* form */
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_unwind_protect_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 1, &value);
	return checkparse_implicit_all_(str, value, ret);
}

static int optparse_unwind_protect_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, list;

	Return_check_optparse(checkparse_unwind_protect_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &list);
	update = 0;
	/* form */
	Return(optparse_inplace_(str, form, &form, &check));
	update |= check;
	/* list */
	Return(optparse_implicit_all_(str, list, &list, &check));
	update |= check;

	/* update */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_UNWIND_PROTECT, 2);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-unwind-protect */
static int checkparse_unwind_protect_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_unwind_protect1_, str, ret);
	Return_or_optparse(checkparse_unwind_protect2_, str, ret);
	Return_or_optparse(checkparse_unwind_protect_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_unwind_protect_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_unwind_protect1_));
	Return(optimize_extract_(str, optparse_unwind_protect2_));
	Return(optimize_extract_(str, optparse_unwind_protect_all_));

	return 0;
}
static int optparse_unwind_protect_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_unwind_protect_run_);
}


/*
 *  tagbody
 */
/* (tagbody) -> nil */
static int checkparse_tagbody1_(OptimizeInfo *str, int *ret)
{
	addr pos, tag, body;
	size_t size1, size2;

	if (! optimize_evaltype_on(str, EVAL_PARSE_TAGBODY))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 0, &tag);
	GetEvalParse(pos, 1, &body);
	if (body == Nil)
		return Result(ret, 1);
	size1 = length_list_unsafe(tag);
	size2 = length_list_unsafe(body);

	return Result(ret, size1 == size2);
}

static int optparse_tagbody1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_tagbody1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (tagbody (call) (call2)) -> (progn (call) (call2) nil) */
static int checkparse_tagbody2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_TAGBODY))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_tagbody2_(OptimizeInfo *str, int *ret)
{
	addr pos, root, list;
	LocalRoot local;

	Return_check_optparse(checkparse_tagbody2_, str, ret);
	GetEvalParse(str->pos, 1, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	cons_local(local, &root, pos, root);
	nreverse(&root, root);
	/* progn */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, root);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_tagbody_all_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_TAGBODY))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos);
	/* Don't use checkparse_implicit. */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_tagbody_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, tag, body;

	Return_check_optparse(checkparse_tagbody_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &tag);
	GetEvalParse(pos, 1, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_TAGBODY, 2);
	SetEvalParse(pos, 0, tag);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-tagbody */
static int checkparse_tagbody_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_tagbody1_, str, ret);
	Return_or_optparse(checkparse_tagbody2_, str, ret);
	Return_or_optparse(checkparse_tagbody_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_tagbody_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_tagbody1_));
	Return(optimize_extract_(str, optparse_tagbody2_));
	Return(optimize_extract_(str, optparse_tagbody_all_));

	return 0;
}
static int optparse_tagbody_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_tagbody_run_);
}


/*
 *  block / return-from
 */
/* (block name) -> nil */
static int checkparse_block1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_BLOCK))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_block1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_block1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (block name ... x) -> x */
static int checkparse_block2_(OptimizeInfo *str, int *ret)
{
	addr list, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_BLOCK))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	if (list == Nil)
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (! optimize_value(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_block2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	Return_check_optparse(checkparse_block2_, str, ret);
	GetEvalParse(str->pos, 1, &list);
	if (list == Nil)
		return Result(ret, 0);
	x = str->pos;
	while (list != Nil) {
		GetCons(list, &x, &list);
	}
	str->pos = x;

	return Result(ret, 1);
}

/* all */
static int checkparse_block_all_(OptimizeInfo *str, int *ret)
{
	addr list;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_BLOCK))
		return Result(ret, 0);
	GetEvalParse(list, 1, &list);
	return checkparse_implicit_declare_(str, Nil, list, ret);
}

static int optparse_block_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, body;

	Return_check_optparse(checkparse_block_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &body);

	Return(optparse_implicit_declare_(str, Nil, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_BLOCK, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* (return-from name expr) -> (return-from name expr) */
static int checkparse_return_from_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_RETURN_FROM))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos);
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_return_from_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, expr;

	Return_check_optparse(checkparse_return_from_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_RETURN_FROM, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-block */
static int checkparse_block_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_block1_, str, ret);
	Return_or_optparse(checkparse_block2_, str, ret);
	Return_or_optparse(checkparse_block_all_, str, ret);
	Return_or_optparse(checkparse_return_from_, str, ret);

	return Result(ret, 0);
}

static int optparse_block_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_block1_));
	Return(optimize_extract_(str, optparse_block2_));
	Return(optimize_extract_(str, optparse_block_all_));
	Return(optimize_extract_(str, optparse_return_from_));

	return 0;
}
static int optparse_block_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_block_run_);
}


/*
 *  catch / throw
 */
/* (catch name) -> (progn name nil) */
static int checkparse_catch1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_catch1_(OptimizeInfo *str, int *ret)
{
	addr pos, name, list;
	LocalRoot local;

	Return_check_optparse(checkparse_catch1_, str, ret);
	/* (name nil) */
	GetEvalParse(str->pos, 0, &name);
	local = str->local;
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name nil) */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* (catch name ... x) -> (progn name x) */
static int checkparse_catch2_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_value(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_catch2_(OptimizeInfo *str, int *ret)
{
	addr pos, name, list;
	LocalRoot local;

	Return_check_optparse(checkparse_catch2_, str, ret);
	/* (name lastcar) */
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &list);
	if (list == Nil)
		return Result(ret, 0);
	for (pos = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
	}
	local = str->local;
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name lastcar) */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_catch_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 1, &value);
	return checkparse_implicit_declare_(str, Nil, value, ret);
}

static int optparse_catch_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, name, list;

	Return_check_optparse(checkparse_catch_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &list);

	update = 0;
	/* name */
	Return(optparse_inplace_(str, name, &name, &check));
	update |= check;
	/* list */
	Return(optparse_implicit_declare_(str, Nil, list, &list, &check));
	update |= check;
	/* result */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_CATCH, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* throw */
static int checkparse_throw_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_THROW))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 1, &value);
	return checkparse_inplace_(str, value, ret);
}

static int optparse_throw_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, name, expr;

	Return_check_optparse(checkparse_throw_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &expr);

	update = 0;
	/* name */
	Return(optparse_inplace_(str, name, &name, &check));
	update |= check;
	/* expr */
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	/* result */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_THROW, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-catch */
static int checkparse_catch_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_catch1_, str, ret);
	Return_or_optparse(checkparse_catch2_, str, ret);
	Return_or_optparse(checkparse_catch_all_, str, ret);
	Return_or_optparse(checkparse_throw_, str, ret);

	return Result(ret, 0);
}

static int optparse_catch_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_catch1_));
	Return(optimize_extract_(str, optparse_catch2_));
	Return(optimize_extract_(str, optparse_catch_all_));
	Return(optimize_extract_(str, optparse_throw_));

	return 0;
}
static int optparse_catch_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_catch_run_);
}


/*
 *  flet / labels
 */
static int optimize_fletlabels(OptimizeInfo *str)
{
	addr pos;
	EvalParse type;

	pos = str->pos;
	if (! eval_parse_p(pos))
		return 0;
	GetEvalParseType(pos, &type);

	return type == EVAL_PARSE_FLET || type == EVAL_PARSE_LABELS;
}

static int optimize_fletlabels_on(OptimizeInfo *str)
{
	return optimize_speed_on(str) && optimize_fletlabels(str);
}

/* (flet ()) -> nil */
static int checkparse_flet1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_flet1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_flet1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* (flet () values... x) -> x */
static int optimize_flet_value(addr pos)
{
	return optimize_value_function(pos, 0);
}

static int checkparse_flet2_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &check);
	if (! empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_flet_value(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_flet2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	Return_check_optparse(checkparse_flet2_, str, ret);
	GetEvalParse(str->pos, 2, &list);
	if (list == Nil)
		return Result(ret, 0);
	for (x = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
	}
	str->pos = x;

	return Result(ret, 1);
}

/* (flet () ...) -> (progn ...) */
static int checkparse_flet3_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 0, &check);
	if (check != Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 1, &check);
	if (! empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &check);
	return Result(ret, check != Nil);
}

static int optparse_flet3_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_flet3_, str, ret);
	GetEvalParse(str->pos, 2, &pos);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (flet () (declare ...) ...) -> (locally (declare ...) ...) */
static int checkparse_flet4_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 0, &check);
	if (check != Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 1, &check);
	if (empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &check);
	return Result(ret, check != Nil);
}

static int optparse_flet4_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, cons;

	Return_check_optparse(checkparse_flet4_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);

	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, cons);
	str->pos = pos;

	return Result(ret, 1);
}

/* flet-args */
static int checkparse_flet_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, value, call, args, decl, doc, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos);
	while (pos != Nil) {
		GetCons(pos, &value, &pos);
		List_bind(value, &call, &args, &decl, &doc, &body, NULL);
		Return(checkparse_lambda_ordinary_(str, args, &check));
		if (check)
			return Result(ret, 1);
		Return(checkparse_implicit_declare_(str, decl, body, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_flet_one_(OptimizeInfo *str, addr list, addr *value, int *ret)
{
	int update, check, check1, check2;
	addr root, call, args, decl, doc, body, x;
	LocalRoot local;

	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &call, &args, &decl, &doc, &body, NULL);

		check1 = check2 = 0;
		Return(checkparse_lambda_ordinary_(str, args, &check));
		if (check) {
			Return(optparse_lambda_ordinary_(str, args, &args, &check1));
		}
		Return(checkparse_implicit_declare_(str, decl, body, &check));
		if (check) {
			Return(optparse_implicit_declare_(str, decl, body, &body, &check2));
		}
		if (check1 || check2) {
			list_local(local, &x, call, args, decl, doc, body, NULL);
			update = 1;
		}
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(value, root);

	return Result(ret, 1);
}

static int optparse_flet_args_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, args, decl, body;

	Return_check_optparse(checkparse_flet_args_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	Return(optparse_flet_one_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* flet-body */
static int checkparse_flet_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_flet_body_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, args, decl, body;

	Return_check_optparse(checkparse_flet_body_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-flet */
static int checkparse_flet_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_flet1_, str, ret);
	Return_or_optparse(checkparse_flet2_, str, ret);
	Return_or_optparse(checkparse_flet3_, str, ret);
	Return_or_optparse(checkparse_flet4_, str, ret);
	Return_or_optparse(checkparse_flet_args_, str, ret);
	Return_or_optparse(checkparse_flet_body_, str, ret);

	return Result(ret, 0);
}

static int optparse_flet_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_flet1_));
	Return(optimize_extract_(str, optparse_flet2_));
	Return(optimize_extract_(str, optparse_flet3_));
	Return(optimize_extract_(str, optparse_flet4_));
	Return(optimize_extract_(str, optparse_flet_args_));
	Return(optimize_extract_(str, optparse_flet_body_));

	return 0;
}
static int optparse_flet_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_flet_run_);
}


/*
 *  the
 */
/* (the type expr) -> (the [type] expr) */
static int checkparse_the1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_THE))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos); /* type */

	/* type-error */
	Return(check_error_type_(str->ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	/* return ! type_optimized_or_subtypep(pos); */
	return Result(ret, ! type_optimized_p(pos));
}

static int optparse_the1_(OptimizeInfo *str, int *ret)
{
	int ignore;
	addr pos, type, expr;
	LocalRoot local;

	Return_check_optparse(checkparse_the1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &expr);

	local = str->local;
	Return(type_optimize_local_(local, type, &type, &ignore));
	eval_parse_local(local, &pos, EVAL_PARSE_THE, 2);
	SetEvalParse(pos, 0, type);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* expr */
static int checkparse_the2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_THE))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_the2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, type, expr;

	Return_check_optparse(checkparse_the2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_THE, 2);
	SetEvalParse(pos, 0, type);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-the */
static int checkparse_the_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_the1_, str, ret);
	Return_or_optparse(checkparse_the2_, str, ret);

	return Result(ret, 0);
}

static int optparse_the_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_the1_));
	Return(optimize_extract_(str, optparse_the2_));

	return 0;
}
static int optparse_the_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_the_run_);
}


/*
 *  eval-when
 */
/* (eval-when cons) -> nil */
static int checkparse_eval_when1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_EVAL_WHEN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 0, &pos); /* body */
	return Result(ret, pos == Nil);
}

static int optparse_eval_when1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_eval_when1_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_eval_when_all_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_EVAL_WHEN))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos); /* body */
	return checkparse_implicit_declare_(str, Nil, pos, ret);
}

static int optparse_eval_when_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, cons, compile, load, exec, toplevel, mode;

	Return_check_optparse(checkparse_eval_when_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &cons);
	GetEvalParse(pos, 1, &compile);
	GetEvalParse(pos, 2, &load);
	GetEvalParse(pos, 3, &exec);
	GetEvalParse(pos, 4, &toplevel);
	GetEvalParse(pos, 5, &mode);

	Return(optparse_implicit_declare_(str, Nil, cons, &cons, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_EVAL_WHEN, 6);
	SetEvalParse(pos, 0, cons);
	SetEvalParse(pos, 1, compile);
	SetEvalParse(pos, 2, load);
	SetEvalParse(pos, 3, exec);
	SetEvalParse(pos, 4, toplevel);
	SetEvalParse(pos, 5, mode);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-eval-when */
static int checkparse_eval_when_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_eval_when1_, str, ret);
	Return_or_optparse(checkparse_eval_when_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_eval_when_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_eval_when1_));
	Return(optimize_extract_(str, optparse_eval_when_all_));

	return 0;
}
static int optparse_eval_when_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_eval_when_run_);
}


/*
 *  values
 */
static int checkparse_values_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_VALUES))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_values_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos;

	Return_check_optparse(checkparse_values_, str, ret);
	GetEvalParse(str->pos, 0, &pos);
	Return(optparse_implicit_all_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_VALUES, pos);
	str->pos = pos;

	return Result(ret, 1);
}


/*
 *  locally
 */
/* (locally ...) -> (progn ...) */
static int checkparse_locally1_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 0, &decl); /* decl */
	GetEvalParse(pos, 1, &body); /* body */
	return Result(ret, empty_nil_declare(decl) && body != Nil);
}

static int optparse_locally1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_locally1_, str, ret);
	GetEvalParse(str->pos, 1, &pos); /* body */
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (locally (declare ...)) -> nil */
static int checkparse_locally2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos); /* body */
	return Result(ret, pos == Nil);
}

static int optparse_locally2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_locally2_, str, ret);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_locally_all_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &body);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_locally_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, decl, body;

	Return_check_optparse(checkparse_locally_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-locally */
static int checkparse_locally_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_locally1_, str, ret);
	Return_or_optparse(checkparse_locally2_, str, ret);
	Return_or_optparse(checkparse_locally_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_locally_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_locally1_));
	Return(optimize_extract_(str, optparse_locally2_));
	Return(optimize_extract_(str, optparse_locally_all_));

	return 0;
}
static int optparse_locally_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_locally_run_);
}


/*
 *  call
 */
/* first argument */
static int checkparse_call1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_CALL))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos); /* call */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_call1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, call, cons;

	Return_check_optparse(checkparse_call1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &cons);

	Return(optparse_inplace_(str, call, &call, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, cons);
	str->pos = pos;

	return Result(ret, 1);
}

/* all */
static int checkparse_call_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list, value;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_CALL))
		return Result(ret, 0);
	GetEvalParse(list, 1, &list); /* cons */
	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(checkparse_inplace_(str, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int optparse_call_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, call, list, root, x;
	LocalRoot local;

	Return_check_optparse(checkparse_call_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &list);

	local = str->local;
	update = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		Return(optparse_inplace_(str, x, &x, &check));
		update |= check;
		cons_local(local, &root, x, root);
	}
	if (! update)
		return Result(ret, 0);
	nreverse(&list, root);

	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-call */
static int checkparse_call_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_call1_, str, ret);
	Return_or_optparse(checkparse_call_all_, str, ret);

	return Result(ret, 0);
}

static int optparse_call_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_call1_));
	Return(optimize_extract_(str, optparse_call_all_));

	return 0;
}
static int optparse_call_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_call_run_);
}


/*
 *  multiple-value-bind
 */
/* expr */
static int checkparse_multiple_value_bind1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_bind1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, vars, expr, decl, doc, body;

	Return_check_optparse(checkparse_multiple_value_bind1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &vars);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(pos, 0, vars);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_multiple_value_bind2_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 4, &body);
	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_multiple_value_bind2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, vars, expr, decl, doc, body;

	Return_check_optparse(checkparse_multiple_value_bind2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &vars);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(pos, 0, vars);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-bind */
static int checkparse_multiple_value_bind_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_multiple_value_bind1_, str, ret);
	Return_or_optparse(checkparse_multiple_value_bind2_, str, ret);

	return Result(ret, 0);
}

static int optparse_multiple_value_bind_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_multiple_value_bind1_));
	Return(optimize_extract_(str, optparse_multiple_value_bind2_));

	return 0;
}
static int optparse_multiple_value_bind_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_bind_run_);
}


/*
 *  multiple-value-call
 */
/* expr */
static int checkparse_multiple_value_call1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL))
		return Result(ret, 0);
	GetEvalParse(pos, 0, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_call1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, call, body;

	Return_check_optparse(checkparse_multiple_value_call1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &body);

	Return(optparse_inplace_(str, call, &call, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_multiple_value_call2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_multiple_value_call2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, call, body;

	Return_check_optparse(checkparse_multiple_value_call2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-call */
static int checkparse_multiple_value_call_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_multiple_value_call1_, str, ret);
	Return_or_optparse(checkparse_multiple_value_call2_, str, ret);

	return Result(ret, 0);
}

static int optparse_multiple_value_call_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_multiple_value_call1_));
	Return(optimize_extract_(str, optparse_multiple_value_call2_));

	return 0;
}
static int optparse_multiple_value_call_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_call_run_);
}


/*
 *  optimize-parse
 */
static int checkparse_all_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_check_, str, ret);
	Return_or_optparse(checkparse_progn_, str, ret);
	Return_or_optparse(checkparse_let_, str, ret);
	Return_or_optparse(checkparse_setq_, str, ret);
	Return_or_optparse(checkparse_defun_, str, ret);
	Return_or_optparse(checkparse_defmacro_, str, ret);
	Return_or_optparse(checkparse_deftype_, str, ret);
	Return_or_optparse(checkparse_define_compiler_macro_, str, ret);
	Return_or_optparse(checkparse_destructuring_bind_, str, ret);
	Return_or_optparse(checkparse_lambda_, str, ret);
	Return_or_optparse(checkparse_if_, str, ret);
	Return_or_optparse(checkparse_unwind_protect_, str, ret);
	Return_or_optparse(checkparse_tagbody_, str, ret);
	Return_or_optparse(checkparse_block_, str, ret);
	Return_or_optparse(checkparse_catch_, str, ret);
	Return_or_optparse(checkparse_flet_, str, ret);
	Return_or_optparse(checkparse_the_, str, ret);
	Return_or_optparse(checkparse_eval_when_, str, ret);
	Return_or_optparse(checkparse_values_, str, ret);
	Return_or_optparse(checkparse_locally_, str, ret);
	Return_or_optparse(checkparse_call_, str, ret);
	Return_or_optparse(checkparse_multiple_value_bind_, str, ret);
	Return_or_optparse(checkparse_multiple_value_call_, str, ret);

	return Result(ret, 0);
}

static int optparse_all_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_check_));
	Return(optimize_extract_(str, optparse_progn_));
	Return(optimize_extract_(str, optparse_let_));
	Return(optimize_extract_(str, optparse_setq_));
	Return(optimize_extract_(str, optparse_defun_));
	Return(optimize_extract_(str, optparse_defmacro_));
	Return(optimize_extract_(str, optparse_deftype_));
	Return(optimize_extract_(str, optparse_define_compiler_macro_));
	Return(optimize_extract_(str, optparse_destructuring_bind_));
	Return(optimize_extract_(str, optparse_lambda_));
	Return(optimize_extract_(str, optparse_if_));
	Return(optimize_extract_(str, optparse_unwind_protect_));
	Return(optimize_extract_(str, optparse_tagbody_));
	Return(optimize_extract_(str, optparse_block_));
	Return(optimize_extract_(str, optparse_catch_));
	Return(optimize_extract_(str, optparse_flet_));
	Return(optimize_extract_(str, optparse_the_));
	Return(optimize_extract_(str, optparse_eval_when_));
	Return(optimize_extract_(str, optparse_values_));
	Return(optimize_extract_(str, optparse_locally_));
	Return(optimize_extract_(str, optparse_call_));
	Return(optimize_extract_(str, optparse_multiple_value_bind_));
	Return(optimize_extract_(str, optparse_multiple_value_call_));

	return 0;
}
static int optparse_all_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_all_run_);
}

int optimize_parse_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	OptimizeInfo str;

	local = ptr->local;
	push_local(local, &stack);
	optimize_initialize(&str, ptr, pos);
	Return(optparse_all_(&str, &check));
	if (check)
		copy_eval_parse_heap(value, str.pos);
	else
		*value = pos;
	rollback_local(local, stack);

	if (ret)
		return Result(ret, check);
	else
		return 0;
}

