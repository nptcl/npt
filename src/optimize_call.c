#include "cons_list.h"
#include "optimize.h"
#include "optimize_call.h"
#include "optimize_parse.h"
#include "parse_object.h"
#include "subtypep_optimize.h"
#include "type_delay.h"
#include "typedef.h"

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
		if (! optimize_value_and_function(value))
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
		valuep = optimize_value_and_function(check);
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
		if (! optimize_value_and_function(check))
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
		valuep = optimize_value_and_function(check);
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
		if (list == Nil || ! optimize_value_and_function(pos))
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
			GetEvalParse(pos, 1, &pos);
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
int checkparse_implicit_all_(OptimizeInfo *str, addr list, int *ret)
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

int optparse_implicit_all_(OptimizeInfo *str, addr list, addr *value, int *ret)
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
int checkparse_implicit_(OptimizeInfo *str, addr pos, int *ret)
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

int optparse_implicit_(OptimizeInfo *str, addr pos, addr *value, int *ret)
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
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return Result(ret, list == Nil);
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
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);

	return Result(ret, singlep(list));
}

static int optparse_progn2_(OptimizeInfo *str, int *ret)
{
	addr list;

	Return_check_optparse(checkparse_progn2_, str, ret);
	GetEvalParse(str->pos, 1, &list);
	GetCar(list, &(str->pos));

	return Result(ret, 1);
}

/* (progn 10 20 30) -> 30 */
static int checkparse_progn3_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit3_(str, list, ret);
}

static int optparse_progn3_(OptimizeInfo *str, int *ret)
{
	int check;
	addr list;

	Return_check_optparse(checkparse_progn3_, str, ret);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit3_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	GetCar(list, &list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn 10 (call1) 20 30 (call2)) -> (progn (call1) (call2)) */
static int checkparse_progn4_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit4_(str, list, ret);
}

static int optparse_progn4_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, list;

	Return_check_optparse(checkparse_progn4_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit4_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn 10 (call1) 20 (call2) 30 40) -> (progn (call1) (call2) 40) */
static int checkparse_progn5_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit5_(str, list, ret);
}

static int optparse_progn5_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, list;

	Return_check_optparse(checkparse_progn5_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit5_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* (progn x (progn y) z) -> (progn x y z) */
static int checkparse_progn6_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return checkparse_implicit6_(str, list, ret);
}

static int optparse_progn6_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, list;

	Return_check_optparse(checkparse_progn6_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit6_(str, list, &list, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

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
	GetEvalParse(list, 1, &list);
	return checkparse_implicit_all_(str, list, ret);
}

static int optparse_progn_all_(OptimizeInfo *str, int *ret)
{
	int ignore;
	addr form, list;

	Return_check_optparse(checkparse_progn_all_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
	Return(optparse_implicit_all_(str, list, &list, &ignore));
	eval_parse2_local(str->local, &list, EVAL_PARSE_PROGN, form, list);
	str->pos = list;

	return Result(ret, 1);
}


/* optparse-progn */
int checkparse_progn_(OptimizeInfo *str, int *ret)
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
int optparse_progn_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(pos, 1, &args); /* args */
	GetEvalParse(pos, 3, &body); /* body */

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
	GetEvalParse(pos, 1, &args); /* args */
	if (args == Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 3, &body); /* body */
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
	GetEvalParse(pos, 1, &pos); /* args */
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
	addr pos, form, args, decl, body, var, init, root;
	LocalRoot local;

	Return_check_optparse(checkparse_let_args_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

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

	eval_parse_local(local, &pos, type, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* let-body */
static int checkparse_let_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_let_body_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, form, args, decl, body;

	Return_check_optparse(checkparse_let_body_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, type, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-let */
int checkparse_let_(OptimizeInfo *str, int *ret)
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
int optparse_let_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_let_run_);
}


/*
 *  setq
 */
/* (setq) -> nil */
static int checkparse_setq1_(OptimizeInfo *str, int *ret)
{
	addr list;

	if (! optimize_evaltype_on(str, EVAL_PARSE_SETQ))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &list);
	return Result(ret, list == Nil);
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
	GetEvalParse(list, 1, &list);
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
	addr form, list, root, var, expr;
	LocalRoot local;

	Return_check_optparse(checkparse_setq_all_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &list);
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
	eval_parse2_local(local, &list, EVAL_PARSE_SETQ, form, list);
	str->pos = list;

	return Result(ret, 1);
}

/* optparse-setq */
int checkparse_setq_(OptimizeInfo *str, int *ret)
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
int optparse_setq_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_setq_run_);
}


/*
 *  destructuring-bind
 */
/* args */
int checkparse_destructuring_bind_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, expr, lambda;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DESTRUCTURING_BIND))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &lambda);

	Return(checkparse_inplace_(str, expr, &check));
	if (check)
		return Result(ret, 1);

	return checkparse_inplace_(str, lambda, ret);
}

int optparse_destructuring_bind_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, expr, lambda;

	Return_check_optparse(checkparse_destructuring_bind_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &lambda);

	update = 0;
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	Return(optparse_inplace_(str, lambda, &lambda, &check));
	update |= check;
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DESTRUCTURING_BIND, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, lambda);
	str->pos = pos;

	return Result(ret, 1);
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
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, optimize_evaltype(pos, EVAL_PARSE_NIL));
}

static int optparse_if1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_if1_, str, ret);
	GetEvalParse(str->pos, 3, &pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (if x a b) -> a */
static int checkparse_if2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return Result(ret, 0);
	GetEvalParse(str->pos, 1, &pos);
	if (optimize_evaltype(pos, EVAL_PARSE_NIL))
		return Result(ret, 0);

	return Result(ret, optimize_value_and_function(pos));
}

static int optparse_if2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_if2_, str, ret);
	GetEvalParse(str->pos, 2, &pos);
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
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 3, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int optparse_if_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, expr, ifthen, ifelse;

	Return_check_optparse(checkparse_if_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &ifthen);
	GetEvalParse(pos, 3, &ifelse);

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

	eval_parse_local(str->local, &pos, EVAL_PARSE_IF, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, ifthen);
	SetEvalParse(pos, 3, ifelse);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-if */
int checkparse_if_(OptimizeInfo *str, int *ret)
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
int optparse_if_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, optimize_value_and_function(pos));
}

static int optparse_unwind_protect1_(OptimizeInfo *str, int *ret)
{
	addr pos, form, expr, list, root;
	LocalRoot local;

	Return_check_optparse(checkparse_unwind_protect1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	cons_local(local, &root, expr, root);
	nreverse(&list, root);
	/* progn */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* (unwind-protect form . all-value) -> form */
static int checkparse_unwind_protect2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (! optimize_value_and_function(x))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_unwind_protect2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	Return_check_optparse(checkparse_unwind_protect2_, str, ret);
	GetEvalParse(str->pos, 1, &pos); /* form */
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
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	return checkparse_implicit_all_(str, value, ret);
}

static int optparse_unwind_protect_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, expr, list;

	Return_check_optparse(checkparse_unwind_protect_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &list);
	update = 0;
	/* expr */
	Return(optparse_inplace_(str, expr, &expr, &check));
	update |= check;
	/* list */
	Return(optparse_implicit_all_(str, list, &list, &check));
	update |= check;

	/* update */
	if (! update)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_UNWIND_PROTECT, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-unwind-protect */
int checkparse_unwind_protect_(OptimizeInfo *str, int *ret)
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
int optparse_unwind_protect_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(pos, 1, &tag);
	GetEvalParse(pos, 2, &body);
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
	GetEvalParse(str->pos, 1, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_tagbody2_(OptimizeInfo *str, int *ret)
{
	addr pos, root, form, list;
	LocalRoot local;

	Return_check_optparse(checkparse_tagbody2_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 2, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	cons_local(local, &root, pos, root);
	nreverse(&root, root);
	/* progn */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, root);
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
	GetEvalParse(pos, 2, &pos);
	/* Don't use checkparse_implicit. */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_tagbody_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, tag, body;

	Return_check_optparse(checkparse_tagbody_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &tag);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_TAGBODY, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, tag);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-tagbody */
int checkparse_tagbody_(OptimizeInfo *str, int *ret)
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
int optparse_tagbody_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(str->pos, 2, &pos);
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
	GetEvalParse(str->pos, 2, &list);
	if (list == Nil)
		return Result(ret, 0);
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (! optimize_value_and_function(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_block2_(OptimizeInfo *str, int *ret)
{
	addr list, x;

	Return_check_optparse(checkparse_block2_, str, ret);
	GetEvalParse(str->pos, 2, &list);
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
	GetEvalParse(list, 2, &list);
	return checkparse_implicit_declare_(str, Nil, list, ret);
}

static int optparse_block_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, name, body;

	Return_check_optparse(checkparse_block_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_declare_(str, Nil, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_BLOCK, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, body);
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
	GetEvalParse(pos, 2, &pos);
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_return_from_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, name, expr;

	Return_check_optparse(checkparse_return_from_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_RETURN_FROM, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-block */
int checkparse_block_(OptimizeInfo *str, int *ret)
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
int optparse_block_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(str->pos, 2, &pos);
	return Result(ret, pos == Nil);
}

static int optparse_catch1_(OptimizeInfo *str, int *ret)
{
	addr pos, form, name, list;
	LocalRoot local;

	Return_check_optparse(checkparse_catch1_, str, ret);
	/* (name nil) */
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &name);
	local = str->local;
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name nil) */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* (catch name ... x) -> (progn name x) */
static int checkparse_catch2_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_value_and_function(check))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int optparse_catch2_(OptimizeInfo *str, int *ret)
{
	addr pos, form, name, list;
	LocalRoot local;

	Return_check_optparse(checkparse_catch2_, str, ret);
	/* (name lastcar) */
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &list);
	if (list == Nil)
		return Result(ret, 0);
	for (pos = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
	}
	local = str->local;
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name lastcar) */
	eval_parse2_local(local, &pos, EVAL_PARSE_PROGN, form, list);
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
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	return checkparse_implicit_declare_(str, Nil, value, ret);
}

static int optparse_catch_all_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, name, list;

	Return_check_optparse(checkparse_catch_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &list);

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
	eval_parse_local(str->local, &pos, EVAL_PARSE_CATCH, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, list);
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
	GetEvalParse(pos, 1, &value);
	Return(checkparse_inplace_(str, value, &check));
	if (check)
		return Result(ret, 1);
	GetEvalParse(pos, 2, &value);
	return checkparse_inplace_(str, value, ret);
}

static int optparse_throw_(OptimizeInfo *str, int *ret)
{
	int update, check;
	addr pos, form, name, expr;

	Return_check_optparse(checkparse_throw_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &expr);

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
	eval_parse_local(str->local, &pos, EVAL_PARSE_THROW, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-catch */
int checkparse_catch_(OptimizeInfo *str, int *ret)
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
int optparse_catch_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_catch_run_);
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
	GetEvalParse(str->pos, 1, &pos); /* type */

	/* type-error */
	Return(check_delay_type_(str->ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	/* return ! type_optimized_or_subtypep(pos); */
	return Result(ret, ! type_optimized_p(pos));
}

static int optparse_the1_(OptimizeInfo *str, int *ret)
{
	int ignore;
	addr pos, form, type, expr;
	LocalRoot local;

	Return_check_optparse(checkparse_the1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &type);
	GetEvalParse(pos, 2, &expr);

	local = str->local;
	Return(type_optimize_local_(local, type, &type, &ignore));
	eval_parse_local(local, &pos, EVAL_PARSE_THE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, type);
	SetEvalParse(pos, 2, expr);
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
	GetEvalParse(pos, 2, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_the2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, type, expr;

	Return_check_optparse(checkparse_the2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &type);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_THE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, type);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-the */
int checkparse_the_(OptimizeInfo *str, int *ret)
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
int optparse_the_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(str->pos, 1, &pos); /* body */
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
	GetEvalParse(pos, 1, &pos); /* body */
	return checkparse_implicit_declare_(str, Nil, pos, ret);
}

static int optparse_eval_when_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, cons, compile, load, exec, toplevel, mode;

	Return_check_optparse(checkparse_eval_when_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &cons);
	GetEvalParse(pos, 2, &compile);
	GetEvalParse(pos, 3, &load);
	GetEvalParse(pos, 4, &exec);
	GetEvalParse(pos, 5, &toplevel);
	GetEvalParse(pos, 6, &mode);

	Return(optparse_implicit_declare_(str, Nil, cons, &cons, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_EVAL_WHEN, 7);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, cons);
	SetEvalParse(pos, 2, compile);
	SetEvalParse(pos, 3, load);
	SetEvalParse(pos, 4, exec);
	SetEvalParse(pos, 5, toplevel);
	SetEvalParse(pos, 6, mode);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-eval-when */
int checkparse_eval_when_(OptimizeInfo *str, int *ret)
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
int optparse_eval_when_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_eval_when_run_);
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
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */
	return Result(ret, empty_nil_declare(decl) && body != Nil);
}

static int optparse_locally1_(OptimizeInfo *str, int *ret)
{
	addr form, pos;

	Return_check_optparse(checkparse_locally1_, str, ret);
	GetEvalParse(str->pos, 0, &form); /* form */
	GetEvalParse(str->pos, 2, &pos); /* body */
	eval_parse2_local(str->local, &pos, EVAL_PARSE_PROGN, form, pos);
	str->pos = pos;

	return Result(ret, 1);
}

/* (locally (declare ...)) -> nil */
static int checkparse_locally2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return Result(ret, 0);
	GetEvalParse(str->pos, 2, &pos); /* body */
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
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_locally_all_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, decl, body;

	Return_check_optparse(checkparse_locally_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-locally */
int checkparse_locally_(OptimizeInfo *str, int *ret)
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
int optparse_locally_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(pos, 1, &pos); /* call */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_call1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, call, cons;

	Return_check_optparse(checkparse_call1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &cons);

	Return(optparse_inplace_(str, call, &call, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, cons);
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
	GetEvalParse(list, 2, &list); /* cons */
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
	addr pos, form, call, list, root, x;
	LocalRoot local;

	Return_check_optparse(checkparse_call_all_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &list);

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

	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, list);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-call */
int checkparse_call_(OptimizeInfo *str, int *ret)
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
int optparse_call_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_call_run_);
}


/*
 *  progv
 */
/* symbols */
static int checkparse_progv1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_PROGV))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* symbols */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_progv1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, symbols, values, body;

	Return_check_optparse(checkparse_progv1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &symbols);
	GetEvalParse(pos, 2, &values);
	GetEvalParse(pos, 3, &body);

	Return(optparse_inplace_(str, symbols, &symbols, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_PROGV, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, symbols);
	SetEvalParse(pos, 2, values);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* values */
static int checkparse_progv2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_PROGV))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* values */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_progv2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, symbols, values, body;

	Return_check_optparse(checkparse_progv2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &symbols);
	GetEvalParse(pos, 2, &values);
	GetEvalParse(pos, 3, &body);

	Return(optparse_inplace_(str, values, &values, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_PROGV, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, symbols);
	SetEvalParse(pos, 2, values);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_progv3_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_PROGV))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_progv3_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, symbols, values, body;

	Return_check_optparse(checkparse_progv3_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &symbols);
	GetEvalParse(pos, 2, &values);
	GetEvalParse(pos, 3, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_PROGV, 4);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, symbols);
	SetEvalParse(pos, 2, values);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-progv */
int checkparse_progv_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_progv1_, str, ret);
	Return_or_optparse(checkparse_progv2_, str, ret);
	Return_or_optparse(checkparse_progv3_, str, ret);

	return Result(ret, 0);
}

static int optparse_progv_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_progv1_));
	Return(optimize_extract_(str, optparse_progv2_));
	Return(optimize_extract_(str, optparse_progv3_));

	return 0;
}
int optparse_progv_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_progv_run_);
}

