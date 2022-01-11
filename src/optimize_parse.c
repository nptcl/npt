#include "callname.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "eval_copy.h"
#include "optimize.h"
#include "optimize_call.h"
#include "optimize_define.h"
#include "optimize_parse.h"
#include "optimize_values.h"
#include "parse_object.h"
#include "strtype.h"
#include "typedef.h"

static int checkparse_all_(OptimizeInfo *str, int *ret);
static int optparse_all_(OptimizeInfo *str, int *ret);

int checkparse_inplace_(OptimizeInfo *str, addr pos, int *ret)
{
	int check;
	OptimizeInfo save;

	save = *str;
	str->pos = pos;
	Return(checkparse_all_(str, &check));
	*str = save;

	return Result(ret, check);
}

int optparse_inplace_(OptimizeInfo *str, addr pos, addr *value, int *ret)
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

int checkparse_implicit_declare_(
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

int optparse_implicit_declare_(OptimizeInfo *str,
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

int optparse_run_(OptimizeInfo *str, int *ret, int (*call)(OptimizeInfo *))
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
	GetEvalParse(call, 1, &left);
	/* function */
	if (! optimize_evaltype(left, EVAL_PARSE_FUNCTION))
		goto skip;
	/* function name */
	GetEvalParse(left, 1, &left);
	GetCallName(left, &left);
	GetConst(SYSTEM_OPTIMIZE_CHECK, &right);
	if (left != right)
		goto skip;
	/* argument */
	GetEvalParse(call, 2, &right);
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
	addr symbol, x, list, quote, form;

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
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&form, quote, list, NULL);
	eval_parse2_heap(&list, EVAL_PARSE_QUOTE, form, list);
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

	GetEvalParse(list, 1, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! optimize_value_function(pos, functionp))
			return 0;
	}

	return 1;
}

static int optmize_value_the(addr pos, int functionp)
{
	GetEvalParse(pos, 2, &pos); /* expr */
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

int optimize_value_and_function(addr pos)
{
	return optimize_value_function(pos, 1);
}

int optimize_value_only(addr pos)
{
	return optimize_value_function(pos, 0);
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
	Return_or_optparse(checkparse_multiple_value_prog1_, str, ret);
	Return_or_optparse(checkparse_nth_value_, str, ret);
	Return_or_optparse(checkparse_progv_, str, ret);

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
	Return(optimize_extract_(str, optparse_multiple_value_prog1_));
	Return(optimize_extract_(str, optparse_nth_value_));
	Return(optimize_extract_(str, optparse_progv_));

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

