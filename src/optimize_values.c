#include "parse_object.h"
#include "optimize_parse.h"
#include "optimize_values.h"
#include "typedef.h"

/*
 *  values
 */
int checkparse_values_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_VALUES))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos);
	return checkparse_implicit_all_(str, pos, ret);
}

int optparse_values_(OptimizeInfo *str, int *ret)
{
	int check;
	addr form, pos;

	Return_check_optparse(checkparse_values_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 1, &pos);
	Return(optparse_implicit_all_(str, pos, &pos, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse2_local(str->local, &pos, EVAL_PARSE_VALUES, form, pos);
	str->pos = pos;

	return Result(ret, 1);
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
	GetEvalParse(str->pos, 2, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_bind1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, vars, expr, decl, doc, body;

	Return_check_optparse(checkparse_multiple_value_bind1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &vars);
	GetEvalParse(pos, 2, &expr);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, vars);
	SetEvalParse(pos, 2, expr);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
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
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 5, &body);
	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_multiple_value_bind2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, vars, expr, decl, doc, body;

	Return_check_optparse(checkparse_multiple_value_bind2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &vars);
	GetEvalParse(pos, 2, &expr);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, vars);
	SetEvalParse(pos, 2, expr);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-bind */
int checkparse_multiple_value_bind_(OptimizeInfo *str, int *ret)
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
int optparse_multiple_value_bind_(OptimizeInfo *str, int *ret)
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
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_call1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, call, body;

	Return_check_optparse(checkparse_multiple_value_call1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &body);

	Return(optparse_inplace_(str, call, &call, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, body);
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
	GetEvalParse(pos, 2, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_multiple_value_call2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, call, body;

	Return_check_optparse(checkparse_multiple_value_call2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &call);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, call);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-call */
int checkparse_multiple_value_call_(OptimizeInfo *str, int *ret)
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
int optparse_multiple_value_call_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_call_run_);
}


/*
 *  multiple-value-prog1
 */
/* expr */
static int checkparse_multiple_value_prog1_1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_multiple_value_prog1_1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, expr, body;

	Return_check_optparse(checkparse_multiple_value_prog1_1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &body);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* body */
static int checkparse_multiple_value_prog1_2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* body */
	return checkparse_implicit_all_(str, pos, ret);
}

static int optparse_multiple_value_prog1_2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, expr, body;

	Return_check_optparse(checkparse_multiple_value_prog1_2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &body);

	Return(optparse_implicit_all_(str, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-multiple-value-prog1 */
int checkparse_multiple_value_prog1_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_multiple_value_prog1_1_, str, ret);
	Return_or_optparse(checkparse_multiple_value_prog1_2_, str, ret);

	return Result(ret, 0);
}

static int optparse_multiple_value_prog1_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_multiple_value_prog1_1_));
	Return(optimize_extract_(str, optparse_multiple_value_prog1_2_));

	return 0;
}
int optparse_multiple_value_prog1_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_multiple_value_prog1_run_);
}


/*
 *  nth-value
 */
/* nth */
static int checkparse_nth_value_1_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_NTH_VALUE))
		return Result(ret, 0);
	GetEvalParse(pos, 1, &pos); /* nth */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_nth_value_1_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, nth, expr;

	Return_check_optparse(checkparse_nth_value_1_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &nth);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, nth, &nth, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_NTH_VALUE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, nth);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* expr */
static int checkparse_nth_value_2_(OptimizeInfo *str, int *ret)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_NTH_VALUE))
		return Result(ret, 0);
	GetEvalParse(pos, 2, &pos); /* expr */
	return checkparse_inplace_(str, pos, ret);
}

static int optparse_nth_value_2_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, form, nth, expr;

	Return_check_optparse(checkparse_nth_value_2_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &nth);
	GetEvalParse(pos, 2, &expr);

	Return(optparse_inplace_(str, expr, &expr, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_NTH_VALUE, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, nth);
	SetEvalParse(pos, 2, expr);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-nth-value */
int checkparse_nth_value_(OptimizeInfo *str, int *ret)
{
	Return_or_optparse(checkparse_nth_value_1_, str, ret);
	Return_or_optparse(checkparse_nth_value_2_, str, ret);

	return Result(ret, 0);
}

static int optparse_nth_value_run_(OptimizeInfo *str)
{
	Return(optimize_extract_(str, optparse_nth_value_1_));
	Return(optimize_extract_(str, optparse_nth_value_2_));

	return 0;
}
int optparse_nth_value_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_nth_value_run_);
}

