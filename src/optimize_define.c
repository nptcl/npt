#include "cons.h"
#include "cons_list.h"
#include "local.h"
#include "object.h"
#include "optimize_define.h"
#include "optimize_parse.h"
#include "parse_object.h"
#include "typedef.h"

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
	GetEvalParse(pos, 1, &pos);
	return checkparse_lambda_ordinary_(str, pos, ret);
}

static int optparse_lambda_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, args, decl, doc, body, form;

	Return_check_optparse(checkparse_lambda_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_lambda_ordinary_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
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
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
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
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-lambda */
int checkparse_lambda_(OptimizeInfo *str, int *ret)
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
int optparse_lambda_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_lambda_run_);
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
	GetEvalParse(pos, 2, &pos); /* args */
	return checkparse_lambda_ordinary_(str, pos, ret);
}

static int optparse_defun_args_(OptimizeInfo *str, int *ret)
{
	int check;
	addr pos, name, args, decl, doc, body, form;

	Return_check_optparse(checkparse_defun_args_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &args);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_lambda_ordinary_(str, args, &args, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, args);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
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
	GetEvalParse(pos, 3, &decl); /* decl */
	GetEvalParse(pos, 5, &body); /* body */
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
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &name);
	GetEvalParse(pos, 2, &args);
	GetEvalParse(pos, 3, &decl);
	GetEvalParse(pos, 4, &doc);
	GetEvalParse(pos, 5, &body);

	Return(optparse_implicit_declare_(str, decl, body, &body, &check));
	if (! check)
		return Result(ret, 0);
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, name);
	SetEvalParse(pos, 2, args);
	SetEvalParse(pos, 3, decl);
	SetEvalParse(pos, 4, doc);
	SetEvalParse(pos, 5, body);
	str->pos = pos;

	return Result(ret, 1);
}

/* optparse-defun */
int checkparse_defun_(OptimizeInfo *str, int *ret)
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
int optparse_defun_(OptimizeInfo *str, int *ret)
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
int checkparse_defmacro_(OptimizeInfo *str, int *ret)
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
int optparse_defmacro_(OptimizeInfo *str, int *ret)
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
int checkparse_deftype_(OptimizeInfo *str, int *ret)
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
int optparse_deftype_(OptimizeInfo *str, int *ret)
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
int checkparse_define_compiler_macro_(OptimizeInfo *str, int *ret)
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
int optparse_define_compiler_macro_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_define_compiler_macro_run_);
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
	GetEvalParse(str->pos, 3, &pos);
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
	return optimize_value_only(pos);
}

static int checkparse_flet2_(OptimizeInfo *str, int *ret)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 2, &check);
	if (! empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &pos);
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
	GetEvalParse(str->pos, 3, &list);
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
	GetEvalParse(pos, 1, &check);
	if (check != Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 2, &check);
	if (! empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &check);
	return Result(ret, check != Nil);
}

static int optparse_flet3_(OptimizeInfo *str, int *ret)
{
	addr form, pos;

	Return_check_optparse(checkparse_flet3_, str, ret);
	GetEvalParse(str->pos, 0, &form);
	GetEvalParse(str->pos, 3, &pos);
	eval_parse2_local(str->local, &pos, EVAL_PARSE_PROGN, form, pos);
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
	GetEvalParse(pos, 1, &check);
	if (check != Nil)
		return Result(ret, 0);
	GetEvalParse(pos, 2, &check);
	if (empty_nil_declare(check))
		return Result(ret, 0);
	GetEvalParse(pos, 3, &check);
	return Result(ret, check != Nil);
}

static int optparse_flet4_(OptimizeInfo *str, int *ret)
{
	addr pos, form, decl, cons;

	Return_check_optparse(checkparse_flet4_, str, ret);
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &cons);

	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, cons);
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
	GetEvalParse(str->pos, 1, &pos);
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
	addr pos, form, args, decl, body;

	Return_check_optparse(checkparse_flet_args_, str, ret);
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);

	Return(optparse_flet_one_(str, args, &args, &check));
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

/* flet-body */
static int checkparse_flet_body_(OptimizeInfo *str, int *ret)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return Result(ret, 0);
	pos = str->pos;
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &body);
	if (body == Nil)
		return Result(ret, 0);

	return checkparse_implicit_declare_(str, decl, body, ret);
}

static int optparse_flet_body_(OptimizeInfo *str, int *ret)
{
	int check;
	EvalParse type;
	addr pos, form, args, decl, body;

	Return_check_optparse(checkparse_flet_body_, str, ret);
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

/* optparse-flet */
int checkparse_flet_(OptimizeInfo *str, int *ret)
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
int optparse_flet_(OptimizeInfo *str, int *ret)
{
	return optparse_run_(str, ret, optparse_flet_run_);
}

