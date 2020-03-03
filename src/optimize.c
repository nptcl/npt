#include "eval_copy.h"
#include "optimize.h"

#if 0
/*
 *  catch / throw
 */
/* throw */
static int checkparse_throw(optvalue opt, addr pos)
{
	addr check;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_THROW)) return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse(opt, check)) return 1;
	GetEvalParse(pos, 1, &check);
	return checkparse(opt, check);
}
static int optparse_throw(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, expr;

	if (! checkparse_throw(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &expr);

	optparse(local, &name, opt, name);
	optparse(local, &expr, opt, expr);
	eval_parse_local(local, &pos, EVAL_PARSE_THROW, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, expr);
	*ret = pos;

	return 1;
}

/* catch / throw */
static int checkparse_catch(optvalue opt, addr pos)
{
	return checkparse_catch1(opt, pos)
		|| checkparse_catch2(opt, pos)
		|| checkparse_catch_all(opt, pos)
		|| checkparse_throw(opt, pos);
}
static int optparse_catch(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_catch1);
		optparse_extract(local, &pos, &update, opt, optparse_catch2);
		optparse_extract(local, &pos, &update, opt, optparse_catch_all);
		optparse_extract(local, &pos, &update, opt, optparse_throw);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  flet / lables
 */
static int check_fletlabels(addr pos)
{
	enum EVAL_PARSE type;

	if (! eval_parse_p(pos)) return 0;
	GetEvalParseType(pos, &type);

	return type == EVAL_PARSE_FLET || type == EVAL_PARSE_LABELS;
}
static int check_fletlabels_on(optvalue opt, addr pos)
{
	return speed_on(opt) && check_fletlabels(pos);
}

/* (flet ()) -> nil */
static int checkparse_flet1(optvalue opt, addr pos)
{
	if (! check_fletlabels_on(opt, pos)) return 0;
	GetEvalParse(pos, 2, &pos);
	return pos == Nil;
}
static int optparse_flet1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_flet1(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* (flet () values... x) -> x */
static int checkparse_flet2(optvalue opt, addr pos)
{
	addr check;

	if (! check_fletlabels_on(opt, pos)) return 0;
	GetEvalParse(pos, 1, &check);
	if (! empty_nil_declare(check)) return 0;
	GetEvalParse(pos, 2, &pos);
	if (pos == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! checkvalue(check)) return 0;
	}

	return 1;
}
static int optparse_flet2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_flet2(opt, pos)) return 0;
	GetEvalParse(pos, 2, &pos);
	if (pos == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, ret, &pos);
	}

	return 1;
}

/* (flet () ...) -> (progn ...) */
static int checkparse_flet3(optvalue opt, addr pos)
{
	addr check;

	if (! check_fletlabels_on(opt, pos)) return 0;
	GetEvalParse(pos, 0, &check);
	if (check != Nil) return 0;
	GetEvalParse(pos, 1, &check);
	if (! empty_nil_declare(check)) return 0;
	GetEvalParse(pos, 2, &check);

	return check != Nil;
}
static int optparse_flet3(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_flet3(opt, pos)) return 0;
	GetEvalParse(pos, 2, &pos);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, pos);
	return 1;
}

/* (flet () (declare ...) ...) -> (locally (declare ...) ...) */
static int checkparse_flet4(optvalue opt, addr pos)
{
	addr check;

	if (! check_fletlabels_on(opt, pos)) return 0;
	GetEvalParse(pos, 0, &check);
	if (check != Nil) return 0;
	GetEvalParse(pos, 1, &check);
	if (empty_nil_declare(check)) return 0;
	GetEvalParse(pos, 2, &check);
	return check != Nil;
}
static int optparse_flet4(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr decl, cons;

	if (! checkparse_flet4(opt, pos)) return 0;
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);

	eval_parse_local(local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* flet-args */
static int checkparse_flet_args(optvalue opt, addr pos)
{
	addr check, args, decl;

	/* Don't check optimize. */
	if (! check_fletlabels(pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		GetCdr(check, &check); /* name */
		GetCons(check, &args, &check); /* args */
		GetCons(check, &decl, &check); /* decl */
		GetCdr(check, &check); /* doc */
		GetCar(check, &check);
		if (checkparse_lambda_ordinary(opt, args)) return 1;
		if (checkparse_implicit_declare(opt, decl, check)) return 1;
	}

	return 0;
}
static void optparse_flet_one(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int check1, check2;
	addr root, name, args, decl, doc, cons, check;

	for (root = Nil; pos != Nil; ) {
		GetCons(pos, &check, &pos);
		GetCons(check, &name, &cons);
		GetCons(cons, &args, &cons);
		GetCons(cons, &decl, &cons);
		GetCons(cons, &doc, &cons);
		GetCar(cons, &cons);
		check1 = checkparse_lambda_ordinary(opt, args);
		check2 = checkparse_implicit_declare(opt, decl, cons);
		if (check1)
			optparse_lambda_ordinary(local, &args, opt, args);
		if (check2)
			optparse_implicit_declare(local, &cons, opt, decl, cons);
		if (check1 || check2)
			list_local(local, &check, name, args, decl, doc, cons, NULL);
		cons_local(local, &root, check, root);
	}
	nreverse_list_unsafe(ret, root);
}
static int optparse_flet_args(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	if (! checkparse_flet_args(opt, pos)) return 0;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);

	optparse_flet_one(local, &args, opt, args);
	eval_parse_local(local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, cons);
	*ret = pos;

	return 1;
}

/* flet-body */
static int checkparse_flet_body(optvalue opt, addr pos)
{
	addr decl, cons;

	/* Don't check optimize. */
	if (! check_fletlabels(pos)) return 0;
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);
	if (cons == Nil) return 0;

	return checkparse_implicit_declare(opt, decl, cons);
}
static int optparse_flet_body(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	if (! checkparse_flet_body(opt, pos)) return 0;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);

	optparse_implicit_declare(local, &cons, opt, decl, cons);
	eval_parse_local(local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, cons);
	*ret = pos;

	return 1;
}

/* flet */
static int checkparse_flet(optvalue opt, addr pos)
{
	return checkparse_flet1(opt, pos)
		|| checkparse_flet2(opt, pos)
		|| checkparse_flet3(opt, pos)
		|| checkparse_flet4(opt, pos)
		|| checkparse_flet_args(opt, pos)
		|| checkparse_flet_body(opt, pos);
}
static int optparse_flet(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_flet1);
		optparse_extract(local, &pos, &update, opt, optparse_flet2);
		optparse_extract(local, &pos, &update, opt, optparse_flet3);
		optparse_extract(local, &pos, &update, opt, optparse_flet4);
		optparse_extract(local, &pos, &update, opt, optparse_flet_args);
		optparse_extract(local, &pos, &update, opt, optparse_flet_body);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  the
 */
/* (the type expr) -> (the [type] expr) */
static int checkparse_the1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_THE)) return 0;
	GetEvalParse(pos, 0, &pos); /* type */
	/* return ! type_optimized_or_subtypep(pos); */
	return ! type_optimized_p(pos);
}
static int optparse_the1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr type, expr;

	if (! checkparse_the1(opt, pos)) return 0;
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &expr);

	type_optimize_local(local, &type, type);
	eval_parse_local(local, &pos, EVAL_PARSE_THE, 2);
	SetEvalParse(pos, 0, type);
	SetEvalParse(pos, 1, expr);
	*ret = pos;

	return 1;
}

/* expr */
static int checkparse_the2(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_THE)) return 0;
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse(opt, pos);
}
static int optparse_the2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr type, expr;

	if (! checkparse_the2(opt, pos)) return 0;
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &expr);

	optparse(local, &expr, opt, expr);
	eval_parse_local(local, &pos, EVAL_PARSE_THE, 2);
	SetEvalParse(pos, 0, type);
	SetEvalParse(pos, 1, expr);
	*ret = pos;

	return 1;
}

/* the */
static int checkparse_the(optvalue opt, addr pos)
{
	return checkparse_the1(opt, pos)
		|| checkparse_the2(opt, pos);
}
static int optparse_the(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_the1);
		optparse_extract(local, &pos, &update, opt, optparse_the2);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  eval-when
 */
/* (eval-when cons) -> nil */
static int checkparse_eval_when1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_EVAL_WHEN)) return 0;
	GetEvalParse(pos, 0, &pos); /* cons */
	return pos == Nil;
}
static int optparse_eval_when1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_eval_when1(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* all */
static int checkparse_eval_when_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_EVAL_WHEN)) return 0;
	GetEvalParse(pos, 0, &pos); /* cons */
	return checkparse_implicit(opt, pos);
}
static int optparse_eval_when_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr cons, compilep, loadp, evalp;

	if (! checkparse_eval_when_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &cons);
	GetEvalParse(pos, 1, &compilep);
	GetEvalParse(pos, 2, &loadp);
	GetEvalParse(pos, 3, &evalp);

	optparse_implicit(local, &cons, opt, cons);
	eval_parse_heap(&pos, EVAL_PARSE_EVAL_WHEN, 4);
	SetEvalParse(pos, 0, cons);
	SetEvalParse(pos, 1, compilep);
	SetEvalParse(pos, 2, loadp);
	SetEvalParse(pos, 3, evalp);
	*ret = pos;

	return 1;
}

/* eval-when */
static int checkparse_eval_when(optvalue opt, addr pos)
{
	return checkparse_eval_when1(opt, pos)
		|| checkparse_eval_when_all(opt, pos);
}
static int optparse_eval_when(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_eval_when1);
		optparse_extract(local, &pos, &update, opt, optparse_eval_when_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  values
 */
static int checkparse_values(optvalue opt, addr pos)
{
	addr check;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_VALUES)) return 0;
	GetEvalParse(pos, 0, &pos);
	/* Don't use implicit function. */
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (checkparse(opt, check)) return 1;
	}

	return 0;
}
static int optparse_values(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr root, check;

	if (! checkparse_values(opt, pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	for (root = Nil; pos != Nil; ) {
		GetCons(pos, &check, &pos);
		optparse(local, &check, opt, check);
		cons_local(local, &root, check, root);
	}
	nreverse_list_unsafe(&pos, root);
	eval_single_parse_local(local, ret, EVAL_PARSE_VALUES, pos);

	return 1;
}


/*
 *  locally
 */
/* (locally ...) -> (progn ...) */
static int checkparse_locally1(optvalue opt, addr pos)
{
	addr decl, cons;

	if (! check_evaltype_on(opt, pos, EVAL_PARSE_LOCALLY)) return 0;
	GetEvalParse(pos, 0, &decl); /* decl */
	GetEvalParse(pos, 1, &cons); /* decl */
	return empty_nil_declare(decl) && cons != Nil;
}
static int optparse_locally1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_locally1(opt, pos)) return 0;
	GetEvalParse(pos, 1, &pos); /* cons */
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, pos);
	return 1;
}

/* (locally (declare ...)) -> nil */
static int checkparse_locally2(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_LOCALLY)) return 0;
	GetEvalParse(pos, 1, &pos); /* cons */
	return pos == Nil;
}
static int optparse_locally2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_locally2(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* all */
static int checkparse_locally_all(optvalue opt, addr pos)
{
	addr decl, cons;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_LOCALLY)) return 0;
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &cons);

	return checkparse_implicit_declare(opt, decl, cons);
}
static int optparse_locally_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr decl, cons;

	if (! checkparse_locally_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &cons);

	optparse_implicit_declare(local, &cons, opt, decl, cons);
	eval_parse_heap(&pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* locally */
static int checkparse_locally(optvalue opt, addr pos)
{
	return checkparse_locally1(opt, pos)
		|| checkparse_locally2(opt, pos)
		|| checkparse_locally_all(opt, pos);
}
static int optparse_locally(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_locally1);
		optparse_extract(local, &pos, &update, opt, optparse_locally2);
		optparse_extract(local, &pos, &update, opt, optparse_locally_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  multiple-value-bind
 */
/* expr argument */
static int checkparse_multiple_value_bind_expr(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND)) return 0;
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse(opt, pos);
}
static int optparse_multiple_value_bind_expr(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	addr vars, expr, decl, doc, cons;

	if (! checkparse_multiple_value_bind_expr(opt, pos)) return 0;
	GetEvalParse(pos, 0, &vars);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &cons);

	optparse(local, &expr, opt, expr);
	eval_parse_heap(&pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(pos, 0, vars);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, cons);
	*ret = pos;

	return 1;
}

/* all */
static int checkparse_multiple_value_bind_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND)) return 0;
	GetEvalParse(pos, 4, &pos); /* cons */
	return checkparse_implicit(opt, pos);
}
static int optparse_multiple_value_bind_all(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	addr vars, expr, decl, doc, cons;

	if (! checkparse_multiple_value_bind_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &vars);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &cons);

	optparse_implicit(local, &cons, opt, cons);
	eval_parse_heap(&pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 4);
	SetEvalParse(pos, 0, vars);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, cons);
	*ret = pos;

	return 1;
}

/* multiple_value_bind */
static int checkparse_multiple_value_bind(optvalue opt, addr pos)
{
	return checkparse_multiple_value_bind_expr(opt, pos)
		|| checkparse_multiple_value_bind_all(opt, pos);
}
static int optparse_multiple_value_bind(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_multiple_value_bind_expr);
		optparse_extract(local, &pos, &update, opt, optparse_multiple_value_bind_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  multiple-value-call
 */
/* expr argument */
static int checkparse_multiple_value_call_expr(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL)) return 0;
	GetEvalParse(pos, 0, &pos); /* expr */
	return checkparse(opt, pos);
}
static int optparse_multiple_value_call_expr(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	addr call, cons;

	if (! checkparse_multiple_value_call_expr(opt, pos)) return 0;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &cons);

	optparse(local, &call, opt, call);
	eval_parse_heap(&pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* all */
static int checkparse_multiple_value_call_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL)) return 0;
	GetEvalParse(pos, 1, &pos); /* cons */
	return checkparse_implicit(opt, pos);
}
static int optparse_multiple_value_call_all(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	addr call, cons;

	if (! checkparse_multiple_value_call_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &cons);

	optparse_implicit(local, &cons, opt, cons);
	eval_parse_heap(&pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* multiple_value_call */
static int checkparse_multiple_value_call(optvalue opt, addr pos)
{
	return checkparse_multiple_value_call_expr(opt, pos)
		|| checkparse_multiple_value_call_all(opt, pos);
}
static int optparse_multiple_value_call(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_multiple_value_call_expr);
		optparse_extract(local, &pos, &update, opt, optparse_multiple_value_call_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  optimize
 */
static int checkparse(optvalue opt, addr pos)
{
	return
		|| checkparse_let(opt, pos)
		|| checkparse_setq(opt, pos)
		|| checkparse_defun(opt, pos)
		|| checkparse_defmacro(opt, pos)
		|| checkparse_lambda(opt, pos)
		|| checkparse_if(opt, pos)
		|| checkparse_unwind_protect(opt, pos)
		|| checkparse_tagbody(opt, pos)
		|| checkparse_block(opt, pos)
		|| checkparse_catch(opt, pos)
		|| checkparse_flet(opt, pos)
		|| checkparse_the(opt, pos)
		|| checkparse_eval_when(opt, pos)
		|| checkparse_values(opt, pos)
		|| checkparse_locally(opt, pos)
		|| checkparse_call(opt, pos)
		|| checkparse_multiple_value_bind(opt, pos)
		|| checkparse_multiple_value_call(opt, pos);
}

static int optparse(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_let);
		optparse_extract(local, &pos, &update, opt, optparse_setq);
		optparse_extract(local, &pos, &update, opt, optparse_defun);
		optparse_extract(local, &pos, &update, opt, optparse_defmacro);
		optparse_extract(local, &pos, &update, opt, optparse_lambda);
		optparse_extract(local, &pos, &update, opt, optparse_if);
		optparse_extract(local, &pos, &update, opt, optparse_unwind_protect);
		optparse_extract(local, &pos, &update, opt, optparse_tagbody);
		optparse_extract(local, &pos, &update, opt, optparse_block);
		optparse_extract(local, &pos, &update, opt, optparse_catch);
		optparse_extract(local, &pos, &update, opt, optparse_flet);
		optparse_extract(local, &pos, &update, opt, optparse_the);
		optparse_extract(local, &pos, &update, opt, optparse_eval_when);
		optparse_extract(local, &pos, &update, opt, optparse_values);
		optparse_extract(local, &pos, &update, opt, optparse_locally);
		optparse_extract(local, &pos, &update, opt, optparse_call);
		optparse_extract(local, &pos, &update, opt, optparse_multiple_value_bind);
		optparse_extract(local, &pos, &update, opt, optparse_multiple_value_call);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}

_g int eval_optparse(LocalRoot local, addr *ret, addr pos)
{
	int result;
	addr check;
	LocalStack stack;
	struct optstruct opt;

	CheckLocal(local);
	push_local(local, &stack);
	copy_eval_parse_local(local, &check, pos);
	initialize_optstruct(&opt);
	result = optparse(local, &check, &opt, check);
	if (result)
		copy_eval_parse_heap(ret, check);
	else
		*ret = pos;
	rollback_local(local, stack);

	return result;
}
#endif

_g void save_optimize_value(const struct optimize_struct *str,
		struct optimize_value *save)
{
	memcpy(save, &(str->value), sizeoft(struct optimize_value));
}
_g void rollback_optimize_value(struct optimize_struct *str,
		const struct optimize_value *save)
{
	memcpy(str->value.local, save->local, sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
}

static int optimize_declare_value(struct optimize_value *opt, int index)
{
	OptimizeType value;
	value = opt->local[index];
	return (value < 0)? opt->declaim[index]: value;
}
_g int optimize_speed_on(struct optimize_struct *str)
{
	/* (on -1 1 2 3) (off 0) */
	return optimize_declare_value(&str->value, EVAL_OPTIMIZE_SPEED) != 0;
}
_g int optimize_evaltype(addr pos, enum EVAL_PARSE type)
{
	return eval_parse_p(pos) && RefEvalParseType(pos) == type;
}
_g int optimize_evaltype_on(struct optimize_struct *str, enum EVAL_PARSE type)
{
	return optimize_speed_on(str) && optimize_evaltype(str->pos, type);
}

static void optimize_initialize_declare(struct optimize_value *value)
{
	int i;

	copy_optimize_declare(value->declaim);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		value->local[i] = -1;
}

_g void optimize_initialize(struct optimize_struct *str, LocalRoot local, addr pos)
{
	clearpoint(str);
	optimize_initialize_declare(&(str->value));
	str->local = local;
	copy_eval_parse_local(local, &(str->pos), pos);
}

_g int optimize_extract(struct optimize_struct *str, optimize_call call)
{
	int update;

	update = 0;
	while (call(str))
		update = 1;
	str->update |= update;

	return update;
}

