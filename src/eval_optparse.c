#include "cons.h"
#include "eval.h"
#include "eval_declare.h"
#include "eval_optparse.h"
#include "eval_parse.h"
#include "sequence.h"
#include "type_number.h"
#include "type_optimize.h"

/*
 *  optstruct
 */
struct optstruct {
	OptimizeType declaim[EVAL_OPTIMIZE_SIZE];
	OptimizeType local[EVAL_OPTIMIZE_SIZE];
};
typedef struct optstruct *optvalue;

static void initialize_optstruct(struct optstruct *opt)
{
	int i;

	copy_optimize_declare(opt->declaim);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		opt->local[i] = -1;
}
static void save_optstruct(const struct optstruct *opt, struct optstruct *ret)
{
	memcpy(ret, opt, sizeoft(struct optstruct));
}
static void rollback_optstruct(struct optstruct *opt, const struct optstruct *pos)
{
	memcpy(opt->local, pos->local, sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
}

static int optimize_value(optvalue opt, int index)
{
	OptimizeType value;
	value = opt->local[index];
	return (value < 0)? opt->declaim[index]: value;
}
static int speed_on(optvalue opt)
{
	/* (on -1 1 2 3) (off 0) */
	return optimize_value(opt, EVAL_OPTIMIZE_SPEED) != 0;
}


/*
 *  optparse
 */
static int checkparse(optvalue opt, addr pos);
static int optparse(LocalRoot local, addr *ret, optvalue opt, addr pos);

_g void optparse_extract(LocalRoot local,
		addr *pos,
		int *update,
		optvalue opt,
		int (*call)(LocalRoot, addr *, optvalue , addr))
{
	addr check;
	while (call(local, &check, opt, *pos)) {
		*update = 1;
		*pos = check;
	}
}

static int check_evaltype(addr pos, enum EVAL_PARSE type)
{
	return eval_parse_p(pos) && RefEvalParseType(pos) == type;
}
static int check_evaltype_on(optvalue opt, addr pos, enum EVAL_PARSE type)
{
	return speed_on(opt) && check_evaltype(pos, type);
}
static int check_lisptype_on(optvalue opt, addr pos, enum LISPTYPE type)
{
	return speed_on(opt) && GetType(pos) == type;
}

static int checkvalue(addr pos);
static int checkvalues(addr right)
{
	addr left;

	GetEvalParse(right, 0, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (! checkvalue(left)) return 0;
	}

	return 1;
}
static int checkvalue_the(addr pos)
{
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkvalue(pos);
}
static int checkvalue(addr pos)
{
	if (! eval_parse_p(pos)) return 0;
	switch (RefEvalParseType(pos)) {
		case EVAL_PARSE_NIL:
		case EVAL_PARSE_T:
		case EVAL_PARSE_INTEGER:
		case EVAL_PARSE_RATIONAL:
		case EVAL_PARSE_CHARACTER:
		case EVAL_PARSE_ARRAY:
		case EVAL_PARSE_VECTOR:
		case EVAL_PARSE_STRING:
		case EVAL_PARSE_FLOAT:
		case EVAL_PARSE_QUOTE:
		case EVAL_PARSE_FUNCTION:
		case EVAL_PARSE_LAMBDA:
			return 1;

		case EVAL_PARSE_VALUES:
			return checkvalues(pos);

		case EVAL_PARSE_THE:
			return checkvalue_the(pos);

		default:
			return 0;
	}
}


/*
 *  implicit
 */
/* (10 20 30) -> (30) */
static int checkparse_implicit3(optvalue opt, addr pos)
{
	addr check;

	if (! check_lisptype_on(opt, pos, LISPTYPE_CONS)) return 0;
	GetCdr(pos, &check);
	if (check == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! checkvalue(check)) return 0;
	}

	return 1;
}
static int optparse_implicit3(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr check;

	if (! checkparse_implicit3(opt, pos)) return 0;
	for (check = Nil; pos != Nil; ) {
		GetCons(pos, &check, &pos);
	}
	conscar_local(local, ret, check);

	return 1;
}

/* (10 (call1) 20 30 (call2)) -> ((call1) (call2)) */
static int checkparse_implicit4(optvalue opt, addr cons)
{
	int update1, update2, valuep;
	addr check;

	if (! check_lisptype_on(opt, cons, LISPTYPE_CONS)) return 0;
	update1 = update2 = 0;
	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		valuep = checkvalue(check);
		if (cons == Nil && ! valuep) update1 = 1;
		if (cons != Nil && valuep) update2 = 1;
	}

	return update1 && update2;
}
static int optparse_implicit4(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr root, check;

	if (! checkparse_implicit4(opt, pos)) return 0;
	for (root = Nil; pos != Nil; ) {
		GetCons(pos, &check, &pos);
		if (! checkvalue(check))
			cons_local(local, &root, check, root);
	}
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* (10 (call1) 20 (call2) 30 40) -> ((call1) (call2) 40) */
static int checkparse_implicit5(optvalue opt, addr cons)
{
	int update1, update2, update3, valuep;
	addr check;

	if (! check_lisptype_on(opt, cons, LISPTYPE_CONS)) return 0;
	update1 = update2 = update3 = 0;
	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		valuep = checkvalue(check);
		if (cons == Nil && valuep) update1 = 1;
		if (cons != Nil && valuep) update2 = 1;
		if (cons != Nil && ! valuep) update3 = 1;
	}

	return update1 && update2 && update3;
}
static int optparse_implicit5(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr root, check;

	if (! checkparse_implicit5(opt, pos)) return 0;
	for (root = Nil; pos != Nil; ) {
		GetCons(pos, &check, &pos);
		if (pos == Nil || ! checkvalue(check))
			cons_local(local, &root, check, root);
	}
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* (x (progn y) z) -> (x y z) */
static int checkparse_implicit6(optvalue opt, addr cons)
{
	addr pos;

	if (! check_lisptype_on(opt, cons, LISPTYPE_CONS)) return 0;
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (check_evaltype(pos, EVAL_PARSE_PROGN))
			return 1;
	}

	return 0;
}
static void remove_nest_exec(LocalRoot local, addr *ret, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (check_evaltype(pos, EVAL_PARSE_PROGN)) {
			GetEvalParse(pos, 0, &pos);
			remove_nest_exec(local, ret, pos);
			continue;
		}
		cons_local(local, ret, pos, *ret);
	}
}
static int optparse_implicit6(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr root;

	if (! checkparse_implicit6(opt, pos)) return 0;
	root = Nil;
	remove_nest_exec(local, &root, pos);
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* (...) */
static int checkparse_implicit_all(optvalue opt, addr cons)
{
	addr pos;

	/* Don't check optimize. */
	if (GetType(cons) != LISPTYPE_CONS) return 0;
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (checkparse(opt, pos)) return 1;
	}

	return 0;
}
static int optparse_implicit_all(LocalRoot local, addr *ret, optvalue opt, addr cons)
{
	addr root, pos;

	if (! checkparse_implicit_all(opt, cons)) return 0;
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		optparse(local, &pos, opt, pos);
		cons_local(local, &root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* implicit */
static int checkparse_implicit(optvalue opt, addr pos)
{
	return checkparse_implicit3(opt, pos)
		|| checkparse_implicit4(opt, pos)
		|| checkparse_implicit5(opt, pos)
		|| checkparse_implicit6(opt, pos)
		|| checkparse_implicit_all(opt, pos);
}
static int optparse_implicit(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_implicit3);
		optparse_extract(local, &pos, &update, opt, optparse_implicit4);
		optparse_extract(local, &pos, &update, opt, optparse_implicit5);
		optparse_extract(local, &pos, &update, opt, optparse_implicit6);
		optparse_extract(local, &pos, &update, opt, optparse_implicit_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  progn
 */
/* (progn) -> nil */
static int checkparse_progn1(optvalue opt, addr pos)
{
	return check_evaltype_on(opt, pos, EVAL_PARSE_PROGN)
		&& RefEvalParse(pos, 0) == Nil;
}
static int optparse_progn1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_progn1(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* (progn x) -> x */
static int checkparse_progn2(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_PROGN)) return 0;
	GetEvalParse(pos, 0, &pos);

	return singlep(pos);
}
static int optparse_progn2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_progn2(opt, pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	GetCar(pos, ret);

	return 1;
}

/* (progn 10 20 30) -> 30 */
static int checkparse_progn3(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_PROGN)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit3(opt, pos);
}
static int optparse_progn3(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_progn3(opt, pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	if (pos == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, ret, &pos);
	}

	return 1;
}

/* (progn 10 (call1) 20 30 (call2)) -> (progn (call1) (call2)) */
static int checkparse_progn4(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_PROGN)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit4(opt, pos);
}
static int optparse_progn4(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_progn4(opt, pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	optparse_implicit4(local, &pos, opt, pos);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, pos);

	return 1;
}

/* (progn 10 (call1) 20 (call2) 30 40) -> (progn (call1) (call2) 40) */
static int checkparse_progn5(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_PROGN)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit5(opt, pos);
}
static int optparse_progn5(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_progn5(opt, pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	optparse_implicit5(local, &pos, opt, pos);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, pos);

	return 1;
}

/* (progn x (progn y) z) -> (progn x y z) */
static int checkparse_progn6(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_PROGN)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit6(opt, pos);
}
static int optparse_progn6(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_progn6(opt, pos)) return 0;
	GetEvalParse(pos, 0, &pos);
	optparse_implicit6(local, &pos, opt, pos);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, pos);

	return 1;
}

/* (progn ...) */
static int checkparse_progn_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_PROGN)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit_all(opt, pos);
}
static int optparse_progn_all(LocalRoot local, addr *ret, optvalue opt, addr cons)
{
	if (! checkparse_progn_all(opt, cons)) return 0;
	GetEvalParse(cons, 0, &cons);
	optparse_implicit_all(local, &cons, opt, cons);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, cons);

	return 1;
}

/* progn */
static int checkparse_progn(optvalue opt, addr pos)
{
	return checkparse_progn1(opt, pos)
		|| checkparse_progn2(opt, pos)
		|| checkparse_progn3(opt, pos)
		|| checkparse_progn4(opt, pos)
		|| checkparse_progn5(opt, pos)
		|| checkparse_progn6(opt, pos)
		|| checkparse_progn_all(opt, pos);
}
static int optparse_progn(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_progn1);
		optparse_extract(local, &pos, &update, opt, optparse_progn2);
		optparse_extract(local, &pos, &update, opt, optparse_progn3);
		optparse_extract(local, &pos, &update, opt, optparse_progn4);
		optparse_extract(local, &pos, &update, opt, optparse_progn5);
		optparse_extract(local, &pos, &update, opt, optparse_progn6);
		optparse_extract(local, &pos, &update, opt, optparse_progn_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  let
 */
static int check_lettype(addr pos)
{
	enum EVAL_PARSE type;

	if (! eval_parse_p(pos)) return 0;
	GetEvalParseType(pos, &type);

	return type == EVAL_PARSE_LET || type == EVAL_PARSE_LETA;
}
static int check_lettype_on(optvalue opt, addr pos)
{
	return speed_on(opt) && check_lettype(pos);
}

/* (let nil . body) -> (progn ,@body) */
static int checkparse_let1(optvalue opt, addr pos)
{
	addr args, decl, cons;

	if (! check_lettype_on(opt, pos)) return 0;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &cons); /* cons */

	return args == Nil && empty_nil_declare(decl) && cons != Nil;
}
static int optparse_let1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_let1(opt, pos)) return 0;
	GetEvalParse(pos, 2, &pos); /* cons */
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, pos);

	return 1;
}

/* (let nil (declare ...) . body) -> (locally (declare ...) . body) */
static int checkparse_let2(optvalue opt, addr pos)
{
	addr args, decl, cons;

	if (! check_lettype_on(opt, pos)) return 0;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &cons); /* cons */

	return args == Nil && (! empty_nil_declare(decl)) && cons != Nil;
}
static int optparse_let2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr decl, cons;

	if (! checkparse_let2(opt, pos)) return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &cons); /* cons */

	eval_parse_local(local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* (let nil . nil) -> nil */
static int checkparse_let3(optvalue opt, addr pos)
{
	addr args, cons;

	if (! check_lettype_on(opt, pos)) return 0;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 2, &cons); /* cons */

	return args == Nil && cons == Nil;
}
static int optparse_let3(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_let3(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* (let (aaa bbb (ccc)) . nil) -> nil */
static int checkparse_let4(optvalue opt, addr pos)
{
	addr args, cons;

	if (! check_lettype_on(opt, pos)) return 0;
	GetEvalParse(pos, 0, &args); /* args */
	if (args == Nil) return 0;
	GetEvalParse(pos, 2, &cons); /* cons */
	if (cons != Nil) return 0;
	while (args != Nil) {
		GetCons(args, &cons, &args);
		GetCdr(cons, &cons); /* (var . init) */
		if (RefEvalParseType(cons) != EVAL_PARSE_NIL)
			return 0;
	}

	return 1;
}
static int optparse_let4(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_let4(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* let-args */
static int checkparse_let_args(optvalue opt, addr pos)
{
	addr check;

	/* Don't check optimize. */
	if (! check_lettype(pos)) return 0;
	GetEvalParse(pos, 0, &pos); /* args */
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		GetCdr(check, &check); /* (var . init) */
		if (checkparse(opt, check))
			return 1;
	}

	return 0;
}
static int optparse_let_args(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	enum EVAL_PARSE type;
	addr args, decl, cons, var, init, root;

	if (! checkparse_let_args(opt, pos)) return 0;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		optparse(local, &init, opt, init);
		cons_local(local, &var, var, init);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(&args, root);

	eval_parse_local(local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, cons);
	*ret = pos;

	return 1;
}

/* let-body */
static int checkparse_implicit_declare(optvalue opt, addr decl, addr cons)
{
	int result;
	struct optstruct save;

	save_optstruct(opt, &save);
	apply_array_declare(opt->local, decl);
	result = checkparse_implicit(opt, cons);
	rollback_optstruct(opt, &save);

	return result;
}

static int optparse_implicit_declare(LocalRoot local,
		addr *ret, optvalue opt, addr decl, addr cons)
{
	int result;
	struct optstruct save;

	save_optstruct(opt, &save);
	apply_array_declare(opt->local, decl);
	result = optparse_implicit(local, ret, opt, cons);
	rollback_optstruct(opt, &save);

	return result;
}

static int checkparse_let_body(optvalue opt, addr pos)
{
	addr decl, cons;

	/* Don't check optimize. */
	if (! check_lettype(pos)) return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &cons); /* cons */
	if (cons == Nil) return 0;

	return checkparse_implicit_declare(opt, decl, cons);
}
static int optparse_let_body(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	if (! checkparse_let_body(opt, pos)) return 0;
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

/* let */
static int checkparse_let(optvalue opt, addr pos)
{
	return checkparse_let1(opt, pos)
		|| checkparse_let2(opt, pos)
		|| checkparse_let3(opt, pos)
		|| checkparse_let4(opt, pos)
		|| checkparse_let_args(opt, pos)
		|| checkparse_let_body(opt, pos) ;
}
static int optparse_let(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_let1);
		optparse_extract(local, &pos, &update, opt, optparse_let2);
		optparse_extract(local, &pos, &update, opt, optparse_let3);
		optparse_extract(local, &pos, &update, opt, optparse_let4);
		optparse_extract(local, &pos, &update, opt, optparse_let_args);
		optparse_extract(local, &pos, &update, opt, optparse_let_body);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  setq
 */
/* (setq) -> nil */
static int checkparse_setq1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_SETQ)) return 0;
	return RefEvalParse(pos, 0) == Nil;
}
static int optparse_setq1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_setq1(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* setq-all */
static int checkparse_setq_all(optvalue opt, addr pos)
{
	addr expr;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_SETQ)) return 0;
	GetEvalParse(pos, 0, &pos);
	while (pos != Nil) {
		GetCons(pos, &expr, &pos);
		GetCdr(expr, &expr); /* (var . expr) */
		if (checkparse(opt, expr))
			return 1;
	}

	return 0;
}
static int optparse_setq_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr root, cons, var, expr;

	if (! checkparse_setq_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &cons);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(var, &var, &expr);
		optparse(local, &expr, opt, expr);
		cons_local(local, &var, var, expr);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(&cons, root);
	eval_single_parse_local(local, ret, EVAL_PARSE_SETQ, cons);

	return 1;
}

/* setq */
static int checkparse_setq(optvalue opt, addr pos)
{
	return checkparse_setq1(opt, pos)
		|| checkparse_setq_all(opt, pos);
}
static int optparse_setq(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_setq1);
		optparse_extract(local, &pos, &update, opt, optparse_setq_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  lambda-ordinary
 */
static int checkparse_opt(optvalue opt, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos); /* var */
		GetCar(pos, &pos); /* init */
		if (checkparse(opt, pos)) return 1;
	}

	return 0;
}
static void optparse_opt(LocalRoot local, addr *ret, optvalue opt, addr cons)
{
	addr root, var, init, svar;

	/* opt -> (var init svar) */
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &svar, &cons);
		GetCons(svar, &var, &svar);
		GetCons(svar, &init, &svar);
		GetCar(svar, &svar);
		optparse(local, &init, opt, init);
		list_local(local, &svar, var, init, svar, NULL);
		cons_local(local, &root, svar, root);
	}
	nreverse_list_unsafe(ret, root);
}

static int checkparse_key(optvalue opt, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos); /* var */
		GetCdr(pos, &pos); /* name */
		GetCar(pos, &pos); /* init */
		if (checkparse(opt, pos)) return 1;
	}

	return 0;
}
static void optparse_key(LocalRoot local, addr *ret, optvalue opt, addr cons)
{
	addr root, var, name, init, svar;

	/* key -> (var name init svar) */
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &svar, &cons);
		GetCons(svar, &var, &svar);
		GetCons(svar, &name, &svar);
		GetCons(svar, &init, &svar);
		GetCar(svar, &svar);
		optparse(local, &init, opt, init);
		list_local(local, &svar, var, name, init, svar, NULL);
		cons_local(local, &root, svar, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void optparse_aux(LocalRoot local, addr *ret, optvalue opt, addr cons)
{
	addr root, var, init;

	/* aux -> (var init) */
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &init, &cons);
		GetCons(init, &var, &init);
		GetCar(init, &init);
		optparse(local, &init, opt, init);
		list_local(local, &init, var, init, NULL);
		cons_local(local, &root, init, root);
	}
	nreverse_list_unsafe(ret, root);
}

static int checkparse_lambda_ordinary(optvalue opt, addr args)
{
	addr aopt, key, aux;

	GetCdr(args, &args);
	GetCons(args, &aopt, &args); /* optional */
	GetCdr(args, &args);
	GetCons(args, &key, &args); /* key */
	GetCdr(args, &args);
	GetCar(args, &aux); /* aux */

	return checkparse_opt(opt, aopt)
		|| checkparse_key(opt, key)
		|| checkparse_opt(opt, aux);
}
static void optparse_lambda_ordinary(LocalRoot local,
		addr *ret, optvalue opt, addr args)
{
	addr var, aopt, rest, key, allow, aux;

	/* args */
	GetCons(args, &var, &args);
	GetCons(args, &aopt, &args);
	GetCons(args, &rest, &args);
	GetCons(args, &key, &args);
	GetCons(args, &allow, &args);
	GetCar(args, &aux);

	/* optparse */
	if (checkparse_opt(opt, aopt))
		optparse_opt(local, &aopt, opt, aopt);
	if (checkparse_key(opt, key))
		optparse_key(local, &key, opt, key);
	if (checkparse_opt(opt, aux))
		optparse_aux(local, &aux, opt, aux);
	list_local(local, ret, var, aopt, rest, key, allow, aux, NULL);
}


/*
 *  defun
 */
static int checkparse_defun_args(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_DEFUN)) return 0;
	GetEvalParse(pos, 1, &pos);
	return checkparse_lambda_ordinary(opt, pos);
}
static int optparse_defun_args(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, args, decl, doc, body;

	if (! checkparse_defun_args(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	optparse_lambda_ordinary(local, &args, opt, args);
	eval_parse_local(local, &pos, EVAL_PARSE_DEFUN, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	*ret = pos;

	return 1;
}

static int checkparse_defun_body(optvalue opt, addr pos)
{
	addr decl, cons;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_DEFUN)) return 0;
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &cons); /* cons */
	if (cons == Nil) return 0;

	return checkparse_implicit_declare(opt, decl, cons);
}
static int optparse_defun_body(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, args, decl, doc, cons;

	if (! checkparse_defun_body(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &cons);

	optparse_implicit_declare(local, &cons, opt, decl, cons);
	eval_parse_local(local, &pos, EVAL_PARSE_DEFUN, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, cons);
	*ret = pos;

	return 1;
}

/* defun */
static int checkparse_defun(optvalue opt, addr pos)
{
	return checkparse_defun_args(opt, pos)
		|| checkparse_defun_body(opt, pos);
}
static int optparse_defun(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_defun_args);
		optparse_extract(local, &pos, &update, opt, optparse_defun_body);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  lambda-macro
 */
static int checkparse_lambda_macro(optvalue opt, addr args);
static int checkparse_macro_var(optvalue opt, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (consp(pos) && checkparse_lambda_macro(opt, pos))
			return 1;
	}

	return 0;
}

static void optparse_lambda_macro(LocalRoot, addr *, optvalue, addr);
static void optparse_macro_var(LocalRoot local, addr *ret, optvalue opt, addr list)
{
	addr root, var;

	/* var */
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var))
			optparse_lambda_macro(local, &var, opt, var);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static int checkparse_lambda_macro(optvalue opt, addr args)
{
	addr var, aopt, key, aux;

	GetCons(args, &var, &args);
	GetCons(args, &aopt, &args); /* optional */
	GetCdr(args, &args);
	GetCons(args, &key, &args); /* key */
	GetCdr(args, &args);
	GetCar(args, &aux); /* aux */

	return checkparse_macro_var(opt, var)
		|| checkparse_opt(opt, aopt)
		|| checkparse_key(opt, key)
		|| checkparse_opt(opt, aux);
}
static void optparse_lambda_macro(LocalRoot local, addr *ret, optvalue opt, addr args)
{
	addr var, aopt, rest, key, allow, aux, whole, env;

	/* args */
	list_bind(args, &var, &aopt, &rest, &key, &allow, &aux, &whole, &env, NULL);

	/* optparse */
	if (checkparse_macro_var(opt, var))
		optparse_macro_var(local, &aopt, opt, aopt);
	if (checkparse_opt(opt, aopt))
		optparse_opt(local, &aopt, opt, aopt);
	if (checkparse_key(opt, key))
		optparse_key(local, &key, opt, key);
	if (checkparse_opt(opt, aux))
		optparse_aux(local, &aux, opt, aux);
	list_local(local, ret, var, aopt, rest, key, allow, aux, whole, env, NULL);
}


/*
 *  defmacro
 */
static int checkparse_defmacro_args(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_DEFMACRO)) return 0;
	GetEvalParse(pos, 1, &pos);
	return checkparse_lambda_macro(opt, pos);
}
static int optparse_defmacro_args(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, args, decl, doc, body;

	if (! checkparse_defmacro_args(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	optparse_lambda_macro(local, &args, opt, args);
	eval_parse_local(local, &pos, EVAL_PARSE_DEFMACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	*ret = pos;

	return 1;
}

static int checkparse_defmacro_body(optvalue opt, addr pos)
{
	addr decl, cons;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_DEFMACRO)) return 0;
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &cons); /* cons */
	if (cons == Nil) return 0;

	return checkparse_implicit_declare(opt, decl, cons);
}
static int optparse_defmacro_body(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, args, decl, doc, cons;

	if (! checkparse_defmacro_body(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &cons);

	optparse_implicit_declare(local, &cons, opt, decl, cons);
	eval_parse_local(local, &pos, EVAL_PARSE_DEFMACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, cons);
	*ret = pos;

	return 1;
}

/* defmacro */
static int checkparse_defmacro(optvalue opt, addr pos)
{
	return checkparse_defmacro_args(opt, pos)
		|| checkparse_defmacro_body(opt, pos);
}
static int optparse_defmacro(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_defmacro_args);
		optparse_extract(local, &pos, &update, opt, optparse_defmacro_body);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  lambda
 */
static int checkparse_lambda_args(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_LAMBDA)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_lambda_ordinary(opt, pos);
}
static int optparse_lambda_args(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr args, decl, doc, body;

	if (! checkparse_lambda_args(opt, pos)) return 0;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);

	optparse_lambda_ordinary(local, &args, opt, args);
	eval_parse_local(local, &pos, EVAL_PARSE_LAMBDA, 4);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	*ret = pos;

	return 1;
}

static int checkparse_lambda_body(optvalue opt, addr pos)
{
	addr decl, cons;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_LAMBDA)) return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 3, &cons); /* cons */
	if (cons == Nil) return 0;

	return checkparse_implicit_declare(opt, decl, cons);
}
static int optparse_lambda_body(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr args, decl, doc, cons;

	if (! checkparse_lambda_body(opt, pos)) return 0;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &cons);

	optparse_implicit_declare(local, &cons, opt, decl, cons);
	eval_parse_local(local, &pos, EVAL_PARSE_LAMBDA, 4);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, cons);
	*ret = pos;

	return 1;
}

/* lambda */
static int checkparse_lambda(optvalue opt, addr pos)
{
	return checkparse_lambda_args(opt, pos)
		|| checkparse_lambda_body(opt, pos);
}
static int optparse_lambda(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_lambda_args);
		optparse_extract(local, &pos, &update, opt, optparse_lambda_body);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  if
 */
/* (if nil a b) -> b */
static int checkparse_if1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_IF)) return 0;
	GetEvalParse(pos, 0, &pos);
	return check_evaltype(pos, EVAL_PARSE_NIL);
}
static int optparse_if1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_if1(opt, pos)) return 0;
	GetEvalParse(pos, 2, ret);
	return 1;
}

/* (if x a b) -> a */
static int checkparse_if2(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_IF)) return 0;
	GetEvalParse(pos, 0, &pos);
	return (! check_evaltype(pos, EVAL_PARSE_NIL)) && checkvalue(pos);
}
static int optparse_if2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_if2(opt, pos)) return 0;
	GetEvalParse(pos, 1, ret);
	return 1;
}

/* all */
static int checkparse_if_all(optvalue opt, addr pos)
{
	addr check;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_IF)) return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse(opt, check)) return 1;
	GetEvalParse(pos, 1, &check);
	if (checkparse(opt, check)) return 1;
	GetEvalParse(pos, 2, &check);
	return checkparse(opt, check);
}
static int optparse_if_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr expr, ifthen, ifelse;

	if (! checkparse_if_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &ifthen);
	GetEvalParse(pos, 2, &ifelse);

	optparse(local, &expr, opt, expr);
	optparse(local, &ifthen, opt, ifthen);
	optparse(local, &ifelse, opt, ifelse);

	eval_parse_local(local, &pos, EVAL_PARSE_IF, 3);
	SetEvalParse(pos, 0, expr);
	SetEvalParse(pos, 1, ifthen);
	SetEvalParse(pos, 2, ifelse);
	*ret = pos;

	return 1;
}

/* if */
static int checkparse_if(optvalue opt, addr pos)
{
	return checkparse_if1(opt, pos)
		|| checkparse_if2(opt, pos)
		|| checkparse_if_all(opt, pos);
}
static int optparse_if(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_if1);
		optparse_extract(local, &pos, &update, opt, optparse_if2);
		optparse_extract(local, &pos, &update, opt, optparse_if_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  unwind-protect
 */
/* (unwind-protect nil . tail) -> (progn . tail) */
static int checkparse_unwind_protect1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_UNWIND_PROTECT)) return 0;
	GetEvalParse(pos, 0, &pos);
	return checkvalue(pos);
}
static int optparse_unwind_protect1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr form, cons, root;

	if (! checkparse_unwind_protect1(opt, pos)) return 0;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &cons);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		cons_local(local, &root, pos, root);
	}
	cons_local(local, &root, form, root);
	nreverse_list_unsafe(&cons, root);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, cons);

	return 1;
}

/* (unwind-protect value) -> value */
static int checkparse_unwind_protect2(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_UNWIND_PROTECT)) return 0;
	GetEvalParse(pos, 1, &pos);
	return pos == Nil;
}
static int optparse_unwind_protect2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_unwind_protect2(opt, pos)) return 0;
	GetEvalParse(pos, 0, ret);
	return 1;
}

/* all */
static int checkparse_unwind_protect_all(optvalue opt, addr pos)
{
	addr check;

	if (! check_evaltype(pos, EVAL_PARSE_UNWIND_PROTECT)) return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse(opt, check)) return 1;
	GetEvalParse(pos, 1, &check);
	if (check == Nil) return 0;
	return checkparse_implicit(opt, check);
}
static int optparse_unwind_protect_all(LocalRoot local,
		addr *ret, optvalue opt, addr pos)
{
	addr form, cons;

	if (! checkparse_unwind_protect_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &cons);
	optparse(local, &form, opt, form);
	optparse_implicit(local, &cons, opt, cons);

	eval_parse_local(local, &pos, EVAL_PARSE_UNWIND_PROTECT, 2);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* unwind-protect */
static int checkparse_unwind_protect(optvalue opt, addr pos)
{
	return checkparse_unwind_protect1(opt, pos)
		|| checkparse_unwind_protect2(opt, pos)
		|| checkparse_unwind_protect_all(opt, pos);
}
static int optparse_unwind_protect(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_unwind_protect1);
		optparse_extract(local, &pos, &update, opt, optparse_unwind_protect2);
		optparse_extract(local, &pos, &update, opt, optparse_unwind_protect_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  tagbody
 */
/* (tagbody) -> nil */
static int checkparse_tagbody1(optvalue opt, addr pos)
{
	addr tag, body;
	size_t size1, size2;

	if (! check_evaltype_on(opt, pos, EVAL_PARSE_TAGBODY)) return 0;
	GetEvalParse(pos, 0, &tag);
	GetEvalParse(pos, 1, &body);
	if (body == Nil) return 1;
	size1 = length_list_unsafe(tag);
	size2 = length_list_unsafe(body);

	return size1 == size2;
}
static int optparse_tagbody1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_tagbody1(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* (tagbody (call) (call2)) -> (progn (call) (call2) nil) */
static int checkparse_tagbody2(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_TAGBODY)) return 0;
	GetEvalParse(pos, 0, &pos);
	return pos == Nil;
}
static int optparse_tagbody2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr root, cons;

	if (! checkparse_tagbody2(opt, pos)) return 0;
	GetEvalParse(pos, 1, &cons);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		cons_local(local, &root, pos, root);
	}
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	cons_local(local, &root, pos, root);
	nreverse_list_unsafe(&root, root);
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, root);

	return 1;
}

/* all */
static int checkparse_tagbody_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_TAGBODY)) return 0;
	GetEvalParse(pos, 1, &pos);
	/* Don't use checkparse_implicit. */
	return checkparse_implicit_all(opt, pos);
}
static int optparse_tagbody_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr tag, body, root;

	if (! checkparse_tagbody_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &tag);
	GetEvalParse(pos, 1, &body);

	for (root = Nil; body != Nil; ) {
		GetCons(body, &pos, &body);
		if ((! optparse(local, &pos, opt, pos)) || (! checkvalue(pos)))
			cons_local(local, &root, pos, root);
	}
	nreverse_list_unsafe(&body, root);

	eval_parse_local(local, &pos, EVAL_PARSE_TAGBODY, 2);
	SetEvalParse(pos, 0, tag);
	SetEvalParse(pos, 1, body);
	*ret = pos;

	return 1;
}

/* tagbody */
static int checkparse_tagbody(optvalue opt, addr pos)
{
	return checkparse_tagbody1(opt, pos)
		|| checkparse_tagbody2(opt, pos)
		|| checkparse_tagbody_all(opt, pos);
}
static int optparse_tagbody(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_tagbody1);
		optparse_extract(local, &pos, &update, opt, optparse_tagbody2);
		optparse_extract(local, &pos, &update, opt, optparse_tagbody_all);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  block / return-from
 */
/* (block name) -> nil */
static int checkparse_block1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_BLOCK)) return 0;
	GetEvalParse(pos, 1, &pos);
	return pos == Nil;
}
static int optparse_block1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_block1(opt, pos)) return 0;
	eval_single_parse_local(local, ret, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* (block name ... x) -> x */
static int checkparse_block2(optvalue opt, addr pos)
{
	addr check;

	if (! check_evaltype_on(opt, pos, EVAL_PARSE_BLOCK)) return 0;
	GetEvalParse(pos, 1, &pos);
	if (pos == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! checkvalue(check)) return 0;
	}

	return 1;
}
static int optparse_block2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	if (! checkparse_block2(opt, pos)) return 0;
	GetEvalParse(pos, 1, &pos);
	if (pos == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, ret, &pos);
	}
	return 1;
}

/* all */
static int checkparse_block_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_BLOCK)) return 0;
	GetEvalParse(pos, 1, &pos);
	return checkparse_implicit(opt, pos);
}
static int optparse_block_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, cons;

	if (! checkparse_block_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &cons);

	optparse_implicit(local, &cons, opt, cons);
	eval_parse_local(local, &pos, EVAL_PARSE_BLOCK, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* (return-from name expr) -> (return-from name expr) */
static int checkparse_return_from(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_RETURN_FROM)) return 0;
	GetEvalParse(pos, 1, &pos);
	return checkparse(opt, pos);
}
static int optparse_return_from(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, expr;

	if (! checkparse_return_from(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &expr);

	optparse(local, &expr, opt, expr);
	eval_parse_local(local, &pos, EVAL_PARSE_RETURN_FROM, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, expr);
	*ret = pos;

	return 1;
}

/* block/return-from */
static int checkparse_block(optvalue opt, addr pos)
{
	return checkparse_block1(opt, pos)
		|| checkparse_block2(opt, pos)
		|| checkparse_block_all(opt, pos)
		|| checkparse_return_from(opt, pos);
}
static int optparse_block(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_block1);
		optparse_extract(local, &pos, &update, opt, optparse_block2);
		optparse_extract(local, &pos, &update, opt, optparse_block_all);
		optparse_extract(local, &pos, &update, opt, optparse_return_from);
		if (update == 0) break;
	}
	if (result) *ret = pos;

	return result;
}


/*
 *  catch / throw
 */
/* (catch name) -> (progn name nil) */
static int checkparse_catch1(optvalue opt, addr pos)
{
	if (! check_evaltype_on(opt, pos, EVAL_PARSE_CATCH)) return 0;
	GetEvalParse(pos, 1, &pos);
	return pos == Nil;
}
static int optparse_catch1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, cons;

	if (! checkparse_catch1(opt, pos)) return 0;
	/* (name nil) */
	GetEvalParse(pos, 0, &name);
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	conscar_local(local, &cons, pos);
	cons_local(local, &cons, name, cons);
	/* (progn name nil) */
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, cons);

	return 1;
}

/* (catch name ... x) -> (progn name x) */
static int checkparse_catch2(optvalue opt, addr pos)
{
	addr check;

	if (! check_evaltype_on(opt, pos, EVAL_PARSE_CATCH)) return 0;
	GetEvalParse(pos, 1, &pos);
	if (pos == Nil) return 0;
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! checkvalue(check)) return 0;
	}

	return 1;
}
static int optparse_catch2(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, cons;

	if (! checkparse_catch2(opt, pos)) return 0;
	/* (name lastcar) */
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &cons);
	for (pos = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
	}
	conscar_local(local, &cons, pos);
	cons_local(local, &cons, name, cons);
	/* (progn name lastcar) */
	eval_single_parse_local(local, ret, EVAL_PARSE_PROGN, cons);

	return 1;
}

/* all */
static int checkparse_catch_all(optvalue opt, addr pos)
{
	addr check;

	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_CATCH)) return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse(opt, check)) return 1;
	GetEvalParse(pos, 1, &check);
	return checkparse_implicit(opt, check);
}
static int optparse_catch_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr name, cons;

	if (! checkparse_catch_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &cons);

	optparse(local, &name, opt, name);
	optparse_implicit(local, &cons, opt, cons);
	eval_parse_local(local, &pos, EVAL_PARSE_CATCH, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

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
 *  call
 */
/* first argument */
static int checkparse_call1(optvalue opt, addr pos)
{
	if (! check_evaltype(pos, EVAL_PARSE_CALL)) return 0;
	GetEvalParse(pos, 0, &pos); /* call */
	return checkparse(opt, pos);
}
static int optparse_call1(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr call, cons;

	if (! checkparse_call1(opt, pos)) return 0;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &cons);

	optparse(local, &call, opt, call);
	eval_parse_heap(&pos, EVAL_PARSE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* all */
static int checkparse_call_all(optvalue opt, addr pos)
{
	/* Don't check optimize. */
	if (! check_evaltype(pos, EVAL_PARSE_CALL)) return 0;
	GetEvalParse(pos, 1, &pos); /* cons */
	return checkparse_implicit(opt, pos);
}
static int optparse_call_all(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	addr call, cons;

	if (! checkparse_call_all(opt, pos)) return 0;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &cons);

	optparse_implicit(local, &cons, opt, cons);
	eval_parse_heap(&pos, EVAL_PARSE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, cons);
	*ret = pos;

	return 1;
}

/* call */
static int checkparse_call(optvalue opt, addr pos)
{
	return checkparse_call1(opt, pos)
		|| checkparse_call_all(opt, pos);
}
static int optparse_call(LocalRoot local, addr *ret, optvalue opt, addr pos)
{
	int update, result;

	for (result = 0; ; result |= update) {
		update = 0;
		optparse_extract(local, &pos, &update, opt, optparse_call1);
		optparse_extract(local, &pos, &update, opt, optparse_call_all);
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
	return checkparse_progn(opt, pos)
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
		optparse_extract(local, &pos, &update, opt, optparse_progn);
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

