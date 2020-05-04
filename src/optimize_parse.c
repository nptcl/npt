#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "eval_copy.h"
#include "function.h"
#include "optimize.h"
#include "parse.h"
#include "strtype.h"
#include "type_optimize.h"

static int checkparse_all(struct optimize_struct *str);
static int optparse_all(struct optimize_struct *str);

static int checkparse_inplace(struct optimize_struct *str, addr pos)
{
	int check;
	struct optimize_struct save;

	save = *str;
	str->pos = pos;
	check = checkparse_all(str);
	*str = save;

	return check;
}

static int optparse_inplace(struct optimize_struct *str, addr pos, addr *ret)
{
	int check;
	struct optimize_struct save;

	save = *str;
	str->pos = pos;
	check = optparse_all(str);
	*ret = check? str->pos: pos;
	*str = save;

	return check;
}


/*
 *  optimize-check
 */
/* (lisp-system::optimize-check parse) -> 0 / 1 */
static int checkparse_optimize_check(struct optimize_struct *str, addr *ret)
{
	addr call, left, right;

	/* call */
	call = str->pos;
	if (! optimize_evaltype(call, EVAL_PARSE_CALL))
		return 0;
	GetEvalParse(call, 0, &left);
	/* function */
	if (! optimize_evaltype(left, EVAL_PARSE_FUNCTION))
		return 0;
	/* function name */
	GetEvalParse(left, 0, &left);
	GetCallName(left, &left);
	GetConst(SYSTEM_OPTIMIZE_CHECK, &right);
	if (left != right)
		return 0;
	/* argument */
	GetEvalParse(call, 1, &right);
	if (! consp_getcons(right, &left, &right))
		return 0;
	/* PARSE */
	if (! optimize_evaltype(left, EVAL_PARSE_SYMBOL))
		return 0;
	GetEvalParse(left, 0, &left);
	if (! string_designer_equal_char(left, "PARSE"))
		return 0;
	/* result */
	*ret = right;
	return 1;
}

static int checkparse_check1(struct optimize_struct *str)
{
	addr list;

	if (! checkparse_optimize_check(str, &list))
		return 0;
	return list == Nil;
}

static int optparse_check1(struct optimize_struct *str)
{
	addr value;

	if (! checkparse_check1(str))
		return 0;
	fixnum_heap(&value, optimize_speed_on(str)? 1: 0);
	eval_single_parse_local(str->local, &str->pos, EVAL_PARSE_INTEGER, value);

	return 1;
}

/* (lisp-system::optimize-check parse list) -> (...) */
static int checkparse_check2(struct optimize_struct *str)
{
	addr list, pos;

	if (! checkparse_optimize_check(str, &list))
		return 0;
	if (list == Nil)
		return 0;
	if (! consp_getcons(list, &pos, &list))
		return 0;
	if (list != Nil)
		return 0;
	if (! optimize_evaltype(pos, EVAL_PARSE_SYMBOL))
		return 0;
	GetEvalParse(pos, 0, &pos);
	return string_designer_equal_char(pos, "LIST");
}

static int optparse_check2(struct optimize_struct *str)
{
	addr symbol, value, list;

	if (! checkparse_check2(str))
		return 0;

	list = Nil;
	/* compilation-speed */
	GetConst(COMMON_COMPILATION_SPEED, &symbol);
	fixnum_heap(&value, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_COMPILATION));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* debug */
	GetConst(COMMON_DEBUG, &symbol);
	fixnum_heap(&value, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_DEBUG));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* safety */
	GetConst(COMMON_SAFETY, &symbol);
	fixnum_heap(&value, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SAFETY));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* space */
	GetConst(COMMON_SPACE, &symbol);
	fixnum_heap(&value, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SPACE));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* speed */
	GetConst(COMMON_SPEED, &symbol);
	fixnum_heap(&value, (fixnum)optimize_declare_value(str, EVAL_OPTIMIZE_SPEED));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* quote */
	eval_single_parse_heap(&list, EVAL_PARSE_QUOTE, list);
	str->pos = list;

	return 1;
}

/* optparse-check */
static int checkparse_check(struct optimize_struct *str)
{
	return checkparse_check1(str)
		|| checkparse_check2(str);
}

static int optparse_run(struct optimize_struct *str,
		void (*call)(struct optimize_struct *))
{
	int update, result;
	addr pos;

	update = str->update;
	pos = str->pos;
	for (result = 0; ; result |= str->update) {
		str->update = 0;
		(*call)(str);

		if (str->update == 0)
			break;
	}

	if (result) {
		str->update = 1;
		return 1;
	}
	else {
		str->pos = pos;
		str->update = update;
		return 0;
	}
}

static void optparse_check_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_check1);
	optimize_extract(str, optparse_check2);
}
static int optparse_check(struct optimize_struct *str)
{
	return optparse_run(str, optparse_check_run);
}


/*
 *  optimize_value
 */
static int optimize_value(addr pos);
static int optimize_value_values(addr list)
{
	addr pos;

	GetEvalParse(list, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! optimize_value(pos))
			return 0;
	}

	return 1;
}

static int optmize_value_the(addr pos)
{
	GetEvalParse(pos, 1, &pos); /* expr */
	return optimize_value(pos);
}

static int optimize_value(addr pos)
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
		case EVAL_PARSE_FUNCTION:
		case EVAL_PARSE_LAMBDA:
			return 1;

		case EVAL_PARSE_THE:
			return optmize_value_the(pos);

		case EVAL_PARSE_VALUES:
			return optimize_value_values(pos);

		default:
			return 0;
	}
}


/*
 *  implicit
 */
static int optimize_lisptype_on(
		struct optimize_struct *str, addr pos, enum LISPTYPE type)
{
	return optimize_speed_on(str) && GetType(pos) == type;
}

/* (10 20 30) -> (30) */
static int checkparse_implicit3(struct optimize_struct *str, addr list)
{
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return 0;
	GetCdr(list, &check);
	if (check == Nil)
		return 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (! optimize_value(check))
			return 0;
	}

	return 1;
}

static int optparse_implicit3(struct optimize_struct *str, addr list, addr *ret)
{
	addr x;

	if (! checkparse_implicit3(str, list))
		return 0;
	if (list == Nil)
		return 0;
	for (x = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
	}
	conscar_local(str->local, ret, x);

	return 1;
}

/* (10 (call1) 20 30 (call2)) -> ((call1) (call2)) */
static int checkparse_implicit4(struct optimize_struct *str, addr list)
{
	int update1, update2, valuep;
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return 0;
	update1 = update2 = 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		valuep = optimize_value(check);
		if (list == Nil && ! valuep)
			update1 = 1;
		if (list != Nil && valuep)
			update2 = 1;
	}

	return update1 && update2;
}

static int optparse_implicit4(struct optimize_struct *str, addr list, addr *ret)
{
	addr root, check;
	LocalRoot local;

	if (! checkparse_implicit4(str, list))
		return 0;
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &check, &list);
		if (! optimize_value(check))
			cons_local(local, &root, check, root);
	}
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* (10 (call1) 20 (call2) 30 40) -> ((call1) (call2) 40) */
static int checkparse_implicit5(struct optimize_struct *str, addr list)
{
	int update1, update2, update3, valuep;
	addr check;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return 0;
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

	return update1 && update2 && update3;
}

static int optparse_implicit5(struct optimize_struct *str, addr list, addr *ret)
{
	addr root, check;
	LocalRoot local;

	if (! checkparse_implicit5(str, list))
		return 0;
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &check, &list);
		if (list == Nil || ! optimize_value(check))
			cons_local(local, &root, check, root);
	}
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* (x (progn y) z) -> (x y z) */
static int checkparse_implicit6(struct optimize_struct *str, addr list)
{
	addr pos;

	if (! optimize_lisptype_on(str, list, LISPTYPE_CONS))
		return 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (optimize_evaltype(pos, EVAL_PARSE_PROGN))
			return 1;
	}

	return 0;
}

static void optparse_implicit6_next(LocalRoot local, addr list, addr *ret)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (optimize_evaltype(pos, EVAL_PARSE_PROGN)) {
			GetEvalParse(pos, 0, &pos);
			optparse_implicit6_next(local, pos, ret);
			continue;
		}
		cons_local(local, ret, pos, *ret);
	}
}

static int optparse_implicit6(struct optimize_struct *str, addr list, addr *ret)
{
	addr root;

	if (! checkparse_implicit6(str, list))
		return 0;
	root = Nil;
	optparse_implicit6_next(str->local, list, &root);
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* (...) */
static int checkparse_implicit_all(struct optimize_struct *str, addr list)
{
	addr pos;

	/* Don't check optimize. */
	if (! consp(list))
		return 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (checkparse_inplace(str, pos))
			return 1;
	}

	return 0;
}

static int optparse_implicit_all(struct optimize_struct *str, addr list, addr *ret)
{
	int result;
	addr root, pos;
	LocalRoot local;

	if (! checkparse_implicit_all(str, list))
		return 0;
	local = str->local;
	result = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		result |= optparse_inplace(str, pos, &pos);
		cons_local(local, &root, pos, root);
	}
	if (! result)
		return 0;
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* implicit */
static int checkparse_implicit(struct optimize_struct *str, addr pos)
{
	return checkparse_implicit3(str, pos)
		|| checkparse_implicit4(str, pos)
		|| checkparse_implicit5(str, pos)
		|| checkparse_implicit6(str, pos)
		|| checkparse_implicit_all(str, pos);
}

static int optparse_implicit(struct optimize_struct *str, addr pos, addr *ret)
{
	int update, result;
	addr var;

	if (! checkparse_implicit(str, pos)) {
		*ret = pos;
		return 0;
	}
	result = 0;
	var = pos;
	for (;;) {
		update = optparse_implicit3(str, var, &var)
			|| optparse_implicit4(str, var, &var)
			|| optparse_implicit5(str, var, &var)
			|| optparse_implicit6(str, var, &var)
			|| optparse_implicit_all(str, var, &var);
		if (update == 0)
			break;
		result = 1;
	}
	if (result) {
		*ret = var;
		return 1;
	}
	else {
		*ret = pos;
		return 0;
	}
}


/*
 *  progn
 */
/* (progn) -> nil */
static int checkparse_progn1(struct optimize_struct *str)
{
	return optimize_evaltype_on(str, EVAL_PARSE_PROGN)
		&& RefEvalParse(str->pos, 0) == Nil;
}

static int optparse_progn1(struct optimize_struct *str)
{
	if (! checkparse_progn1(str)) return 0;
	eval_single_parse_local(str->local, &str->pos, EVAL_PARSE_NIL, Nil);
	return 1;
}

/* (progn x) -> x */
static int checkparse_progn2(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return 0;
	GetEvalParse(str->pos, 0, &pos);

	return singlep(pos);
}

static int optparse_progn2(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_progn2(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	GetCar(pos, &(str->pos));

	return 1;
}

/* (progn 10 20 30) -> 30 */
static int checkparse_progn3(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit3(str, pos);
}

static int optparse_progn3(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_progn3(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	if (! optparse_implicit3(str, pos, &pos))
		return 0;
	GetCar(pos, &pos);
	str->pos = pos;

	return 1;
}

/* (progn 10 (call1) 20 30 (call2)) -> (progn (call1) (call2)) */
static int checkparse_progn4(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit4(str, pos);
}

static int optparse_progn4(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_progn4(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	if (! optparse_implicit4(str, pos, &pos))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return 1;
}

/* (progn 10 (call1) 20 (call2) 30 40) -> (progn (call1) (call2) 40) */
static int checkparse_progn5(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit5(str, pos);
}

static int optparse_progn5(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_progn5(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	if (! optparse_implicit5(str, pos, &pos))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return 1;
}

/* (progn x (progn y) z) -> (progn x y z) */
static int checkparse_progn6(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_PROGN))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return checkparse_implicit6(str, pos);
}

static int optparse_progn6(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_progn6(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	if (! optparse_implicit6(str, pos, &pos))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return 1;
}

/* (progn ...) */
static int checkparse_progn_all(struct optimize_struct *str)
{
	addr list;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_PROGN))
		return 0;
	GetEvalParse(list, 0, &list);
	return checkparse_implicit_all(str, list);
}

static int optparse_progn_all(struct optimize_struct *str)
{
	addr list;

	if (! checkparse_progn_all(str))
		return 0;
	GetEvalParse(str->pos, 0, &list);
	optparse_implicit_all(str, list, &list);
	eval_single_parse_local(str->local, &list, EVAL_PARSE_PROGN, list);
	str->pos = list;

	return 1;
}


/* optparse-progn */
static int checkparse_progn(struct optimize_struct *str)
{
	return checkparse_progn1(str)
		|| checkparse_progn2(str)
		|| checkparse_progn3(str)
		|| checkparse_progn4(str)
		|| checkparse_progn5(str)
		|| checkparse_progn6(str)
		|| checkparse_progn_all(str);
}

static void optparse_progn_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_progn1);
	optimize_extract(str, optparse_progn2);
	optimize_extract(str, optparse_progn3);
	optimize_extract(str, optparse_progn4);
	optimize_extract(str, optparse_progn5);
	optimize_extract(str, optparse_progn6);
	optimize_extract(str, optparse_progn_all);
}
static int optparse_progn(struct optimize_struct *str)
{
	return optparse_run(str, optparse_progn_run);
}


/*
 *  let
 */
static int optimize_lettype(addr pos)
{
	enum EVAL_PARSE type;

	if (! eval_parse_p(pos))
		return 0;
	GetEvalParseType(pos, &type);
	return type == EVAL_PARSE_LET || type == EVAL_PARSE_LETA;
}

static int optimize_lettype_on(struct optimize_struct *str)
{
	return optimize_speed_on(str) && optimize_lettype(str->pos);
}

/* (let nil . body) -> (progn ,@body) */
static int checkparse_let1(struct optimize_struct *str)
{
	addr pos, args, decl, body;

	if (! optimize_lettype_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */

	return args == Nil && empty_nil_declare(decl) && body != Nil;
}

static int optparse_let1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_let1(str))
		return 0;
	GetEvalParse(str->pos, 2, &pos); /* body */
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return 1;
}

/* (let nil (declare ...) . body) -> (locally (declare ...) . body) */
static int checkparse_let2(struct optimize_struct *str)
{
	addr pos, args, decl, body;

	if (! optimize_lettype_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */

	return args == Nil && (! empty_nil_declare(decl)) && body != Nil;
}

static int optparse_let2(struct optimize_struct *str)
{
	addr pos, decl, body;

	if (! checkparse_let2(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */

	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return 1;
}

/* (let nil . nil) -> nil */
static int checkparse_let3(struct optimize_struct *str)
{
	addr pos, args, body;

	if (! optimize_lettype_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args); /* args */
	GetEvalParse(pos, 2, &body); /* body */

	return args == Nil && body == Nil;
}

static int optparse_let3(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_let3(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* (let (aaa bbb (ccc)) . nil) -> nil */
static int checkparse_let4(struct optimize_struct *str)
{
	addr pos, args, body, x;

	if (! optimize_lettype_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args); /* args */
	if (args == Nil)
		return 0;
	GetEvalParse(pos, 2, &body); /* body */
	if (body != Nil)
		return 0;
	while (args != Nil) {
		GetCons(args, &x, &args);
		GetCdr(x, &x); /* (var . init) */
		if (RefEvalParseType(x) != EVAL_PARSE_NIL)
			return 0;
	}

	return 1;
}

static int optparse_let4(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_let4(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* let-args */
static int checkparse_let_args(struct optimize_struct *str)
{
	addr pos, check;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return 0;
	GetEvalParse(pos, 0, &pos); /* args */
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		GetCdr(check, &check); /* (var . init) */
		if (checkparse_inplace(str, check))
			return 1;
	}

	return 0;
}

static int optparse_let_args(struct optimize_struct *str)
{
	int result;
	enum EVAL_PARSE type;
	addr pos, args, decl, body, var, init, root;
	LocalRoot local;

	if (! checkparse_let_args(str))
		return 0;
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	local = str->local;
	result = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		result |= optparse_inplace(str, init, &init);
		cons_local(local, &var, var, init);
		cons_local(local, &root, var, root);
	}
	if (! result)
		return 0;
	nreverse_list_unsafe(&args, root);

	eval_parse_local(local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return 1;
}

/* let-body */
static int checkparse_implicit_declare(
		struct optimize_struct *str, addr decl, addr cons)
{
	int result;
	struct optimize_struct save;

	save = *str;
	if (decl != Nil)
		apply_array_declare(str->value.local, decl);
	result = checkparse_implicit(str, cons);
	*str = save;

	return result;
}

static int optparse_implicit_declare(struct optimize_struct *str,
		addr decl, addr cons, addr *ret)
{
	int result;
	struct optimize_struct save;

	save = *str;
	if (decl != Nil)
		apply_array_declare(str->value.local, decl);
	result = optparse_implicit(str, cons, ret);
	*str = save;

	return result;
}

static int checkparse_let_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_lettype(pos))
		return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_let_body(struct optimize_struct *str)
{
	enum EVAL_PARSE type;
	addr pos, args, decl, body;

	if (! checkparse_let_body(str))
		return 0;
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return 1;
}

/* optparse-let */
static int checkparse_let(struct optimize_struct *str)
{
	return checkparse_let1(str)
		|| checkparse_let2(str)
		|| checkparse_let3(str)
		|| checkparse_let4(str)
		|| checkparse_let_args(str)
		|| checkparse_let_body(str);
}

static void optparse_let_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_let1);
	optimize_extract(str, optparse_let2);
	optimize_extract(str, optparse_let3);
	optimize_extract(str, optparse_let4);
	optimize_extract(str, optparse_let_args);
	optimize_extract(str, optparse_let_body);
}
static int optparse_let(struct optimize_struct *str)
{
	return optparse_run(str, optparse_let_run);
}


/*
 *  setq
 */
/* (setq) -> nil */
static int checkparse_setq1(struct optimize_struct *str)
{
	if (! optimize_evaltype_on(str, EVAL_PARSE_SETQ))
		return 0;
	return RefEvalParse(str->pos, 0) == Nil;
}

static int optparse_setq1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_setq1(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* setq-all */
static int checkparse_setq_all(struct optimize_struct *str)
{
	addr list, x;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_SETQ))
		return 0;
	GetEvalParse(list, 0, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCdr(x, &x); /* (var . expr) */
		if (checkparse_inplace(str, x))
			return 1;
	}

	return 0;
}

static int optparse_setq_all(struct optimize_struct *str)
{
	int check;
	addr list, root, var, expr;
	LocalRoot local;

	if (! checkparse_setq_all(str))
		return 0;
	GetEvalParse(str->pos, 0, &list);
	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		GetCons(var, &var, &expr);
		check |= optparse_inplace(str, expr, &expr);
		cons_local(local, &var, var, expr);
		cons_local(local, &root, var, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(&list, root);
	eval_single_parse_local(local, &list, EVAL_PARSE_SETQ, list);
	str->pos = list;

	return 1;
}

/* optparse-setq */
static int checkparse_setq(struct optimize_struct *str)
{
	return checkparse_setq1(str)
		|| checkparse_setq_all(str);
}

static void optparse_setq_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_setq1);
	optimize_extract(str, optparse_setq_all);
}
static int optparse_setq(struct optimize_struct *str)
{
	return optparse_run(str, optparse_setq_run);
}


/*
 *  lambda-ordinary
 */
/* &optional */
static int checkparse_opt(struct optimize_struct *str, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos); /* var */
		GetCar(pos, &pos); /* init */
		if (checkparse_inplace(str, pos))
			return 1;
	}

	return 0;
}

static int optparse_opt(struct optimize_struct *str, addr list, addr *ret)
{
	int check;
	addr root, x, var, init, svar;
	LocalRoot local;

	/* opt -> (var init svar) */
	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, &svar, NULL);
		check |= optparse_inplace(str, init, &init);
		list_local(local, &x, var, init, svar, NULL);
		cons_local(local, &root, x, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* &key */
static int checkparse_key(struct optimize_struct *str, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos); /* var */
		GetCdr(pos, &pos); /* name */
		GetCar(pos, &pos); /* init */
		if (checkparse_inplace(str, pos))
			return 1;
	}

	return 0;
}

static int optparse_key(struct optimize_struct *str, addr list, addr *ret)
{
	int check;
	addr root, x, var, name, init, svar;
	LocalRoot local;

	/* key -> (var name init svar) */
	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &name, &init, &svar, NULL);
		check |= optparse_inplace(str, init, &init);
		list_local(local, &x, var, name, init, svar, NULL);
		cons_local(local, &root, x, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* &aux */
static int checkparse_aux(struct optimize_struct *str, addr list)
{
	return checkparse_opt(str, list);
}

static int optparse_aux(struct optimize_struct *str, addr list, addr *ret)
{
	int check;
	addr root, x, var, init;
	LocalRoot local;

	/* aux -> (var init) */
	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, NULL);
		check |= optparse_inplace(str, init, &init);
		list_local(local, &x, var, init, NULL);
		cons_local(local, &root, x, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(ret, root);

	return 1;
}

/* interface */
static int checkparse_lambda_ordinary(struct optimize_struct *str, addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	return checkparse_opt(str, opt)
		|| checkparse_key(str, key)
		|| checkparse_aux(str, aux);
}

static int optparse_lambda_ordinary(struct optimize_struct *str, addr args, addr *ret)
{
	int check;
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	check = 0;
	if (checkparse_opt(str, opt))
		check |= optparse_opt(str, opt, &opt);
	if (checkparse_key(str, key))
		check |= optparse_key(str, key, &key);
	if (checkparse_aux(str, aux))
		check |= optparse_aux(str, aux, &aux);
	if (! check)
		return 0;
	list_local(str->local, ret, var, opt, rest, key, allow, aux, NULL);

	return 1;
}


/*
 *  defun
 */
/* args */
static int checkparse_defun_args(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFUN))
		return 0;
	GetEvalParse(pos, 1, &pos);
	return checkparse_lambda_ordinary(str, pos);
}

static int optparse_defun_args(struct optimize_struct *str)
{
	addr pos, name, args, decl, doc, body, form;

	if (! checkparse_defun_args(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);
	GetEvalParse(pos, 5, &form);

	if (! optparse_lambda_ordinary(str, args, &args))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	SetEvalParse(pos, 5, form);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_defun_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFUN))
		return 0;
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_defun_body(struct optimize_struct *str)
{
	addr pos, name, args, decl, doc, body, form;

	if (! checkparse_defun_body(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);
	GetEvalParse(pos, 5, &form);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	SetEvalParse(pos, 5, form);
	str->pos = pos;

	return 1;
}

/* optparse-defun */
static int checkparse_defun(struct optimize_struct *str)
{
	return checkparse_defun_args(str)
		|| checkparse_defun_body(str);
}

static void optparse_defun_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_defun_args);
	optimize_extract(str, optparse_defun_body);
}
static int optparse_defun(struct optimize_struct *str)
{
	return optparse_run(str, optparse_defun_run);
}


/*
 *  macro-lambda
 */
static int checkparse_lambda_macro(struct optimize_struct *str, addr args);
static int checkparse_macro_var(struct optimize_struct *str, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (consp(pos) && checkparse_lambda_macro(str, pos))
			return 1;
	}

	return 0;
}

static int optparse_lambda_macro(struct optimize_struct *str, addr pos, addr *ret);
static int optparse_macro_var(struct optimize_struct *str, addr list, addr *ret)
{
	int check;
	addr root, var;
	LocalRoot local;

	/* var */
	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var))
			check |= optparse_lambda_macro(str, var, &var);
		cons_local(local, &root, var, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(ret, root);

	return 1;
}

static int checkparse_lambda_macro(struct optimize_struct *str, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	return checkparse_macro_var(str, var)
		|| checkparse_opt(str, opt)
		|| checkparse_key(str, key)
		|| checkparse_aux(str, aux);
}

static int optparse_lambda_macro(struct optimize_struct *str, addr args, addr *ret)
{
	int check;
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	check = 0;
	if (checkparse_macro_var(str, var))
		check |= optparse_macro_var(str, var, &var);
	if (checkparse_opt(str, opt))
		check |= optparse_opt(str, opt, &opt);
	if (checkparse_key(str, key))
		check |= optparse_key(str, key, &key);
	if (checkparse_opt(str, aux))
		check |= optparse_aux(str, aux, &aux);
	if (! check)
		return 0;
	list_local(str->local, ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 1;
}


/*
 *  defmacro
 */
/* args */
static int checkparse_defmacro_args(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MACRO_LAMBDA))
		return 0;
	GetEvalParse(pos, 0, &pos); /* args */
	return checkparse_lambda_macro(str, pos);
}

static int optparse_defmacro_args(struct optimize_struct *str)
{
	addr pos, args, decl, doc, body;

	if (! checkparse_defmacro_args(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);

	if (! optparse_lambda_macro(str, args, &args))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_defmacro_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MACRO_LAMBDA))
		return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_defmacro_body(struct optimize_struct *str)
{
	addr pos, args, decl, doc, body;

	if (! checkparse_defmacro_body(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	str->pos = pos;

	return 1;
}

/* optparse-defmacro */
static int checkparse_defmacro(struct optimize_struct *str)
{
	return checkparse_defmacro_args(str)
		|| checkparse_defmacro_body(str);
}

static void optparse_defmacro_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_defmacro_args);
	optimize_extract(str, optparse_defmacro_body);
}
static int optparse_defmacro(struct optimize_struct *str)
{
	return optparse_run(str, optparse_defmacro_run);
}


/*
 *  deftype
 */
/* args */
static int checkparse_deftype_args(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFTYPE))
		return 0;
	GetEvalParse(pos, 1, &pos); /* args */
	return checkparse_lambda_macro(str, pos);
}

static int optparse_deftype_args(struct optimize_struct *str)
{
	addr pos, name, args, decl, doc, body;

	if (! checkparse_deftype_args(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	if (! optparse_lambda_macro(str, args, &args))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_deftype_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFTYPE))
		return 0;
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_deftype_body(struct optimize_struct *str)
{
	addr pos, name, args, decl, doc, body;

	if (! checkparse_deftype_body(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return 1;
}

/* optparse-deftype */
static int checkparse_deftype(struct optimize_struct *str)
{
	return checkparse_deftype_args(str)
		|| checkparse_deftype_body(str);
}

static void optparse_deftype_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_deftype_args);
	optimize_extract(str, optparse_deftype_body);
}
static int optparse_deftype(struct optimize_struct *str)
{
	return optparse_run(str, optparse_deftype_run);
}


/*
 *  define-compiler-macro
 */
/* args */
static int checkparse_define_compiler_macro_args(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_COMPILER_MACRO))
		return 0;
	GetEvalParse(pos, 1, &pos); /* args */
	return checkparse_lambda_macro(str, pos);
}

static int optparse_define_compiler_macro_args(struct optimize_struct *str)
{
	addr pos, name, args, decl, doc, body;

	if (! checkparse_define_compiler_macro_args(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	if (! optparse_lambda_macro(str, args, &args))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_define_compiler_macro_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_COMPILER_MACRO))
		return 0;
	GetEvalParse(pos, 2, &decl); /* decl */
	GetEvalParse(pos, 4, &body); /* body */
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_define_compiler_macro_body(struct optimize_struct *str)
{
	addr pos, name, args, decl, doc, body;

	if (! checkparse_define_compiler_macro_body(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &args);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, args);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return 1;
}

/* optparse-define-compiler-macro */
static int checkparse_define_compiler_macro(struct optimize_struct *str)
{
	return checkparse_define_compiler_macro_args(str)
		|| checkparse_define_compiler_macro_body(str);
}

static void optparse_define_compiler_macro_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_define_compiler_macro_args);
	optimize_extract(str, optparse_define_compiler_macro_body);
}
static int optparse_define_compiler_macro(struct optimize_struct *str)
{
	return optparse_run(str, optparse_define_compiler_macro_run);
}


/*
 *  destructuring-bind
 */
/* args */
static int checkparse_destructuring_bind(struct optimize_struct *str)
{
	addr pos, expr, lambda;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DESTRUCTURING_BIND))
		return 0;
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &lambda);
	return checkparse_inplace(str, expr)
		|| checkparse_inplace(str, lambda);
}

static int optparse_destructuring_bind(struct optimize_struct *str)
{
	int check;
	addr pos, expr, lambda;

	if (! checkparse_destructuring_bind(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &lambda);

	check = optparse_inplace(str, expr, &expr)
		|| optparse_inplace(str, lambda, &lambda);
	if (! check)
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DESTRUCTURING_BIND, 2);
	SetEvalParse(pos, 0, expr);
	SetEvalParse(pos, 1, lambda);
	str->pos = pos;

	return 1;
}


/*
 *  define-symbol-macro
 */
static int checkparse_define_symbol_macro(struct optimize_struct *str)
{
	addr pos;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_DEFINE_SYMBOL_MACRO))
		return 0;
	GetEvalParse(pos, 1, &pos); /* body */
	return checkparse_inplace(str, pos);
}

static int optparse_define_symbol_macro(struct optimize_struct *str)
{
	addr pos, symbol, body, form;

	if (! checkparse_define_symbol_macro(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &symbol);
	GetEvalParse(pos, 1, &body);
	GetEvalParse(pos, 2, &form);

	if (! optparse_inplace(str, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_DEFINE_SYMBOL_MACRO, 3);
	SetEvalParse(pos, 0, symbol);
	SetEvalParse(pos, 1, body);
	SetEvalParse(pos, 2, form);
	str->pos = pos;

	return 1;
}


/*
 *  symbol-macrolet
 */
/* args */
static int checkparse_symbol_macrolet_args(struct optimize_struct *str)
{
	addr pos, x, symbol, expr, env;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_SYMBOL_MACROLET))
		return 0;
	GetEvalParse(pos, 0, &pos); /* args */
	while (pos != Nil) {
		GetCons(pos, &x, &pos);
		List_bind(x, &symbol, &expr, &env, NULL);
		if (checkparse_inplace(str, expr))
			return 1;
	}

	return 0;
}

static int optparse_symbol_macrolet_args(struct optimize_struct *str)
{
	int check;
	addr pos, args, decl, body, root, symbol, expr, env;
	LocalRoot local;

	if (! checkparse_symbol_macrolet_args(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	local = str->local;
	check = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &pos, &args);
		List_bind(pos, &symbol, &expr, &env, NULL);
		check |= optparse_inplace(str, expr, &expr);
		list_local(local, &pos, symbol, expr, env, NULL);
		cons_local(local, &root, pos, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(&args, root);

	eval_parse_local(str->local, &pos, EVAL_PARSE_SYMBOL_MACROLET, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return 1;
}

static int checkparse_symbol_macrolet_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_SYMBOL_MACROLET))
		return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 2, &body); /* body */
	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_symbol_macrolet_body(struct optimize_struct *str)
{
	addr pos, args, decl, body;

	if (! checkparse_symbol_macrolet_body(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_SYMBOL_MACROLET, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return 1;
}

/* optparse-symbol-macrolet */
static int checkparse_symbol_macrolet(struct optimize_struct *str)
{
	return checkparse_symbol_macrolet_args(str)
		|| checkparse_symbol_macrolet_body(str);
}

static void optparse_symbol_macrolet_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_symbol_macrolet_args);
	optimize_extract(str, optparse_symbol_macrolet_body);
}
static int optparse_symbol_macrolet(struct optimize_struct *str)
{
	return optparse_run(str, optparse_symbol_macrolet_run);
}


/*
 *  lambda
 */
/* args */
static int checkparse_lambda_args(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LAMBDA))
		return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_lambda_ordinary(str, pos);
}

static int optparse_lambda_args(struct optimize_struct *str)
{
	addr pos, args, decl, doc, body, form;

	if (! checkparse_lambda_args(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &form);

	if (! optparse_lambda_ordinary(str, args, &args))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, form);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_lambda_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LAMBDA))
		return 0;
	GetEvalParse(pos, 1, &decl); /* decl */
	GetEvalParse(pos, 3, &body); /* body */
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_lambda_body(struct optimize_struct *str)
{
	addr pos, args, decl, doc, body, form;

	if (! checkparse_lambda_body(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &doc);
	GetEvalParse(pos, 3, &body);
	GetEvalParse(pos, 4, &form);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, doc);
	SetEvalParse(pos, 3, body);
	SetEvalParse(pos, 4, form);
	str->pos = pos;

	return 1;
}

/* optparse-lambda */
static int checkparse_lambda(struct optimize_struct *str)
{
	return checkparse_lambda_args(str)
		|| checkparse_lambda_body(str);
}

static void optparse_lambda_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_lambda_args);
	optimize_extract(str, optparse_lambda_body);
}
static int optparse_lambda(struct optimize_struct *str)
{
	return optparse_run(str, optparse_lambda_run);
}


/*
 *  if
 */
/* (if nil a b) -> b */
static int checkparse_if1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return optimize_evaltype(pos, EVAL_PARSE_NIL);
}

static int optparse_if1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_if1(str))
		return 0;
	GetEvalParse(str->pos, 2, &pos);
	str->pos = pos;

	return 1;
}

/* (if x a b) -> a */
static int checkparse_if2(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_IF))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return (! optimize_evaltype(pos, EVAL_PARSE_NIL)) && optimize_value(pos);
}

static int optparse_if2(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_if2(str))
		return 0;
	GetEvalParse(str->pos, 1, &pos);
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_if_all(struct optimize_struct *str)
{
	addr pos, check;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_IF))
		return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse_inplace(str, check))
		return 1;
	GetEvalParse(pos, 1, &check);
	if (checkparse_inplace(str, check))
		return 1;
	GetEvalParse(pos, 2, &check);
	if (checkparse_inplace(str, check))
		return 1;

	return 0;
}

static int optparse_if_all(struct optimize_struct *str)
{
	int check;
	addr pos, expr, ifthen, ifelse;

	if (! checkparse_if_all(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &expr);
	GetEvalParse(pos, 1, &ifthen);
	GetEvalParse(pos, 2, &ifelse);

	check = optparse_inplace(str, expr, &expr)
		|| optparse_inplace(str, ifthen, &ifthen)
		|| optparse_inplace(str, ifelse, &ifelse);
	if (! check)
		return 0;

	eval_parse_local(str->local, &pos, EVAL_PARSE_IF, 3);
	SetEvalParse(pos, 0, expr);
	SetEvalParse(pos, 1, ifthen);
	SetEvalParse(pos, 2, ifelse);
	str->pos = pos;

	return 1;
}

/* optparse-if */
static int checkparse_if(struct optimize_struct *str)
{
	return checkparse_if1(str)
		|| checkparse_if2(str)
		|| checkparse_if_all(str);
}

static void optparse_if_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_if1);
	optimize_extract(str, optparse_if2);
	optimize_extract(str, optparse_if_all);
}
static int optparse_if(struct optimize_struct *str)
{
	return optparse_run(str, optparse_if_run);
}


/*
 *  unwind-protect
 */
/* (unwind-protect value . tail) -> (progn ,@tail value) */
static int checkparse_unwind_protect1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return optimize_value(pos);
}

static int optparse_unwind_protect1(struct optimize_struct *str)
{
	addr pos, form, list, root;
	LocalRoot local;

	if (! checkparse_unwind_protect1(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	cons_local(local, &root, form, root);
	nreverse_list_unsafe(&list, root);
	/* progn */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, list);
	str->pos = pos;

	return 1;
}

/* (unwind-protect form . all-value) -> form */
static int checkparse_unwind_protect2(struct optimize_struct *str)
{
	addr list, x;

	if (! optimize_evaltype_on(str, EVAL_PARSE_UNWIND_PROTECT))
		return 0;
	GetEvalParse(str->pos, 1, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (! optimize_value(x))
			return 0;
	}

	return 1;
}

static int optparse_unwind_protect2(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_unwind_protect2(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos); /* form */
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_unwind_protect_all(struct optimize_struct *str)
{
	addr pos, check;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_UNWIND_PROTECT))
		return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse_inplace(str, check))
		return 1;
	GetEvalParse(pos, 1, &check);
	return checkparse_implicit_all(str, check);
}

static int optparse_unwind_protect_all(struct optimize_struct *str)
{
	int check;
	addr pos, form, list;

	if (! checkparse_unwind_protect_all(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &form);
	GetEvalParse(pos, 1, &list);
	check = optparse_inplace(str, form, &form)
		|| optparse_implicit_all(str, list, &list);
	if (! check)
		return 0;

	eval_parse_local(str->local, &pos, EVAL_PARSE_UNWIND_PROTECT, 2);
	SetEvalParse(pos, 0, form);
	SetEvalParse(pos, 1, list);
	str->pos = pos;

	return 1;
}

/* optparse-unwind-protect */
static int checkparse_unwind_protect(struct optimize_struct *str)
{
	return checkparse_unwind_protect1(str)
		|| checkparse_unwind_protect2(str)
		|| checkparse_unwind_protect_all(str);
}

static void optparse_unwind_protect_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_unwind_protect1);
	optimize_extract(str, optparse_unwind_protect2);
	optimize_extract(str, optparse_unwind_protect_all);
}
static int optparse_unwind_protect(struct optimize_struct *str)
{
	return optparse_run(str, optparse_unwind_protect_run);
}


/*
 *  tagbody
 */
/* (tagbody) -> nil */
static int checkparse_tagbody1(struct optimize_struct *str)
{
	addr pos, tag, body;
	size_t size1, size2;

	if (! optimize_evaltype_on(str, EVAL_PARSE_TAGBODY))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &tag);
	GetEvalParse(pos, 1, &body);
	if (body == Nil)
		return 1;
	size1 = length_list_unsafe(tag);
	size2 = length_list_unsafe(body);

	return size1 == size2;
}

static int optparse_tagbody1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_tagbody1(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* (tagbody (call) (call2)) -> (progn (call) (call2) nil) */
static int checkparse_tagbody2(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_TAGBODY))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	return pos == Nil;
}

static int optparse_tagbody2(struct optimize_struct *str)
{
	addr pos, root, list;
	LocalRoot local;

	if (! checkparse_tagbody2(str))
		return 0;
	GetEvalParse(str->pos, 1, &list);
	local = str->local;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		cons_local(local, &root, pos, root);
	}
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	cons_local(local, &root, pos, root);
	nreverse_list_unsafe(&root, root);
	/* progn */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, root);
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_tagbody_all(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_TAGBODY))
		return 0;
	GetEvalParse(pos, 1, &pos);
	/* Don't use checkparse_implicit. */
	return checkparse_implicit_all(str, pos);
}

static int optparse_tagbody_all(struct optimize_struct *str)
{
	addr pos, tag, body;

	if (! checkparse_tagbody_all(str)) return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &tag);
	GetEvalParse(pos, 1, &body);

	if (! optparse_implicit_all(str, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_TAGBODY, 2);
	SetEvalParse(pos, 0, tag);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return 1;
}

/* optparse-tagbody */
static int checkparse_tagbody(struct optimize_struct *str)
{
	return checkparse_tagbody1(str)
		|| checkparse_tagbody2(str)
		|| checkparse_tagbody_all(str);
}

static void optparse_tagbody_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_tagbody1);
	optimize_extract(str, optparse_tagbody2);
	optimize_extract(str, optparse_tagbody_all);
}
static int optparse_tagbody(struct optimize_struct *str)
{
	return optparse_run(str, optparse_tagbody_run);
}


/*
 *  block / return-from
 */
/* (block name) -> nil */
static int checkparse_block1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_BLOCK))
		return 0;
	GetEvalParse(str->pos, 1, &pos);
	return pos == Nil;
}

static int optparse_block1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_block1(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* (block name ... x) -> x */
static int checkparse_block2(struct optimize_struct *str)
{
	addr list, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_BLOCK))
		return 0;
	GetEvalParse(str->pos, 1, &list);
	if (list == Nil)
		return 0;
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (! optimize_value(check))
			return 0;
	}

	return 1;
}

static int optparse_block2(struct optimize_struct *str)
{
	addr list, x;

	if (! checkparse_block2(str))
		return 0;
	GetEvalParse(str->pos, 1, &list);
	if (list == Nil)
		return 0;
	x = str->pos;
	while (list != Nil) {
		GetCons(list, &x, &list);
	}
	str->pos = x;

	return 1;
}

/* all */
static int checkparse_block_all(struct optimize_struct *str)
{
	addr list;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_BLOCK))
		return 0;
	GetEvalParse(list, 1, &list);
	return checkparse_implicit_declare(str, Nil, list);
}

static int optparse_block_all(struct optimize_struct *str)
{
	addr pos, name, body;

	if (! checkparse_block_all(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &body);

	if (! optparse_implicit_declare(str, Nil, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_BLOCK, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return 1;
}

/* (return-from name expr) -> (return-from name expr) */
static int checkparse_return_from(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_RETURN_FROM))
		return 0;
	GetEvalParse(pos, 1, &pos);
	return checkparse_inplace(str, pos);
}

static int optparse_return_from(struct optimize_struct *str)
{
	addr pos, name, expr;

	if (! checkparse_return_from(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &expr);

	if (! optparse_inplace(str, expr, &expr))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_RETURN_FROM, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return 1;
}

/* optparse-block */
static int checkparse_block(struct optimize_struct *str)
{
	return checkparse_block1(str)
		|| checkparse_block2(str)
		|| checkparse_block_all(str)
		|| checkparse_return_from(str);
}

static void optparse_block_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_block1);
	optimize_extract(str, optparse_block2);
	optimize_extract(str, optparse_block_all);
	optimize_extract(str, optparse_return_from);
}
static int optparse_block(struct optimize_struct *str)
{
	return optparse_run(str, optparse_block_run);
}


/*
 *  catch / throw
 */
/* (catch name) -> (progn name nil) */
static int checkparse_catch1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return 0;
	GetEvalParse(str->pos, 1, &pos);
	return pos == Nil;
}

static int optparse_catch1(struct optimize_struct *str)
{
	addr pos, name, list;
	LocalRoot local;

	if (! checkparse_catch1(str))
		return 0;
	/* (name nil) */
	GetEvalParse(str->pos, 0, &name);
	local = str->local;
	eval_single_parse_local(local, &pos, EVAL_PARSE_NIL, Nil);
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name nil) */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, list);
	str->pos = pos;

	return 1;
}

/* (catch name ... x) -> (progn name x) */
static int checkparse_catch2(struct optimize_struct *str)
{
	addr pos, check;

	if (! optimize_evaltype_on(str, EVAL_PARSE_CATCH))
		return 0;
	GetEvalParse(str->pos, 1, &pos);
	if (pos == Nil)
		return 0;
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_value(check))
			return 0;
	}

	return 1;
}

static int optparse_catch2(struct optimize_struct *str)
{
	addr pos, name, list;
	LocalRoot local;

	if (! checkparse_catch2(str))
		return 0;
	/* (name lastcar) */
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &list);
	if (list == Nil)
		return 0;
	for (pos = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
	}
	local = str->local;
	conscar_local(local, &list, pos);
	cons_local(local, &list, name, list);
	/* (progn name lastcar) */
	eval_single_parse_local(local, &pos, EVAL_PARSE_PROGN, list);
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_catch_all(struct optimize_struct *str)
{
	addr pos, check;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_CATCH))
		return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse_inplace(str, check))
		return 1;
	GetEvalParse(pos, 1, &check);
	return checkparse_implicit_declare(str, Nil, check);
}

static int optparse_catch_all(struct optimize_struct *str)
{
	int check;
	addr pos, name, list;

	if (! checkparse_catch_all(str)) return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &list);

	check = optparse_inplace(str, name, &name)
		|| optparse_implicit_declare(str, Nil, list, &list);
	if (! check)
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_CATCH, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, list);
	str->pos = pos;

	return 1;
}

/* throw */
static int checkparse_throw(struct optimize_struct *str)
{
	addr pos, check;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_THROW))
		return 0;
	GetEvalParse(pos, 0, &check);
	if (checkparse_inplace(str, check))
		return 1;
	GetEvalParse(pos, 1, &check);
	return checkparse_inplace(str, check);
}

static int optparse_throw(struct optimize_struct *str)
{
	int check;
	addr pos, name, expr;

	if (! checkparse_throw(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &expr);

	check = optparse_inplace(str, name, &name)
		|| optparse_inplace(str, expr, &expr);
	if (! check)
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_THROW, 2);
	SetEvalParse(pos, 0, name);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return 1;
}

/* optparse-catch */
static int checkparse_catch(struct optimize_struct *str)
{
	return checkparse_catch1(str)
		|| checkparse_catch2(str)
		|| checkparse_catch_all(str)
		|| checkparse_throw(str);
}

static void optparse_catch_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_catch1);
	optimize_extract(str, optparse_catch2);
	optimize_extract(str, optparse_catch_all);
	optimize_extract(str, optparse_throw);
}
static int optparse_catch(struct optimize_struct *str)
{
	return optparse_run(str, optparse_catch_run);
}


/*
 *  flet / labels
 */
static int optimize_fletlabels(struct optimize_struct *str)
{
	addr pos;
	enum EVAL_PARSE type;

	pos = str->pos;
	if (! eval_parse_p(pos))
		return 0;
	GetEvalParseType(pos, &type);

	return type == EVAL_PARSE_FLET || type == EVAL_PARSE_LABELS;
}

static int optimize_fletlabels_on(struct optimize_struct *str)
{
	return optimize_speed_on(str) && optimize_fletlabels(str);
}

/* (flet ()) -> nil */
static int checkparse_flet1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_fletlabels_on(str))
		return 0;
	GetEvalParse(str->pos, 2, &pos);
	return pos == Nil;
}

static int optparse_flet1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_flet1(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* (flet () values... x) -> x */
static int checkparse_flet2(struct optimize_struct *str)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 1, &check);
	if (! empty_nil_declare(check))
		return 0;
	GetEvalParse(pos, 2, &pos);
	if (pos == Nil)
		return 0;
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		if (! optimize_value(check))
			return 0;
	}

	return 1;
}

static int optparse_flet2(struct optimize_struct *str)
{
	addr list, x;

	if (! checkparse_flet2(str))
		return 0;
	GetEvalParse(str->pos, 2, &list);
	if (list == Nil)
		return 0;
	for (x = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
	}
	str->pos = x;

	return 1;
}

/* (flet () ...) -> (progn ...) */
static int checkparse_flet3(struct optimize_struct *str)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &check);
	if (check != Nil)
		return 0;
	GetEvalParse(pos, 1, &check);
	if (! empty_nil_declare(check))
		return 0;
	GetEvalParse(pos, 2, &check);
	return check != Nil;
}

static int optparse_flet3(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_flet3(str))
		return 0;
	GetEvalParse(str->pos, 2, &pos);
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return 1;
}

/* (flet () (declare ...) ...) -> (locally (declare ...) ...) */
static int checkparse_flet4(struct optimize_struct *str)
{
	addr pos, check;

	if (! optimize_fletlabels_on(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &check);
	if (check != Nil)
		return 0;
	GetEvalParse(pos, 1, &check);
	if (empty_nil_declare(check))
		return 0;
	GetEvalParse(pos, 2, &check);
	return check != Nil;
}

static int optparse_flet4(struct optimize_struct *str)
{
	addr pos, decl, cons;

	if (! checkparse_flet4(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &cons);

	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, cons);
	str->pos = pos;

	return 1;
}

/* flet-args */
static int checkparse_flet_args(struct optimize_struct *str)
{
	addr pos, check, call, args, decl, doc, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	while (pos != Nil) {
		GetCons(pos, &check, &pos);
		List_bind(check, &call, &args, &decl, &doc, &body, NULL);
		if (checkparse_lambda_ordinary(str, args))
			return 1;
		if (checkparse_implicit_declare(str, decl, body))
			return 1;
	}

	return 0;
}

static int optparse_flet_one(struct optimize_struct *str, addr list, addr *ret)
{
	int check, check1, check2;
	addr root, call, args, decl, doc, body, x;
	LocalRoot local;

	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		List_bind(x, &call, &args, &decl, &doc, &body, NULL);

		check1 = check2 = 0;
		if (checkparse_lambda_ordinary(str, args))
			check1 = optparse_lambda_ordinary(str, args, &args);
		if (checkparse_implicit_declare(str, decl, body))
			check2 = optparse_implicit_declare(str, decl, body, &body);
		if (check1 || check2) {
			list_local(local, &x, call, args, decl, doc, body, NULL);
			check = 1;
		}
		cons_local(local, &root, x, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(ret, root);

	return 1;
}

static int optparse_flet_args(struct optimize_struct *str)
{
	enum EVAL_PARSE type;
	addr pos, args, decl, body;

	if (! checkparse_flet_args(str))
		return 0;
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	if (! optparse_flet_one(str, args, &args))
		return 0;
	eval_parse_local(str->local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return 1;
}

/* flet-body */
static int checkparse_flet_body(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	if (! optimize_fletlabels(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);
	if (body == Nil)
		return 0;

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_flet_body(struct optimize_struct *str)
{
	enum EVAL_PARSE type;
	addr pos, args, decl, body;

	if (! checkparse_flet_body(str))
		return 0;
	pos = str->pos;
	GetEvalParseType(pos, &type);
	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, type, 3);
	SetEvalParse(pos, 0, args);
	SetEvalParse(pos, 1, decl);
	SetEvalParse(pos, 2, body);
	str->pos = pos;

	return 1;
}

/* optparse-flet */
static int checkparse_flet(struct optimize_struct *str)
{
	return checkparse_flet1(str)
		|| checkparse_flet2(str)
		|| checkparse_flet3(str)
		|| checkparse_flet4(str)
		|| checkparse_flet_args(str)
		|| checkparse_flet_body(str);
}

static void optparse_flet_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_flet1);
	optimize_extract(str, optparse_flet2);
	optimize_extract(str, optparse_flet3);
	optimize_extract(str, optparse_flet4);
	optimize_extract(str, optparse_flet_args);
	optimize_extract(str, optparse_flet_body);
}
static int optparse_flet(struct optimize_struct *str)
{
	return optparse_run(str, optparse_flet_run);
}


/*
 *  the
 */
/* (the type expr) -> (the [type] expr) */
static int checkparse_the1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_THE))
		return 0;
	GetEvalParse(str->pos, 0, &pos); /* type */
	/* return ! type_optimized_or_subtypep(pos); */
	return ! type_optimized_p(pos);
}

static int optparse_the1(struct optimize_struct *str)
{
	addr pos, type, expr;
	LocalRoot local;

	if (! checkparse_the1(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &expr);

	local = str->local;
	type_optimize_local(local, &type, type);
	eval_parse_local(local, &pos, EVAL_PARSE_THE, 2);
	SetEvalParse(pos, 0, type);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return 1;
}

/* expr */
static int checkparse_the2(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_THE))
		return 0;
	GetEvalParse(pos, 1, &pos); /* expr */
	return checkparse_inplace(str, pos);
}

static int optparse_the2(struct optimize_struct *str)
{
	addr pos, type, expr;

	if (! checkparse_the2(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &expr);

	if (! optparse_inplace(str, expr, &expr))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_THE, 2);
	SetEvalParse(pos, 0, type);
	SetEvalParse(pos, 1, expr);
	str->pos = pos;

	return 1;
}

/* optparse-the */
static int checkparse_the(struct optimize_struct *str)
{
	return checkparse_the1(str)
		|| checkparse_the2(str);
}

static void optparse_the_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_the1);
	optimize_extract(str, optparse_the2);
}
static int optparse_the(struct optimize_struct *str)
{
	return optparse_run(str, optparse_the_run);
}


/*
 *  eval-when
 */
/* (eval-when cons) -> nil */
static int checkparse_eval_when1(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_EVAL_WHEN))
		return 0;
	GetEvalParse(str->pos, 0, &pos); /* body */
	return pos == Nil;
}

static int optparse_eval_when1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_eval_when1(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_eval_when_all(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_EVAL_WHEN))
		return 0;
	GetEvalParse(pos, 0, &pos); /* body */
	return checkparse_implicit_declare(str, Nil, pos);
}

static int optparse_eval_when_all(struct optimize_struct *str)
{
	addr pos, body, compilep, loadp, evalp;

	if (! checkparse_eval_when_all(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &body);
	GetEvalParse(pos, 1, &compilep);
	GetEvalParse(pos, 2, &loadp);
	GetEvalParse(pos, 3, &evalp);

	if (! optparse_implicit_declare(str, Nil, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_EVAL_WHEN, 4);
	SetEvalParse(pos, 0, body);
	SetEvalParse(pos, 1, compilep);
	SetEvalParse(pos, 2, loadp);
	SetEvalParse(pos, 3, evalp);
	str->pos = pos;

	return 1;
}

/* optparse-eval-when */
static int checkparse_eval_when(struct optimize_struct *str)
{
	return checkparse_eval_when1(str)
		|| checkparse_eval_when_all(str);
}

static void optparse_eval_when_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_eval_when1);
	optimize_extract(str, optparse_eval_when_all);
}
static int optparse_eval_when(struct optimize_struct *str)
{
	return optparse_run(str, optparse_eval_when_run);
}


/*
 *  values
 */
static int checkparse_values(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_VALUES))
		return 0;
	GetEvalParse(pos, 0, &pos);
	return checkparse_implicit_all(str, pos);
}

static int optparse_values(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_values(str))
		return 0;
	GetEvalParse(str->pos, 0, &pos);
	if (! optparse_implicit_all(str, pos, &pos))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_VALUES, pos);
	str->pos = pos;

	return 1;
}


/*
 *  locally
 */
/* (locally ...) -> (progn ...) */
static int checkparse_locally1(struct optimize_struct *str)
{
	addr pos, decl, body;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &decl); /* decl */
	GetEvalParse(pos, 1, &body); /* body */
	return empty_nil_declare(decl) && body != Nil;
}

static int optparse_locally1(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_locally1(str))
		return 0;
	GetEvalParse(str->pos, 1, &pos); /* body */
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_PROGN, pos);
	str->pos = pos;

	return 1;
}

/* (locally (declare ...)) -> nil */
static int checkparse_locally2(struct optimize_struct *str)
{
	addr pos;

	if (! optimize_evaltype_on(str, EVAL_PARSE_LOCALLY))
		return 0;
	GetEvalParse(str->pos, 1, &pos); /* body */
	return pos == Nil;
}

static int optparse_locally2(struct optimize_struct *str)
{
	addr pos;

	if (! checkparse_locally2(str))
		return 0;
	eval_single_parse_local(str->local, &pos, EVAL_PARSE_NIL, Nil);
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_locally_all(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_LOCALLY))
		return 0;
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &body);

	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_locally_all(struct optimize_struct *str)
{
	addr pos, decl, body;

	if (! checkparse_locally_all(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return 1;
}

/* optparse-locally */
static int checkparse_locally(struct optimize_struct *str)
{
	return checkparse_locally1(str)
		|| checkparse_locally2(str)
		|| checkparse_locally_all(str);
}

static void optparse_locally_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_locally1);
	optimize_extract(str, optparse_locally2);
	optimize_extract(str, optparse_locally_all);
}
static int optparse_locally(struct optimize_struct *str)
{
	return optparse_run(str, optparse_locally_run);
}


/*
 *  call
 */
/* first argument */
static int checkparse_call1(struct optimize_struct *str)
{
	addr pos;

	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_CALL))
		return 0;
	GetEvalParse(pos, 0, &pos); /* call */
	return checkparse_inplace(str, pos);
}

static int optparse_call1(struct optimize_struct *str)
{
	addr pos, call, cons;

	if (! checkparse_call1(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &cons);

	if (! optparse_inplace(str, call, &call))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, cons);
	str->pos = pos;

	return 1;
}

/* all */
static int checkparse_call_all(struct optimize_struct *str)
{
	addr list, x;

	/* Don't check optimize. */
	list = str->pos;
	if (! optimize_evaltype(list, EVAL_PARSE_CALL))
		return 0;
	GetEvalParse(list, 1, &list); /* cons */
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (checkparse_inplace(str, x))
			return 1;
	}

	return 0;
}

static int optparse_call_all(struct optimize_struct *str)
{
	int check;
	addr pos, call, list, root, x;
	LocalRoot local;

	if (! checkparse_call_all(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &list);

	local = str->local;
	check = 0;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		check |= optparse_inplace(str, x, &x);
		cons_local(local, &root, x, root);
	}
	if (! check)
		return 0;
	nreverse_list_unsafe(&list, root);

	eval_parse_local(str->local, &pos, EVAL_PARSE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, list);
	str->pos = pos;

	return 1;
}

/* optparse-call */
static int checkparse_call(struct optimize_struct *str)
{
	return checkparse_call1(str)
		|| checkparse_call_all(str);
}

static void optparse_call_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_call1);
	optimize_extract(str, optparse_call_all);
}
static int optparse_call(struct optimize_struct *str)
{
	return optparse_run(str, optparse_call_run);
}


/*
 *  multiple-value-bind
 */
/* expr */
static int checkparse_multiple_value_bind1(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND))
		return 0;
	GetEvalParse(str->pos, 1, &pos); /* expr */
	return checkparse_inplace(str, pos);
}

static int optparse_multiple_value_bind1(struct optimize_struct *str)
{
	addr pos, vars, expr, decl, doc, body;

	if (! checkparse_multiple_value_bind1(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &vars);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	if (! optparse_inplace(str, expr, &expr))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(pos, 0, vars);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_multiple_value_bind2(struct optimize_struct *str)
{
	addr pos, decl, body;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_BIND))
		return 0;
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 4, &body);
	return checkparse_implicit_declare(str, decl, body);
}

static int optparse_multiple_value_bind2(struct optimize_struct *str)
{
	addr pos, vars, expr, decl, doc, body;

	if (! checkparse_multiple_value_bind2(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &vars);
	GetEvalParse(pos, 1, &expr);
	GetEvalParse(pos, 2, &decl);
	GetEvalParse(pos, 3, &doc);
	GetEvalParse(pos, 4, &body);

	if (! optparse_implicit_declare(str, decl, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(pos, 0, vars);
	SetEvalParse(pos, 1, expr);
	SetEvalParse(pos, 2, decl);
	SetEvalParse(pos, 3, doc);
	SetEvalParse(pos, 4, body);
	str->pos = pos;

	return 1;
}

/* optparse-multiple-value-bind */
static int checkparse_multiple_value_bind(struct optimize_struct *str)
{
	return checkparse_multiple_value_bind1(str)
		|| checkparse_multiple_value_bind2(str);
}

static void optparse_multiple_value_bind_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_multiple_value_bind1);
	optimize_extract(str, optparse_multiple_value_bind2);
}
static int optparse_multiple_value_bind(struct optimize_struct *str)
{
	return optparse_run(str, optparse_multiple_value_bind_run);
}


/*
 *  multiple-value-call
 */
/* expr */
static int checkparse_multiple_value_call1(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL))
		return 0;
	GetEvalParse(pos, 0, &pos); /* expr */
	return checkparse_inplace(str, pos);
}

static int optparse_multiple_value_call1(struct optimize_struct *str)
{
	addr pos, call, body;

	if (! checkparse_multiple_value_call1(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &body);

	if (! optparse_inplace(str, call, &call))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return 1;
}

/* body */
static int checkparse_multiple_value_call2(struct optimize_struct *str)
{
	addr pos;

	/* Don't check optimize. */
	pos = str->pos;
	if (! optimize_evaltype(pos, EVAL_PARSE_MULTIPLE_VALUE_CALL))
		return 0;
	GetEvalParse(pos, 1, &pos); /* body */
	return checkparse_implicit_all(str, pos);
}

static int optparse_multiple_value_call2(struct optimize_struct *str)
{
	addr pos, call, body;

	if (! checkparse_multiple_value_call2(str))
		return 0;
	pos = str->pos;
	GetEvalParse(pos, 0, &call);
	GetEvalParse(pos, 1, &body);

	if (! optparse_implicit_all(str, body, &body))
		return 0;
	eval_parse_local(str->local, &pos, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(pos, 0, call);
	SetEvalParse(pos, 1, body);
	str->pos = pos;

	return 1;
}

/* optparse-multiple-value-call */
static int checkparse_multiple_value_call(struct optimize_struct *str)
{
	return checkparse_multiple_value_call1(str)
		|| checkparse_multiple_value_call2(str);
}

static void optparse_multiple_value_call_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_multiple_value_call1);
	optimize_extract(str, optparse_multiple_value_call2);
}
static int optparse_multiple_value_call(struct optimize_struct *str)
{
	return optparse_run(str, optparse_multiple_value_call_run);
}


/*
 *  optimize-parse
 */
static int checkparse_all(struct optimize_struct *str)
{
	return checkparse_check(str)
		|| checkparse_progn(str)
		|| checkparse_let(str)
		|| checkparse_setq(str)
		|| checkparse_defun(str)
		|| checkparse_defmacro(str)
		|| checkparse_deftype(str)
		|| checkparse_define_compiler_macro(str)
		|| checkparse_destructuring_bind(str)
		|| checkparse_define_symbol_macro(str)
		|| checkparse_symbol_macrolet(str)
		|| checkparse_lambda(str)
		|| checkparse_if(str)
		|| checkparse_unwind_protect(str)
		|| checkparse_tagbody(str)
		|| checkparse_block(str)
		|| checkparse_catch(str)
		|| checkparse_flet(str)
		|| checkparse_the(str)
		|| checkparse_eval_when(str)
		|| checkparse_values(str)
		|| checkparse_locally(str)
		|| checkparse_call(str)
		|| checkparse_multiple_value_bind(str)
		|| checkparse_multiple_value_call(str);
}

static void optparse_all_run(struct optimize_struct *str)
{
	optimize_extract(str, optparse_check);
	optimize_extract(str, optparse_progn);
	optimize_extract(str, optparse_let);
	optimize_extract(str, optparse_setq);
	optimize_extract(str, optparse_defun);
	optimize_extract(str, optparse_defmacro);
	optimize_extract(str, optparse_deftype);
	optimize_extract(str, optparse_define_compiler_macro);
	optimize_extract(str, optparse_destructuring_bind);
	optimize_extract(str, optparse_define_symbol_macro);
	optimize_extract(str, optparse_symbol_macrolet);
	optimize_extract(str, optparse_lambda);
	optimize_extract(str, optparse_if);
	optimize_extract(str, optparse_unwind_protect);
	optimize_extract(str, optparse_tagbody);
	optimize_extract(str, optparse_block);
	optimize_extract(str, optparse_catch);
	optimize_extract(str, optparse_flet);
	optimize_extract(str, optparse_the);
	optimize_extract(str, optparse_eval_when);
	optimize_extract(str, optparse_values);
	optimize_extract(str, optparse_locally);
	optimize_extract(str, optparse_call);
	optimize_extract(str, optparse_multiple_value_bind);
	optimize_extract(str, optparse_multiple_value_call);
}
static int optparse_all(struct optimize_struct *str)
{
	return optparse_run(str, optparse_all_run);
}

_g int optimize_parse(LocalRoot local, addr *ret, addr pos)
{
	int result;
	LocalStack stack;
	struct optimize_struct str;

	CheckLocal(local);
	push_local(local, &stack);
	optimize_initialize(&str, local, pos);
	result = optparse_all(&str);
	if (result)
		copy_eval_parse_heap(ret, str.pos);
	else
		*ret = pos;
	rollback_local(local, stack);

	return result;
}

