#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "declare.h"
#include "eval_copy.h"
#include "eval_object.h"
#include "heap.h"
#include "local.h"
#include "load_time_value.h"
#include "parse.h"
#include "parse_object.h"
#include "parse_macro.h"
#include "step.h"
#include "type.h"
#include "type_copy.h"

/* single */
static void copy_eval_single(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

/* double */
static void copy_eval_double(LocalRoot local, addr *ret, addr eval)
{
	addr x, y;
	EvalParse type;

	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &x);
	GetEvalParse(eval, 1, &y);
	eval_parse2_alloc(local, ret, type, x, y);
}

/* declaim */
static void copy_eval_declaim_nil(LocalRoot local, addr *ret, addr pos)
{
	if (pos == Nil)
		*ret = Nil;
	else
		copy_eval_declare_alloc(local, ret, pos);
}

static void copy_eval_declaim(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;

	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	copy_eval_declaim_nil(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

/* progn */
static void copy_eval_allcons(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		copy_eval_parse(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_progn(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, list;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGN, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &list);
	copy_eval_allcons(local, &list, list);
	eval_parse2_alloc(local, ret, type, form, list);
}

/* let */
static void copy_eval_let_args(LocalRoot local, addr *ret, addr args)
{
	addr root, init, pos;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &init);
		copy_eval_parse(local, &init, init);
		cons_alloc(local, &pos, pos, init);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_let(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LET && type != EVAL_PARSE_LETA, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &cons);
	copy_eval_let_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);
	*ret = eval;
}

/* setq */
static void copy_eval_setq_args(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos, value;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		GetCons(pos, &pos, &value);
		copy_eval_parse(local, &value, value);
		cons_alloc(local, &pos, pos, value);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_setq(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, list;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_SETQ, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &list);
	copy_eval_setq_args(local, &list, list);
	eval_parse2_alloc(local, ret, EVAL_PARSE_SETQ, form, list);
}

/* defun */
static void copy_eval_ordinary_optional(LocalRoot local, addr *ret, addr cons)
{
	addr root, var, init, svar;

	for (root = Nil; cons != Nil; ) {
		/* (var init svar) */
		GetCons(cons, &svar, &cons);
		GetCons(svar, &var, &svar);
		GetCons(svar, &init, &svar);
		GetCar(svar, &svar);
		copy_eval_parse(local, &init, init);
		list_alloc(local, &svar, var, init, svar, NULL);
		cons_alloc(local, &root, svar, root);
	}
	nreverse(ret, root);
}

static void copy_eval_ordinary_key(LocalRoot local, addr *ret, addr cons)
{
	addr root, var, name, init, svar;

	for (root = Nil; cons != Nil; ) {
		/* (var name init svar) */
		GetCons(cons, &svar, &cons);
		GetCons(svar, &var, &svar);
		GetCons(svar, &name, &svar);
		GetCons(svar, &init, &svar);
		GetCar(svar, &svar);
		copy_eval_parse(local, &init, init);
		list_alloc(local, &svar, var, name, init, svar, NULL);
		cons_alloc(local, &root, svar, root);
	}
	nreverse(ret, root);
}

static void copy_eval_ordinary_aux(LocalRoot local, addr *ret, addr cons)
{
	addr root, var, init;

	for (root = Nil; cons != Nil; ) {
		/* (var init) */
		GetCons(cons, &init, &cons);
		GetCons(init, &var, &init);
		GetCar(init, &init);
		copy_eval_parse(local, &init, init);
		list_alloc(local, &init, var, init, NULL);
		cons_alloc(local, &root, init, root);
	}
	nreverse(ret, root);
}

static void copy_eval_ordinary(LocalRoot local, addr *ret, addr cons)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(cons, &var, &opt, &rest, &key, &allow, &aux, NULL);
	copy_eval_ordinary_optional(local, &opt, opt);
	copy_eval_ordinary_key(local, &key, key);
	copy_eval_ordinary_aux(local, &aux, aux);
	list_alloc(local, ret, var, opt, rest, key, allow, aux, NULL);
}

static void copy_eval_defun(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, args, decl, doc, body, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFUN, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &args);
	GetEvalParse(eval, 3, &decl);
	GetEvalParse(eval, 4, &doc);
	GetEvalParse(eval, 5, &body);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, args);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);
	*ret = eval;
}

/* defmacro */
static void copy_eval_defmacro(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, lambda;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFMACRO, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &lambda);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, lambda);
	*ret = eval;
}

/* macro-lambda */
static void copy_eval_macro_arguments(LocalRoot local, addr *ret, addr cons);
static void copy_eval_macro_var(LocalRoot local, addr *ret, addr list)
{
	addr root, var;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var))
			copy_eval_macro_arguments(local, &var, var);
		cons_alloc(local, &root, var, root);
	}
	nreverse(ret, root);
}

static void copy_eval_macro_rest(LocalRoot local, addr *ret, addr list)
{
	addr car, cdr;

	if (list != Nil) {
		GetCons(list, &car, &cdr);
		cons_heap(ret, car, cdr);
	}
}

static void copy_eval_macro_arguments(LocalRoot local, addr *ret, addr cons)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(cons, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	copy_eval_macro_var(local, &var, var);
	copy_eval_ordinary_optional(local, &opt, opt);
	copy_eval_macro_rest(local, &rest, rest);
	copy_eval_ordinary_key(local, &key, key);
	copy_eval_ordinary_aux(local, &aux, aux);
	list_alloc(local, ret, var, opt, rest, key, allow, aux, whole, env, NULL);
}

/* macro-lambda */
static void copy_eval_macro_lambda(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr args, decl, doc, body, call;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MACRO_LAMBDA, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &doc);
	GetEvalParse(eval, 3, &body);
	GetEvalParse(eval, 4, &call);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, body);
	SetEvalParse(eval, 4, call);
	*ret = eval;
}

/* deftype */
static void copy_eval_deftype(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, args, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFTYPE, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &body);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	*ret = eval;
}

/* define-compiler-macro */
static void copy_eval_define_compiler_macro(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr name, args, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFINE_COMPILER_MACRO, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &body);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	*ret = eval;
}

/* destructuring-bind */
static void copy_eval_destructuring_bind(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, lambda;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DESTRUCTURING_BIND, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &lambda);

	copy_eval_parse(local, &expr, expr);
	copy_eval_macro_lambda(local, &lambda, lambda);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, lambda);
	*ret = eval;
}

/* lambda */
static void copy_eval_lambda(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr args, decl, doc, cons, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LAMBDA, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &cons);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;
}

/* if */
static void copy_eval_if(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, then, last;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_IF, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &then);
	GetEvalParse(eval, 3, &last);

	copy_eval_parse(local, &expr, expr);
	copy_eval_parse(local, &then, then);
	copy_eval_parse(local, &last, last);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, then);
	SetEvalParse(eval, 3, last);
	*ret = eval;
}

/* unwind-protect */
static void copy_eval_unwind_protect(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, expr, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_UNWIND_PROTECT, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &expr, expr);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* tagbody */
static void copy_eval_tagbody(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAGBODY, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &cons);

	copy_eval_allcons(local, &tag, tag);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* tag */
static void copy_eval_tag(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr tag;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAG, "parse error");
	GetEvalParse(eval, 0, &tag);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	*ret = eval;
}

/* block */
static void copy_eval_block(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, name, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_BLOCK, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &cons);

	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* return-from */
static void copy_eval_return_from(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, name, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_RETURN_FROM, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &value);

	copy_eval_parse(local, &value, value);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, value);
	*ret = eval;
}

/* catch */
static void copy_eval_catch(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CATCH, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &tag, tag);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* throw */
static void copy_eval_throw(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, tag, result;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_THROW, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &result);

	copy_eval_parse(local, &tag, tag);
	copy_eval_parse(local, &result, result);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, result);
	*ret = eval;
}

/* flet / labels */
static void copy_eval_flet_one(LocalRoot local, addr *ret, addr cons)
{
	addr name, args, decl, doc, body;

	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	list_alloc(local, ret, name, args, decl, doc, body, NULL);
}

static void copy_eval_flet_args(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		copy_eval_flet_one(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(ret, root);
}

static void copy_eval_flet(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_FLET && type != EVAL_PARSE_LABELS, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &cons);

	copy_eval_flet_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);
	*ret = eval;
}

/* the */
static void copy_eval_the(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, the, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_THE, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &the);
	GetEvalParse(eval, 2, &expr);

	type_copy_alloc(local, &the, the);
	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, the);
	SetEvalParse(eval, 2, expr);
	*ret = eval;
}

/* when */
static void copy_eval_eval_when(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, cons, compile, load, exec, toplevel, mode;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_EVAL_WHEN, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &cons);
	GetEvalParse(eval, 2, &compile);
	GetEvalParse(eval, 3, &load);
	GetEvalParse(eval, 4, &exec);
	GetEvalParse(eval, 5, &toplevel);
	GetEvalParse(eval, 6, &mode);

	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 7);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	SetEvalParse(eval, 2, compile);
	SetEvalParse(eval, 3, load);
	SetEvalParse(eval, 4, exec);
	SetEvalParse(eval, 5, toplevel);
	SetEvalParse(eval, 6, mode);
	*ret = eval;
}

/* values */
static void copy_eval_values(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_VALUES, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &eval);
	copy_eval_allcons(local, &eval, eval);
	eval_parse2_alloc(local, ret, type, form, eval);
}

/* locally */
static void copy_eval_locally(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOCALLY, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* call */
static void copy_eval_call(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CALL, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &call);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* multiple-value-bind */
static void copy_eval_multiple_value_bind(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, vars, expr, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_BIND, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &vars);
	GetEvalParse(eval, 2, &expr);
	GetEvalParse(eval, 3, &decl);
	GetEvalParse(eval, 4, &doc);
	GetEvalParse(eval, 5, &body);

	copy_eval_parse(local, &expr, expr);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, vars);
	SetEvalParse(eval, 2, expr);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);
	*ret = eval;
}

/* multiple-value-call */
static void copy_eval_multiple_value_call(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_CALL, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &call);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* multiple-value-prog1 */
static void copy_eval_multiple_value_prog1(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_PROG1, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &call);
	GetEvalParse(eval, 2, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* nth-value */
static void copy_eval_nth_value(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, nth, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_NTH_VALUE, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &nth);
	GetEvalParse(eval, 2, &expr);

	copy_eval_parse(local, &nth, nth);
	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, nth);
	SetEvalParse(eval, 2, expr);
	*ret = eval;
}

/* progv */
static void copy_eval_progv(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr form, symbols, values, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGV, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &symbols);
	GetEvalParse(eval, 2, &values);
	GetEvalParse(eval, 3, &body);

	copy_eval_parse(local, &symbols, symbols);
	copy_eval_parse(local, &values, values);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, symbols);
	SetEvalParse(eval, 2, values);
	SetEvalParse(eval, 3, body);
	*ret = eval;
}


/*
 *  interface
 */
void copy_eval_parse_alloc(LocalRoot local, addr *ret, addr eval)
{
	copy_eval_parse(local, ret, eval);
}

void copy_eval_parse_local(LocalRoot local, addr *ret, addr eval)
{
	Check(local == NULL, "local error");
	copy_eval_parse_alloc(local, ret, eval);
}

void copy_eval_parse_heap(addr *ret, addr eval)
{
	copy_eval_parse_alloc(NULL, ret, eval);
}


/*
 *  initialize
 */
typedef void (*copy_eval_calltype)(LocalRoot, addr *, addr);
static copy_eval_calltype EvalCopyTable[EVAL_PARSE_SIZE];

void copy_eval_parse(LocalRoot local, addr *ret, addr pos)
{
	EvalParse type;
	copy_eval_calltype call;

	Check(! eval_parse_p(pos), "type error");
	GetEvalParseType(pos, &type);
	if (EVAL_PARSE_SIZE <= type)
		goto error;
	call = EvalCopyTable[type];
	if (call == NULL)
		goto error;
	(*call)(local, ret, pos);
	return;

error:
	*ret = Nil;
	infobit(pos);
	Abort("parse-error.");
}

void init_eval_copy(void)
{
	EvalCopyTable[EVAL_PARSE_NIL] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_T] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_CLOS] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_INTEGER] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_RATIONAL] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_COMPLEX] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_CHARACTER] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_ARRAY] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_VECTOR] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_BITVECTOR] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_STRING] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_SYMBOL] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_FLOAT] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_FUNCTION] = copy_eval_double;
	EvalCopyTable[EVAL_PARSE_PACKAGE] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_RANDOM_STATE] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_PATHNAME] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_ENVIRONMENT] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_PAPER] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_QUOTE] = copy_eval_double;
	EvalCopyTable[EVAL_PARSE_GO] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_DECLAIM] = copy_eval_declaim;
	EvalCopyTable[EVAL_PARSE_PROGN] = copy_eval_progn;
	EvalCopyTable[EVAL_PARSE_LET] = copy_eval_let;
	EvalCopyTable[EVAL_PARSE_LETA] = copy_eval_let;
	EvalCopyTable[EVAL_PARSE_SETQ] = copy_eval_setq;
	EvalCopyTable[EVAL_PARSE_DEFUN] = copy_eval_defun;
	EvalCopyTable[EVAL_PARSE_DEFMACRO] = copy_eval_defmacro;
	EvalCopyTable[EVAL_PARSE_MACRO_LAMBDA] = copy_eval_macro_lambda;
	EvalCopyTable[EVAL_PARSE_DEFTYPE] = copy_eval_deftype;
	EvalCopyTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = copy_eval_define_compiler_macro;
	EvalCopyTable[EVAL_PARSE_DESTRUCTURING_BIND] = copy_eval_destructuring_bind;
	EvalCopyTable[EVAL_PARSE_LAMBDA] = copy_eval_lambda;
	EvalCopyTable[EVAL_PARSE_IF] = copy_eval_if;
	EvalCopyTable[EVAL_PARSE_UNWIND_PROTECT] = copy_eval_unwind_protect;
	EvalCopyTable[EVAL_PARSE_TAGBODY] = copy_eval_tagbody;
	EvalCopyTable[EVAL_PARSE_TAG] = copy_eval_tag;
	EvalCopyTable[EVAL_PARSE_BLOCK] = copy_eval_block;
	EvalCopyTable[EVAL_PARSE_RETURN_FROM] = copy_eval_return_from;
	EvalCopyTable[EVAL_PARSE_CATCH] = copy_eval_catch;
	EvalCopyTable[EVAL_PARSE_THROW] = copy_eval_throw;
	EvalCopyTable[EVAL_PARSE_FLET] = copy_eval_flet;
	EvalCopyTable[EVAL_PARSE_LABELS] = copy_eval_flet;
	EvalCopyTable[EVAL_PARSE_THE] = copy_eval_the;
	EvalCopyTable[EVAL_PARSE_EVAL_WHEN] = copy_eval_eval_when;
	EvalCopyTable[EVAL_PARSE_VALUES] = copy_eval_values;
	EvalCopyTable[EVAL_PARSE_LOCALLY] = copy_eval_locally;
	EvalCopyTable[EVAL_PARSE_CALL] = copy_eval_call;
	EvalCopyTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = copy_eval_multiple_value_bind;
	EvalCopyTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = copy_eval_multiple_value_call;
	EvalCopyTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = copy_eval_multiple_value_prog1;
	EvalCopyTable[EVAL_PARSE_NTH_VALUE] = copy_eval_nth_value;
	EvalCopyTable[EVAL_PARSE_PROGV] = copy_eval_progv;
	EvalCopyTable[EVAL_PARSE_LOAD_TIME_VALUE] = copy_eval_load_time_value;
	EvalCopyTable[EVAL_PARSE_STEP] = copy_eval_step;
}

