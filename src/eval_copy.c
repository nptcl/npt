#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "eval_declare.h"
#include "eval_parse.h"
#include "heap.h"
#include "local.h"
#include "type.h"
#include "type_copy.h"

static void copy_eval_parse(LocalRoot local, addr *ret, addr pos);

/* single */
static void copy_eval_single(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	eval_single_parse_alloc(local, ret, type, eval);
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
	enum EVAL_PARSE type;

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
	nreverse_list_unsafe(ret, root);
}

static void copy_eval_progn(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGN, "parse error");
	GetEvalParse(eval, 0, &eval);
	copy_eval_allcons(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
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
	nreverse_list_unsafe(ret, root);
}

static void copy_eval_let(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LET && type != EVAL_PARSE_LETA, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);
	copy_eval_let_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
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
	nreverse_list_unsafe(ret, root);
}

static void copy_eval_setq(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_SETQ, "parse error");
	GetEvalParse(eval, 0, &cons);
	copy_eval_setq_args(local, &cons, cons);
	eval_single_parse_alloc(local, ret, EVAL_PARSE_SETQ, cons);
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
	nreverse_list_unsafe(ret, root);
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
	nreverse_list_unsafe(ret, root);
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
	nreverse_list_unsafe(ret, root);
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
	enum EVAL_PARSE type;
	addr name, args, decl, doc, body, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFUN, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &body);
	GetEvalParse(eval, 5, &form);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 6);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	SetEvalParse(eval, 5, form);
	*ret = eval;
}

/* defmacro */
static void copy_eval_defmacro(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
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
	nreverse_list_unsafe(ret, root);
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
	enum EVAL_PARSE type;
	addr args, decl, doc, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MACRO_LAMBDA, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &doc);
	GetEvalParse(eval, 3, &body);

	copy_eval_macro_arguments(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, body);
	*ret = eval;
}

/* deftype */
static void copy_eval_deftype(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
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
	enum EVAL_PARSE type;
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
	enum EVAL_PARSE type;
	addr expr, lambda;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DESTRUCTURING_BIND, "parse error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &lambda);

	copy_eval_parse(local, &expr, expr);
	copy_eval_macro_lambda(local, &lambda, lambda);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, lambda);
	*ret = eval;
}

/* define-symbol-macro */
static void copy_eval_define_symbol_macro(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr symbol, form, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFINE_SYMBOL_MACRO, "parse error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &form);
	GetEvalParse(eval, 2, &body);

	copy_eval_parse(local, &form, form);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, symbol);
	SetEvalParse(eval, 1, form);
	SetEvalParse(eval, 2, body);
	*ret = eval;
}

/* symbol-macrolet */
static void copy_eval_symbol_macrolet_args(LocalRoot local, addr *ret, addr args)
{
	addr root, list, symbol, form, env;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &symbol, &form, &env, NULL);
		copy_eval_parse(local, &form, form);
		copy_environment(&env, env);
		list_alloc(local, &symbol, symbol, form, env, NULL);
		cons_alloc(local, &root, symbol, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void copy_eval_symbol_macrolet(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_SYMBOL_MACROLET, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	copy_eval_symbol_macrolet_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* lambda */
static void copy_eval_lambda(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, doc, cons, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LAMBDA, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &doc);
	GetEvalParse(eval, 3, &cons);
	GetEvalParse(eval, 4, &form);

	copy_eval_ordinary(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, form);
	*ret = eval;
}

/* if */
static void copy_eval_if(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr expr, then, last;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_IF, "parse error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &then);
	GetEvalParse(eval, 2, &last);

	copy_eval_parse(local, &expr, expr);
	copy_eval_parse(local, &then, then);
	copy_eval_parse(local, &last, last);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, then);
	SetEvalParse(eval, 2, last);
	*ret = eval;
}

/* unwind-protect */
static void copy_eval_unwind_protect(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr form, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_UNWIND_PROTECT, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &form, form);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* tagbody */
static void copy_eval_tagbody(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAGBODY, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	copy_eval_allcons(local, &tag, tag);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* tag */
static void copy_eval_tag(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAG, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &value);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}

/* block */
static void copy_eval_block(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_BLOCK, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &cons);

	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* return-from */
static void copy_eval_return_from(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_RETURN_FROM, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &value);

	copy_eval_parse(local, &value, value);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}

/* catch */
static void copy_eval_catch(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CATCH, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &tag, tag);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* throw */
static void copy_eval_throw(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, result;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_THROW, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &result);

	copy_eval_parse(local, &tag, tag);
	copy_eval_parse(local, &result, result);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, result);
	*ret = eval;
}

/* flet / labels */
static void copy_eval_flet_one(LocalRoot local, addr *ret, addr cons)
{
	addr name, args, decl, doc, body;

	list_bind(cons, &name, &args, &decl, &doc, &body, NULL);

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
	nreverse_list_unsafe(ret, root);
}

static void copy_eval_flet(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_FLET && type != EVAL_PARSE_LABELS, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	copy_eval_flet_args(local, &args, args);
	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

/* the */
static void copy_eval_the(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr ptype, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_THE, "parse error");
	GetEvalParse(eval, 0, &ptype);
	GetEvalParse(eval, 1, &expr);

	type_copy_alloc(local, &ptype, ptype);
	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, ptype);
	SetEvalParse(eval, 1, expr);
	*ret = eval;
}

/* when */
static void copy_eval_eval_when(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr cons, compilep, loadp, evalp;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_EVAL_WHEN, "parse error");
	GetEvalParse(eval, 0, &cons);
	GetEvalParse(eval, 1, &compilep);
	GetEvalParse(eval, 2, &loadp);
	GetEvalParse(eval, 3, &evalp);

	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, compilep);
	SetEvalParse(eval, 2, loadp);
	SetEvalParse(eval, 3, evalp);
	*ret = eval;
}

/* values */
static void copy_eval_values(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_VALUES, "parse error");
	GetEvalParse(eval, 0, &eval);
	copy_eval_allcons(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

/* locally */
static void copy_eval_locally(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOCALLY, "parse error");
	GetEvalParse(eval, 0, &decl);
	GetEvalParse(eval, 1, &cons);

	copy_eval_declaim_nil(local, &decl, decl);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, decl);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* call */
static void copy_eval_call(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CALL, "parse error");
	GetEvalParse(eval, 0, &call);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* multiple-value-bind */
static void copy_eval_multiple_value_bind(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr vars, expr, decl, doc, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_BIND, "parse error");
	GetEvalParse(eval, 0, &vars);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &form);

	copy_eval_parse(local, &expr, expr);
	copy_eval_allcons(local, &form, form);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, vars);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, form);
	*ret = eval;
}

/* multiple-value-call */
static void copy_eval_multiple_value_call(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_CALL, "parse error");
	GetEvalParse(eval, 0, &call);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* multiple-value-prog1 */
static void copy_eval_multiple_value_prog1(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_PROG1, "parse error");
	GetEvalParse(eval, 0, &call);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &call, call);
	copy_eval_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

/* nth-value */
static void copy_eval_nth_value(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr nth, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_NTH_VALUE, "parse error");
	GetEvalParse(eval, 0, &nth);
	GetEvalParse(eval, 1, &expr);

	copy_eval_parse(local, &nth, nth);
	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, nth);
	SetEvalParse(eval, 1, expr);
	*ret = eval;
}

/* progv */
static void copy_eval_progv(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr symbols, values, body;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGV, "parse error");
	GetEvalParse(eval, 0, &symbols);
	GetEvalParse(eval, 1, &values);
	GetEvalParse(eval, 2, &body);

	copy_eval_parse(local, &symbols, symbols);
	copy_eval_parse(local, &values, values);
	copy_eval_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, symbols);
	SetEvalParse(eval, 1, values);
	SetEvalParse(eval, 2, body);
	*ret = eval;
}


/*
 *  interface
 */
_g void copy_eval_parse_alloc(LocalRoot local, addr *ret, addr eval)
{
	copy_eval_parse(local, ret, eval);
}

_g void copy_eval_parse_local(LocalRoot local, addr *ret, addr eval)
{
	Check(local == NULL, "local error");
	copy_eval_parse_alloc(local, ret, eval);
}

_g void copy_eval_parse_heap(addr *ret, addr eval)
{
	copy_eval_parse_alloc(NULL, ret, eval);
}


/*
 *  initialize
 */
typedef void (*copy_eval_calltype)(LocalRoot, addr *, addr);
static copy_eval_calltype EvalCopyTable[EVAL_PARSE_SIZE];

static void copy_eval_parse(LocalRoot local, addr *ret, addr pos)
{
	enum EVAL_PARSE type;
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
	_fmte("parse-error: ~S.", pos, NULL);
	*ret = Nil;
}

_g void init_eval_copy(void)
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
	EvalCopyTable[EVAL_PARSE_FUNCTION] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_PATHNAME] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_ENVIRONMENT] = copy_eval_single;
	EvalCopyTable[EVAL_PARSE_QUOTE] = copy_eval_single;
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
	EvalCopyTable[EVAL_PARSE_DEFINE_SYMBOL_MACRO] = copy_eval_define_symbol_macro;
	EvalCopyTable[EVAL_PARSE_SYMBOL_MACROLET] = copy_eval_symbol_macrolet;
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
}

