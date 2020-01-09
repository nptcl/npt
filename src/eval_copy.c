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

static void copy_single(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	copylocal_object(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

static void copy_declaim_nil(LocalRoot local, addr *ret, addr pos)
{
	if (pos == Nil)
		*ret = Nil;
	else
		copy_eval_declare_alloc(local, ret, pos);
}

static void copy_declaim(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	GetEvalParseType(eval, &type);
	GetEvalParse(eval, 0, &eval);
	copy_declaim_nil(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

static void copy_allcons(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		copy_eval_parse(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void copy_progn(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_PROGN, "parse error");
	GetEvalParse(eval, 0, &eval);
	copy_allcons(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

static void copy_let_args(LocalRoot local, addr *ret, addr args)
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

static void copy_let(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LET && type != EVAL_PARSE_LETA, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);
	copy_let_args(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

static void copy_setq_args(LocalRoot local, addr *ret, addr cons)
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

static void copy_setq(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_SETQ, "parse error");
	GetEvalParse(eval, 0, &cons);
	copy_setq_args(local, &cons, cons);
	eval_single_parse_heap(ret, EVAL_PARSE_SETQ, cons);
}

static void copy_ordinary_optional(LocalRoot local, addr *ret, addr cons)
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

static void copy_ordinary_key(LocalRoot local, addr *ret, addr cons)
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

static void copy_ordinary_aux(LocalRoot local, addr *ret, addr cons)
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

static void copy_ordinary(LocalRoot local, addr *ret, addr cons)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(cons, &var, &opt, &rest, &key, &allow, &aux, NULL);
	copylocal_object(local, &var, var);
	copy_ordinary_optional(local, &opt, opt);
	copy_ordinary_key(local, &key, key);
	copy_ordinary_aux(local, &aux, aux);
	list_alloc(local, ret, var, opt, rest, key, allow, aux, NULL);
}

static void copy_defun(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, args, decl, doc, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFUN, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &cons);

	copylocal_object(local, &name, name);
	copy_ordinary(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;
}

static void copy_defmacro(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, lambda;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFMACRO, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &lambda);

	copylocal_object(local, &name, name);
	copylocal_object(local, &lambda, lambda);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, lambda);
	*ret = eval;
}

static void copy_macro_lambda(LocalRoot local, addr *ret, addr cons);
static void copy_macro_var(LocalRoot local, addr *ret, addr list)
{
	addr root, var;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		if (consp(var))
			copy_macro_lambda(local, &var, var);
		cons_alloc(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void copy_macro_rest(LocalRoot local, addr *ret, addr list)
{
	addr car, cdr;

	if (list != Nil) {
		GetCons(list, &car, &cdr);
		cons_heap(ret, car, cdr);
	}
}

static void copy_macro_lambda(LocalRoot local, addr *ret, addr cons)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(cons, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	copy_macro_var(local, &var, var);
	copy_ordinary_optional(local, &opt, opt);
	copy_macro_rest(local, &rest, rest);
	copy_ordinary_key(local, &key, key);
	copy_ordinary_aux(local, &aux, aux);
	list_alloc(local, ret, var, opt, rest, key, allow, aux, whole, env, NULL);
}

static void copy_deftype(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, args, decl, doc, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFTYPE, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &cons);

	copylocal_object(local, &name, name);
	copy_macro_lambda(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;
}

static void copy_define_compiler_macro(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, args, decl, doc, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFINE_COMPILER_MACRO, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &args);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &doc);
	GetEvalParse(eval, 4, &cons);

	copylocal_object(local, &name, name);
	copy_macro_lambda(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;
}

static void copy_define_symbol_macro(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr symbol, form;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DEFINE_SYMBOL_MACRO, "parse error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &form);

	copy_eval_parse(local, &form, form);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, symbol);
	SetEvalParse(eval, 1, form);
	*ret = eval;
}

static void copy_symbol_macrolet_args(LocalRoot local, addr *ret, addr args)
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

static void copy_symbol_macrolet(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_SYMBOL_MACROLET, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	copy_symbol_macrolet_args(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

static void copy_lambda(LocalRoot local, addr *ret, addr eval)
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

	copy_ordinary(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, form);
	*ret = eval;
}

static void copy_destructuring_bind(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, expr, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_DESTRUCTURING_BIND, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &decl);
	GetEvalParse(eval, 3, &cons);

	copy_macro_lambda(local, &args, args);
	copy_eval_parse(local, &expr, expr);
	copy_declaim_nil(local, &decl, decl);
	copy_allcons(local, &cons, cons);

	eval_parse_heap(&eval, type, 4);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);
	*ret = eval;
}

static void copy_if(LocalRoot local, addr *ret, addr eval)
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

static void copy_unwind_protect(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr form, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_UNWIND_PROTECT, "parse error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &form, form);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_tagbody(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAGBODY, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	copy_allcons(local, &tag, tag);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_tag(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_TAG, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &value);

	copylocal_object(local, &tag, tag);
	copylocal_object(local, &value, value);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}

static void copy_block(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_BLOCK, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &cons);

	copylocal_object(local, &name, name);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_return_from(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr name, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_RETURN_FROM, "parse error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &value);

	copylocal_object(local, &name, name);
	copy_eval_parse(local, &value, value);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}

static void copy_catch(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr tag, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CATCH, "parse error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &tag, tag);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_throw(LocalRoot local, addr *ret, addr eval)
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

static void copy_flet_one(LocalRoot local, addr *ret, addr cons)
{
	addr name, args, decl, doc;

	GetCons(cons, &name, &cons);
	GetCons(cons, &args, &cons);
	GetCons(cons, &decl, &cons);
	GetCons(cons, &doc, &cons);
	GetCar(cons, &cons);

	copylocal_object(local, &name, name);
	copy_ordinary(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copylocal_object(local, &doc, doc);
	copy_allcons(local, &cons, cons);

	list_alloc(local, ret, name, args, decl, doc, cons, NULL);
}

static void copy_flet_args(LocalRoot local, addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		copy_flet_one(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void copy_flet(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr args, decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_FLET && type != EVAL_PARSE_LABELS, "parse error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	copy_flet_args(local, &args, args);
	copy_declaim_nil(local, &decl, decl);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;
}

static void copy_the(LocalRoot local, addr *ret, addr eval)
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

static void copy_eval_when(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr cons, compilep, loadp, evalp;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_EVAL_WHEN, "parse error");
	GetEvalParse(eval, 0, &cons);
	GetEvalParse(eval, 1, &compilep);
	GetEvalParse(eval, 2, &loadp);
	GetEvalParse(eval, 3, &evalp);

	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, compilep);
	SetEvalParse(eval, 2, loadp);
	SetEvalParse(eval, 3, evalp);
	*ret = eval;
}

static void copy_values(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_VALUES, "parse error");
	GetEvalParse(eval, 0, &eval);
	copy_allcons(local, &eval, eval);
	eval_single_parse_alloc(local, ret, type, eval);
}

static void copy_locally(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr decl, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOCALLY, "parse error");
	GetEvalParse(eval, 0, &decl);
	GetEvalParse(eval, 1, &cons);

	copy_declaim_nil(local, &decl, decl);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, decl);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_call(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_CALL, "parse error");
	GetEvalParse(eval, 0, &call);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &call, call);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_multiple_value_bind(LocalRoot local, addr *ret, addr eval)
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
	copy_allcons(local, &form, form);

	eval_parse_alloc(local, &eval, type, 5);
	SetEvalParse(eval, 0, vars);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, form);
	*ret = eval;
}

static void copy_multiple_value_call(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_CALL, "parse error");
	GetEvalParse(eval, 0, &call);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &call, call);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_multiple_value_prog1(LocalRoot local, addr *ret, addr eval)
{
	enum EVAL_PARSE type;
	addr call, cons;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_MULTIPLE_VALUE_PROG1, "parse error");
	GetEvalParse(eval, 0, &call);
	GetEvalParse(eval, 1, &cons);

	copy_eval_parse(local, &call, call);
	copy_allcons(local, &cons, cons);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
}

static void copy_nth_value(LocalRoot local, addr *ret, addr eval)
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

static void copy_progv(LocalRoot local, addr *ret, addr eval)
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
	copy_allcons(local, &body, body);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, symbols);
	SetEvalParse(eval, 1, values);
	SetEvalParse(eval, 2, body);
	*ret = eval;
}

static void copy_eval_parse(LocalRoot local, addr *ret, addr pos)
{
	Check(! eval_parse_p(pos), "type error");
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
		case EVAL_PARSE_SYMBOL:
		case EVAL_PARSE_FLOAT:
		case EVAL_PARSE_FUNCTION:
		case EVAL_PARSE_PATHNAME:
		case EVAL_PARSE_ENVIRONMENT:
		case EVAL_PARSE_QUOTE:
		case EVAL_PARSE_GO:
			copy_single(local, ret, pos);
			break;

		case EVAL_PARSE_DECLAIM:
			copy_declaim(local, ret, pos);
			break;

		case EVAL_PARSE_PROGN:
			copy_progn(local, ret, pos);
			break;

		case EVAL_PARSE_LET:
		case EVAL_PARSE_LETA:
			copy_let(local, ret, pos);
			break;

		case EVAL_PARSE_SETQ:
			copy_setq(local, ret, pos);
			break;

		case EVAL_PARSE_DEFUN:
			copy_defun(local, ret, pos);
			break;

		case EVAL_PARSE_DEFMACRO:
			copy_defmacro(local, ret, pos);
			break;

		case EVAL_PARSE_DEFTYPE:
			copy_deftype(local, ret, pos);
			break;

		case EVAL_PARSE_DEFINE_COMPILER_MACRO:
			copy_define_compiler_macro(local, ret, pos);
			break;

		case EVAL_PARSE_DEFINE_SYMBOL_MACRO:
			copy_define_symbol_macro(local, ret, pos);
			break;

		case EVAL_PARSE_SYMBOL_MACROLET:
			copy_symbol_macrolet(local, ret, pos);
			break;

		case EVAL_PARSE_MACRO_LAMBDA:
			copy_macro_lambda(local, ret, pos);
			break;

		case EVAL_PARSE_LAMBDA:
			copy_lambda(local, ret, pos);
			break;

		case EVAL_PARSE_DESTRUCTURING_BIND:
			copy_destructuring_bind(local, ret, pos);
			break;

		case EVAL_PARSE_IF:
			copy_if(local, ret, pos);
			break;

		case EVAL_PARSE_UNWIND_PROTECT:
			copy_unwind_protect(local, ret, pos);
			break;

		case EVAL_PARSE_TAGBODY:
			copy_tagbody(local, ret, pos);
			break;

		case EVAL_PARSE_TAG:
			copy_tag(local, ret, pos);
			break;

		case EVAL_PARSE_BLOCK:
			copy_block(local, ret, pos);
			break;

		case EVAL_PARSE_RETURN_FROM:
			copy_return_from(local, ret, pos);
			break;

		case EVAL_PARSE_CATCH:
			copy_catch(local, ret, pos);
			break;

		case EVAL_PARSE_THROW:
			copy_throw(local, ret, pos);
			break;

		case EVAL_PARSE_FLET:
		case EVAL_PARSE_LABELS:
			copy_flet(local, ret, pos);
			break;

		case EVAL_PARSE_THE:
			copy_the(local, ret, pos);
			break;

		case EVAL_PARSE_EVAL_WHEN:
			copy_eval_when(local, ret, pos);
			break;

		case EVAL_PARSE_VALUES:
			copy_values(local, ret, pos);
			break;

		case EVAL_PARSE_LOCALLY:
			copy_locally(local, ret, pos);
			break;

		case EVAL_PARSE_CALL:
			copy_call(local, ret, pos);
			break;

		case EVAL_PARSE_MULTIPLE_VALUE_BIND:
			copy_multiple_value_bind(local, ret, pos);
			break;

		case EVAL_PARSE_MULTIPLE_VALUE_CALL:
			copy_multiple_value_call(local, ret, pos);
			break;

		case EVAL_PARSE_MULTIPLE_VALUE_PROG1:
			copy_multiple_value_prog1(local, ret, pos);
			break;

		case EVAL_PARSE_NTH_VALUE:
			copy_nth_value(local, ret, pos);
			break;

		case EVAL_PARSE_PROGV:
			copy_progv(local, ret, pos);
			break;

		default:
			fmte("parse-error: ~S.", pos, NULL);
			break;
	}
}

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

