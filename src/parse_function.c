#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "control_operator.h"
#include "declare.h"
#include "equal.h"
#include "hold.h"
#include "integer.h"
#include "lambda.h"
#include "load_time_value.h"
#include "make_load_form.h"
#include "parse.h"
#include "parse_function.h"
#include "parse_macro.h"
#include "parse_object.h"
#include "quote.h"
#include "type_parse.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  declare
 */
static int parse_declare_body(Execute ptr, addr cons, addr *retdecl, addr *retbody)
{
	int check;
	addr env;
	LocalHold hold;

	environment_heap(ptr, &env);
	hold = LocalHold_local_push(ptr, env);
	check = declare_body(ptr, env, cons, retdecl, retbody);
	close_environment(env);
	localhold_end(hold);

	return check;
}

static int parse_declare_body_documentation(Execute ptr,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody)
{
	int check;
	addr env;
	LocalHold hold;

	environment_heap(ptr, &env);
	hold = LocalHold_local_push(ptr, env);
	check = declare_body_documentation(ptr, env, cons, rdoc, rdecl, rbody);
	close_environment(env);
	localhold_end(hold);

	return check;
}

static int parse_parse_type(Execute ptr, addr *ret, addr type)
{
	int check;
	addr env;
	LocalHold hold;

	environment_heap(ptr, &env);
	hold = LocalHold_local_push(ptr, env);
	check = parse_type(ptr, ret, type, env);
	close_environment(env);
	localhold_end(hold);

	return check;
}


/*
 *  eval-parse
 */
/* progn */
static int parse_progn(Execute ptr, addr *ret, addr cons)
{
	Return(parse_allcons(ptr, &cons, cons));
	eval_single_parse_heap(ret, EVAL_PARSE_PROGN, cons);
	return 0;
}

/* let / let* */
static void parse_letone(addr one, addr *rets, addr *retv)
{
	addr symbol, value;

	/* symbol */
	if (symbolp(one)) {
		*rets = one;
		*retv = Nil;
		return;
	}

	/* not cons */
	if (! consp(one))
		fmte("Invalid let argument ~S.", one, NULL);

	/* (symbol) */
	GetCons(one, &symbol, &one);
	if (one == Nil) {
		*rets = symbol;
		*retv = Nil;
		return;
	}

	/* (symbol . value) */
	if (! consp(one))
		fmte("Invalid let argument ~S.", one, NULL);

	/* (symbol value . tail) */
	GetCons(one, &value, &one);
	if (one != Nil)
		fmte("Invalid let argument ~S.", one, NULL);

	/* (symbol value) */
	*rets = symbol;
	*retv = value;
}

static int parse_letarg(Execute ptr, addr *ret, addr args)
{
	addr cons, one, symbol, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (cons = Nil; args != Nil; ) {
		getcons(args, &one, &args);
		parse_letone(one, &symbol, &value);
		check_variable(symbol);
		Return(parse_self(ptr, value));
		cons_heap(&one, symbol, value);
		cons_heap(&cons, one, cons);
		localhold_set(hold, 0, cons);
	}
	localhold_end(hold);
	nreverse(ret, cons);

	return 0;
}

static int parse_let(Execute ptr, addr *ret, EvalParse type, addr cons)
{
	addr args, decl, eval;
	LocalHold hold;

	/* args, decl, body */
	if (! consp(cons)) {
		if (type == EVAL_PARSE_LET)
			fmte("let form must be a (let args . body).", NULL);
		else
			fmte("let* form must be a (let* args . body).", NULL);
	}
	hold = LocalHold_local(ptr);
	getcons(cons, &args, &cons);
	Return(parse_letarg(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);;
}

/* setq */
static int parse_setq_symbol_p(Execute ptr, addr list)
{
	addr symbol;

	while (list != Nil) {
		getcons(list, &symbol, &list);
		Return(symbol_macrolet_envstack_p(ptr, symbol, NULL));
		getcons(list, &symbol, &list);
	}

	return 0;
}

static int parse_setq_macrolet(Execute ptr, addr *ret, addr cons)
{
	addr progn, root, setq, setf, var, value;

	/* symbol-macrolet
	 *   `(progn
	 *     (setq var1 value1)
	 *     (setf expand2 value2)
	 *     ...)
	 */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_SETF, &setf);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(cons, &value, &cons);
		if (symbol_macrolet_envstack_p(ptr, var, &var))
			list_heap(&var, setf, var, value, NULL);
		else
			list_heap(&var, setq, var, value, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(&root, root);
	GetConst(COMMON_PROGN, &progn);
	cons_heap(&progn, progn, root);

	return eval_parse(ptr, ret, progn);
}

static int parse_setq_symbol(Execute ptr, addr *ret, addr cons)
{
	addr root, one, symbol;
	LocalHold hold;

	/* parse */
	hold = LocalHold_array(ptr, 1);
	symbol = NULL;
	for (root = Nil; cons != Nil; ) {
		getcons(cons, &one, &cons);
		if (symbol == NULL) {
			check_variable(one);
			symbol = one;
		}
		else {
			Return(parse_self(ptr, one));
			cons_heap(&one, symbol, one);
			cons_heap(&root, one, root);
			localhold_set(hold, 0, root);
			symbol = NULL;
		}
	}
	localhold_end(hold);
	if (symbol != NULL)
		fmte("setq symbol ~S don't have a value argument.", symbol, NULL);
	nreverse(&root, root);

	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_SETQ, root);

	return 0;
}

static int parse_setq(Execute ptr, addr *ret, addr cons)
{
	if (parse_setq_symbol_p(ptr, cons))
		return parse_setq_macrolet(ptr, ret, cons);
	else
		return parse_setq_symbol(ptr, ret, cons);
}

/* defun */
static inline void check_variable_notnil(addr *ret)
{
	if (*ret != Nil)
		check_variable(*ret);
}

static void parse_var(addr *ret, addr cons)
{
	addr root, var;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		check_variable(var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static int parse_optional(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var, init;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		check_variable(var);
		Return(parse_self(ptr, init));
		check_variable_notnil(&pos);
		/* push */
		list_heap(&pos, var, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_key(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var, name, init;
	LocalHold hold;

	if (cons == T)
		return Result(ret, Nil);
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var name init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &name, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		check_variable(var);
		Return(parse_self(ptr, init));
		check_variable_notnil(&pos);
		/* push */
		list_heap(&pos, var, name, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_aux(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init) */
		GetCons(pos, &var, &pos);
		GetCar(pos, &pos);
		check_variable(var);
		Return(parse_self(ptr, pos));
		/* push */
		list_heap(&pos, var, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	nreverse(ret, root);
	localhold_end(hold);

	return 0;
}

static int parse_ordinary_cons(Execute ptr, addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux;
	LocalHold hold;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	hold = LocalHold_local(ptr);
	/* var */
	parse_var(&var, var);
	localhold_push(hold, var);
	/* opt */
	Return(parse_optional(ptr, &opt, opt));
	localhold_push(hold, opt);
	/* rest */
	check_variable_notnil(&rest);
	/* key */
	Return(parse_key(ptr, &key, key));
	localhold_push(hold, key);
	/* aux */
	Return(parse_aux(ptr, &aux, aux));
	localhold_push(hold, aux);
	/* result */
	localhold_end(hold);
	list_heap(ret, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

_g int parse_ordinary(Execute ptr, addr *ret, addr args)
{
	lambda_ordinary(ptr->local, &args, args);
	return parse_ordinary_cons(ptr, ret, args);
}

static void implicit_block(addr *ret, addr name, addr list)
{
	addr block;

	GetConst(COMMON_BLOCK, &block);
	if (callnamep(name))
		GetCallName(name, &name);
	lista_heap(&list, block, name, list, NULL);
	conscar_heap(ret, list);
}

static int parse_defun(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, body, form;
	LocalHold hold;

	/* (eval::defun name args decl doc body form) */
	List_bind(cons, &name, &args, &decl, &doc, &body, &form, NULL);

	/* parse */
	hold = LocalHold_local(ptr);
	Return(parse_ordinary_cons(ptr, &args, args));
	localhold_push(hold, args);
	implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);
	SetEvalParse(eval, 5, form);

	return Result(ret, eval);
}

/* defmacro */
_g int parse_macro_lambda_list(Execute ptr, addr *ret, addr args);
static int parse_macro_var(Execute ptr, addr *ret, addr cons)
{
	addr root, var;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		if (consp(var)) {
			Return(parse_macro_lambda_list(ptr, &var, var));
		}
		else {
			check_variable(var);
		}
		cons_heap(&root, var, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static inline void parse_macro_rest(addr *ret)
{
	addr pos;

	if (*ret != Nil) {
		/* (var . &rest) (var . &body) (var . nil) */
		GetCar(*ret, &pos);
		check_variable(pos);
		SetCar(*ret, pos);
	}
}

_g int parse_macro_lambda_list(Execute ptr, addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;
	LocalHold hold;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	hold = LocalHold_local(ptr);
	/* var */
	Return(parse_macro_var(ptr, &var, var));
	localhold_push(hold, var);
	/* opt */
	Return(parse_optional(ptr, &opt, opt));
	localhold_push(hold, opt);
	/* rest */
	parse_macro_rest(&rest);
	/* key */
	Return(parse_key(ptr, &key, key));
	localhold_push(hold, key);
	/* aux */
	Return(parse_aux(ptr, &aux, aux));
	localhold_push(hold, aux);
	/* others */
	check_variable_notnil(&whole);
	check_variable_notnil(&env);
	/* result */
	localhold_end(hold);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int execute_macro_lambda(Execute ptr, addr *ret, addr eval)
{
	addr control;
	LocalHold hold;

	/* push */
	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	/* code */
	Return(eval_execute_parse(ptr, eval));
	getresult_control(ptr, ret);
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int make_macro_function(Execute ptr,
		addr *ret, addr args, addr decl, addr doc, addr cons)
{
	addr eval;

	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	return execute_macro_lambda(ptr, ret, eval);
}

static int parse_defmacro(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, body, lambda;
	LocalHold hold;

	/* (eval::defmacro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list(ptr, &args, args));
	localhold_push(hold, args);
	Return(localhold_parse_allcons(hold, ptr, &body, body));
	Return(make_macro_function(ptr, &lambda, args, decl, doc, body));
	localhold_push(hold, lambda);
	defmacro_envstack(ptr, name, lambda);
	localhold_end(hold);

	/* defmacro */
	eval_parse_heap(&eval, EVAL_PARSE_DEFMACRO, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, lambda);

	return Result(ret, eval);
}

/* macro-lambda */
static int parse_macro_lambda(Execute ptr, addr *ret, addr cons)
{
	addr eval, args, decl, doc;
	LocalHold hold;

	/* (macro-lambda args . body) */
	if (! consp(cons))
		fmte("MACRO-LAMBDA argument ~S must be (lambda-list . form).", cons, NULL);
	hold = LocalHold_local(ptr);
	GetCons(cons, &args, &cons);
	lambda_macro(ptr->local, &args, args, Nil);
	localhold_push(hold, args);
	Return(parse_macro_lambda_list(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* macro-lambda */
	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);

	return Result(ret, eval);
}

/* deftype */
static int parse_deftype(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, body;
	LocalHold hold;

	/* (eval::deftype name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list(ptr, &args, args));
	localhold_push(hold, args);
	implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons(hold, ptr, &body, body));
	localhold_end(hold);

	/* deftype */
	eval_parse_heap(&eval, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);

	return Result(ret, eval);
}

/* define-compiler-macro */
static int parse_define_compiler_macro(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, body;
	LocalHold hold;

	/* (eval::define-compiler-macro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list(ptr, &args, args));
	localhold_push(hold, args);
	implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons(hold, ptr, &body, body));
	localhold_end(hold);

	/* define-compiler-macro */
	eval_parse_heap(&eval, EVAL_PARSE_DEFINE_COMPILER_MACRO, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, body);

	return Result(ret, eval);
}

/* destructuring-bind */
static int parse_destructuring_bind(Execute ptr, addr *ret, addr cons)
{
	addr eval, lambda, args, expr, decl, body;
	LocalHold hold;

	/* (eval::destructuring-bind args expr decl body) */
	List_bind(cons, &args, &expr, &decl, &body, NULL);
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list(ptr, &args, args));
	localhold_push(hold, args);
	Return(localhold_parse_allcons(hold, ptr, &body, body));
	Return(localhold_parse_self(hold, ptr, expr));
	localhold_end(hold);

	/* lambda */
	eval_parse_heap(&lambda, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(lambda, 0, args);
	SetEvalParse(lambda, 1, decl);
	SetEvalParse(lambda, 2, Nil);
	SetEvalParse(lambda, 3, body);
	/* destructuring-bind */
	eval_parse_heap(&eval, EVAL_PARSE_DESTRUCTURING_BIND, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, lambda);

	return Result(ret, eval);
}

/* define-symbol-macro */
static void check_define_symbol_macro(addr symbol)
{
	addr value;

	if (specialp_symbol(symbol))
		fmte("define-symbol-macro cannot bind the special symbol ~S.", symbol, NULL);
	GetValueSymbol(symbol, &value);
	if (value != Unbound)
		fmte("define-symbol-macro cannot bind the bounded symbol ~S.", symbol, NULL);
}

static int parse_define_symbol_macro(Execute ptr, addr *ret, addr cons)
{
	addr eval, symbol, form, body;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	/* symbol */
	if (! consp(cons))
		goto error;
	GetCons(cons, &symbol, &cons);
	check_function_variable(symbol);
	localhold_push(hold, symbol);
	/* body */
	if (! consp(cons))
		goto error;
	GetCons(cons, &form, &cons);
	if (cons != Nil)
		goto error;
	localhold_push(hold, form);
	/* form */
	check_define_symbol_macro(symbol);
	define_symbol_macro_envstack(ptr, symbol, form); /* before parse */
	Return(eval_parse(ptr, &body, form));
	localhold_push(hold, body);
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_DEFINE_SYMBOL_MACRO, 3);
	SetEvalParse(eval, 0, symbol);
	SetEvalParse(eval, 1, body);
	SetEvalParse(eval, 2, form);
	return Result(ret, eval);

error:
	fmte("define-symbol-macro arguments ~S must be (symbol form).", cons, NULL);
	return 0;
}

/* symbol-macrolet */
static int parse_symbol_macrolet_args(Execute ptr, addr *ret, addr args)
{
	addr root, cons, symbol, expansion, env;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		/* parse */
		getcons(args, &cons, &args);
		if (! consp(cons))
			goto error;
		GetCons(cons, &symbol, &cons);
		check_function_variable(symbol);
		if (! consp(cons))
			goto error;
		GetCons(cons, &expansion, &cons);
		if (cons != Nil)
			goto error;
		symbol_macrolet_envstack(ptr, symbol, expansion); /* before parse */
		Return(parse_self(ptr, expansion));
		/* (symbol expansion env) */
		environment_heap(ptr, &env);
		list_heap(&cons, symbol, expansion, env, NULL);
		cons_heap(&root, cons, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);
	return 0;

error:
	localhold_end(hold);
	fmte("The symbol-macrolet arguemnt ~A "
			"must be a (symbol expansion) form.", cons, NULL);
	return 0;
}

static void check_symbol_macrolet(addr args, addr decl)
{
	addr symbol;

	/* If symbols in the argument are special, error */
	if (eval_declare_p(decl)) {
		getall_special_declare(decl, &decl);
		while (args != Nil) {
			GetCar(args, &symbol);
			if (find_list_eq_unsafe(symbol, decl))
				fmte("The symbol ~S cannot declare the special.", symbol, NULL);
		}
	}
}

static int parse_symbol_macrolet(Execute ptr, addr *ret, addr cons)
{
	addr eval, args, decl, rollback;
	LocalHold hold;

	if (! consp(cons))
		fmte("symbol-macrolet form must be (symbol-macrolet args . body).", NULL);
	getcons(cons, &args, &cons);
	/* local scope environment */
	snapshot_envstack(ptr, &rollback);
	hold = LocalHold_local(ptr);
	/* args */
	Return(parse_symbol_macrolet_args(ptr, &args, args));
	localhold_push(hold, args);
	/* decl */
	Return(parse_declare_body(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	check_symbol_macrolet(args, decl);
	/* body */
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);
	rollback_envstack(ptr, rollback);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_SYMBOL_MACROLET, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* macrolet */
static int parse_macrolet_one(Execute ptr, addr cons)
{
	addr name, args, doc, decl;
	LocalHold hold;

	/* parse */
	if (! consp(cons))
		goto error;
	GetCons(cons, &name, &cons);
	if (! symbolp(name))
		fmte("The name ~S must be a symbol.", name, NULL);
	check_function_variable(name);
	if (! consp(cons))
		goto error;
	GetCons(cons, &args, &cons);
	/* make macro-function */
	hold = LocalHold_local(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	Return(parse_macro_lambda_list(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	implicit_block(&cons, name, cons);
	localhold_push(hold, cons);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	Return(make_macro_function(ptr, &cons, args, decl, doc, cons));
	localhold_push(hold, cons);
	macrolet_envstack(ptr, name, cons);
	localhold_end(hold);
	return 0;

error:
	fmte("macrolet argument must be (name (...) . body) form.", NULL);
	return 0;
}

static void parse_macrolet_args(Execute ptr, addr args)
{
	addr pos;

	while (args != Nil) {
		getcons(args, &pos, &args);
		parse_macrolet_one(ptr, pos);
	}
}

static int parse_macrolet(Execute ptr, addr *ret, addr cons)
{
	addr eval, args, decl, rollback;
	LocalHold hold;

	if (! consp(cons))
		fmte("macrolet form must be (macrolet args . body).", NULL);
	getcons(cons, &args, &cons);
	/* local scope environment */
	snapshot_envstack(ptr, &rollback);
	parse_macrolet_args(ptr, args);
	/* arguments */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);
	rollback_envstack(ptr, rollback);

	/* macrolet -> locally */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(eval, 0, decl);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* quote */
static int parse_quote(addr *ret, addr cons)
{
	addr value;

	if (! consp(cons))
		fmte("quote form must have a one argument.", NULL);
	getcons(cons, &value, &cons);
	if (cons != Nil)
		fmte("quote form must have a one argument.", NULL);

	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_QUOTE, value);
	return 0;
}

/* function */
static int parse_lambda(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, args, doc, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp(cons))
		fmte("function lambda must be (lambda (...) body) form.", NULL);
	GetCons(cons, &args, &cons);
	hold = LocalHold_local(ptr);
	/* args */
	Return(parse_ordinary(ptr, &args, args));
	localhold_push(hold, args);
	/* doc, decl */
	Return(parse_declare_body_documentation(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, NULL);
	/* cons */
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, form);

	return Result(ret, eval);
}

static int parse_function_argument(Execute ptr, addr *ret, addr value)
{
	addr check, symbol;

	/* symbol function */
	if (! parse_callname_heap(&value, value)) {
		check_function_variable(value);
		eval_single_parse_heap(ret, EVAL_PARSE_FUNCTION, value);
		return 0;
	}

	/* lambda function */
	if (! consp(value))
		fmte("function ~S must be a fdefinition form.", value, NULL);
	GetConst(COMMON_LAMBDA, &symbol);
	GetCar(value, &check);
	if (check == symbol)
		return parse_lambda(ptr, ret, value);

	/* others */
	fmte("function ~S must be a fdefinition form.", value, NULL);

	return 0;
}

static int parse_function(Execute ptr, addr *ret, addr cons)
{
	addr value;

	if (! consp(cons))
		fmte("function form must have a one argument.", NULL);
	getcons(cons, &value, &cons);
	if (cons != Nil)
		fmte("function form must have a one argument.", NULL);

	return parse_function_argument(ptr, ret, value);
}

/* if */
static int parse_if(Execute ptr, addr *ret, addr cons)
{
	addr eval, expr, then, last;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	GetCons(cons, &expr, &cons);
	if (! consp(cons))
		goto error;
	GetCons(cons, &then, &cons);
	if (cons == Nil) {
		last = Nil;
	}
	else {
		if (! consp(cons))
			goto error;
		GetCons(cons, &last, &cons);
		if (cons != Nil)
			goto error;
	}

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, expr));
	Return(localhold_parse_self(hold, ptr, then));
	Return(localhold_parse_self(hold, ptr, last));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_IF, 3);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, then);
	SetEvalParse(eval, 2, last);
	return Result(ret, eval);

error:
	fmte("if form must be (if expr then &optnioal else).", NULL);
	return 0;
}

/* unwind-protect */
static int parse_unwind_protect(Execute ptr, addr *ret, addr cons)
{
	addr eval, form;
	LocalHold hold;

	if (! consp(cons))
		fmte("unwind-protect form must be a (unwind-protect form . body).", NULL);
	GetCons(cons, &form, &cons);
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, form));
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_UNWIND_PROTECT, 2);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* tagbody */
static int parse_tagbody_findtag(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		GetEvalParse(check, 0, &check);
		if (eql_function(check, key))
			return 1;
	}

	return 0;
}

static void parse_tagbody_maketag(addr *ret, addr pos)
{
	eval_parse_heap(ret, EVAL_PARSE_TAG, 1);
	SetEvalParse(*ret, 0, pos);
}

static int parse_tagbody_check(Execute ptr, addr cons, addr *rtag, addr *rbody)
{
	addr tag, body, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	for (tag = body = Nil; cons != Nil; ) {
		getcons(cons, &pos, &cons);
		if (consp(pos)) {
			Return(parse_self(ptr, pos));
			cons_heap(&body, pos, body);
		}
		else if (tagbody_tag_p(pos)) {
			if (parse_tagbody_findtag(pos, tag))
				fmte("The tag ~S is already exists.", pos, NULL);
			parse_tagbody_maketag(&pos, pos);
			cons_heap(&tag, pos, tag);
			cons_heap(&body, pos, body);
			localhold_set(hold, 1, tag);
		}
		else {
			fmte("The tag ~S must be a symbol or integer.", pos, NULL);
		}
		localhold_set(hold, 0, body);
	}
	localhold_end(hold);
	nreverse(rtag, tag);
	nreverse(rbody, body);

	return 0;
}

static int parse_tagbody(Execute ptr, addr *ret, addr cons)
{
	addr eval, tag;

	Return(parse_tagbody_check(ptr, cons, &tag, &cons));
	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_TAGBODY, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* go */
static int parse_go(addr *ret, addr cons)
{
	addr tag;

	if (! consp(cons))
		fmte("go form must be (go tag).", NULL);
	GetCons(cons, &tag, &cons);
	if (cons != Nil)
		fmte("go form must be (go tag).", NULL);
	if (! tagbody_tag_p(tag))
		fmte("The tag ~S must be a symbol or integer.", tag, NULL);
	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_GO, tag);

	return 0;
}

/* block */
static int parse_block(Execute ptr, addr *ret, addr cons)
{
	addr eval, name;

	if (! consp(cons))
		fmte("block form must be (block name . body).", NULL);
	GetCons(cons, &name, &cons);
	if (! symbolp(name))
		fmte("block name ~S must be a symbol type.", name, NULL);
	Return(parse_allcons(ptr, &cons, cons));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_BLOCK, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* return-from */
static int parse_return_from(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, value;

	if (! consp(cons))
		goto error;
	GetCons(cons, &name, &cons);
	if (cons == Nil) {
		value = Nil;
	}
	else {
		if (! consp(cons))
			goto error;
		GetCons(cons, &value, &cons);
		if (cons != Nil)
			goto error;
	}
	Return(parse_self(ptr, value));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_RETURN_FROM, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, value);
	return Result(ret, eval);

error:
	fmte("return-from form must be (return-from name [value]).", NULL);
	return 0;
}

/* catch */
static int parse_catch(Execute ptr, addr *ret, addr cons)
{
	addr eval, tag;
	LocalHold hold;

	if (! consp(cons))
		fmte("catch form must be (catch tag . body).", NULL);
	GetCons(cons, &tag, &cons);
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, tag));
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CATCH, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* throw */
static int parse_throw(Execute ptr, addr *ret, addr cons)
{
	addr eval, tag, result;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	GetCons(cons, &tag, &cons);
	if (! consp(cons))
		goto error;
	GetCons(cons, &result, &cons);
	if (cons != Nil)
		goto error;
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, tag));
	Return(localhold_parse_self(hold, ptr, result));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THROW, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, result);
	return Result(ret, eval);

error:
	fmte("throw form must be (throw tag result).", NULL);
	return 0;
}

/* flet / labels */
static int parse_flet_one(Execute ptr, addr *ret, addr cons)
{
	addr name, call, args, doc, decl;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	GetCons(cons, &name, &cons);
	parse_callname_error(&call, name);
	check_function_variable(call);
	if (! consp(cons))
		goto error;
	GetCons(cons, &args, &cons);

	hold = LocalHold_local(ptr);
	Return(parse_ordinary(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	implicit_block(&cons, call, cons);
	localhold_push(hold, cons);
	Return(parse_allcons(ptr, &cons, cons));
	localhold_push(hold, cons);
	localhold_end(hold);
	list_heap(ret, call, args, decl, doc, cons, NULL);
	return 0;

error:
	fmte("flet/labels argument must be (name (...) . body) form.", NULL);
	return 0;
}

static int parse_flet_args(Execute ptr, addr *ret, addr args)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		getcons(args, &pos, &args);
		Return(parse_flet_one(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_flet_labels(Execute ptr, addr *ret, EvalParse type, addr cons)
{
	addr eval, args, decl;
	LocalHold hold;

	if (! consp(cons)) {
		if (type == EVAL_PARSE_FLET)
			fmte("flet form must be (flet args . body).", NULL);
		else
			fmte("labels form must be (labels args . body).", NULL);
	}
	getcons(cons, &args, &cons);

	hold = LocalHold_local(ptr);
	Return(parse_flet_args(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* the */
static int parse_the(Execute ptr, addr *ret, addr cons)
{
	addr eval, type, expr;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	GetCons(cons, &type, &cons);
	if (! consp(cons))
		goto error;
	GetCons(cons, &expr, &cons);
	if (cons != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(parse_parse_type(ptr, &type, type));
	localhold_push(hold, type);
	Return(localhold_parse_self(hold, ptr, expr));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THE, 2);
	SetEvalParse(eval, 0, type);
	SetEvalParse(eval, 1, expr);
	return Result(ret, eval);

error:
	fmte("the form must be (the type expr).", NULL);
	return 0;
}

/* eval-when */
static int parse_eval_when(Execute ptr, addr *ret, addr cons)
{
	addr eval, left, right;
	addr compilep, loadp, evalp;
	addr compile1, compile2, load1, load2, eval1, eval2;

	if (! consp(cons))
		fmte("eval-when form must be (eval-when (...) . body).", NULL);
	GetCons(cons, &right, &cons);
	Return(parse_allcons(ptr, &cons, cons));

	/* type */
	GetConst(KEYWORD_COMPILE_TOPLEVEL, &compile1);
	GetConst(KEYWORD_LOAD_TOPLEVEL, &load1);
	GetConst(KEYWORD_EXECUTE, &eval1);
	GetConst(COMMON_COMPILE, &compile2);
	GetConst(COMMON_LOAD, &load2);
	GetConst(COMMON_EVAL, &eval2);
	compilep = loadp = evalp = Nil;
	while (right != Nil) {
		getcons(right, &left, &right);
		if (left == compile1 || left == compile2)
			compilep = T;
		if (left == load1 || left == load2)
			loadp = T;
		if (left == eval1 || left == eval2)
			evalp = T;
	}

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_EVAL_WHEN, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, compilep);
	SetEvalParse(eval, 2, loadp);
	SetEvalParse(eval, 3, evalp);

	return Result(ret, eval);
}

/* values */
static int parse_values(Execute ptr, addr *ret, addr cons)
{
	Return(parse_allcons(ptr, &cons, cons));
	eval_single_parse_heap(ret, EVAL_PARSE_VALUES, cons);
	return 0;
}

/* locally */
static int parse_locally(Execute ptr, addr *ret, addr cons)
{
	addr eval, decl;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(parse_declare_body(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(eval, 0, decl);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* declaim */
static int parse_declaim(addr *ret, addr args)
{
#ifdef LISP_DEBUG
	addr right;

	CheckType(args, LISPTYPE_CONS);
	GetCdr(args, &right);
	Check(right != Nil, "argument error");
#endif
	GetCar(args, &args);
	eval_single_parse_heap(ret, EVAL_PARSE_DECLAIM, args);

	return 0;
}

/* call */
static int parse_call(Execute ptr, addr *ret, addr call, addr cons)
{
	addr eval;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(parse_function_argument(ptr, &call, call));
	localhold_push(hold, call);
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CALL, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);

	return Result(ret, eval);
}

/* multiple-value-bind */
static int parse_multiple_value_bind(Execute ptr, addr *ret, addr cons)
{
	addr eval, vars, expr, decl, doc, form;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	getcons(cons, &vars, &cons);
	getcons(cons, &expr, &cons);
	getcons(cons, &decl, &cons);
	getcons(cons, &doc, &cons);
	getcons(cons, &form, &cons);
	if (cons != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, expr));
	Return(localhold_parse_allcons(hold, ptr, &form, form));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(eval, 0, vars);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, form);
	return Result(ret, eval);

error:
	fmte("The form ~S must be (system::multiple-value-bind "
			"(vars expr decl doc form).", cons, NULL);
	return 0;
}

/* multiple-value-call */
static int parse_multiple_value_call(Execute ptr, addr *ret, addr cons)
{
	addr eval, expr;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	GetCons(cons, &expr, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, expr));
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, cons);
	return Result(ret, eval);

error:
	fmte("The form ~S must be (multiple-value-call function . body).", cons, NULL);
	return 0;
}

/* multiple-value-prog1 */
static int parse_multiple_value_prog1(Execute ptr, addr *ret, addr cons)
{
	addr eval, expr;
	LocalHold hold;

	if (! consp(cons))
		goto error;
	GetCons(cons, &expr, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, expr));
	Return(localhold_parse_allcons(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, cons);
	return Result(ret, eval);

error:
	fmte("The form ~S must be (multiple-value-prog1 first-form . body).", cons, NULL);
	return 0;
}

/* nth-value */
static int parse_nth_value(Execute ptr, addr *ret, addr list)
{
	addr eval, next, nth, expr;
	LocalHold hold;

	if (! consp(list))
		goto error;
	GetCons(list, &nth, &next);
	if (! consp(next))
		goto error;
	GetCons(next, &expr, &next);
	if (next != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, nth));
	Return(localhold_parse_self(hold, ptr, expr));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_NTH_VALUE, 2);
	SetEvalParse(eval, 0, nth);
	SetEvalParse(eval, 1, expr);
	return Result(ret, eval);

error:
	fmte("The form ~S must be (nth-value nth expr).", list, NULL);
	return 0;
}

/* progv */
static int parse_progv(Execute ptr, addr *ret, addr form)
{
	addr eval, symbols, values, body;
	LocalHold hold;

	if (! consp(form))
		goto error;
	GetCons(form, &symbols, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &values, &body);

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, symbols));
	Return(localhold_parse_self(hold, ptr, values));
	Return(localhold_parse_allcons(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_PROGV, 3);
	SetEvalParse(eval, 0, symbols);
	SetEvalParse(eval, 1, values);
	SetEvalParse(eval, 2, body);
	return Result(ret, eval);

error:
	fmte("The form ~S must be (progv symbols values . body).", form, NULL);
	return 0;
}

/* macro */
static int parse_macro(Execute ptr, addr *ret, addr call, addr cons)
{
	int check;
	addr env;
	LocalHold hold;

	environment_heap(ptr, &env);
	hold = LocalHold_local_push(ptr, env);
	check = call_macroexpand_hook(ptr, &cons, call, cons, env);
	close_environment(env);
	localhold_end(hold);

	return check || parse_execute(ptr, ret, cons);
}

static int parse_backquote(Execute ptr, addr *ret, addr pos)
{
	if (! quote_back_p(pos))
		fmte("Invalid quote type.", NULL);
	getvalue_quote(pos, &pos);
	return parse_execute(ptr, ret, pos);
}

/* parse_cons */
static int parse_cons_check_constant(addr call, constindex index)
{
	addr check;
	GetConstant(index, &check);
	return check == call;
}

static int parse_cons_car(Execute ptr, addr *ret, addr cons)
{
	addr call, args;

	GetCons(cons, &call, &args);
	if (parse_cons_check_constant(call, CONSTANT_COMMON_PROGN)) {
		return parse_progn(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_LET)) {
		return parse_let(ptr, ret, EVAL_PARSE_LET, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_LETA)) {
		return parse_let(ptr, ret, EVAL_PARSE_LETA, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_SETQ)) {
		return parse_setq(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_QUOTE)) {
		return parse_quote(ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_FUNCTION)) {
		return parse_function(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_IF)) {
		return parse_if(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_UNWIND_PROTECT)) {
		return parse_unwind_protect(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_TAGBODY)) {
		return parse_tagbody(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_GO)) {
		return parse_go(ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_BLOCK)) {
		return parse_block(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_RETURN_FROM)) {
		return parse_return_from(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_CATCH)) {
		return parse_catch(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_THROW)) {
		return parse_throw(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_FLET)) {
		return parse_flet_labels(ptr, ret, EVAL_PARSE_FLET, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_LABELS)) {
		return parse_flet_labels(ptr, ret, EVAL_PARSE_LABELS, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_THE)) {
		return parse_the(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_EVAL_WHEN)) {
		return parse_eval_when(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_VALUES)) {
		return parse_values(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_LOCALLY)) {
		return parse_locally(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DECLAIM)) {
		return parse_declaim(ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DEFUN)) {
		return parse_defun(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DEFMACRO)) {
		return parse_defmacro(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DEFTYPE)) {
		return parse_deftype(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DEFINE_COMPILER_MACRO)) {
		return parse_define_compiler_macro(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DESTRUCTURING_BIND)) {
		return parse_destructuring_bind(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_MACRO_LAMBDA)) {
		return parse_macro_lambda(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_MACROLET)) {
		return parse_macrolet(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_DEFINE_SYMBOL_MACRO)) {
		return parse_define_symbol_macro(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_SYMBOL_MACROLET)) {
		return parse_symbol_macrolet(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_MULTIPLE_VALUE_BIND)) {
		return parse_multiple_value_bind(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_MULTIPLE_VALUE_CALL)) {
		return parse_multiple_value_call(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_MULTIPLE_VALUE_PROG1)) {
		return parse_multiple_value_prog1(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_SYSTEM_NTH_VALUE)) {
		return parse_nth_value(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_PROGV)) {
		return parse_progv(ptr, ret, args);
	}
	if (parse_cons_check_constant(call, CONSTANT_COMMON_LOAD_TIME_VALUE)) {
		return parse_load_time_value(ptr, ret, args);
	}
	if (parse_cons_check_macro(ptr, call, &call)) {
		return parse_macro(ptr, ret, call, cons);
	}

	return parse_call(ptr, ret, call, args);
}

static int compiler_macroexpand_p(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_COMPILER_MACRO, &pos);
	getspecial_local(ptr, pos, &pos);

	return pos != Unbound && pos != Nil;
}

static int parse_cons_expander_p(Execute ptr, addr *ret, addr cons)
{
	addr check;

	if (! compiler_macroexpand_p(ptr))
		return 0;
	GetCar(cons, &check);
	if (! symbolp(check))
		return 0;
	get_compiler_macro_symbol(check, &check);
	if (check == Nil)
		return 0;
	*ret = check;
	return 1;
}

static int parse_cons_expander(Execute ptr, addr *ret, addr call, addr cons)
{
	int check;
	addr env, pos;
	LocalHold hold;

	environment_heap(ptr, &env);
	hold = LocalHold_local_push(ptr, env);
	check = call_macroexpand_hook(ptr, &pos, call, cons, env);
	close_environment(env);
	localhold_end(hold);
	if (check)
		return 1;
	/* equal */
	if (equal_function(cons, pos))
		return parse_cons_car(ptr, ret, cons);
	else
		return parse_execute(ptr, ret, pos);
}

static int parse_cons(Execute ptr, addr *ret, addr cons)
{
	addr call;

	if (parse_cons_expander_p(ptr, &call, cons))
		return parse_cons_expander(ptr, ret, call, cons);
	else
		return parse_cons_car(ptr, ret, cons);
}

static void parse_array(addr *ret, addr pos)
{
	if (strarrayp(pos))
		eval_single_parse_heap(ret, EVAL_PARSE_STRING, pos);
	else
		eval_single_parse_heap(ret, EVAL_PARSE_ARRAY, pos);
}

static int parse_switch(Execute ptr, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			return parse_cons(ptr, ret, pos);

		case LISPTYPE_NIL:
			eval_single_parse_heap(ret, EVAL_PARSE_NIL, Nil);
			break;

		case LISPTYPE_T:
			eval_single_parse_heap(ret, EVAL_PARSE_T, T);
			break;

		case LISPTYPE_TYPE:
			eval_single_parse_heap(ret, EVAL_PARSE_TYPE, pos);
			break;

		case LISPTYPE_CLOS:
			return parse_clos(ptr, ret, pos);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			eval_single_parse_heap(ret, EVAL_PARSE_INTEGER, pos);
			break;

		case LISPTYPE_RATIO:
			eval_single_parse_heap(ret, EVAL_PARSE_RATIONAL, pos);
			break;

		case LISPTYPE_COMPLEX:
			eval_single_parse_heap(ret, EVAL_PARSE_COMPLEX, pos);
			break;

		case LISPTYPE_CHARACTER:
			eval_single_parse_heap(ret, EVAL_PARSE_CHARACTER, pos);
			break;

		case LISPTYPE_ARRAY:
			parse_array(ret, pos);
			break;

		case LISPTYPE_VECTOR:
			eval_single_parse_heap(ret, EVAL_PARSE_VECTOR, pos);
			break;

		case LISPTYPE_BITVECTOR:
			eval_single_parse_heap(ret, EVAL_PARSE_BITVECTOR, pos);
			break;

		case LISPTYPE_STRING:
			eval_single_parse_heap(ret, EVAL_PARSE_STRING, pos);
			break;

		case LISPTYPE_SYMBOL:
			eval_single_parse_heap(ret, EVAL_PARSE_SYMBOL, pos);
			break;

		case LISPTYPE_FUNCTION:
			eval_single_parse_heap(ret, EVAL_PARSE_FUNCTION, pos);
			break;

		case LISPTYPE_RANDOM_STATE:
			eval_single_parse_heap(ret, EVAL_PARSE_RANDOM_STATE, pos);
			break;

		case LISPTYPE_PATHNAME:
			eval_single_parse_heap(ret, EVAL_PARSE_PATHNAME, pos);
			break;

		case LISPTYPE_ENVIRONMENT:
			eval_single_parse_heap(ret, EVAL_PARSE_ENVIRONMENT, pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
		case LISPTYPE_SHORT_FLOAT:
			eval_single_parse_heap(ret, EVAL_PARSE_FLOAT, pos);
			break;

		case LISPTYPE_EVAL:
			*ret = pos;
			break;

		case LISPTYPE_QUOTE:
			return parse_backquote(ptr, ret, pos);

		default:
			fmte("parse-error: ~S.", pos, NULL);
			break;
	}

	return 0;
}


/*
 *  parse-execute
 */
_g int localhold_parse_execute(LocalHold hold, Execute ptr, addr *ret, addr pos)
{
	Return(parse_execute(ptr, ret, pos));
	localhold_push(hold, *ret);
	return 0;
}

_g int localhold_parse_allcons(LocalHold hold, Execute ptr, addr *ret, addr cons)
{
	Return(parse_allcons(ptr, ret, cons));
	localhold_push(hold, *ret);
	return 0;
}

_g int parse_allcons(Execute ptr, addr *ret, addr cons)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		getcons(cons, &pos, &cons);
		Return(parse_self(ptr, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

_g int parse_execute(Execute ptr, addr *ret, addr pos)
{
	LocalHold hold;

	hold = LocalHold_local_push(ptr, pos);
	Return(parse_switch(ptr, ret, pos));
	localhold_end(hold);

	return 0;
}

