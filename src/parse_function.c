#include "callname.h"
#include "compile_file.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "control_operator.h"
#include "declare.h"
#include "equal.h"
#include "eval_execute.h"
#include "eval_object.h"
#include "eval_value.h"
#include "hold.h"
#include "integer.h"
#include "lambda.h"
#include "load_instance.h"
#include "load_gensym.h"
#include "load_time_value.h"
#include "parse.h"
#include "parse_function.h"
#include "parse_macro.h"
#include "parse_object.h"
#include "quote.h"
#include "type_parse.h"
#include "step.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  declare
 */
int parse_declare_body_(Execute ptr, addr cons, addr *retdecl, addr *retbody)
{
	addr env;
	LocalHold hold;

	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(declare_body_(ptr, env, cons, retdecl, retbody));
	close_environment(env);
	localhold_end(hold);

	return 0;
}

static int parse_declare_body_documentation_(Execute ptr,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr env;
	LocalHold hold;

	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(declare_body_documentation_(ptr, env, cons, rdoc, rdecl, rbody));
	close_environment(env);
	localhold_end(hold);

	return 0;
}

static int parse_parse_type_(Execute ptr, addr *ret, addr type)
{
	addr env;
	LocalHold hold;

	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(parse_type_values(ptr, ret, type, env));
	close_environment(env);
	localhold_end(hold);

	return 0;
}


/*
 *  eval-parse
 */
static int parse_allcons_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, cons);
	for (root = Nil; cons != Nil; ) {
		Return_getcons(cons, &pos, &cons);
		Return(parse_execute_(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 1, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int localhold_parse_execute_(LocalHold hold, Execute ptr, addr *ret, addr pos)
{
	Return(parse_execute_(ptr, ret, pos));
	localhold_push(hold, *ret);
	return 0;
}

static int localhold_parse_allcons_(LocalHold hold, Execute ptr, addr *ret, addr cons)
{
	Return(parse_allcons_(ptr, ret, cons));
	localhold_push(hold, *ret);
	return 0;
}

/* progn */
static int parse_progn_(Execute ptr, addr *ret, addr cons)
{
	addr list;

	GetCdr(cons, &list);
	Return(parse_allcons_(ptr, &list, list));
	eval_parse2_heap(ret, EVAL_PARSE_PROGN, cons, list);

	return 0;
}

/* let */
static int parse_letone_(addr one, addr *rets, addr *retv)
{
	addr symbol, value;

	/* symbol */
	if (symbolp(one)) {
		*rets = one;
		*retv = Nil;
		return 0;
	}

	/* not cons */
	if (! consp(one)) {
		*rets = *retv = Nil;
		return fmte_("Invalid let argument ~S.", one, NULL);
	}

	/* (symbol) */
	GetCons(one, &symbol, &one);
	if (one == Nil) {
		*rets = symbol;
		*retv = Nil;
		return 0;
	}

	/* (symbol . value) */
	if (! consp(one)) {
		*rets = *retv = Nil;
		return fmte_("Invalid let argument ~S.", one, NULL);
	}

	/* (symbol value . tail) */
	GetCons(one, &value, &one);
	if (one != Nil) {
		*rets = *retv = Nil;
		return fmte_("Invalid let argument ~S.", one, NULL);
	}

	/* (symbol value) */
	*rets = symbol;
	*retv = value;
	return 0;
}

static int parse_let_arg_(Execute ptr, addr *ret, addr args)
{
	addr cons, one, symbol, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (cons = Nil; args != Nil; ) {
		Return_getcons(args, &one, &args);
		Return(parse_letone_(one, &symbol, &value));
		Return(check_variable_(symbol));
		Return(parse_self_(ptr, value));
		cons_heap(&one, symbol, value);
		cons_heap(&cons, one, cons);
		localhold_set(hold, 0, cons);
	}
	localhold_end(hold);
	nreverse(ret, cons);

	/* macro */
	args = *ret;
	while (args != Nil) {
		GetCons(args, &one, &args);
		GetCar(one, &symbol);
		Return(lexical_envstack_(ptr, symbol));
	}

	return 0;
}

static int parse_let_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, cdr, args, decl, eval;
	LocalHold hold;

	/* args, decl, body */
	GetCdr(cons, &cdr);
	if (! consp(cdr))
		return fmte_("let form must be a (let args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return_getcons(cdr, &args, &cdr);
	Return(parse_let_arg_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cdr, &decl, &cdr));
	localhold_pushva(hold, decl, cdr, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cdr, cdr));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LET, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cdr);
	return Result(ret, eval);
}

/* let* */
static int parse_leta_arg_(Execute ptr, addr *ret, addr args)
{
	addr cons, one, symbol, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (cons = Nil; args != Nil; ) {
		Return_getcons(args, &one, &args);
		Return(parse_letone_(one, &symbol, &value));
		Return(check_variable_(symbol));
		Return(parse_self_(ptr, value));
		cons_heap(&one, symbol, value);
		cons_heap(&cons, one, cons);
		localhold_set(hold, 0, cons);
		/* macro */
		Return(lexical_envstack_(ptr, symbol));
	}
	localhold_end(hold);
	nreverse(ret, cons);

	return 0;
}

static int parse_leta_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, cdr, args, decl, eval;
	LocalHold hold;

	/* args, decl, body */
	GetCdr(cons, &cdr);
	if (! consp(cdr))
		return fmte_("let* form must be a (let* args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return_getcons(cdr, &args, &cdr);
	Return(parse_leta_arg_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cdr, &decl, &cdr));
	localhold_pushva(hold, decl, cdr, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cdr, cdr));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LETA, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cdr);
	return Result(ret, eval);
}

/* setq */
static int parse_setq_symbol_p_(Execute ptr, addr list, int *ret)
{
	int check;
	addr symbol, value;

	while (list != Nil) {
		Return_getcons(list, &symbol, &list);
		Return(check_variable_(symbol));
		Return(symbol_macrolet_envstack_p_(ptr, symbol, &value, &check));
		if (check)
			return Result(ret, 1);
		Return_getcons(list, &symbol, &list);
	}

	return Result(ret, 0);
}

static int parse_setq_macrolet_(Execute ptr, addr *ret, addr cons)
{
	int check;
	addr progn, root, setq, setf, var, value;

	/* symbol-macrolet
	 *  `(progn
	 *     (setq var1 value1)
	 *     (setf expand2 value2)
	 *     ...)
	 */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_SETF, &setf);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(cons, &value, &cons);
		Return(symbol_macrolet_envstack_p_(ptr, var, &var, &check));
		if (check)
			list_heap(&var, setf, var, value, NULL);
		else
			list_heap(&var, setq, var, value, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(&root, root);
	GetConst(COMMON_PROGN, &progn);
	cons_heap(&progn, progn, root);

	return parse_execute_(ptr, ret, progn);
}

static int parse_setq_symbol_(Execute ptr, addr *ret, addr cons)
{
	addr cdr, root, one, symbol;
	LocalHold hold;

	/* parse */
	GetCdr(cons, &cdr);
	hold = LocalHold_array(ptr, 1);
	symbol = NULL;
	for (root = Nil; cdr != Nil; ) {
		Return_getcons(cdr, &one, &cdr);
		if (symbol == NULL) {
			Return(check_variable_(one));
			symbol = one;
		}
		else {
			Return(parse_self_(ptr, one));
			cons_heap(&one, symbol, one);
			cons_heap(&root, one, root);
			localhold_set(hold, 0, root);
			symbol = NULL;
		}
	}
	localhold_end(hold);
	if (symbol != NULL)
		return fmte_("setq symbol ~S don't have a value argument.", symbol, NULL);
	nreverse(&root, root);

	/* eval */
	eval_parse2_heap(ret, EVAL_PARSE_SETQ, cons, root);
	return 0;
}

static int parse_setq_(Execute ptr, addr *ret, addr cons)
{
	int check;
	addr args;

	GetCdr(cons, &args);
	Return(parse_setq_symbol_p_(ptr, args, &check));
	if (check)
		return parse_setq_macrolet_(ptr, ret, args);
	else
		return parse_setq_symbol_(ptr, ret, cons);
}

/* defun */
static int check_variable_env_(Execute ptr, addr x)
{
	Return(check_variable_(x));
	Return(lexical_envstack_(ptr, x));
	return 0;
}

static inline int check_variable_notnil_(Execute ptr, addr x)
{
	if (x != Nil)
		return check_variable_env_(ptr, x);
	return 0;
}

static int parse_var_(Execute ptr, addr *ret, addr cons)
{
	addr root, var;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		Return(check_variable_env_(ptr, var));
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int parse_optional_(Execute ptr, addr *ret, addr cons)
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
		Return(check_variable_env_(ptr, var));
		Return(parse_self_(ptr, init));
		Return(check_variable_notnil_(ptr, pos));
		/* push */
		list_heap(&pos, var, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_key_(Execute ptr, addr *ret, addr cons)
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
		Return(check_variable_env_(ptr, var));
		Return(parse_self_(ptr, init));
		Return(check_variable_notnil_(ptr, pos));
		/* push */
		list_heap(&pos, var, name, init, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_aux_(Execute ptr, addr *ret, addr cons)
{
	addr root, pos, var;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init) */
		GetCons(pos, &var, &pos);
		GetCar(pos, &pos);
		Return(check_variable_env_(ptr, var));
		Return(parse_self_(ptr, pos));
		/* push */
		list_heap(&pos, var, pos, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	nreverse(ret, root);
	localhold_end(hold);

	return 0;
}

static int parse_ordinary_cons_(Execute ptr, addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux;
	LocalHold hold;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	hold = LocalHold_local(ptr);
	/* var */
	Return(parse_var_(ptr, &var, var));
	localhold_push(hold, var);
	/* opt */
	Return(parse_optional_(ptr, &opt, opt));
	localhold_push(hold, opt);
	/* rest */
	Return(check_variable_notnil_(ptr, rest));
	/* key */
	Return(parse_key_(ptr, &key, key));
	localhold_push(hold, key);
	/* aux */
	Return(parse_aux_(ptr, &aux, aux));
	localhold_push(hold, aux);
	/* result */
	localhold_end(hold);
	list_heap(ret, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

int parse_ordinary_(Execute ptr, addr *ret, addr args)
{
	Return(lambda_ordinary_(ptr->local, &args, args));
	return parse_ordinary_cons_(ptr, ret, args);
}

static void parse_implicit_block(addr *ret, addr name, addr list)
{
	addr block;

	GetConst(COMMON_BLOCK, &block);
	if (callnamep(name))
		GetCallName(name, &name);
	lista_heap(&list, block, name, list, NULL);
	conscar_heap(ret, list);
}

static int parse_defun_(Execute ptr, addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, body, form;
	LocalHold hold;

	/* (eval::defun name args decl doc body form) */
	List_bind(cons, &name, &args, &decl, &doc, &body, &form, NULL);

	/* parse */
	hold = LocalHold_local(ptr);
	Return(parse_ordinary_cons_(ptr, &args, args));
	localhold_push(hold, args);
	parse_implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_DEFUN, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, args);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);

	return Result(ret, eval);
}

/* defmacro */
static int parse_macro_lambda_list_(Execute ptr, addr *ret, addr args);
static int parse_macro_var_(Execute ptr, addr *ret, addr cons)
{
	addr root, var;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		if (consp(var)) {
			Return(parse_macro_lambda_list_(ptr, &var, var));
		}
		else {
			Return(check_variable_env_(ptr, var));
		}
		cons_heap(&root, var, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static inline int parse_macro_rest_(Execute ptr, addr *ret)
{
	addr pos;

	if (*ret != Nil) {
		/* (var . &rest) (var . &body) (var . nil) */
		GetCar(*ret, &pos);
		Return(check_variable_env_(ptr, pos));
		SetCar(*ret, pos);
	}

	return 0;
}

static int parse_macro_lambda_list_(Execute ptr, addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;
	LocalHold hold;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	hold = LocalHold_local(ptr);
	/* var */
	Return(parse_macro_var_(ptr, &var, var));
	localhold_push(hold, var);
	/* opt */
	Return(parse_optional_(ptr, &opt, opt));
	localhold_push(hold, opt);
	/* rest */
	Return(parse_macro_rest_(ptr, &rest));
	/* key */
	Return(parse_key_(ptr, &key, key));
	localhold_push(hold, key);
	/* aux */
	Return(parse_aux_(ptr, &aux, aux));
	localhold_push(hold, aux);
	/* others */
	Return(check_variable_notnil_(ptr, whole));
	Return(check_variable_notnil_(ptr, env));
	/* result */
	localhold_end(hold);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int make_macro_function_(Execute ptr, addr *ret, addr *reval,
		addr name, addr args, addr decl, addr doc, addr cons)
{
	addr eval, call;

	callname_heap(&call, name, CALLNAME_SYMBOL);
	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, call);
	if (reval)
		*reval = eval;

	return eval_result_macro_(ptr, eval, ret);
}

static int parse_defmacro_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, name, args, decl, doc, body, lambda, macro;
	LocalHold hold;

	/* (eval::defmacro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	Return(make_macro_function_(ptr, &lambda, &macro, name, args, decl, doc, body));
	localhold_push(hold, lambda);
	localhold_push(hold, macro);
	Return(defmacro_envstack_(ptr, name, lambda));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* defmacro */
	eval_parse_heap(&eval, EVAL_PARSE_DEFMACRO, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, macro);

	return Result(ret, eval);
}

/* macro-lambda */
static int parse_macro_lambda_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, args, decl, doc;
	LocalHold hold;

	/* (macro-lambda args . body) */
	if (! consp(cons)) {
		return fmte_("MACRO-LAMBDA argument ~S "
				"must be (lambda-list . form).", cons, NULL);
	}

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	GetCons(cons, &args, &cons);
	Return(lambda_macro_(ptr->local, &args, args, Nil));
	localhold_push(hold, args);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* macro-lambda */
	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, Nil);

	return Result(ret, eval);
}

/* deftype */
static int parse_deftype_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, name, args, decl, doc, body;
	LocalHold hold;

	/* (eval::deftype name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	parse_implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

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
static int parse_define_compiler_macro_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, name, args, decl, doc, body;
	LocalHold hold;

	/* (eval::define-compiler-macro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &body, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	parse_implicit_block(&body, name, body);
	localhold_push(hold, body);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

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
static int parse_destructuring_bind_(Execute ptr, addr *ret, addr cons)
{
	addr rollback, eval, lambda, args, expr, decl, body, form;
	LocalHold hold;

	/* (eval::destructuring-bind args expr decl body) */
	List_bind(cons, &args, &expr, &decl, &body, &form, NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	Return(localhold_parse_self_(hold, ptr, expr));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* lambda */
	eval_parse_heap(&lambda, EVAL_PARSE_MACRO_LAMBDA, 5);
	SetEvalParse(lambda, 0, args);
	SetEvalParse(lambda, 1, decl);
	SetEvalParse(lambda, 2, Nil);
	SetEvalParse(lambda, 3, body);
	SetEvalParse(lambda, 4, Nil);

	/* destructuring-bind */
	eval_parse_heap(&eval, EVAL_PARSE_DESTRUCTURING_BIND, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, lambda);

	return Result(ret, eval);
}

/* symbol-macrolet */
static int check_symbol_macrolet_(addr pos, addr decl)
{
	if (specialp_symbol(pos))
		return 1;
	if (! eval_declare_p(decl))
		return 0;
	getall_special_declare(decl, &decl);
	return find_list_eq_unsafe(pos, decl);

}

static int parse_symbol_macrolet_push_(Execute ptr, addr list, addr decl)
{
	addr pos, expr;

	/* parse */
	if (! consp_getcons(list, &pos, &list))
		goto error1;
	Return(check_function_variable_(pos));
	if (! consp_getcons(list, &expr, &list))
		goto error1;
	if (list != Nil)
		goto error1;
	/* special check */
	if (check_symbol_macrolet_(pos, decl))
		goto error2;
	/* push symbol-macro */
	return symbol_macrolet_envstack_(ptr, pos, expr);

error1:
	return fmte_("The symbol-macrolet arguemnt ~A "
			"must be a (symbol expansion) form.", list, NULL);

error2:
	return call_simple_program_error_va_(ptr,
			"The symbol ~S cannot declare the special.", pos, NULL);
}

int parse_symbol_macrolet_args_(Execute ptr, addr args, addr decl)
{
	addr pos;

	while (args != Nil) {
		if (! consp_getcons(args, &pos, &args))
			goto error;
		Return(parse_symbol_macrolet_push_(ptr, pos, decl));
	}
	return 0;

error:
	return fmte_("The symbol-macrolet arguemnt ~A "
			"must be a (symbol expansion) form.", args, NULL);
}

static int parse_symbol_macrolet_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, args, decl, rollback;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons)) {
		return fmte_("symbol-macrolet form must be "
				"(symbol-macrolet args . body).", NULL);
	}
	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	/* decl */
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	/* args */
	Return(parse_symbol_macrolet_args_(ptr, args, decl));
	/* body */
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);
}

/* macrolet */
static int parse_macrolet_one_(Execute ptr, addr cons)
{
	addr rollback, name, args, doc, decl;
	LocalHold hold;

	/* parse */
	if (! consp_getcons(cons, &name, &cons))
		goto error;
	if (! symbolp(name))
		return fmte_("The name ~S must be a symbol.", name, NULL);
	Return(check_function_variable_(name));
	if (! consp_getcons(cons, &args, &cons))
		goto error;

	/* make macro-function */
	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(lambda_macro_(ptr->local, &args, args, Nil));
	Return(parse_macro_lambda_list_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	parse_implicit_block(&cons, name, cons);
	localhold_push(hold, cons);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	Return(make_macro_function_(ptr, &cons, NULL, name, args, decl, doc, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* add environment */
	Return(macrolet_envstack_(ptr, name, cons));

	return 0;

error:
	return fmte_("macrolet argument must be (name (...) . body) form.", NULL);
}

int parse_macrolet_args_(Execute ptr, addr args)
{
	addr pos;

	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		Return(parse_macrolet_one_(ptr, pos));
	}

	return 0;
}

static int parse_macrolet_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, args, decl, rollback;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("macrolet form must be (macrolet args . body).", NULL);
	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));
	Return(parse_macrolet_args_(ptr, args));
	/* arguments */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* macrolet -> locally */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);
}

/* quote */
static int parse_quote_(Execute ptr, addr *ret, addr cons)
{
	addr args, value;

	GetCdr(cons, &args);
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* eval */
	Return(load_value_(ptr, value));
	eval_parse2_heap(ret, EVAL_PARSE_QUOTE, cons, value);
	return 0;

error:
	return fmte_("QUOTE form must have one argument, ~S.", cons, NULL);
}

/* function */
static int parse_lambda_(Execute ptr, addr *ret, addr form)
{
	addr rollback, cons, eval, args, doc, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("function lambda must be (lambda (...) body) form.", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	/* args */
	Return(parse_ordinary_(ptr, &args, args));
	localhold_push(hold, args);
	/* doc, decl */
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, NULL);
	/* cons */
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);

	return Result(ret, eval);
}

static int parse_function_argument_(Execute ptr, addr *ret, addr value)
{
	addr check, symbol, call;

	/* symbol function */
	if (! parse_callname_heap(&call, value)) {
		Return(check_function_variable_(call));
		eval_parse2_heap(ret, EVAL_PARSE_FUNCTION, value, call);
		return 0;
	}

	/* lambda function */
	if (! consp(value))
		return fmte_("FUNCTION ~S must be a fdefinition form.", value, NULL);
	GetConst(COMMON_LAMBDA, &symbol);
	GetCar(value, &check);
	if (check == symbol)
		return parse_lambda_(ptr, ret, value);

	/* others */
	return fmte_("FUNCTION ~S must be a fdefinition form.", value, NULL);
}

static int parse_function_(Execute ptr, addr *ret, addr cons)
{
	addr args, value;

	if (! consp_getcons(cons, &value, &args))
		goto error;
	if (args != Nil)
		goto error;

	return parse_function_argument_(ptr, ret, value);

error:
	return fmte_("FUNCTION form ~S must have one argument.", cons, NULL);
}

/* if */
static int parse_if_(Execute ptr, addr *ret, addr form)
{
	addr args, eval, expr, then, last;
	LocalHold hold;

	GetCdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (! consp_getcons(args, &then, &args))
		goto error;
	if (args == Nil) {
		last = Nil;
	}
	else {
		if (! consp_getcons(args, &last, &args))
			goto error;
		if (args != Nil)
			goto error;
	}

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_self_(hold, ptr, then));
	Return(localhold_parse_self_(hold, ptr, last));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_IF, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, then);
	SetEvalParse(eval, 3, last);
	return Result(ret, eval);

error:
	return fmte_("IF form must be (if expr then &optnioal else).", NULL);
}

/* unwind-protect */
static int parse_unwind_protect_(Execute ptr, addr *ret, addr form)
{
	addr expr, args, eval;
	LocalHold hold;

	GetCdr(form, &args);
	if (! consp_getcons(args, &expr, &args)) {
		return fmte_("UNWIND-PROTECT form must be "
				"a (unwind-protect form . body).", NULL);
	}
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &args, args));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_UNWIND_PROTECT, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, args);

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

static int parse_tagbody_check_(Execute ptr, addr cons, addr *rtag, addr *rbody)
{
	addr tag, body, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	for (tag = body = Nil; cons != Nil; ) {
		Return_getcons(cons, &pos, &cons);
		if (consp(pos)) {
			Return(parse_self_(ptr, pos));
			cons_heap(&body, pos, body);
		}
		else if (tagbody_tag_p(pos)) {
			if (parse_tagbody_findtag(pos, tag))
				return fmte_("The tag ~S is already exists.", pos, NULL);
			parse_tagbody_maketag(&pos, pos);
			cons_heap(&tag, pos, tag);
			cons_heap(&body, pos, body);
			localhold_set(hold, 1, tag);
		}
		else {
			return fmte_("The tag ~S must be a symbol or integer.", pos, NULL);
		}
		localhold_set(hold, 0, body);
	}
	localhold_end(hold);
	nreverse(rtag, tag);
	nreverse(rbody, body);

	return 0;
}

static int parse_tagbody_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, tag;

	GetCdr(form, &cons);
	Return(parse_tagbody_check_(ptr, cons, &tag, &cons));
	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_TAGBODY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* go */
static int parse_go_(addr *ret, addr cons)
{
	addr tag;

	if (! consp_getcons(cons, &tag, &cons))
		return fmte_("GO form must be (go tag).", NULL);
	if (cons != Nil)
		return fmte_("GO form must be (go tag).", NULL);
	if (! tagbody_tag_p(tag))
		return fmte_("The tag ~S must be a symbol or integer.", tag, NULL);
	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_GO, tag);

	return 0;
}

/* block */
static int parse_block_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, name;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &name, &cons))
		return fmte_("BLOCK form must be (block name . body).", NULL);
	if (! symbolp(name))
		return fmte_("BLOCK name ~S must be a symbol type.", name, NULL);
	Return(parse_allcons_(ptr, &cons, cons));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_BLOCK, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* return-from */
static int parse_return_from_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, name, value;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &name, &cons))
		goto error;
	if (cons == Nil) {
		value = Nil;
	}
	else {
		if (! consp_getcons(cons, &value, &cons))
			goto error;
		if (cons != Nil)
			goto error;
	}
	Return(parse_self_(ptr, value));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_RETURN_FROM, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, name);
	SetEvalParse(eval, 2, value);
	return Result(ret, eval);

error:
	return fmte_("RETURN-FROM form must be (return-from name [value]).", NULL);
}

/* catch */
static int parse_catch_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, tag;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &tag, &cons))
		return fmte_("CATCH form must be (catch tag . body).", NULL);
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, tag));
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CATCH, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* throw */
static int parse_throw_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, tag, result;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &tag, &cons))
		goto error;
	if (! consp_getcons(cons, &result, &cons))
		goto error;
	if (cons != Nil)
		goto error;
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, tag));
	Return(localhold_parse_self_(hold, ptr, result));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THROW, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, tag);
	SetEvalParse(eval, 2, result);
	return Result(ret, eval);

error:
	return fmte_("THROW form must be (throw tag result).", NULL);
}

/* flet */
static int parse_flet_one_(Execute ptr, addr *ret, addr cons)
{
	addr name, call, args, doc, decl;
	LocalHold hold;

	if (! consp_getcons(cons, &name, &cons))
		goto error;
	hold = LocalHold_local(ptr);
	Return(parse_callname_error_(&call, name));
	localhold_push(hold, call);
	Return(check_function_variable_(call));
	if (! consp_getcons(cons, &args, &cons))
		goto error;

	Return(parse_ordinary_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_documentation_(ptr, cons, &doc, &decl, &cons));
	localhold_pushva(hold, doc, decl, cons, NULL);
	parse_implicit_block(&cons, call, cons);
	localhold_push(hold, cons);
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_push(hold, cons);
	localhold_end(hold);
	list_heap(ret, call, args, decl, doc, cons, NULL);
	return 0;

error:
	return fmte_("flet/labels argument must be (name (...) . body) form.", NULL);
}

static int parse_flet_args_(Execute ptr, addr *ret, addr args)
{
	addr root, pos;
	LocalHold hold;

	/* flet */
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		Return(parse_flet_one_(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	/* macro */
	args = *ret;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCar(pos, &pos); /* call */
		Return(function_envstack_(ptr, pos));
	}

	return 0;
}

static int parse_flet_(Execute ptr, addr *ret, addr form)
{
	addr cons, rollback, eval, args, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("FLET form must be (flet args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_flet_args_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_FLET, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);

	return Result(ret, eval);
}

/* labels */
static int parse_labels_args_(Execute ptr, addr *ret, addr args)
{
	addr root, pos;
	LocalHold hold;

	/* macro */
	root = args;
	while (root != Nil) {
		GetCons(root, &pos, &root);
		Return_getcar(pos, &pos); /* call */
		Return(parse_callname_error_(&pos, pos));
		Return(function_envstack_(ptr, pos));
	}

	/* labels */
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		Return(parse_flet_one_(ptr, &pos, pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int parse_labels_(Execute ptr, addr *ret, addr form)
{
	addr cons, rollback, eval, args, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &args, &cons))
		return fmte_("labels form must be (labels args . body).", NULL);

	Return(snapshot_envstack_(ptr, &rollback));
	hold = LocalHold_local(ptr);
	Return(parse_labels_args_(ptr, &args, args));
	localhold_push(hold, args);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_pushva(hold, decl, cons, NULL);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);
	Return(rollback_envstack_(ptr, rollback));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LABELS, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, cons);

	return Result(ret, eval);
}

/* the */
static int parse_the_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, type, expr;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &type, &cons))
		goto error;
	if (! consp_getcons(cons, &expr, &cons))
		goto error;
	if (cons != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(parse_parse_type_(ptr, &type, type));
	localhold_push(hold, type);
	Return(localhold_parse_self_(hold, ptr, expr));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THE, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, type);
	SetEvalParse(eval, 2, expr);
	return Result(ret, eval);

error:
	return fmte_("the form must be (the type expr).", NULL);
}

/* eval-when */
int parse_eval_when_list_(addr list, addr *rcompile, addr *rload, addr *rexec)
{
	addr compile1, compile2, load1, load2, exec1, exec2;
	addr pos;

	/* constant */
	GetConst(KEYWORD_COMPILE_TOPLEVEL, &compile1);
	GetConst(KEYWORD_LOAD_TOPLEVEL, &load1);
	GetConst(KEYWORD_EXECUTE, &exec1);
	GetConst(COMMON_COMPILE, &compile2);
	GetConst(COMMON_LOAD, &load2);
	GetConst(COMMON_EVAL, &exec2);

	/* check */
	*rcompile = *rload = *rexec = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (pos == compile1 || pos == compile2) {
			*rcompile = T;
		}
		else if (pos == load1 || pos == load2) {
			*rload = T;
		}
		else if (pos == exec1 || pos == exec2) {
			*rexec = T;
		}
		else {
			return fmte_("Invalid situation ~S in EVAL-WHEN.", pos, NULL);
		}
	}

	return 0;
}

int parse_eval_when_process(Execute ptr,
		addr compile, addr load, addr exec, addr toplevel, addr mode)
{
	/* not toplevel */
	if (toplevel == Nil) {
		return exec != Nil;
	}

	/* not compile */
	if (! eval_compile_p(ptr)) {
		return exec != Nil;
	}

	/* Discard */
	if (compile == Nil && load == Nil) {
		return exec != Nil
			&& mode != Nil; /* compile-time-too */
	}

	/* Process */
	if (compile != Nil && load != Nil) {
		set_compile_time_eval(ptr, T); /* compile-time-too */
		return 1;
	}
	if (compile == Nil && load != Nil && exec != Nil) {
		set_compile_time_eval(ptr, toplevel);
		return 1;
	}
	if (compile == Nil && load != Nil && exec == Nil) {
		set_compile_time_eval(ptr, Nil); /* not-compile-time */
		return 1;
	}

	/* Evaluate */
	return 1;
}

static int parse_eval_when_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, list;
	addr compile, load, exec, toplevel, mode, value;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &list, &cons))
		return fmte_("eval-when form must be (eval-when (...) . body).", NULL);

	/* arguments */
	Return(parse_eval_when_list_(list, &compile, &load, &exec));
	Return(get_toplevel_eval_(ptr, &toplevel));
	Return(get_compile_time_eval_(ptr, &value));

	/* discard */
	if (! parse_eval_when_process(ptr, compile, load, exec, toplevel, value)) {
		eval_single_parse_heap(ret, EVAL_PARSE_NIL, Nil);
		return 0;
	}

	/* body */
	Return(get_compile_time_eval_(ptr, &mode));
	Return(parse_allcons_(ptr, &cons, cons));
	set_compile_time_eval(ptr, value);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_EVAL_WHEN, 7);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	SetEvalParse(eval, 2, compile);   /* :compile-toplevel */
	SetEvalParse(eval, 3, load);      /* :load-toplevel */
	SetEvalParse(eval, 4, exec);      /* :execute */
	SetEvalParse(eval, 5, toplevel);  /* toplevel */
	SetEvalParse(eval, 6, mode);      /* compile-time */
	return Result(ret, eval);
}

/* values */
static int parse_values_(Execute ptr, addr *ret, addr form)
{
	addr cons;

	GetCdr(form, &cons);
	Return(parse_allcons_(ptr, &cons, cons));
	eval_parse2_heap(ret, EVAL_PARSE_VALUES, form, cons);

	return 0;
}

/* locally */
static int parse_locally_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, decl;
	LocalHold hold;

	GetCdr(form, &cons);
	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, cons);
	Return(parse_declare_body_(ptr, cons, &decl, &cons));
	localhold_set(hold, 1, decl);
	localhold_set(hold, 2, cons);
	Return(parse_allcons_(ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);
}

/* declaim */
static int parse_declaim_(addr *ret, addr args)
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
static int parse_call_(Execute ptr, addr *ret, addr form)
{
	addr call, cons, eval;
	LocalHold hold;

	GetCons(form, &call, &cons);
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, call);
	localhold_set(hold, 1, cons);
	Return(parse_function_argument_(ptr, &call, call));
	localhold_set(hold, 0, call);
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CALL, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, call);
	SetEvalParse(eval, 2, cons);

	return Result(ret, eval);
}

/* multiple-value-bind */
static int parse_multiple_value_bind_(Execute ptr, addr *ret, addr cons)
{
	addr form, eval, vars, expr, decl, doc, body;
	LocalHold hold;

	GetCdr(cons, &cons);
	if (! consp(cons))
		goto error;
	Return_getcons(cons, &vars, &cons);
	Return_getcons(cons, &expr, &cons);
	Return_getcons(cons, &decl, &cons);
	Return_getcons(cons, &doc, &cons);
	Return_getcons(cons, &body, &cons);
	Return_getcons(cons, &form, &cons);
	if (cons != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_BIND, 6);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, vars);
	SetEvalParse(eval, 2, expr);
	SetEvalParse(eval, 3, decl);
	SetEvalParse(eval, 4, doc);
	SetEvalParse(eval, 5, body);
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be (system::multiple-value-bind "
			"(vars expr decl doc form).", cons, NULL);
}

/* multiple-value-call */
static int parse_multiple_value_call_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, expr;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &expr, &cons))
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_CALL, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);

error:
	return fmte_("The form ~S "
			"must be (multiple-value-call function . body).", cons, NULL);
}

/* multiple-value-prog1 */
static int parse_multiple_value_prog1_(Execute ptr, addr *ret, addr form)
{
	addr cons, eval, expr;
	LocalHold hold;

	GetCdr(form, &cons);
	if (! consp_getcons(cons, &expr, &cons))
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	Return(localhold_parse_allcons_(hold, ptr, &cons, cons));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, cons);
	return Result(ret, eval);

error:
	return fmte_("The form ~S "
			"must be (multiple-value-prog1 first-form . body).", cons, NULL);
}

/* nth-value */
static int parse_nth_value_(Execute ptr, addr *ret, addr list)
{
	addr eval, next, nth, expr, form;
	LocalHold hold;

	if (! consp_getcons(list, &nth, &next))
		goto error;
	if (! consp_getcons(next, &expr, &next))
		goto error;
	if (! consp_getcons(next, &form, &next))
		goto error;
	if (next != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, nth));
	Return(localhold_parse_self_(hold, ptr, expr));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_NTH_VALUE, 3);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, nth);
	SetEvalParse(eval, 2, expr);
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be (nth-value nth expr).", list, NULL);
}

/* progv */
static int parse_progv_(Execute ptr, addr *ret, addr form)
{
	addr eval, symbols, values, body;
	LocalHold hold;

	GetCdr(form, &body);
	if (! consp_getcons(body, &symbols, &body))
		goto error;
	if (! consp_getcons(body, &values, &body))
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, symbols));
	Return(localhold_parse_self_(hold, ptr, values));
	Return(localhold_parse_allcons_(hold, ptr, &body, body));
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_PROGV, 4);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, symbols);
	SetEvalParse(eval, 2, values);
	SetEvalParse(eval, 3, body);
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be (progv symbols values . body).", form, NULL);
}

/* symbol */
static int parse_symbol_(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value;

	/* symbol macro */
	Return(symbol_macrolet_envstack_p_(ptr, pos, &value, &check));
	if (check)
		return parse_execute_(ptr, ret, value);

	/* symbol */
	eval_single_parse_heap(ret, EVAL_PARSE_SYMBOL, pos);
	return 0;
}

/* backquote */
static int parse_backquote_(Execute ptr, addr *ret, addr pos)
{
	if (! quote_back_p(pos))
		return fmte_("Invalid quote type.", NULL);
	getvalue_quote(pos, &pos);
	return parse_execute_(ptr, ret, pos);
}

/* parse_cons */
static int parse_cons_check_constant(addr call, constindex index)
{
	addr check;
	GetConstant(index, &check);
	return check == call;
}
#define ParseConsConstant(x, y) parse_cons_check_constant((x), CONSTANT_##y)

static int parse_cons_(Execute ptr, addr *ret, addr cons)
{
	addr call, check, args;

	/* macro */
	Return(parse_macroexpand_(ptr, &check, cons));
	if (check != Unbound) {
		return parse_execute_(ptr, ret, check);
	}

	/* operator */
	GetCons(cons, &call, &args);
	if (ParseConsConstant(call, COMMON_PROGN)) {
		return parse_progn_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LOCALLY)) {
		return parse_locally_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_MACROLET)) {
		return parse_macrolet_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_SYMBOL_MACROLET)) {
		return parse_symbol_macrolet_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_EVAL_WHEN)) {
		return parse_eval_when_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LET)) {
		return parse_let_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LETA)) {
		return parse_leta_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_SETQ)) {
		return parse_setq_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_QUOTE)) {
		return parse_quote_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_FUNCTION)) {
		return parse_function_(ptr, ret, args);
	}
	if (ParseConsConstant(call, COMMON_IF)) {
		return parse_if_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_UNWIND_PROTECT)) {
		return parse_unwind_protect_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_TAGBODY)) {
		return parse_tagbody_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_GO)) {
		return parse_go_(ret, args);
	}
	if (ParseConsConstant(call, COMMON_BLOCK)) {
		return parse_block_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_RETURN_FROM)) {
		return parse_return_from_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_CATCH)) {
		return parse_catch_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_THROW)) {
		return parse_throw_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_FLET)) {
		return parse_flet_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LABELS)) {
		return parse_labels_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_THE)) {
		return parse_the_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_VALUES)) {
		return parse_values_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, SYSTEM_DECLAIM)) {
		return parse_declaim_(ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFUN)) {
		return parse_defun_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFMACRO)) {
		return parse_defmacro_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFTYPE)) {
		return parse_deftype_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DEFINE_COMPILER_MACRO)) {
		return parse_define_compiler_macro_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_DESTRUCTURING_BIND)) {
		return parse_destructuring_bind_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_MACRO_LAMBDA)) {
		return parse_macro_lambda_(ptr, ret, args);
	}
	if (ParseConsConstant(call, SYSTEM_MULTIPLE_VALUE_BIND)) {
		return parse_multiple_value_bind_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_MULTIPLE_VALUE_CALL)) {
		return parse_multiple_value_call_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_MULTIPLE_VALUE_PROG1)) {
		return parse_multiple_value_prog1_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, SYSTEM_NTH_VALUE)) {
		return parse_nth_value_(ptr, ret, args);
	}
	if (ParseConsConstant(call, COMMON_PROGV)) {
		return parse_progv_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, COMMON_LOAD_TIME_VALUE)) {
		return parse_load_time_value_(ptr, ret, cons);
	}
	if (ParseConsConstant(call, SYSTEM_STEP)) {
		return parse_step_(ptr, ret, args);
	}

	return parse_call_(ptr, ret, cons);
}

static int parse_clos_(Execute ptr, addr *ret, addr pos)
{
	Return(load_value_(ptr, pos));
	eval_single_parse_heap(ret, EVAL_PARSE_CLOS, pos);
	return 0;
}

static void parse_array(addr *ret, addr pos)
{
	if (strarrayp(pos))
		eval_single_parse_heap(ret, EVAL_PARSE_STRING, pos);
	else
		eval_single_parse_heap(ret, EVAL_PARSE_ARRAY, pos);
}

static int parse_switch_(Execute ptr, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			return parse_cons_(ptr, ret, pos);

		case LISPTYPE_NIL:
			eval_single_parse_heap(ret, EVAL_PARSE_NIL, Nil);
			break;

		case LISPTYPE_T:
			eval_single_parse_heap(ret, EVAL_PARSE_T, T);
			break;

		case LISPTYPE_CLOS:
			return parse_clos_(ptr, ret, pos);

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
			return parse_symbol_(ptr, ret, pos);

		case LISPTYPE_FUNCTION:
			eval_parse2_heap(ret, EVAL_PARSE_FUNCTION, pos, pos);
			break;

		case LISPTYPE_PACKAGE:
			eval_single_parse_heap(ret, EVAL_PARSE_PACKAGE, pos);
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

		case LISPTYPE_QUOTE:
			return parse_backquote_(ptr, ret, pos);

		default:
			return fmte_("parse-error: ~S.", pos, NULL);
	}

	return 0;
}

int parse_execute_(Execute ptr, addr *ret, addr pos)
{
	addr toplevel;

	/* toplevel */
	Return(get_toplevel_eval_(ptr, &toplevel));
	if (toplevel == Nil)
		return parse_switch_(ptr, ret, pos);

	/* parse */
	set_toplevel_eval(ptr, Nil);
	Return(parse_switch_(ptr, ret, pos));
	set_toplevel_eval(ptr, toplevel);

	return 0;
}

