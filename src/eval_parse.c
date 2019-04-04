#include "cons.h"
#include "constant.h"
#include "condition.h"
#include "control.h"
#include "copy.h"
#include "equal.h"
#include "execute.h"
#include "eval_code.h"
#include "eval_declare.h"
#include "eval_parse.h"
#include "eval_scope.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "lambda.h"
#include "object.h"
#include "quote.h"
#include "sequence.h"
#include "strtype.h"
#include "symbol.h"
#include "type.h"
#include "type_copy.h"
#include "type_parse.h"

static int parse_execute(addr *ret, addr pos);
#define parse_self(x) parse_execute(&(x), (x))

/*
 *  environment
 */
static void environment_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_PARSE_ENVIRONMENT, ret);
}

static void envroot_heap(addr *ret)
{
	/* global, local */
	heap_array2(ret, LISPSYSTEM_ENVROOT, 2);
}

static void envstack_heap(addr *ret, addr next, addr call, addr lambda, addr callp)
{
	addr pos;

	/* next, call, lambda, macro/symbol */
	heap_array2(&pos, LISPSYSTEM_ENVSTACK, 4);
	SetArrayA2(pos, 0, next);
	SetArrayA2(pos, 1, call);
	SetArrayA2(pos, 2, lambda);
	SetArrayA2(pos, 3, callp);
	*ret = pos;
}

static void init_environment(Execute ptr)
{
	addr symbol, pos;

	environment_symbol(&symbol);
	envroot_heap(&pos);
	pushspecial_control(ptr, symbol, pos);
}

static void snapshot_envstack(addr *ret)
{
	Execute ptr;
	addr root;

	ptr = Execute_Thread;
	environment_symbol(&root);
	getspecialcheck_local(ptr, root, &root);
	GetArrayA2(root, 1, ret); /* local */
}

static void push_envstack(int index, addr name, addr lambda, addr callp)
{
	Execute ptr;
	addr root, pos, next;

	ptr = Execute_Thread;
	environment_symbol(&root);
	getspecialcheck_local(ptr, root, &root);
	GetArrayA2(root, index, &next);
	envstack_heap(&pos, next, name, lambda, callp);
	SetArrayA2(root, index, pos);
}

static void rollback_envstack(addr pos)
{
	Execute ptr;
	addr root, local, next;

	ptr = Execute_Thread;
	environment_symbol(&root);
	getspecialcheck_local(ptr, root, &root);
	for (;;) {
		GetArrayA2(root, 1, &local); /* local */
		if (local == pos) break;
		if (local == Nil)
			fmte("environment stack error.", NULL);
		GetArrayA2(local, 0, &next); /* next */
		SetArrayA2(local, 0, Nil); /* next */
		SetArrayA2(root, 1, next); /* local */
	}
}

static void defmacro_envstack(addr name, addr lambda)
{
	push_envstack(0, name, lambda, T); /* global, macrolet */
}

static void macrolet_envstack(addr name, addr lambda)
{
	push_envstack(1, name, lambda, T); /* local, macrolet */
}

static void define_symbol_macro_envstack(addr name, addr form)
{
	push_envstack(0, name, form, Nil); /* global, define-symbol-macro */
}

static void symbol_macrolet_envstack(addr name, addr form)
{
	push_envstack(1, name, form, Nil); /* local, symbol-macrolet */
}

static int symbol_macrolet_envroot_p(addr name, addr root, int index, addr *ret)
{
	addr next, check, callp;

	GetArrayA2(root, index, &next);
	while (next != Nil) {
		GetArrayA2(next, 3, &callp);
		if (callp == Nil) {
			/* symbol macro */
			GetArrayA2(next, 1, &check);
			if (name == check) {
				if (ret)
					GetArrayA2(next, 2, ret);
				return 1;
			}
		}
		GetArrayA2(next, 0, &next);
	}

	return 0;
}

static int symbol_macrolet_envstack_p(addr name, addr *ret)
{
	Execute ptr;
	addr root;

	ptr = Execute_Thread;
	environment_symbol(&root);
	getspecialcheck_local(ptr, root, &root);
	return symbol_macrolet_envroot_p(name, root, 0, ret)
		|| symbol_macrolet_envroot_p(name, root, 1, ret);
}

static void environment_heap(addr *ret)
{
	Execute ptr;
	addr pos, env, local;

	/* envstack */
	ptr = Execute_Thread;
	environment_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetArrayA2(pos, 0, &env);
	GetArrayA2(pos, 1, &local);

	/* environment */
	heap_array2(&pos, LISPTYPE_ENVIRONMENT, 2);
	/*copy_list_heap_unsafe(&env, env);*/
	/*copy_list_heap_unsafe(&local, local);*/
	SetArrayA2(pos, 0, env);
	SetArrayA2(pos, 1, local);
	SetUser(pos, 1); /* dynamic-extent check */
	*ret = pos;
}

static void copy_environment(LocalRoot local, addr *ret, addr pos)
{
	*ret = pos; /* do nothing */
}

static void close_environment(addr pos)
{
	Check(GetType(pos) != LISPTYPE_ENVIRONMENT, "type error");
	SetArrayA2(pos, 0, Nil);
	SetArrayA2(pos, 1, Nil);
	SetUser(pos, 0);
}

static int closep_environment(addr pos)
{
	Check(GetType(pos) != LISPTYPE_ENVIRONMENT, "type error");
	return GetUser(pos) == 0;
}


/*
 *  eval_declare
 */
static int parse_declare_body(addr cons, addr *retdecl, addr *retbody)
{
	int check;
	addr env;
	Execute ptr;

	ptr = Execute_Thread;
	environment_heap(&env);
	check = declare_body(ptr, env, cons, retdecl, retbody);
	close_environment(env);

	return check;
}

static int parse_declare_body_documentation(addr cons,
		addr *rdoc, addr *rdecl, addr *rbody)
{
	int check;
	addr env;
	Execute ptr;

	ptr = Execute_Thread;
	environment_heap(&env);
	check = declare_body_documentation(ptr, env, cons, rdoc, rdecl, rbody);
	close_environment(env);

	return check;
}

static int parse_parse_type(addr *ret, addr type)
{
	int check;
	addr env;
	Execute ptr;

	ptr = Execute_Thread;
	environment_heap(&env);
	check = parse_type(ptr, ret, type, env);
	close_environment(env);

	return check;
}


/*
 *  memory
 */
void eval_parse_alloc(LocalRoot local, addr *ret, enum EVAL_PARSE type, byte array)
{
	addr pos;

	Check(0xFF < sizeof(struct eval_parse), "struct size error");
	eval_alloc(local, &pos, EVAL_TYPE_PARSE, array, sizeoft(struct eval_parse));
	SetEvalParseType(pos, type);

	*ret = pos;
}
void eval_parse_local(LocalRoot local, addr *ret, enum EVAL_PARSE type, byte array)
{
	Check(local == NULL, "local error");
	eval_parse_alloc(local, ret, type, array);
}
void eval_parse_heap(addr *ret, enum EVAL_PARSE type, byte array)
{
	eval_parse_alloc(NULL, ret, type, array);
}

void eval_single_parse_alloc(LocalRoot local,
		addr *ret, enum EVAL_PARSE type, addr value)
{
	eval_parse_alloc(local, ret, type, 1);
	SetEvalParse(*ret, 0, value);
}
void eval_single_parse_local(LocalRoot local,
		addr *ret, enum EVAL_PARSE type, addr value)
{
	Check(local == NULL, "local error");
	eval_single_parse_alloc(local, ret, type, value);
}
void eval_single_parse_heap(addr *ret, enum EVAL_PARSE type, addr value)
{
	eval_single_parse_alloc(NULL, ret, type, value);
}

struct eval_parse *structevalparse(addr pos)
{
	Check(! eval_parse_p(pos), "type error");
	return StructEvalParse_Low(pos);
}
addr refevalparse(addr pos, size_t index)
{
	Check(! eval_parse_p(pos), "type error");
	return RefEvalParse_Low(pos, index);
}
void getevalparse(addr pos, size_t index, addr *ret)
{
	Check(! eval_parse_p(pos), "type error");
	GetEvalParse_Low(pos, index, ret);
}
void setevalparse(addr pos, size_t index, addr value)
{
	Check(! eval_parse_p(pos), "type error");
	SetEvalParse_Low(pos, index, value);
}
enum EVAL_PARSE refevalparsetype(addr pos)
{
	Check(! eval_parse_p(pos), "type error");
	return RefEvalParseType_Low(pos);
}
void getevalparsetype(addr pos, enum EVAL_PARSE *ret)
{
	Check(! eval_parse_p(pos), "type error");
	GetEvalParseType_Low(pos, ret);
}
void setevalparsetype(addr pos, enum EVAL_PARSE value)
{
	Check(! eval_parse_p(pos), "type error");
	SetEvalParseType_Low(pos, value);
}


/*
 *  eval-parse
 */
/* progn */
static int parse_allcons(addr *ret, addr cons)
{
	addr root, pos;

	for (root = Nil; cons != Nil; ) {
		getcons(cons, &pos, &cons);
		if (parse_self(pos)) return 1;
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int parse_progn(addr *ret, addr cons)
{
	if (parse_allcons(&cons, cons)) return 1;
	eval_single_parse_heap(ret, EVAL_PARSE_PROGN, cons);

	return 0;
}

/* let / let* */
void check_variable(addr symbol)
{
	if (! IsSymbol(symbol))
		fmte("The variable ~S must be a symbol.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		fmte("The variable ~S don't allow constant symbol.", symbol, NULL);
}

void check_function_variable(addr symbol)
{
	addr check;

	if (IsSymbol(symbol)) {
		if (GetStatusReadOnly(symbol))
			fmte("The variable ~S don't allow constant symbol.", symbol, NULL);
	}
	else if (GetType(symbol) == LISPTYPE_CALLNAME) {
		GetCallName(symbol, &check);
		if (! IsSymbol(check))
			fmte("The variable ~S must be a symbol.", check, NULL);
		if (callname_constant_p(symbol))
			fmte("The variable ~S don't allow constant symbol.", check, NULL);
	}
	else {
		fmte("The ~S don't allow variable.", symbol, NULL);
	}
}

static void parse_letone(addr one, addr *rets, addr *retv)
{
	addr symbol, value;

	/* symbol */
	if (IsSymbol(one)) {
		*rets = one;
		*retv = Nil;
		return;
	}

	/* not cons */
	if (GetType(one) != LISPTYPE_CONS)
		fmte("Invalid let argument ~S.", one, NULL);

	/* (symbol) */
	GetCons(one, &symbol, &one);
	if (one == Nil) {
		*rets = symbol;
		*retv = Nil;
		return;
	}

	/* (symbol . value) */
	if (GetType(one) != LISPTYPE_CONS)
		fmte("Invalid let argument ~S.", one, NULL);

	/* (symbol value . tail) */
	GetCons(one, &value, &one);
	if (one != Nil)
		fmte("Invalid let argument ~S.", one, NULL);

	/* (symbol value) */
	*rets = symbol;
	*retv = value;
}

static int parse_letarg(addr *ret, addr args)
{
	addr cons, one, symbol, value;

	for (cons = Nil; args != Nil; ) {
		getcons(args, &one, &args);
		parse_letone(one, &symbol, &value);
		check_variable(symbol);
		if (parse_self(value)) return 1;
		cons_heap(&one, symbol, value);
		cons_heap(&cons, one, cons);
	}
	nreverse_list_unsafe(ret, cons);

	return 0;
}

static int parse_let(addr *ret, enum EVAL_PARSE type, addr cons)
{
	addr args, decl, eval;

	/* args, decl, body */
	if (GetType(cons) != LISPTYPE_CONS) {
		if (type == EVAL_PARSE_LET)
			fmte("let form must be a (let args . body).", NULL);
		else
			fmte("let* form must be a (let* args . body).", NULL);
	}
	getcons(cons, &args, &cons);
	if (parse_letarg(&args, args)) return 1;
	if (parse_declare_body(cons, &decl, &cons)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;

	return 0;
}

/* setq */
static int parse_setq_symbol(addr *ret, addr cons)
{
	addr root, one, symbol;

	/* parse */
	symbol = NULL;
	for (root = Nil; cons != Nil; ) {
		getcons(cons, &one, &cons);
		if (symbol == NULL) {
			check_variable(one);
			symbol = one;
		}
		else {
			if (parse_self(one)) return 1;
			cons_heap(&one, symbol, one);
			cons_heap(&root, one, root);
			symbol = NULL;
		}
	}
	if (symbol != NULL)
		fmte("setq symbol ~S don't have a value argument.", symbol, NULL);
	nreverse_list_unsafe(&root, root);

	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_SETQ, root);

	return 0;
}

static int parse_setq_symbol_p(addr list)
{
	addr symbol;

	while (list != Nil) {
		getcons(list, &symbol, &list);
		if (symbol_macrolet_envstack_p(symbol, NULL)) return 1;
		getcons(list, &symbol, &list);
	}

	return 0;
}

static int parse_setq(addr *ret, addr cons)
{
	addr progn, root, setq, setf, var, value;

	/* symbol only */
	if (! parse_setq_symbol_p(cons))
		return parse_setq_symbol(ret, cons);

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
		if (symbol_macrolet_envstack_p(var, &var))
			list_heap(&var, setf, var, value, NULL);
		else
			list_heap(&var, setq, var, value, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(&root, root);
	GetConst(COMMON_PROGN, &progn);
	cons_heap(&progn, progn, root);

	return eval_parse(ret, progn);
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
	nreverse_list_unsafe(ret, root);
}

static int parse_optional(addr *ret, addr cons)
{
	addr root, pos, var, init;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		check_variable(var);
		if (parse_self(init)) return 1;
		check_variable_notnil(&pos);
		/* push */
		list_heap(&pos, var, init, pos, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int parse_key(addr *ret, addr cons)
{
	addr root, pos, var, name, init;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var name init svar) */
		GetCons(pos, &var, &pos);
		GetCons(pos, &name, &pos);
		GetCons(pos, &init, &pos);
		GetCar(pos, &pos);
		check_variable(var);
		if (parse_self(init)) return 1;
		check_variable_notnil(&pos);
		/* push */
		list_heap(&pos, var, name, init, pos, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int parse_aux(addr *ret, addr cons)
{
	addr root, pos, var;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		/* (var init) */
		GetCons(pos, &var, &pos);
		GetCar(pos, &pos);
		check_variable(var);
		if (parse_self(pos)) return 1;
		/* push */
		list_heap(&pos, var, pos, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int parse_ordinary_cons(addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	parse_var(&var, var);
	if (parse_optional(&opt, opt)) return 1;
	check_variable_notnil(&rest);
	if (parse_key(&key, key)) return 1;
	if (parse_aux(&aux, aux)) return 1;
	list_heap(ret, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

static int parse_ordinary(addr *ret, addr args)
{
	lambda_ordinary(Local_Thread, &args, args);
	return parse_ordinary_cons(ret, args);
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

static int parse_defun(addr *ret, addr cons)
{
	addr eval, name, args, decl, doc;

	/* (eval::defun name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &cons, NULL);

	/* parse */
	if (parse_ordinary_cons(&args, args)) return 1;
	implicit_block(&cons, name, cons);
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_DEFUN, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;

	return 0;
}

/* defmacro */
int parse_macro_lambda_list(addr *ret, addr args);
static int parse_macro_var(addr *ret, addr cons)
{
	addr root, var;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		if (consp(var)) {
			if (parse_macro_lambda_list(&var, var)) return 1;
		}
		else {
			check_variable(var);
		}
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);

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

int parse_macro_lambda_list(addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	if (parse_macro_var(&var, var)) return 1;
	if (parse_optional(&opt, opt)) return 1;
	parse_macro_rest(&rest);
	if (parse_key(&key, key)) return 1;
	if (parse_aux(&aux, aux)) return 1;
	check_variable_notnil(&whole);
	check_variable_notnil(&env);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int execute_macro_lambda_eval(Execute ptr, addr eval)
{
	if (eval_scope(ptr, &eval, eval)) return 1;
	eval_code(ptr->local, &eval, eval);
	if (runcode_control(ptr, eval)) return 1;
	return 0;
}

static int execute_macro_lambda(addr *ret, addr eval)
{
	Execute ptr;
	addr control;

	/* push */
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	/* code */
	if (execute_macro_lambda_eval(ptr, eval)) {
		return runcode_free_control(ptr, control);
	}
	else {
		getresult_control(ptr, ret);
		return free_control(ptr, control);
	}
}

static int make_macro_function(addr *ret, addr args, addr decl, addr doc, addr cons)
{
	addr eval;

	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	return execute_macro_lambda(ret, eval);
}

static int parse_defmacro(addr *ret, addr cons)
{
	addr eval, name, args, decl, doc, lambda;

	/* (eval::defmacro name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &cons, NULL);
	if (parse_macro_lambda_list(&args, args)) return 1;
	if (parse_allcons(&cons, cons)) return 1;
	if (make_macro_function(&lambda, args, decl, doc, cons)) return 1;
	defmacro_envstack(name, lambda);
	/* defmacro */
	eval_parse_heap(&eval, EVAL_PARSE_DEFMACRO, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, lambda);
	*ret = eval;

	return 0;
}

/* deftype */
static int parse_deftype(addr *ret, addr cons)
{
	addr eval, name, args, decl, doc;

	/* (eval::deftype name args decl doc body) */
	List_bind(cons, &name, &args, &decl, &doc, &cons, NULL);
	if (parse_macro_lambda_list(&args, args)) return 1;
	implicit_block(&cons, name, cons);
	if (parse_allcons(&cons, cons)) return 1;
	/* deftype */
	eval_parse_heap(&eval, EVAL_PARSE_DEFTYPE, 5);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, args);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, cons);
	*ret = eval;

	return 0;
}

/* macro-lambda */
static int parse_macro_lambda(addr *ret, addr cons)
{
	addr eval, args, decl, doc;

	/* (macro-lambda args . body) */
	if (! consp(cons))
		fmte("MACRO-LAMBDA argument ~S must be (lambda-list . form).", cons, NULL);
	GetCons(cons, &args, &cons);
	lambda_macro(Local_Thread, &args, args, Nil);
	if (parse_macro_lambda_list(&args, args)) return 1;
	if (parse_declare_body_documentation(cons, &doc, &decl, &cons)) return 1;
	if (parse_allcons(&cons, cons)) return 1;
	/* macro-lambda */
	eval_parse_heap(&eval, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	*ret = eval;

	return 0;
}

/* destructuring-bind */
static int parse_destructuring_bind(addr *ret, addr cons)
{
	addr eval, lambda, args, expr, decl;

	/* (eval::destructuring-bind args expr decl body) */
	List_bind(cons, &args, &expr, &decl, &cons, NULL);
	if (parse_macro_lambda_list(&args, args)) return 1;
	if (parse_allcons(&cons, cons)) return 1;
	if (parse_self(expr)) return 1;
	eval_parse_heap(&lambda, EVAL_PARSE_MACRO_LAMBDA, 4);
	SetEvalParse(lambda, 0, args);
	SetEvalParse(lambda, 1, decl);
	SetEvalParse(lambda, 2, Nil);
	SetEvalParse(lambda, 3, cons);
	/* destructuring-bind */
	eval_parse_heap(&eval, EVAL_PARSE_DESTRUCTURING_BIND, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, lambda);
	*ret = eval;

	return 0;
}

/* macrolet */
static int parse_macrolet_one(addr cons)
{
	LocalRoot local;
	addr name, args, doc, decl;

	/* parse */
	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &name, &cons);
	if (! symbolp(name))
		fmte("The name ~S must be a symbol.", name, NULL);
	check_function_variable(name);
	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &args, &cons);
	/* make macro-function */
	local = Local_Thread;
	lambda_macro(local, &args, args, Nil);
	if (parse_macro_lambda_list(&args, args)) return 1;
	if (parse_declare_body_documentation(cons, &doc, &decl, &cons)) return 1;
	implicit_block(&cons, name, cons);
	if (parse_allcons(&cons, cons)) return 1;
	if (make_macro_function(&cons, args, decl, doc, cons)) return 1;
	macrolet_envstack(name, cons);
	return 0;

error:
	fmte("macrolet argument must be (name (...) . body) form.", NULL);
	return 1;
}

static void parse_macrolet_args(addr args)
{
	addr pos;

	while (args != Nil) {
		getcons(args, &pos, &args);
		parse_macrolet_one(pos);
	}
}

static int parse_macrolet(addr *ret, addr cons)
{
	addr eval, args, decl, rollback;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("macrolet form must be (macrolet args . body).", NULL);
	getcons(cons, &args, &cons);
	/* local scope environment */
	snapshot_envstack(&rollback);
	parse_macrolet_args(args);
	if (parse_declare_body(cons, &decl, &cons)) return 1;
	if (parse_allcons(&cons, cons)) return 1;
	rollback_envstack(rollback);

	/* macrolet -> locally */
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(eval, 0, decl);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
}

static void check_define_symbol_macro(addr symbol)
{
	addr value;

	if (specialp_symbol(symbol))
		fmte("define-symbol-macro cannot bind the special symbol ~S.", symbol, NULL);
	GetValueSymbol(symbol, &value);
	if (value != Unbound)
		fmte("define-symbol-macro cannot bind the bounded symbol ~S.", symbol, NULL);
}

static int parse_define_symbol_macro(addr *ret, addr cons)
{
	addr eval, symbol, body, form;

	if (! consp(cons)) goto error;
	GetCons(cons, &symbol, &cons);
	check_function_variable(symbol);
	if (! consp(cons)) goto error;
	GetCons(cons, &body, &cons);
	if (cons != Nil) goto error;
	check_define_symbol_macro(symbol);
	define_symbol_macro_envstack(symbol, body); /* before parse */
	if (eval_parse(&form, body)) return 1;

	eval_parse_heap(&eval, EVAL_PARSE_DEFINE_SYMBOL_MACRO, 3);
	SetEvalParse(eval, 0, symbol);
	SetEvalParse(eval, 1, form);
	SetEvalParse(eval, 2, body);
	*ret = eval;

	return 0;

error:
	fmte("define-symbol-macro arguments ~S must be (symbol form).", cons, NULL);
	return 1;
}

static int parse_symbol_macrolet_args(addr *ret, addr args)
{
	addr root, cons, symbol, expansion, env;

	for (root = Nil; args != Nil; ) {
		/* parse */
		getcons(args, &cons, &args);
		if (! consp(cons)) goto error;
		GetCons(cons, &symbol, &cons);
		check_function_variable(symbol);
		if (! consp(cons)) goto error;
		GetCons(cons, &expansion, &cons);
		if (cons != Nil) goto error;
		symbol_macrolet_envstack(symbol, expansion); /* before parse */
		if (parse_self(expansion)) return 1;
		/* (symbol expansion env) */
		environment_heap(&env);
		list_heap(&cons, symbol, expansion, env, NULL);
		cons_heap(&root, cons, root);
	}
	nreverse_list_unsafe(ret, root);
	return 0;

error:
	fmte("The symbol-macrolet arguemnt ~A "
			"must be a (symbol expansion) form.", cons, NULL);
	return 1;
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

static int parse_symbol_macrolet(addr *ret, addr cons)
{
	addr eval, args, decl, rollback;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("symbol-macrolet form must be (symbol-macrolet args . body).", NULL);
	getcons(cons, &args, &cons);
	/* local scope environment */
	snapshot_envstack(&rollback);
	if (parse_symbol_macrolet_args(&args, args)) return 1;
	if (parse_declare_body(cons, &decl, &cons)) return 1;
	check_symbol_macrolet(args, decl);
	if (parse_allcons(&cons, cons)) return 1;
	rollback_envstack(rollback);

	eval_parse_heap(&eval, EVAL_PARSE_SYMBOL_MACROLET, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;

	return 0;
}

/* quote */
static int parse_quote(addr *ret, addr cons)
{
	addr value;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("quote form must have a one argument.", NULL);
	getcons(cons, &value, &cons);
	if (cons != Nil)
		fmte("quote form must have a one argument.", NULL);

	/* eval */
	eval_single_parse_heap(ret, EVAL_PARSE_QUOTE, value);
	return 0;
}

/* function */
static int parse_lambda(addr *ret, addr form)
{
	addr cons, eval, args, doc, decl;

	GetCdr(form, &cons);
	if (GetType(cons) != LISPTYPE_CONS)
		fmte("function lambda must be (lambda (...) body) form.", NULL);
	GetCons(cons, &args, &cons);

	if (parse_ordinary(&args, args)) return 1;
	if (parse_declare_body_documentation(cons, &doc, &decl, &cons)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LAMBDA, 5);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, doc);
	SetEvalParse(eval, 3, cons);
	SetEvalParse(eval, 4, form);
	*ret = eval;

	return 0;
}

static int parse_function_argument(addr *ret, addr value)
{
	addr check, symbol;

	/* symbol function */
	if (! parse_callname_heap(&value, value)) {
		check_function_variable(value);
		eval_single_parse_heap(ret, EVAL_PARSE_FUNCTION, value);
		return 0;
	}

	/* lambda function */
	if (GetType(value) != LISPTYPE_CONS)
		fmte("function must be a fdefinition form.", NULL);
	GetConst(COMMON_LAMBDA, &symbol);
	GetCar(value, &check);
	if (check == symbol)
		return parse_lambda(ret, value);

	/* others */
	fmte("function must be a fdefinition form.", NULL);

	return 0;
}

static int parse_function(addr *ret, addr cons)
{
	addr value;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("function form must have a one argument.", NULL);
	getcons(cons, &value, &cons);
	if (cons != Nil)
		fmte("function form must have a one argument.", NULL);

	return parse_function_argument(ret, value);
}

/* if */
static int parse_if(addr *ret, addr cons)
{
	addr eval, expr, then, last;

	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &expr, &cons);
	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &then, &cons);
	if (cons == Nil) {
		last = Nil;
	}
	else {
		if (GetType(cons) != LISPTYPE_CONS) goto error;
		GetCons(cons, &last, &cons);
		if (cons != Nil) goto error;
	}
	if (parse_self(expr)) return 1;
	if (parse_self(then)) return 1;
	if (parse_self(last)) return 1;
	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_IF, 3);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, then);
	SetEvalParse(eval, 2, last);
	*ret = eval;
	return 0;

error:
	fmte("if form must be (if expr then &optnioal else).", NULL);
	return 1;
}

/* unwind-protect */
static int parse_unwind_protect(addr *ret, addr cons)
{
	addr eval, form;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("unwind-protect form must be a (unwind-protect form . body).", NULL);
	GetCons(cons, &form, &cons);
	if (parse_self(form)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_UNWIND_PROTECT, 2);
	SetEvalParse(eval, 0, form);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* tagbody */
int tagbody_tag_p(addr pos)
{
	/*
	 * Common Lisp the Language, 2nd Edition
	 * 7.8.5. The ``Program Feature''
	 * a symbol or an integer, in which case it is called a tag, ...
	 */
	return IsSymbol(pos) || integerp(pos);
}

static int findtag(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		GetEvalParse(check, 0, &check);
		if (eql_function(check, key)) return 1;
	}

	return 0;
}

static void make_tag(addr *ret, addr pos)
{
	addr eval;

	eval_parse_heap(&eval, EVAL_PARSE_TAG, 2);
	SetEvalParse(eval, 0, pos);
	fixnum_heap(&pos, 0);
	SetEvalParse(eval, 1, pos);
	*ret = eval;
}

static int check_tagbody(addr cons, addr *rtag, addr *rbody)
{
	addr tag, body, pos;

	for (tag = body = Nil; cons != Nil; ) {
		getcons(cons, &pos, &cons);
		if (GetType(pos) == LISPTYPE_CONS) {
			if (parse_self(pos)) return 1;
			cons_heap(&body, pos, body);
		}
		else if (tagbody_tag_p(pos)) {
			if (findtag(pos, tag))
				fmte("The tag ~S is already exists.", pos, NULL);
			make_tag(&pos, pos);
			cons_heap(&tag, pos, tag);
			cons_heap(&body, pos, body);
		}
		else {
			fmte("The tag ~S must be a symbol or integer.", pos, NULL);
		}
	}
	nreverse_list_unsafe(rtag, tag);
	nreverse_list_unsafe(rbody, body);

	return 0;
}

static int parse_tagbody(addr *ret, addr cons)
{
	addr eval, tag;

	if (check_tagbody(cons, &tag, &cons)) return 1;
	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_TAGBODY, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* go */
static int parse_go(addr *ret, addr cons)
{
	addr tag;

	if (GetType(cons) != LISPTYPE_CONS)
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
static int parse_block(addr *ret, addr cons)
{
	addr eval, name;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("block form must be (block name . body).", NULL);
	GetCons(cons, &name, &cons);
	if (! IsSymbol(name))
		fmte("block name ~S must be a symbol type.", name, NULL);
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_BLOCK, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* return-from */
static int parse_return_from(addr *ret, addr cons)
{
	addr eval, name, value;

	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &name, &cons);
	if (cons == Nil) {
		value = Nil;
	}
	else {
		if (GetType(cons) != LISPTYPE_CONS) goto error;
		GetCons(cons, &value, &cons);
		if (cons != Nil) goto error;
	}
	if (parse_self(value)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_RETURN_FROM, 2);
	SetEvalParse(eval, 0, name);
	SetEvalParse(eval, 1, value);
	*ret = eval;
	return 0;

error:
	fmte("return-from form must be (return-from name [value]).", NULL);
	return 1;
}

/* catch */
static int parse_catch(addr *ret, addr cons)
{
	addr eval, tag;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("catch form must be (catch tag . body).", NULL);
	GetCons(cons, &tag, &cons);
	if (parse_self(tag)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_CATCH, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* throw */
static int parse_throw(addr *ret, addr cons)
{
	addr eval, tag, result;

	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &tag, &cons);
	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &result, &cons);
	if (cons != Nil) goto error;
	if (parse_self(tag)) return 1;
	if (parse_self(result)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THROW, 2);
	SetEvalParse(eval, 0, tag);
	SetEvalParse(eval, 1, result);
	*ret = eval;
	return 0;

error:
	fmte("throw form must be (throw tag result).", NULL);
	return 1;
}

/* flet / labels */
static int parse_flet_one(addr *ret, addr cons)
{
	addr name, call, args, doc, decl;

	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &name, &cons);
	if (parse_callname_heap(&call, name))
		fmte("Invalid of function name ~S.", name, NULL);
	check_function_variable(call);
	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &args, &cons);
	if (parse_ordinary(&args, args)) return 1;
	if (parse_declare_body_documentation(cons, &doc, &decl, &cons)) return 1;
	implicit_block(&cons, call, cons);
	if (parse_allcons(&cons, cons)) return 1;
	list_heap(ret, call, args, decl, doc, cons, NULL);
	return 0;

error:
	fmte("flet/labels argument must be (name (...) . body) form.", NULL);
	return 1;
}

static int parse_flet_args(addr *ret, addr args)
{
	addr root, pos;

	for (root = Nil; args != Nil; ) {
		getcons(args, &pos, &args);
		if (parse_flet_one(&pos, pos)) return 1;
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int parse_flet_labels(addr *ret, enum EVAL_PARSE type, addr cons)
{
	addr eval, args, decl;

	if (GetType(cons) != LISPTYPE_CONS) {
		if (type == EVAL_PARSE_FLET)
			fmte("flet form must be (flet args . body).", NULL);
		else
			fmte("labels form must be (labels args . body).", NULL);
	}
	getcons(cons, &args, &cons);
	if (parse_flet_args(&args, args)) return 1;
	if (parse_declare_body(cons, &decl, &cons)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, type, 3);
	SetEvalParse(eval, 0, args);
	SetEvalParse(eval, 1, decl);
	SetEvalParse(eval, 2, cons);
	*ret = eval;

	return 0;
}

/* the */
static int parse_the(addr *ret, addr cons)
{
	addr eval, type, expr;

	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &type, &cons);
	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &expr, &cons);
	if (cons != Nil) goto error;
	if (parse_parse_type(&type, type)) return 1;
	if (parse_self(expr)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_THE, 2);
	SetEvalParse(eval, 0, type);
	SetEvalParse(eval, 1, expr);
	*ret = eval;
	return 0;

error:
	fmte("the form must be (the type expr).", NULL);
	return 1;
}

/* eval-when */
static int parse_eval_when(addr *ret, addr cons)
{
	addr eval, left, right;
	addr compilep, loadp, evalp;
	addr compile1, compile2, load1, load2, eval1, eval2;

	if (GetType(cons) != LISPTYPE_CONS)
		fmte("eval-when form must be (eval-when (...) . body).", NULL);
	GetCons(cons, &right, &cons);
	if (parse_allcons(&cons, cons)) return 1;

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
		if (left == compile1 || left == compile2) compilep = T;
		if (left == load1 || left == load2) loadp = T;
		if (left == eval1 || left == eval2) evalp = T;
	}

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_EVAL_WHEN, 4);
	SetEvalParse(eval, 0, cons);
	SetEvalParse(eval, 1, compilep);
	SetEvalParse(eval, 2, loadp);
	SetEvalParse(eval, 3, evalp);
	*ret = eval;

	return 0;
}

/* values */
static int parse_values(addr *ret, addr cons)
{
	if (parse_allcons(&cons, cons)) return 1;
	eval_single_parse_heap(ret, EVAL_PARSE_VALUES, cons);
	return 0;
}

/* locally */
static int parse_locally(addr *ret, addr cons)
{
	addr eval, decl;

	if (parse_declare_body(cons, &decl, &cons)) return 1;
	if (parse_allcons(&cons, cons)) return 1;
	eval_parse_heap(&eval, EVAL_PARSE_LOCALLY, 2);
	SetEvalParse(eval, 0, decl);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
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
static int parse_call(addr *ret, addr call, addr cons)
{
	addr eval;

	if (parse_function_argument(&call, call)) return 1;
	if (parse_allcons(&cons, cons)) return 1;
	eval_parse_heap(&eval, EVAL_PARSE_CALL, 2);
	SetEvalParse(eval, 0, call);
	SetEvalParse(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* multiple-value-bind */
static int parse_multiple_value_bind(addr *ret, addr cons)
{
	addr eval, vars, expr, decl, doc, form;

	if (! consp(cons)) goto error;
	getcons(cons, &vars, &cons);
	getcons(cons, &expr, &cons);
	getcons(cons, &decl, &cons);
	getcons(cons, &doc, &cons);
	getcons(cons, &form, &cons);
	if (cons != Nil) goto error;

	if (parse_self(expr)) return 1;
	if (parse_allcons(&form, form)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_BIND, 5);
	SetEvalParse(eval, 0, vars);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, decl);
	SetEvalParse(eval, 3, doc);
	SetEvalParse(eval, 4, form);
	*ret = eval;
	return 0;

error:
	fmte("The form ~S must be (system::multiple-value-bind "
			"(vars expr decl doc form).", cons, NULL);
	return 1;
}

/* multiple-value-call */
static int parse_multiple_value_call(addr *ret, addr cons)
{
	addr eval, expr;

	if (GetType(cons) != LISPTYPE_CONS) goto error;
	GetCons(cons, &expr, &cons);
	if (parse_self(expr)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_CALL, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
	return 0;

error:
	fmte("The form ~S must be (multiple-value-call function . body).", cons, NULL);
	return 1;
}

/* multiple-value-prog1 */
static int parse_multiple_value_prog1(addr *ret, addr cons)
{
	addr eval, expr;

	if (! consp(cons)) goto error;
	GetCons(cons, &expr, &cons);
	if (parse_self(expr)) return 1;
	if (parse_allcons(&cons, cons)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_MULTIPLE_VALUE_PROG1, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, cons);
	*ret = eval;
	return 0;

error:
	fmte("The form ~S must be (multiple-value-prog1 first-form . body).", cons, NULL);
	return 1;
}

/* nth-value */
static int parse_nth_value(addr *ret, addr list)
{
	addr eval, next, nth, expr;

	if (! consp(list)) goto error;
	GetCons(list, &nth, &next);
	if (! consp(next)) goto error;
	GetCons(next, &expr, &next);
	if (next != Nil) goto error;
	if (parse_self(nth)) return 1;
	if (parse_self(expr)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_NTH_VALUE, 2);
	SetEvalParse(eval, 0, nth);
	SetEvalParse(eval, 1, expr);
	*ret = eval;
	return 0;

error:
	fmte("The form ~S must be (nth-value nth expr).", list, NULL);
	return 1;
}

/* progv */
static int parse_progv(addr *ret, addr form)
{
	addr eval, symbols, values, body;

	if (! consp(form)) goto error;
	GetCons(form, &symbols, &form);
	if (! consp(form)) goto error;
	GetCons(form, &values, &body);
	if (parse_self(symbols)) return 1;
	if (parse_self(values)) return 1;
	if (parse_allcons(&body, body)) return 1;

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_PROGV, 3);
	SetEvalParse(eval, 0, symbols);
	SetEvalParse(eval, 1, values);
	SetEvalParse(eval, 2, body);
	*ret = eval;
	return 0;

error:
	fmte("The form ~S must be (progv symbols values . body).", form, NULL);
	return 1;
}


/*
 *  macro
 */
static int find_envstack(addr symbol, addr root, addr callp, addr *ret)
{
	addr pos;

	while (root != Nil) {
		GetArrayA2(root, 3, &pos);
		if (pos == callp) {
			GetArrayA2(root, 1, &pos); /* call */
			if (pos == symbol) {
				GetArrayA2(root, 2, ret); /* lambda */
				return 1;
			}
		}
		GetArrayA2(root, 0, &root);
	}

	return 0;
}

static int check_macro_function(addr symbol, addr *ret)
{
	Execute ptr;
	addr root, list, call;

	if (! IsSymbol(symbol)) return 0;
	ptr = Execute_Thread;
	environment_symbol(&root);
	getspecialcheck_local(ptr, root, &root);
	GetArrayA2(root, 1, &list); /* local */
	if (find_envstack(symbol, list, T, ret)) return 1;
	GetArrayA2(root, 0, &list); /* global */
	if (find_envstack(symbol, list, T, ret)) return 1;
	getmacro_symbol(symbol, &call);
	if (call != Unbound) {
		*ret = call;
		return 1;
	}

	return 0;
}

static int findsymbol_environment(addr symbol, addr env, addr *ret)
{
	addr list;

	if (env != Nil) {
		Check(GetType(env) != LISPTYPE_ENVIRONMENT, "type error");
		if (closep_environment(env))
			fmte("The environment object ~S is already closed.", env, NULL);
		GetArrayA2(env, 1, &list); /* local */
		if (find_envstack(symbol, list, Nil, ret)) return 1;
		GetArrayA2(env, 0, &list); /* global */
		if (find_envstack(symbol, list, Nil, ret)) return 1;
	}
	formsymbol_macro_symbol(symbol, ret);
	if (*ret != Unbound) return 1;

	return 0;
}

int findmacro_environment(addr symbol, addr env, addr *ret)
{
	addr list;

	if (env != Nil) {
		Check(GetType(env) != LISPTYPE_ENVIRONMENT, "type error");
		if (closep_environment(env))
			fmte("The environment object ~S is already closed.", env, NULL);
		GetArrayA2(env, 1, &list); /* local */
		if (find_envstack(symbol, list, T, ret)) return 1;
		GetArrayA2(env, 0, &list); /* global */
		if (find_envstack(symbol, list, T, ret)) return 1;
	}
	getmacro_symbol(symbol, ret);
	if (*ret != Unbound) return 1;

	return 0;
}

static int call_macroexpand_hook(addr *ret, addr call, addr cons, addr env)
{
	Execute ptr;
	addr hook;

	ptr = Execute_Thread;
	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	getspecialcheck_local(ptr, hook, &hook);
	return callclang_funcall(ptr, ret, hook, call, cons, env, NULL);
}

static int macroexpand1_symbol(addr *ret, addr symbol, addr env, int *result)
{
	addr call, pos;

	if (! findsymbol_environment(symbol, env, &pos)) {
		*result = 0;
		return 0;
	}
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &call);
	GetFunctionSymbol(call, &call);
	if (call_macroexpand_hook(ret, call, pos, env)) {
		return 1;
	}
	else {
		*result = 1;
		return 0;
	}
}

static int macroexpand1_function(addr *ret, addr form, addr env, int *result)
{
	addr call;

	GetCar(form, &call);
	if (! findmacro_environment(call, env, &call)) {
		*result = 0;
		return 0;
	}
	if (call_macroexpand_hook(ret, call, form, env)) {
		return 1;
	}
	else {
		*result = 1;
		return 0;
	}
}

int macroexpand1(addr *ret, addr form, addr env, int *result)
{
	if (symbolp(form))
		return macroexpand1_symbol(ret, form, env, result);
	else if (consp(form))
		return macroexpand1_function(ret, form, env, result);
	else {
		*result = 0;
		return 0;
	}
}

int macroexpand(addr *ret, addr form, addr env, int *result)
{
	int check, value;
	addr pos;

	check = 0;
	for (;;) {
		if (macroexpand1(&pos, form, env, &value)) return 1;
		if (value == 0) break;
		check = 1;
		form = pos;
	}
	*ret = check? pos: Nil;
	*result = check;

	return 0;
}

static int parse_macro(addr *ret, addr call, addr cons)
{
	int check;
	addr env;

	environment_heap(&env);
	check = call_macroexpand_hook(&cons, call, cons, env);
	close_environment(env);
	if (check) return 1;
	return parse_execute(ret, cons);
}

static int parse_backquote(addr *ret, addr pos)
{
	if (! quote_back_p(pos))
		fmte("Invalid quote type.", NULL);
	getvalue_quote(pos, &pos);
	return parse_execute(ret, pos);
}

/* parse_cons */
static int check_constant(addr call, enum CONSTANT_INDEX index)
{
	addr check;
	GetConstant(index, &check);
	return check == call;
}

static int parse_cons(addr *ret, addr cons)
{
	addr call, args;

	GetCons(cons, &call, &args);
	if (check_constant(call, CONSTANT_COMMON_PROGN)) {
		return parse_progn(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_LET)) {
		return parse_let(ret, EVAL_PARSE_LET, args);
	}
	if (check_constant(call, CONSTANT_COMMON_LETA)) {
		return parse_let(ret, EVAL_PARSE_LETA, args);
	}
	if (check_constant(call, CONSTANT_COMMON_SETQ)) {
		return parse_setq(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_QUOTE)) {
		return parse_quote(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_FUNCTION)) {
		return parse_function(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_IF)) {
		return parse_if(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_UNWIND_PROTECT)) {
		return parse_unwind_protect(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_TAGBODY)) {
		return parse_tagbody(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_GO)) {
		return parse_go(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_BLOCK)) {
		return parse_block(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_RETURN_FROM)) {
		return parse_return_from(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_CATCH)) {
		return parse_catch(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_THROW)) {
		return parse_throw(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_FLET)) {
		return parse_flet_labels(ret, EVAL_PARSE_FLET, args);
	}
	if (check_constant(call, CONSTANT_COMMON_LABELS)) {
		return parse_flet_labels(ret, EVAL_PARSE_LABELS, args);
	}
	if (check_constant(call, CONSTANT_COMMON_THE)) {
		return parse_the(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_EVAL_WHEN)) {
		return parse_eval_when(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_VALUES)) {
		return parse_values(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_LOCALLY)) {
		return parse_locally(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_DECLAIM)) {
		return parse_declaim(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_DEFUN)) {
		return parse_defun(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_DEFMACRO)) {
		return parse_defmacro(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_DEFTYPE)) {
		return parse_deftype(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_MACRO_LAMBDA)) {
		return parse_macro_lambda(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_DESTRUCTURING_BIND)) {
		return parse_destructuring_bind(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_MACROLET)) {
		return parse_macrolet(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_DEFINE_SYMBOL_MACRO)) {
		return parse_define_symbol_macro(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_SYMBOL_MACROLET)) {
		return parse_symbol_macrolet(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_MULTIPLE_VALUE_BIND)) {
		return parse_multiple_value_bind(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_MULTIPLE_VALUE_CALL)) {
		return parse_multiple_value_call(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_MULTIPLE_VALUE_PROG1)) {
		return parse_multiple_value_prog1(ret, args);
	}
	if (check_constant(call, CONSTANT_SYSTEM_NTH_VALUE)) {
		return parse_nth_value(ret, args);
	}
	if (check_constant(call, CONSTANT_COMMON_PROGV)) {
		return parse_progv(ret, args);
	}
	if (check_macro_function(call, &call)) {
		return parse_macro(ret, call, cons);
	}

	return parse_call(ret, call, args);
}

static void parse_array(addr *ret, addr pos)
{
	if (strarrayp(pos))
		eval_single_parse_heap(ret, EVAL_PARSE_STRING, pos);
	else
		eval_single_parse_heap(ret, EVAL_PARSE_ARRAY, pos);
}

static int parse_execute(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			return parse_cons(ret, pos);

		case LISPTYPE_NIL:
			eval_single_parse_heap(ret, EVAL_PARSE_NIL, Nil);
			break;

		case LISPTYPE_T:
			eval_single_parse_heap(ret, EVAL_PARSE_T, T);
			break;

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

		case LISPTYPE_PATHNAME:
			eval_single_parse_heap(ret, EVAL_PARSE_PATHNAME, pos);
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
			return parse_backquote(ret, pos);

		default:
			fmte("parse-error: ~S.", pos, NULL);
			break;
	}

	return 0;
}

int eval_parse(addr *ret, addr pos)
{
	int check;
	addr control;
	Execute ptr;

	/* push */
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	/* code */
	init_environment(ptr);
	check = parse_execute(ret, pos);
	return free_check_control(ptr, control, check);
}


/*
 *  copy eval-parse
 */
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
		copy_environment(local, &env, env);
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

