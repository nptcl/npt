#include "callname.h"
#include "compile_file.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "equal.h"
#include "eval_value.h"
#include "heap.h"
#include "hold.h"
#include "parse_macro.h"
#include "symbol.h"

/*
 *  environment root
 */
void environment_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_PARSE_ENVIRONMENT, ret);
}

void init_parse_environment(Execute ptr)
{
	addr symbol, pos;

	environment_symbol(&symbol);
	heap_array2(&pos, LISPSYSTEM_ENVROOT, 2); /* global, root */
	pushspecial_control(ptr, symbol, pos);
}

static int getobject_envroot_(Execute ptr, addr *ret)
{
	addr pos;
	environment_symbol(&pos);
	return getspecialcheck_local_(ptr, pos, ret);
}

static void getglobal_envroot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	GetArrayA2(pos, 0, ret);
}
static void setglobal_envroot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	SetArrayA2(pos, 0, value);
}

static void getlocal_envroot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	GetArrayA2(pos, 1, ret);
}
static void setlocal_envroot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVROOT);
	SetArrayA2(pos, 1, value);
}


/*
 *  environment stack
 */
enum EnvStack_Index {
	EnvStack_next,
	EnvStack_call,
	EnvStack_lambda,
	EnvStack_size
};

enum EnvStackType {
	EnvStackType_macro,     /* macrolet */
	EnvStackType_symbol,    /* symbol-macro */
	EnvStackType_function,  /* function */
	EnvStackType_variable,  /* variable */
};

struct envstack_struct {
	enum EnvStackType type;
};

static void setnext_envstack(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	SetArraySS(pos, EnvStack_next, value);
}
static void getnext_envstack(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	GetArraySS(pos, EnvStack_next, ret);
}

static void setcall_envstack(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	SetArraySS(pos, EnvStack_call, value);
}
static void getcall_envstack(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	GetArraySS(pos, EnvStack_call, ret);
}

static void setlambda_envstack(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	SetArraySS(pos, EnvStack_lambda, value);
}
static void getlambda_envstack(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ENVSTACK);
	GetArraySS(pos, EnvStack_lambda, ret);
}

static void settype_envstack(addr pos, enum EnvStackType value)
{
	struct envstack_struct *ptr;

	CheckType(pos, LISPSYSTEM_ENVSTACK);
	ptr = (struct envstack_struct *)PtrBodySS(pos);
	ptr->type = value;
}
static void gettype_envstack(addr pos, enum EnvStackType *ret)
{
	struct envstack_struct *ptr;

	CheckType(pos, LISPSYSTEM_ENVSTACK);
	ptr = (struct envstack_struct *)PtrBodySS(pos);
	*ret = ptr->type;
}

static void envstack_heap(addr *ret,
		addr next, addr call, addr lambda, enum EnvStackType type)
{
	addr pos;

	heap_smallsize(&pos, LISPSYSTEM_ENVSTACK,
			EnvStack_size, sizeoft(struct envstack_struct));
	setnext_envstack(pos, next);
	setcall_envstack(pos, call);
	setlambda_envstack(pos, lambda);
	settype_envstack(pos, type);
	*ret = pos;
}

int snapshot_envstack_(Execute ptr, addr *ret)
{
	addr root;

	Return(getobject_envroot_(ptr, &root));
	getlocal_envroot(root, ret); /* local */

	return 0;
}

static int push_global_envstack_(Execute ptr,
		addr name, addr lambda, enum EnvStackType type)
{
	addr root, pos, next;

	Return(getobject_envroot_(ptr, &root));
	getglobal_envroot(root, &next);
	envstack_heap(&pos, next, name, lambda, type);
	setglobal_envroot(root, pos);

	return 0;
}

static int push_local_envstack_(Execute ptr,
		addr name, addr lambda, enum EnvStackType type)
{
	addr root, pos, next;

	Return(getobject_envroot_(ptr, &root));
	getlocal_envroot(root, &next);
	envstack_heap(&pos, next, name, lambda, type);
	setlocal_envroot(root, pos);

	return 0;
}

int rollback_envstack_(Execute ptr, addr pos)
{
	addr root, local, next;

	Return(getobject_envroot_(ptr, &root));
	for (;;) {
		getlocal_envroot(root, &local); /* local */
		if (local == pos)
			break;
		if (local == Nil)
			return fmte_("environment stack error.", NULL);
		getnext_envstack(local, &next);
		setnext_envstack(local, Nil);
		setlocal_envroot(root, next); /* local */
	}

	return 0;
}

int defvar_envstack_(Execute ptr, addr name)
{
	/* global, defvar */
	return push_global_envstack_(ptr, name, Nil, EnvStackType_variable);
}

int lexical_envstack_(Execute ptr, addr name)
{
	/* local, let */
	return push_local_envstack_(ptr, name, Nil, EnvStackType_variable);
}

int defun_envstack_(Execute ptr, addr name)
{
	/* global, defun */
	CheckType(name, LISPTYPE_CALLNAME);
	return push_global_envstack_(ptr, name, Nil, EnvStackType_function);
}

int function_envstack_(Execute ptr, addr name)
{
	/* local, flet/labels */
	CheckType(name, LISPTYPE_CALLNAME);
	return push_local_envstack_(ptr, name, Nil, EnvStackType_function);
}

int defmacro_envstack_(Execute ptr, addr name, addr lambda)
{
	/* global, macrolet */
	return push_global_envstack_(ptr, name, lambda, EnvStackType_macro);
}

int macrolet_envstack_(Execute ptr, addr name, addr lambda)
{
	/* local, macrolet */
	return push_local_envstack_(ptr, name, lambda, EnvStackType_macro);
}

int define_symbol_macro_envstack_(Execute ptr, addr name, addr form)
{
	/* global, define-symbol-macro */
	return push_global_envstack_(ptr, name, form, EnvStackType_symbol);
}

int symbol_macrolet_envstack_(Execute ptr, addr name, addr form)
{
	/* local, symbol-macrolet */
	return push_local_envstack_(ptr, name, form, EnvStackType_symbol);
}

static int find_macro_environment_p(addr name, addr check)
{
	Check(! symbolp(name), "type error");
	CheckType(check, LISPTYPE_CALLNAME);
	if (! symbolp_callname(check))
		return 0;
	GetCallName(check, &check);

	return name == check;
}

static int find_macro_environment(addr name, addr pos, addr *ret)
{
	enum EnvStackType type;
	addr check;

	while (pos != Nil) {
		gettype_envstack(pos, &type);
		if (type == EnvStackType_function) {
			/* shadow function */
			getcall_envstack(pos, &check);
			if (find_macro_environment_p(name, check)) {
				*ret = Unbound;
				return 1;
			}
		}
		if (type == EnvStackType_macro) {
			/* macro */
			getcall_envstack(pos, &check);
			if (name == check) {
				getlambda_envstack(pos, ret);
				return 1;
			}
		}
		getnext_envstack(pos, &pos);
	}

	return 0;
}

static int find_symbol_environment(addr name, addr pos, addr *ret)
{
	enum EnvStackType type;
	addr check;

	while (pos != Nil) {
		gettype_envstack(pos, &type);
		if (type == EnvStackType_variable) {
			/* shadow symbol-macro */
			getcall_envstack(pos, &check);
			if (name == check) {
				*ret = Unbound;
				return 1;
			}
		}
		if (type == EnvStackType_symbol) {
			/* symbol-macro */
			getcall_envstack(pos, &check);
			if (name == check) {
				getlambda_envstack(pos, ret);
				return 1;
			}
		}
		getnext_envstack(pos, &pos);
	}

	return 0;
}

static int get_symbol_macrolet_envstack_(Execute ptr, addr name, addr *value, int *ret)
{
	addr root, pos;

	Return(getobject_envroot_(ptr, &root));
	/* local */
	getlocal_envroot(root, &pos);
	if (find_symbol_environment(name, pos, value))
		return Result(ret, 1);
	/* global */
	getglobal_envroot(root, &pos);
	if (find_symbol_environment(name, pos, value))
		return Result(ret, 1);

	return Result(ret, 0);
}

int symbol_macrolet_envstack_p_(Execute ptr, addr name, addr *value, int *ret)
{
	int check;
	addr pos;

	/* environment */
	Return(get_symbol_macrolet_envstack_(ptr, name, &pos, &check));
	if (check) {
		if (pos == Unbound)
			return Result(ret, 0);
		*value = pos;
		return Result(ret, 1);
	}

	/* global symbol */
	getsymbol_macro_symbol(name, &pos);
	if (pos == Unbound)
		return Result(ret, 0);
	*value = pos;
	return Result(ret, 1);
}

static void getglobal_environment(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	GetArrayA2(pos, 0, ret);
}
static void setglobal_environment(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	SetArrayA2(pos, 0, value);
}

static void getlocal_environment(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	GetArrayA2(pos, 1, ret);
}
static void setlocal_environment(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	SetArrayA2(pos, 1, value);
}

static void getcheck_environment(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	*ret = GetUser(pos)? 1: 0;
}
static void setcheck_environment(addr pos, int value)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	SetUser(pos, value? 1: 0);
}

int environment_heap_(Execute ptr, addr *ret)
{
	addr pos, global, local;

	/* envstack */
	Return(getobject_envroot_(ptr, &pos));
	getglobal_envroot(pos, &global);
	getlocal_envroot(pos, &local);

	/* environment */
	heap_array2(&pos, LISPTYPE_ENVIRONMENT, 2);
	setglobal_environment(pos, global);
	setlocal_environment(pos, local);
	setcheck_environment(pos, 1); /* dynamic-extent check */

	return Result(ret, pos);
}

void copy_environment(addr *ret, addr pos)
{
	*ret = pos; /* do nothing */
}

void close_environment(addr pos)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	setglobal_environment(pos, Nil);
	setlocal_environment(pos, Nil);
	setcheck_environment(pos, 0);
}

static int closep_environment(addr pos)
{
	int check;

	CheckType(pos, LISPTYPE_ENVIRONMENT);
	getcheck_environment(pos, &check);
	return check == 0;
}

static int close_check_environment_(addr pos)
{
	CheckType(pos, LISPTYPE_ENVIRONMENT);
	if (closep_environment(pos))
		return fmte_("The environment object ~S is already closed.", pos, NULL);

	return 0;
}


/*
 *  macroexpand
 */
static int macroexpand1_symbol_find_(addr symbol, addr env, addr *ret)
{
	addr list;

	if (env != Nil) {
		Return(close_check_environment_(env));
		/* local */
		getlocal_environment(env, &list);
		if (find_symbol_environment(symbol, list, ret))
			return 0;
		/* global */
		getglobal_environment(env, &list);
		if (find_symbol_environment(symbol, list, ret))
			return 0;
	}
	getsymbol_macro_symbol(symbol, ret);

	return 0;
}

int find_environment_(addr symbol, addr env, addr *ret)
{
	addr list;

	if (! symbolp(symbol))
		return Result(ret, Unbound);
	if (env != Nil) {
		Return(close_check_environment_(env));
		/* local */
		getlocal_environment(env, &list);
		if (find_macro_environment(symbol, list, ret))
			return 0;
		/* global */
		getglobal_environment(env, &list);
		if (find_macro_environment(symbol, list, ret))
			return 0;
	}
	getmacro_symbol(symbol, ret);

	return 0;
}

static int call_macroexpand_hook_(Execute ptr,
		addr *ret, addr call, addr cons, addr env)
{
	addr hook;

	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	Return(getspecialcheck_local_(ptr, hook, &hook));
	return callclang_funcall(ptr, ret, hook, call, cons, env, NULL);
}

static int macroexpand1_symbol_(Execute ptr,
		addr *value, addr symbol, addr env, int *ret)
{
	addr call, pos;

	Return(macroexpand1_symbol_find_(symbol, env, &pos));
	if (pos == Unbound)
		return Result(ret, 0);
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &call);
	GetFunctionSymbol(call, &call);
	Return(call_macroexpand_hook_(ptr, value, call, pos, env));
	return Result(ret, 1);
}

static int macroexpand1_function_(Execute ptr,
		addr *value, addr form, addr env, int *ret)
{
	addr call;

	GetCar(form, &call);
	Return(find_environment_(call, env, &call));
	if (call == Unbound)
		return Result(ret, 0);
	Return(call_macroexpand_hook_(ptr, value, call, form, env));
	return Result(ret, 1);
}

int macroexpand1_(Execute ptr, addr *ret, addr form, addr env, int *result)
{
	if (symbolp(form))
		return macroexpand1_symbol_(ptr, ret, form, env, result);
	if (consp(form))
		return macroexpand1_function_(ptr, ret, form, env, result);
	*ret = form;
	*result = 0;
	return 0;
}

int macroexpand_(Execute ptr, addr *ret, addr form, addr env, int *result)
{
	int check, value;
	addr pos, fail;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	check = 0;
	fail = form;
	for (;;) {
		Return(macroexpand1_(ptr, &pos, form, env, &value));
		if (value == 0)
			break;
		check = 1;
		form = pos;
		localhold_set(hold, 0, form);
	}
	localhold_end(hold);
	*ret = check? pos: fail;
	*result = check;

	return 0;
}


/*
 *  macro eval-when
 */
#define ParseMacroCompile(pos, x) { \
	addr __check; \
	GetConst(COMMON_##x, &__check); \
	if (pos == __check) \
	return 1; \
}
static int parse_macro_compile_symbol(addr pos)
{
	ParseMacroCompile(pos, DECLAIM);
	ParseMacroCompile(pos, DEFCLASS);
	ParseMacroCompile(pos, DEFINE_COMPILER_MACRO);
	ParseMacroCompile(pos, DEFINE_CONDITION);
	ParseMacroCompile(pos, DEFINE_MODIFY_MACRO);
	ParseMacroCompile(pos, DEFINE_SETF_EXPANDER);
	ParseMacroCompile(pos, DEFINE_SYMBOL_MACRO);
	ParseMacroCompile(pos, DEFMACRO);
	ParseMacroCompile(pos, DEFPACKAGE);
	ParseMacroCompile(pos, DEFSETF);
	ParseMacroCompile(pos, DEFSTRUCT);
	ParseMacroCompile(pos, DEFTYPE);
	ParseMacroCompile(pos, IN_PACKAGE);

	return 0;
}

static void parse_make_eval_when(addr compile, addr load, addr execute, addr *ret)
{
	addr list, key;

	list = Nil;
	if (compile != Nil) {
		GetConst(KEYWORD_COMPILE_TOPLEVEL, &key);
		cons_heap(&list, key, list);
	}
	if (load != Nil) {
		GetConst(KEYWORD_LOAD_TOPLEVEL, &key);
		cons_heap(&list, key, list);
	}
	if (execute != Nil) {
		GetConst(KEYWORD_EXECUTE, &key);
		cons_heap(&list, key, list);
	}

	*ret = list;
}

static int parse_macro_eval_when_(Execute ptr, addr expr, addr list, addr *ret)
{
	addr compile, load, exec, toplevel, mode, eval;

	/* compile */
	if (! eval_compile_p(ptr))
		goto return_throw;

	/* type */
	if (! consp(expr))
		goto return_throw;
	GetCar(expr, &expr);
	if (! parse_macro_compile_symbol(expr))
		goto return_throw;

	/* toplevel */
	Return(get_toplevel_eval_(ptr, &toplevel));
	if (toplevel == Nil)
		goto return_throw;

	/* :compile-toplevel */
	Return(get_compile_toplevel_eval_(ptr, &compile));
	if (compile != Nil)
		goto return_throw;

	/* compile-time-too */
	Return(get_compile_time_eval_(ptr, &mode));
	if (mode != Nil)
		goto return_throw;

	/* eval-when */
	Return(get_load_toplevel_eval_(ptr, &load));
	Return(get_execute_eval_(ptr, &exec));

	/* `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body) */
	GetConst(COMMON_EVAL_WHEN, &eval);
	parse_make_eval_when(compile, load, exec, &expr);
	list_heap(&list, eval, expr, list, NULL);

return_throw:
	return Result(ret, list);
}


/*
 *  parse-macroexpand
 */
static int parse_macroexpand_local_(Execute ptr, addr *ret, addr form, addr env)
{
	addr call, list;

	/* environment */
	if (env == Nil)
		goto unbound;
	Return(close_check_environment_(env));
	getlocal_environment(env, &list);

	/* local */
	GetCar(form, &call);
	if (! find_macro_environment(call, list, &call))
		goto unbound;
	if (call == Unbound)
		goto unbound;

	/* execute */
	return call_macroexpand_hook_(ptr, ret, call, form, env);

unbound:
	return Result(ret, Unbound);
}

static int parse_macroexpand_compile_(Execute ptr, addr *ret, addr form, addr env)
{
	int check;
	addr call;

	/* *compiler-macro* */
	if (! enable_compiler_macro_p(ptr))
		goto unbound;

	/* define-compiler-macro */
	GetCar(form, &call);
	get_compiler_macro_symbol(call, &call);
	if (call == Nil)
		goto unbound;

	/* execute */
	Return(call_macroexpand_hook_(ptr, &call, call, form, env));

	/* equal check */
	Return(equal_function_(form, call, &check));
	if (check)
		goto unbound;
	return Result(ret, call);

unbound:
	return Result(ret, Unbound);
}

static int parse_macroexpand_global1_(Execute ptr, addr *ret, addr form, addr env)
{
	addr call, list;

	/* environment */
	if (env == Nil)
		goto unbound;
	Return(close_check_environment_(env));
	getglobal_environment(env, &list);

	/* global */
	GetCar(form, &call);
	if (! find_macro_environment(call, list, &call))
		goto unbound;
	if (call == Unbound)
		goto unbound;

	/* execute */
	return call_macroexpand_hook_(ptr, ret, call, form, env);

unbound:
	return Result(ret, Unbound);
}

static int parse_macroexpand_global2_(Execute ptr, addr *ret, addr form, addr env)
{
	addr call;

	GetCar(form, &call);
	getmacro_symbol(call, &call);
	if (call == Unbound)
		return Result(ret, Unbound);

	return call_macroexpand_hook_(ptr, ret, call, form, env);
}

static int parse_macroexpand_function_(Execute ptr, addr *ret, addr form, addr env)
{
	addr pos;

	/* calltype */
	GetCar(form, &pos);
	if (! symbolp(pos))
		return Result(ret, Unbound);

	/* macrolet [env.local] */
	Return(parse_macroexpand_local_(ptr, &pos, form, env));
	if (pos != Unbound)
		return Result(ret, pos);

	/* define-compiler-macro */
	Return(parse_macroexpand_compile_(ptr, &pos, form, env));
	if (pos != Unbound)
		return Result(ret, pos);

	/* defmacro [env.global] */
	Return(parse_macroexpand_global1_(ptr, &pos, form, env));
	if (pos != Unbound)
		return Result(ret, pos);

	/* defmacro [symbol] */
	return parse_macroexpand_global2_(ptr, ret, form, env);
}

static int parse_macroexpand_symbol_(Execute ptr, addr *ret, addr form, addr env)
{
	int check;
	Return(macroexpand1_symbol_(ptr, &form, form, env, &check));
	return Result(ret, check? form: Unbound);
}

static int parse_macroexpand1_call_(Execute ptr, addr *ret, addr form, addr env)
{
	if (symbolp(form))
		return parse_macroexpand_symbol_(ptr, ret, form, env);
	if (consp(form))
		return parse_macroexpand_function_(ptr, ret, form, env);

	return Result(ret, Unbound);
}

static int parse_macroexpand1_(Execute ptr, addr *ret, addr form, addr env)
{
	addr value;

	Return(parse_macroexpand1_call_(ptr, &value, form, env));
	if (value == Unbound)
		return Result(ret, Unbound);

	return parse_macro_eval_when_(ptr, form, value, ret);
}

static int parse_macroexpand_loop_(Execute ptr, addr *ret, addr form, addr env)
{
	int check;
	addr pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (check = 0; ; check = 1) {
		Return(parse_macroexpand1_(ptr, &pos, form, env));
		if (pos == Unbound)
			break;
		form = pos;
		localhold_set(hold, 0, form);
	}
	localhold_end(hold);

	return Result(ret, check? form: Unbound);
}

int parse_macroexpand_(Execute ptr, addr *ret, addr form)
{
	addr env;
	LocalHold hold;

	/* macroexpand */
	Return(environment_heap_(ptr, &env));
	hold = LocalHold_local_push(ptr, env);
	Return(parse_macroexpand_loop_(ptr, ret, form, env));
	close_environment(env);
	localhold_end(hold);

	return 0;
}

