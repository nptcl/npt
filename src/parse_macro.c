#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "heap.h"
#include "parse_macro.h"
#include "symbol.h"

/*
 *  environment
 */
_g void environment_symbol(addr *ret)
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

_g void init_parse_environment(Execute ptr)
{
	addr symbol, pos;

	environment_symbol(&symbol);
	envroot_heap(&pos);
	pushspecial_control(ptr, symbol, pos);
}

_g int snapshot_envstack_(Execute ptr, addr *ret)
{
	addr root;

	environment_symbol(&root);
	Return(getspecialcheck_local_(ptr, root, &root));
	GetArrayA2(root, 1, ret); /* local */
	
	return 0;
}

static int push_envstack_(Execute ptr, int index, addr name, addr lambda, addr callp)
{
	addr root, pos, next;

	environment_symbol(&root);
	Return(getspecialcheck_local_(ptr, root, &root));
	GetArrayA2(root, index, &next);
	envstack_heap(&pos, next, name, lambda, callp);
	SetArrayA2(root, index, pos);

	return 0;
}

_g int rollback_envstack_(Execute ptr, addr pos)
{
	addr root, local, next;

	environment_symbol(&root);
	Return(getspecialcheck_local_(ptr, root, &root));
	for (;;) {
		GetArrayA2(root, 1, &local); /* local */
		if (local == pos)
			break;
		if (local == Nil)
			return fmte_("environment stack error.", NULL);
		GetArrayA2(local, 0, &next); /* next */
		SetArrayA2(local, 0, Nil); /* next */
		SetArrayA2(root, 1, next); /* local */
	}

	return 0;
}

_g int defmacro_envstack_(Execute ptr, addr name, addr lambda)
{
	return push_envstack_(ptr, 0, name, lambda, T); /* global, macrolet */
}

_g int macrolet_envstack_(Execute ptr, addr name, addr lambda)
{
	return push_envstack_(ptr, 1, name, lambda, T); /* local, macrolet */
}

_g int define_symbol_macro_envstack_(Execute ptr, addr name, addr form)
{
	return push_envstack_(ptr, 0, name, form, Nil); /* global, define-symbol-macro */
}

_g int symbol_macrolet_envstack_(Execute ptr, addr name, addr form)
{
	return push_envstack_(ptr, 1, name, form, Nil); /* local, symbol-macrolet */
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

_g int symbol_macrolet_envstack_p_(Execute ptr, addr name, addr *value, int *ret)
{
	addr root;

	environment_symbol(&root);
	Return(getspecialcheck_local_(ptr, root, &root));
	if (symbol_macrolet_envroot_p(name, root, 0, value))
		return Result(ret, 1);
	if (symbol_macrolet_envroot_p(name, root, 1, value))
		return Result(ret, 1);

	return Result(ret, 0);
}

_g int environment_heap_(Execute ptr, addr *ret)
{
	addr pos, env, local;

	/* envstack */
	environment_symbol(&pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	GetArrayA2(pos, 0, &env);
	GetArrayA2(pos, 1, &local);

	/* environment */
	heap_array2(&pos, LISPTYPE_ENVIRONMENT, 2);
	/*copy_list_heap_unsafe(&env, env);*/
	/*copy_list_heap_unsafe(&local, local);*/
	SetArrayA2(pos, 0, env);
	SetArrayA2(pos, 1, local);
	SetUser(pos, 1); /* dynamic-extent check */

	return Result(ret, pos);
}

_g void copy_environment(addr *ret, addr pos)
{
	*ret = pos; /* do nothing */
}

_g void close_environment(addr pos)
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
 *  macro
 */
static int findstack_environment(addr symbol, addr root, addr callp, addr *ret)
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

_g int parse_cons_check_macro(Execute ptr, addr symbol, addr *ret)
{
	addr root, list, call;

	if (! symbolp(symbol))
		return 0;
	environment_symbol(&root);
	getspecial_local(ptr, root, &root);
	Check(root == Unbound, "unbound error");
	GetArrayA2(root, 1, &list); /* local */
	if (findstack_environment(symbol, list, T, ret))
		return 1;
	GetArrayA2(root, 0, &list); /* global */
	if (findstack_environment(symbol, list, T, ret))
		return 1;
	getmacro_symbol(symbol, &call);
	if (call != Unbound) {
		*ret = call;
		return 1;
	}

	return 0;
}

static int macroexpand1_symbol_find_(addr symbol, addr env, addr *ret)
{
	addr list;

	if (env != Nil) {
		Check(GetType(env) != LISPTYPE_ENVIRONMENT, "type error");
		if (closep_environment(env))
			return fmte_("The environment object ~S is already closed.", env, NULL);
		GetArrayA2(env, 1, &list); /* local */
		if (findstack_environment(symbol, list, Nil, ret))
			return 0;
		GetArrayA2(env, 0, &list); /* global */
		if (findstack_environment(symbol, list, Nil, ret))
			return 0;
	}
	formsymbol_macro_symbol(symbol, ret);
	return 0;
}

_g int find_environment_(addr symbol, addr env, addr *ret)
{
	addr list;

	if (env != Nil) {
		Check(GetType(env) != LISPTYPE_ENVIRONMENT, "type error");
		if (closep_environment(env))
			return fmte_("The environment object ~S is already closed.", env, NULL);
		GetArrayA2(env, 1, &list); /* local */
		if (findstack_environment(symbol, list, T, ret))
			return 0;
		GetArrayA2(env, 0, &list); /* global */
		if (findstack_environment(symbol, list, T, ret))
			return 0;
	}
	getmacro_symbol(symbol, ret);
	return 0;
}

_g int call_macroexpand_hook(Execute ptr, addr *ret, addr call, addr cons, addr env)
{
	addr hook;

	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	Return(getspecialcheck_local_(ptr, hook, &hook));
	return callclang_funcall(ptr, ret, hook, call, cons, env, NULL);
}

static int macroexpand1_symbol(Execute ptr,
		addr *value, addr symbol, addr env, int *ret)
{
	addr call, pos;

	Return(macroexpand1_symbol_find_(symbol, env, &pos));
	if (pos == Unbound)
		return Result(ret, 0);
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &call);
	GetFunctionSymbol(call, &call);
	Return(call_macroexpand_hook(ptr, value, call, pos, env));
	return Result(ret, 1);
}

static int macroexpand1_function(Execute ptr,
		addr *value, addr form, addr env, int *ret)
{
	addr call;

	GetCar(form, &call);
	Return(find_environment_(call, env, &call));
	if (call == Unbound)
		return Result(ret, 0);
	Return(call_macroexpand_hook(ptr, value, call, form, env));
	return Result(ret, 1);
}

_g int macroexpand1(Execute ptr, addr *ret, addr form, addr env, int *result)
{
	if (symbolp(form))
		return macroexpand1_symbol(ptr, ret, form, env, result);
	if (consp(form))
		return macroexpand1_function(ptr, ret, form, env, result);
	*result = 0;
	return 0;
}

_g int macroexpand(Execute ptr, addr *ret, addr form, addr env, int *result)
{
	int check, value;
	addr pos;

	check = 0;
	for (;;) {
		Return(macroexpand1(ptr, &pos, form, env, &value));
		if (value == 0)
			break;
		check = 1;
		form = pos;
	}
	*ret = check? pos: Nil;
	*result = check;

	return 0;
}

