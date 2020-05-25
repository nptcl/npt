#include "callname.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "copy.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "function.h"
#include "hold.h"
#include "typedef.h"
#include "symbol.h"

/*
 *  apply_declare
 */
static int specialp_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* special declaration */
	GetConst(SYSTEM_TYPE_SCOPE, &key);
	if (getplist(table, key, &value) == 0 && find_list_eq_unsafe(symbol, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	if (getvalue_scope_evalstack(stack, symbol, &value)) {
		*ret = getspecialp_tablevalue(value);
		return 1;
	}

	return 0;
}

_g int specialp_tablevalue(Execute ptr, addr stack, addr symbol)
{
	int result;
	addr global_stack;

	/* symbol */
	if (specialp_symbol(symbol)) {
		return 1;
	}

	/* global stack */
	getglobal_eval(ptr, &global_stack);
	if (specialp_stack_tablevalue(global_stack, symbol, &result)) {
		if (result)
			return result;
		/* If symbol is lexical scope, find current stack. */
	}

	/* local stack */
	while (stack != Nil) {
		if (specialp_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* lexical */
	return 0;
}

_g int find_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr value;

	if (ret == NULL)
		ret = &value;
	return getvalue_scope_evalstack(stack, symbol, ret);
}

_g int find_tablefunction(addr stack, addr call, addr *ret)
{
	addr value;

	if (ret == NULL)
		ret = &value;
	return getfunction_scope_evalstack(stack, call, ret);
}

static void check_value_scope(Execute ptr, addr stack, addr symbol, addr *ret)
{
	int specialp;
	addr pos;

	specialp = specialp_tablevalue(ptr, stack, symbol);
	make_tablevalue(&pos, symbol);
	setspecialp_tablevalue(pos, specialp);
	*ret = pos;
}

static void check_value_declare(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablevalue(stack, key, NULL)) {
			check_value_scope(ptr, stack, key, &key);
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}
}

static int globalp_stack_tablefunction(addr stack, addr call)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* free declaration */
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		return 1;
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		return 1;
	}

	return 0;
}

_g int globalp_tablefunction(Execute ptr, addr stack, addr call)
{
	addr value, global_stack;

	/* local scope */
	while (stack != Nil) {
		if (globalp_stack_tablefunction(stack, call)) {
			return globalp_stack_eval(stack);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global scope */
	getglobal_eval(ptr, &global_stack);
	if (globalp_stack_tablefunction(global_stack, call)) {
		return 1;
	}

	/* symbol */
	getglobal_callname(call, &value);
	if (value != Unbound) {
		return 1;
	}

	/* global */
	return 1;
}

static void check_function_scope(Execute ptr, addr stack, addr call, addr *ret)
{
	int globalp;
	addr pos;

	copylocal_object(NULL, &call, call);
	globalp = globalp_tablefunction(ptr, stack, call);
	make_tablefunction(&pos, call);
	setglobalp_tablefunction(pos, globalp);
	*ret = pos;
}

static void check_function_declare(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablefunction(stack, key, NULL)) {
			check_function_scope(ptr, stack, key, &key);
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}
}

static void check_declare_stack(Execute ptr, addr stack, addr decl, addr *ret)
{
	addr root, cons;

	/* check */
	root = Nil;
	getall_type_value_declare(decl, &cons);
	check_value_declare(ptr, stack, cons, &root);
	getall_type_function_declare(decl, &cons);
	check_function_declare(ptr, stack, cons, &root);
	/* result */
	nreverse_list_unsafe(ret, root);
}

_g void apply_declare(Execute ptr, addr stack, addr decl, addr *ret)
{
	if (decl == Nil) {
		*ret = Nil;
		return;
	}
	check_declare_stack(ptr, stack, decl, ret);
	apply_declare_stack(ptr->local, stack, decl);
	gchold_push_local(ptr->local, *ret);
}

