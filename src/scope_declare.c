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
#include "scope_declare.h"
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

int specialp_tablevalue_(Execute ptr, addr stack, addr symbol, int *ret)
{
	int result;
	addr global_stack;

	/* symbol */
	if (specialp_symbol(symbol)) {
		return Result(ret, 1);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &global_stack));
	if (specialp_stack_tablevalue(global_stack, symbol, &result)) {
		if (result)
			return Result(ret, result);
		/* If symbol is lexical scope, find current stack. */
	}

	/* local stack */
	while (stack != Nil) {
		if (specialp_stack_tablevalue(stack, symbol, &result)) {
			return Result(ret, result);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* lexical */
	return Result(ret, 0);
}

int find_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr value;

	if (ret == NULL)
		ret = &value;
	return getvalue_scope_evalstack(stack, symbol, ret);
}

int find_tablefunction(addr stack, addr call, addr *ret)
{
	addr value;

	if (ret == NULL)
		ret = &value;
	return getfunction_scope_evalstack(stack, call, ret);
}

static int check_value_scope_(Execute ptr, addr stack, addr symbol, addr *ret)
{
	int specialp;
	addr pos;

	Return(specialp_tablevalue_(ptr, stack, symbol, &specialp));
	make_tablevalue(&pos, symbol);
	setspecialp_tablevalue(pos, specialp);

	return Result(ret, pos);
}

static int check_value_declare_(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablevalue(stack, key, NULL)) {
			Return(check_value_scope_(ptr, stack, key, &key));
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}

	return 0;
}

static int globalp_stack_tablefunction(addr stack, addr call)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* free declaration */
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		return 1;
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		return 1;
	}

	return 0;
}

int globalp_tablefunction_(Execute ptr, addr stack, addr call, int *ret)
{
	addr value, global_stack;

	/* local scope */
	while (stack != Nil) {
		if (globalp_stack_tablefunction(stack, call)) {
			return Result(ret, globalp_stack_eval(stack));
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global scope */
	Return(getglobal_eval_(ptr, &global_stack));
	if (globalp_stack_tablefunction(global_stack, call)) {
		return Result(ret, 1);
	}

	/* symbol */
	getglobal_callname(call, &value);
	if (value != Unbound) {
		return Result(ret, 1);
	}

	/* global */
	return Result(ret, 1);
}

static int check_function_scope_(Execute ptr, addr stack, addr call, addr *ret)
{
	int globalp;
	addr pos;

	copylocal_object(NULL, &call, call);
	Return(globalp_tablefunction_(ptr, stack, call, &globalp));
	make_tablefunction(&pos, call);
	setglobalp_tablefunction(pos, globalp);

	return Result(ret, pos);
}

static int check_function_declare_(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablefunction(stack, key, NULL)) {
			Return(check_function_scope_(ptr, stack, key, &key));
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}

	return 0;
}

static int check_declare_stack_(Execute ptr, addr stack, addr decl, addr *ret)
{
	addr root, cons;

	/* check */
	root = Nil;
	getall_type_value_declare(decl, &cons);
	Return(check_value_declare_(ptr, stack, cons, &root));
	getall_type_function_declare(decl, &cons);
	Return(check_function_declare_(ptr, stack, cons, &root));
	/* result */
	nreverse(ret, root);

	return 0;
}

int apply_declare_(Execute ptr, addr stack, addr decl, addr *ret)
{
	if (decl == Nil)
		return Result(ret, Nil);
	Return(check_declare_stack_(ptr, stack, decl, ret));
	apply_declare_stack(ptr->local, stack, decl);
	gchold_push_local(ptr->local, *ret);

	return 0;
}

