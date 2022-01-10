#include "condition.h"
#include "constant.h"
#include "callname.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "copy.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "function.h"
#include "parse_object.h"
#include "scope_declare.h"
#include "scope_function.h"
#include "scope_object.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  function
 */
static int dynamic_stack_tablefunction(addr stack, addr call, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_FUNCTION, &key);
	if (getplist(table, key, &value) == 0
			&& find_list_callname_unsafe(call, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getdynamic_tablefunction(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablefunction(addr stack, addr call)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablefunction(addr stack, addr call, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_FUNCTION, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_IGNORE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignore;
			return 1;
		}
		GetConst(COMMON_IGNORABLE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignorable;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getignore_tablefunction(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablefunction(addr stack, addr call)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int inline_stack_tablefunction(addr stack, addr call, enum InlineType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* inline, notinline declaration */
	GetConst(SYSTEM_INLINE, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_INLINE, &check);
		if (check == value) {
			*ret = InlineType_Inline;
			return 1;
		}
		GetConst(COMMON_NOTINLINE, &check);
		if (check == value) {
			*ret = InlineType_NotInline;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getinline_tablefunction(value);
		return 1;
	}

	return 0;
}
static int inline_tablefunction_(Execute ptr,
		addr stack, addr call, enum InlineType *ret)
{
	enum InlineType result;

	/* local stack */
	while (stack != Nil) {
		if (inline_stack_tablefunction(stack, call, &result)) {
			return Result(ret, result);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	if (inline_stack_tablefunction(stack, call, &result)) {
		return Result(ret, result);
	}

	/* not inline or notinline */
	return Result(ret, InlineType_None);
}

static void gettype_global_callname(LocalRoot local, addr call, addr *ret)
{
	CallNameType check;

	GetCallNameType(call, &check);
	GetCallName(call, &call);
	if (check == CALLNAME_SYMBOL)
		gettype_function_symbol(call, ret);
	else
		gettype_setf_symbol(call, ret);
}

static int type_free_tablefunction(addr stack, addr call, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	return getpplist_callname(stack, key, call, ret) == 0;
}

static int type_boundary_tablefunction(addr stack, addr call, addr *ret)
{
	if (! find_tablefunction(stack, call, &call))
		return 0;
	gettype_tablefunction(call, ret);
	return *ret != Nil;
}

static int type_tablefunction_(Execute ptr, LocalRoot local,
		addr stack, addr call, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablefunction(stack, call, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablefunction(stack, call, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	/* free declaration */
	if (type_free_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_global_callname(local, call, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse(ret, root);
	return 0;
}

static void push_tablefunction_lexical(Execute ptr, addr stack, addr pos)
{
	addr name;

	if (stack == Nil) {
		setglobalp_tablefunction(pos, 1);
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tablefunction(pos, &name);
		setlexical_tablefunction(pos, increment_stack_eval(stack));
		setfunction_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablefunction_lexical(ptr, stack, pos);
	}
}

static void tablefunction_update(Execute ptr, addr stack, addr call)
{
	push_tablefunction_lexical(ptr, stack, call);
}

int make_tablefunction_stack(Execute ptr, addr *ret, addr stack, addr call)
{
	addr pos, aster;

	CheckType(call, LISPTYPE_CALLNAME);
	if (getfunction_scope_evalstack(stack, call, ret))
		return 0;
	copyheap(&call, call);
	make_tablefunction(&pos, call);
	GetTypeTable(&aster, Function);
	settype_tablefunction(pos, aster);
	setfunction_scope_evalstack(stack, pos);
	tablefunction_update(ptr, stack, pos);
	*ret = pos;

	return 1;
}

static int function_type_and_array(addr cons, addr *ret)
{
	addr array, type, pos;
	size_t size;

	/* array size */
	size = 0;
	type = Nil;
	for (array = cons; array != Nil; ) {
		GetCons(array, &pos, &array);
		if (! type_function_aster_p(pos)) {
			type = pos;
			size++;
		}
	}
	if (size == 0) {
		return 1;
	}
	if (size == 1) {
		CheckType(type, LISPTYPE_TYPE);
		copylocal_object(NULL, ret, type);
		return 0;
	}

	/* type-and */
	vector4_heap(&array, size);
	for (size = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		CheckType(pos, LISPTYPE_TYPE);
		if (! type_function_aster_p(pos)) {
			copylocal_object(NULL, &pos, pos);
			SetArrayA4(array, size++, pos);
		}
	}
	type1_heap(LISPDECL_AND, array, ret);
	return 0;
}

int update_tablefunction_(Execute ptr, addr stack, addr pos)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr name, type;

	/* scope */
	getname_tablefunction(pos, &name);
	Return(globalp_tablefunction_(ptr, stack, name, &globalp));
	dynamic = dynamic_tablefunction(stack, name);
	ignore = ignore_tablefunction(stack, name);
	Return(inline_tablefunction_(ptr, stack, name, &Inline));
	Return(type_tablefunction_(ptr, NULL, stack, name, &type));
	if (function_type_and_array(type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);

	return 0;
}

int push_tablefunction_global_(Execute ptr, addr stack, addr call, addr *ret)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr pos, type;

	/* scope */
	Return(globalp_tablefunction_(ptr, stack, call, &globalp));
	dynamic = dynamic_tablefunction(stack, call);
	ignore = ignore_tablefunction(stack, call);
	Return(inline_tablefunction_(ptr, stack, call, &Inline));
	Return(type_tablefunction_(ptr, NULL, stack, call, &type));
	if (function_type_and_array(type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	make_tablefunction_stack(ptr, &pos, stack, call);
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);

	return Result(ret, pos);
}

static int callname_global_tablefunction_(Execute ptr, addr *ret, addr call)
{
	addr stack;

	Return(getglobal_eval_(ptr, &stack));
	if (! find_tablefunction(stack, call, ret)) {
		Return(push_tablefunction_global_(ptr, stack, call, ret));
	}

	return 0;
}

static void push_closure_function(addr stack, addr call, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tablefunction(value);
	/* destination table */
	make_redirect_tablefunction(&pos, value);
	setclosurep_tablefunction(pos, 1);
	setclosure_tablefunction(pos, lexical);
	setlexical_tablefunction(pos, increment_stack_eval(stack));
	setfunction_lexical_evalstack(stack, pos);
	setfunction_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int callname_tablefunction_(Execute ptr,
		addr stack, addr call, addr *value, int *ret)
{
	int check;
	addr next;

	/* global */
	if (stack == Nil) {
		Return(callname_global_tablefunction_(ptr, value, call));
		return Result(ret, 1); /* global-scope */
	}

	/* local */
	if (getfunction_scope_evalstack(stack, call, value)) {
		setreference_tablefunction(*value, 1);
		return Result(ret, getglobalp_tablefunction(*value));
	}

	/* next */
	GetEvalStackNext(stack, &next);
	Return(callname_tablefunction_(ptr, next, call, value, &check));
	if (check) {
		return Result(ret, 1); /* global-scope */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_function(stack, call, *value, value);
	}

	return Result(ret, 0); /* local-scope */
}

static int scope_function_object_(Execute ptr, addr *ret, addr eval)
{
	addr form, value, type;

	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &value);
	gettype_function(value, &type);
	if (type == Nil)
		GetTypeTable(&type, Function);

	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_FUNCTION, type, form));
	SetEvalScopeIndex(eval, 0, value);
	return Result(ret, eval);
}

static int scope_function_callname_(Execute ptr, addr *ret, addr eval)
{
	int ignore;
	addr form, value, type, stack;

	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &value);
	Return(getstack_eval_(ptr, &stack));
	Return(callname_tablefunction_(ptr, stack, value, &value, &ignore));
	gettype_tablefunction(value, &type);

	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_FUNCTION, type, form));
	SetEvalScopeIndex(eval, 0, value);
	return Result(ret, eval);
}

int scope_function_call_(Execute ptr, addr *ret, addr eval)
{
	addr value;

	GetEvalParse(eval, 1, &value);
	if (functionp(value))
		return scope_function_object_(ptr, ret, eval);
	if (callnamep(value))
		return scope_function_callname_(ptr, ret, eval);

	return fmte_("Invalid object type ~S", value, NULL);
}

