#include "condition.h"
#include "control_execute.h"
#include "extern_develop.h"
#include "extern_object.h"
#include "extern_sequence.h"
#include "function.h"
#include "hold.h"
#include "object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  function
 */
int lisp0_function_(addr *ret, addr value)
{
	hold_value(value, &value);
	if (functionp(value))
		return Result(ret, value);
	if (symbolp(value))
		return getfunction_global_(value, ret);

	/* error */
	*ret = Nil;
	return TypeError_(value, SYMBOL);
}

int lisp0_function8_(addr *ret, const void *str)
{
	addr value;
	Return(lisp0_intern8_(&value, NULL, str));
	return lisp0_function_(ret, value);
}

int lisp0_function16_(addr *ret, const void *str)
{
	addr value;
	Return(lisp0_intern16_(&value, NULL, str));
	return lisp0_function_(ret, value);
}

int lisp0_function32_(addr *ret, const void *str)
{
	addr value;
	Return(lisp0_intern32_(&value, NULL, str));
	return lisp0_function_(ret, value);
}

int lisp_function_(addr x, addr value)
{
	Return(lisp0_function_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_function8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_function8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_function16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_function16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_function32_(const addr x, void *str)
{
	addr pos;

	Return(lisp0_function32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  funcall
 */
int lisp0_funcall_(addr *ret, addr call, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_funcall8_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_funcall16_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_funcall32_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp_funcall_(addr x, addr call, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_funcall8_(addr x, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_funcall16_(addr x, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_funcall32_(addr x, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}


/*
 *  apply
 */
int lisp0_apply_(addr *ret, addr call, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_apply8_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_apply16_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_apply32_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp_apply_(addr x, addr call, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_apply8_(addr x, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_apply16_(addr x, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_apply32_(addr x, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

