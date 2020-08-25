#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "eval_main.h"
#include "execute_object.h"
#include "execute_values.h"
#include "extern_develop.h"
#include "extern_error.h"
#include "extern_function.h"
#include "extern_object.h"
#include "extern_sequence.h"
#include "extern_type.h"
#include "function.h"
#include "hold.h"
#include "object.h"
#include "symbol.h"
#include "typedef.h"
#include "unicode.h"

/*
 *  function
 */
void lisp0_get_function(addr *ret, addr value)
{
	hold_value(value, &value);
	if (! symbolp(value)) {
		*ret = Nil;
		Lisp_abort_type(value, SYMBOL);
		return;
	}
	GetFunctionSymbol(value, &value);
	*ret = (value == Unbound)? NULL: value;
}

void lisp0_get_setf(addr *ret, addr value)
{
	hold_value(value, &value);
	if (! symbolp(value)) {
		*ret = Nil;
		Lisp_abort_type(value, SYMBOL);
		return;
	}
	getsetf_symbol(value, &value);
	*ret = (value == Unbound)? NULL: value;
}

void lisp_get_function(addr x, addr value)
{
	lisp0_get_function(&value, value);
	hold_set(x, value);
}

void lisp_get_setf(addr x, addr value)
{
	lisp0_get_setf(&value, value);
	hold_set(x, value);
}

int lisp0_get_function_(addr *ret, addr value)
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

int lisp0_get_function8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp0_get_function16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp0_get_function32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp_get_function_(addr x, addr value)
{
	Return(lisp0_get_function_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_get_function8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_function16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_function32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function32_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp0_get_setf_(addr *ret, addr value)
{
	hold_value(value, &value);
	if (functionp(value))
		return Result(ret, value);
	if (symbolp(value))
		return getsetf_global_(value, ret);

	/* error */
	*ret = Nil;
	return TypeError_(value, SYMBOL);
}

int lisp0_get_setf8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp0_get_setf16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp0_get_setf32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp_get_setf_(addr x, addr value)
{
	Return(lisp0_get_setf_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_get_setf8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_setf16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_setf32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  eval
 */
int lisp0_eval_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	Return(eval_execute_partial(Execute_Thread, pos));
	if (ret)
		getresult_control(Execute_Thread, ret);

	return 0;
}

int lisp0_eval8_(addr *ret, const void *str)
{
	addr pos;

	Return(lisp0_reader8_(&pos, str));
	if (lisp_null_p(pos)) {
		Return(lisp0_string8_(&pos, str));
		return fmte_("Invalid eval string ~S.", str, NULL);
	}

	return lisp0_eval_(ret, pos);
}

int lisp0_eval16_(addr *ret, const void *str)
{
	addr pos;

	Return(lisp0_reader16_(&pos, str));
	if (lisp_null_p(pos)) {
		Return(lisp0_string16_(&pos, str));
		return fmte_("Invalid eval string ~S.", str, NULL);
	}

	return lisp0_eval_(ret, pos);
}

int lisp0_eval32_(addr *ret, const void *str)
{
	addr pos;

	Return(lisp0_reader32_(&pos, str));
	if (lisp_null_p(pos)) {
		Return(lisp0_string32_(&pos, str));
		return fmte_("Invalid eval string ~S.", str, NULL);
	}

	return lisp0_eval_(ret, pos);
}

int lisp_eval_(addr x, addr value)
{
	addr pos;

	Return(lisp0_eval_(&pos, value));
	hold_set_null(x, pos);
	return 0;
}

int lisp_eval8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_eval8_(&pos, str));
	hold_set_null(x, pos);
	return 0;
}

int lisp_eval16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_eval16_(&pos, str));
	hold_set_null(x, pos);
	return 0;
}

int lisp_eval32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_eval32_(&pos, str));
	hold_set_null(x, pos);
	return 0;
}


/*
 *  call
 */
int lisp0_call_(addr *ret, addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	Return(callclang_apply(Execute_Thread, &call, call, args));
	if (ret)
		*ret = call;

	return 0;
}

int lisp_call_(addr x, addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	Return(callclang_apply(Execute_Thread, &call, call, args));
	hold_set_null(x, call);

	return 0;
}


/*
 *  funcall
 */
int lisp0_funcall_(addr *ret, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_funcall8_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_funcall16_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_funcall32_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall_(addr x, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall8_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall16_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall32_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}


/*
 *  apply
 */
int lisp0_apply_(addr *ret, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function_(&call, call));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_apply8_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_apply16_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp0_apply32_(addr *ret, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp0_call_(ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply_(addr x, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function_(&call, call));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply8_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function8_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply16_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function16_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply32_(addr x, const void *str, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, str);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_get_function32_(&call, str));
	Return(lisp_call_(x, call, args));
	rollback_local(local, stack);

	return 0;
}


/*
 *  lowlevel
 */
int lisp_eval_control_(addr eval)
{
	hold_value(eval, &eval);
	return eval_execute_partial(Execute_Thread, eval);
}

int lisp_eval_string_control_(addr eval)
{
	addr pos;

	hold_value(eval, &eval);
	Return(lisp0_reader_(&pos, eval));
	if (pos == NULL)
		return fmte_("Invalid eval string ~S.", eval, NULL);

	return lisp_eval_control_(pos);
}

int lisp_call_control_(addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	return apply_control(Execute_Thread, call, args);
}

int lisp_funcall_control_(addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);
	Return(lisp_call_control_(call, args));

	return 0;
}

int lisp_apply_control_(addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = Local_Thread;
	push_local(local, &stack);
	va_start(va, call);
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);
	Return(lisp_call_control_(call, args));
	rollback_local(local, stack);

	return 0;
}

void lisp_clean_control(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	setvalues_nil_control(ptr);
	normal_throw_control(ptr);
}

int lisp_eval_loop_(void)
{
	return eval_main_loop_(Execute_Thread);
}


/*
 *  values
 */
void lisp0_nth_values_control(addr *ret, size_t index)
{
	addr pos;
	getvalues_control(Execute_Thread, index, &pos);
	*ret = (pos == Unbound)? Nil: pos;
}

void lisp0_values_control(addr *ret)
{
	getvalues_list_control_heap(Execute_Thread, ret);
}

void lisp0_result_control(addr *ret)
{
	getresult_control(Execute_Thread, ret);
}

void lisp0_result2_control(addr *ret1, addr *ret2)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	getvalues_control(ptr, 0, &x);
	getvalues_control(ptr, 1, &y);
	*ret1 = (x == Unbound)? Nil: x;
	*ret2 = (y == Unbound)? Nil: y;
}

void lisp_nth_values_control(addr x, size_t index)
{
	addr pos;
	lisp0_nth_values_control(&pos, index);
	hold_set(x, pos);
}

void lisp_values_control(addr x)
{
	addr pos;
	lisp0_values_control(&pos);
	hold_set(x, pos);
}

void lisp_result_control(addr x)
{
	addr pos;
	lisp0_result_control(&pos);
	hold_set(x, pos);
}

void lisp_result2_control(addr x, addr y)
{
	addr pos1, pos2;
	lisp0_result2_control(&pos1, &pos2);
	hold_set(x, pos1);
	hold_set(y, pos2);
}

