#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "extern_object.h"
#include "extern_develop.h"
#include "extern_error.h"
#include "extern_execute.h"
#include "extern_function.h"
#include "extern_sequence.h"
#include "extern_type.h"
#include "eval_execute.h"
#include "eval_main.h"
#include "execute.h"
#include "execute_object.h"
#include "execute_values.h"
#include "hold.h"
#include "local.h"
#include "prompt.h"
#include "typedef.h"

/*
 *  eval
 */
int lisp0_eval_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	Return(eval_execute_partial_(Execute_Thread, pos));
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
	Return(apply1_control_(Execute_Thread, &call, call, args));
	if (ret)
		*ret = call;

	return 0;
}

int lisp_call_(addr x, addr call, addr args)
{
	Return(lisp0_get_function_(&call, call));
	hold_value(call, &call);
	hold_value(args, &args);
	Return(apply1_control_(Execute_Thread, &call, call, args));
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
	return eval_execute_partial_(Execute_Thread, eval);
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
	return apply_control_(Execute_Thread, call, args);
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


/*
 *  values
 */
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

void lisp0_values_control(addr *ret)
{
	getvalues_list_control_heap(Execute_Thread, ret);
}

void lisp0_nth_value_control(addr *ret, size_t index)
{
	addr pos;
	getvalues_control(Execute_Thread, index, &pos);
	*ret = (pos == Unbound)? Nil: pos;
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

void lisp_values_control(addr x)
{
	addr pos;
	lisp0_values_control(&pos);
	hold_set(x, pos);
}

void lisp_nth_value_control(addr x, size_t index)
{
	addr pos;
	lisp0_nth_value_control(&pos, index);
	hold_set(x, pos);
}

void lisp_set_result_control(addr value)
{
	hold_value(value, &value);
	setresult_control(Execute_Thread, value);
}

void lisp_set_values_control(addr first, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	/* list */
	va_start(va, first);
	lisp0_list_va_alloc(local, &args, va);
	hold_value(first, &first);
	cons_local(local, &args, first, args);

	/* setvalues */
	setvalues_list_control(ptr, args);
	rollback_local(local, stack);
}

void lisp_set_values_nil_control(void)
{
	setvalues_nil_control(Execute_Thread);
}

void lisp_set_values_list_control(addr list)
{
	hold_value(list, &list);
	setvalues_list_control(Execute_Thread, list);
}


/*
 *  escape
 */
int lisp_equal_control(addr control)
{
	Execute ptr;

	ptr = Execute_Thread;
	return (ptr->throw_value != throw_normal)
		&& (ptr->throw_control == control);
}

int lisp_break_control(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	return (ptr->throw_value != throw_normal)
		&& (ptr->throw_control == ptr->control);
}

int lisp_escape_control(void)
{
	Execute ptr;
	ptr = Execute_Thread;
	return ptr->throw_value != throw_normal;
}

void lisp_reset_control(void)
{
	Execute ptr;
	ptr = Execute_Thread;
	normal_throw_control(ptr);
}

enum lisp_escape lisp_escape_type_control(void)
{
	Execute ptr;
	ptr = Execute_Thread;
	switch (ptr->throw_value) {
		case throw_normal:
			return lisp_escape_normal;

		case throw_tagbody:
			return lisp_escape_tagbody;

		case throw_block:
			return lisp_escape_block;

		case throw_catch:
			return lisp_escape_catch;

		case throw_handler_case:
			return lisp_escape_handler_case;

		case throw_restart_case:
			return lisp_escape_restart_case;

		default:
			lisp_abortf("Invalid escape type.", NULL);
			return lisp_escape_normal;
	}
}

void lisp_save_control(addr *ret)
{
	save_execute_control(Execute_Thread, ret);
}

void lisp_rollback_control(addr value)
{
	restore_execute_control(Execute_Thread, value);
}


/*
 *  system
 */
int lisp_eval_loop_(void)
{
	return eval_main_loop_(Execute_Thread);
}

