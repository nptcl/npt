#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "execute_object.h"
#include "extern_control.h"
#include "extern_error.h"
#include "extern_object.h"
#include "eval_execute.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "local.h"
#include "pointer.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "unicode.h"

/*
 *  control
 */
void lisp_push_control(addr *ret)
{
	push_control(Execute_Thread, ret);
}

int lisp_pop_control_(addr control)
{
	if (GetType(control) != LISPTYPE_CONTROL)
		return fmte_("Invalid argument ~S.", control, NULL);

	return pop_control_(Execute_Thread, control);
}

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
	Return(lisp_eval_control_(pos));

	return 0;
}

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

static void lisp0_result_control_null(addr *ret)
{
	if (ret)
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

int lisp0_eval8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &pos, (const char *)str));
	Return(lisp0_reader_(&value, pos));
	if (pos == NULL)
		return fmte_("Invalid eval string ~S.", pos, NULL);
	Return(lisp_eval_control_(value));
	lisp0_result_control_null(ret);
	rollback_local(local, stack);

	return 0;
}

int lisp0_eval16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &pos, (const byte16 *)str));
	Return(lisp0_reader_(&value, pos));
	if (pos == NULL)
		return fmte_("Invalid eval string ~S.", pos, NULL);
	Return(lisp_eval_control_(value));
	lisp0_result_control_null(ret);
	rollback_local(local, stack);

	return 0;
}

int lisp0_eval32_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &pos, (const unicode *)str));
	Return(lisp0_reader_(&value, pos));
	if (pos == NULL)
		return fmte_("Invalid eval string ~S.", pos, NULL);
	Return(lisp_eval_control_(value));
	lisp0_result_control_null(ret);
	rollback_local(local, stack);

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

void lisp_eval_clean(void)
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
 *  format
 */
static int lisp_format_call_(addr stream, addr format, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	if (stream == NULL)
		stream = T;
	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, stream, format, args, NULL);
	Return(format_lisp(ptr, stream, format, args, &args));
	localhold_end(hold);

	return lisp_pop_control_(control);
}

int lisp_format8_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format16_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format32_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}


/*
 *  syscall
 */
static void lisp_system_check(int index)
{
	addr pos;

	if (index < 0 && LISP_POINTER_EXTEND <= index) {
		make_index_integer_heap(&pos, index);
		lisp_abort("Invalid index value ~S.", pos, NULL);
	}
}

void lisp_syscall_rest(int index, lisp_calltype_syscall call)
{
	lisp_system_check(index);
	index += (int)p_size;
	SetPointer_extend_dynamic(index, call);
}

void lisp_syscall_dynamic(int index, lisp_calltype_syscall call)
{
	lisp_system_check(index);
	index += (int)p_size;
	SetPointer_extend_rest(index, call);
}

void lisp_syscall_function(int index, addr name, addr *ret)
{
	addr pos;

	lisp_system_check(index);
	index += (int)p_size;
	hold_value(name, &name);
	if (name == NULL)
		name = Nil;

	/* function */
	compiled_system(&pos, name);
	switch (pointer_table[index].type) {
		case CallBind_extend_dynamic:
			setcompiled_extend_dynamic(pos, (pointer)index);
			break;

		case CallBind_extend_rest:
			setcompiled_extend_rest(pos, (pointer)index);
			break;

		default:
			make_index_integer_heap(&pos, index);
			lisp_abort("Invalid callbind type, index = ~A.", pos, NULL);
			*ret = Nil;
			return;
	}

	*ret = pos;
}

void lisp_syscall_setvalue(addr pos, addr value)
{
	hold_value(pos, &pos);
	hold_value(value, &value);
	if (! functionp(pos))
		lisp_abort("Invalid function object ~S.", pos, NULL);
	if (value == NULL)
		value = Nil;
	SetDataFunction(pos, value);
}

void lisp_syscall_getvalue(addr *ret)
{
	addr value;

	getdata_control(Execute_Thread, &value);
	if (value == Unbound)
		value = Nil;
	*ret = value;
}


/*
 *  unwind-protect
 */
void lisp_set_unwind_protect(addr clean)
{
	hold_value(clean, &clean);
	set_protect_control(Execute_Thread, clean);
}


/*
 *  initialize
 */
_g void init_extern_control(void)
{
	/* SetPointerType(empty, extern_unwind_protect); */
}


/*
 *  error
 */
int lisp_error8_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error32_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}


/*
 *  throw
 */
static int lisp_catch_call_(Execute ptr, LocalHold hold,
		addr symbol, addr code, addr *ret)
{
	addr control, value;

	/* begin catch */
	push_control(ptr, &control);
	catch_control(ptr, symbol);
	(void)apply_control(ptr, code, Nil);
	Return(pop_control_(ptr, control));
	/* end catch */
	getresult_control(ptr, &value);
	localhold_set(hold, 0, value);

	return Result(ret, value);
}

int lisp_catch_(addr symbol, addr code, addr *ret)
{
	Execute ptr;
	addr control, value;
	LocalHold hold;

	hold_value(symbol, &symbol);
	hold_value(code, &code);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return fmte_("CATCH argument ~S must be a symbol.", symbol, NULL);
	}
	ptr = Execute_Thread;
	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	value = Nil;
	(void)lisp_catch_call_(ptr, hold, symbol, code, &value);
	Return(pop_control_(ptr, control));
	localhold_end(hold);
	if (ret)
		*ret = value;

	return 0;
}

int lisp_throw_(addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol))
		return fmte_("THROW argument ~S must be a symbol.", symbol, NULL);
	else
		return throw_control_(Execute_Thread, symbol);
}

