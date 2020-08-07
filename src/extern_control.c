#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
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
	push_new_control(Execute_Thread, ret);
}

int lisp_free_control_(addr control)
{
	if (GetType(control) != LISPTYPE_CONTROL)
		return fmte_("Invalid argument ~S.", control, NULL);
	return free_control_(Execute_Thread, control);
}

int lisp_eval_control_(addr eval)
{
	return eval_execute_partial(Execute_Thread, eval);
}

int lisp_eval_string_control_(addr eval)
{
	addr x;

	Return(lisp_reader_(&x, eval));
	if (x == NULL)
		return fmte_("Invalid eval string ~S.", eval, NULL);
	Return(lisp_eval_control_(x));

	return 0;
}

void lisp_nth_values_control(size_t index, addr *ret)
{
	addr x;
	getvalues_control(Execute_Thread, index, &x);
	*ret = (x == Unbound)? Nil: x;
}

void lisp_values_control(addr *ret)
{
	getvalues_list_control_heap(Execute_Thread, ret);
}

void lisp_result_control(addr *ret)
{
	getresult_control(Execute_Thread, ret);
}

void lisp_result2_control(addr *ret1, addr *ret2)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	getvalues_control(ptr, 0, &x);
	getvalues_control(ptr, 1, &y);
	*ret1 = (x == Unbound)? Nil: x;
	*ret2 = (y == Unbound)? Nil: y;
}

int lisp_eval8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(lisp_reader_(&y, x));
	if (x == NULL)
		return fmte_("Invalid eval string ~S.", x, NULL);
	Return(lisp_eval_control_(y));
	lisp_result_control(ret);
	rollback_local(local, stack);

	return 0;
}

int lisp_eval16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(lisp_reader_(&y, x));
	if (x == NULL)
		return fmte_("Invalid eval string ~S.", x, NULL);
	Return(lisp_eval_control_(y));
	getresult_control(Execute_Thread, ret);
	rollback_local(local, stack);

	return 0;
}

int lisp_eval_loop_(void)
{
	return eval_main_loop_(Execute_Thread);
}


/*
 *  format
 */
static int lisp_format_call(addr stream, addr format, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, stream, format, args, NULL);
	Return(format_lisp(ptr, stream, format, args, &args));
	localhold_end(hold);

	return lisp_free_control_(control);
}

int lisp_format8_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp_string8_(&format, str));
	va_start(va, str);
	list_stdarg_alloc(NULL, &args, va);
	va_end(va);

	return lisp_format_call(stream, format, args);
}

int lisp_format16_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp_string16_(&format, str));
	va_start(va, str);
	list_stdarg_alloc(NULL, &args, va);
	va_end(va);

	return lisp_format_call(stream, format, args);
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
#if 0
static int extern_unwind_protect(Execute ptr)
{
	addr clean;
	getdata_control(ptr, &clean);
	return funcall_control(ptr, clean, NULL);
}

int lisp_unwind_protect(addr code, addr clean)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	ptr = Execute_Thread;
	hold = LocalHold_array(ptr, 2);

	/* finalize */
	push_new_control(ptr, &control);
	localhold_set(hold, 0, code);
	localhold_set(hold, 1, clean);
	/* cleanup form */
	setprotect_control(ptr, p_extern_unwind_protect, clean);
	/* code */
	Return(lisp_funcall(&code, code, NULL));
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}
#endif

void lisp_set_unwind_protect(addr clean)
{
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

	Return(lisp_string8_(&format, str));
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}

int lisp_error16_(const void *str, ...)
{
	addr format, args;
	va_list va;

	Return(lisp_string16_(&format, str));
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);

	return call_simple_error_(NULL, format, args);
}


/*
 *  throw
 */
int lisp_catch_(addr symbol, addr code, addr *ret)
{
	Execute ptr;
	addr control, value;
	LocalHold hold;

	if (! symbolp(symbol))
		return fmte_("CATCH argument ~S must be a symbol.", symbol, NULL);
	ptr = Execute_Thread;
	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	/* begin catch */
	push_new_control(ptr, &value);
	catch_control(ptr, symbol);
	Return(apply_control(ptr, code, Nil));
	Return(free_control_(ptr, value));
	/* end catch */
	getresult_control(ptr, &value);
	localhold_set(hold, 0, value);
	Return(free_control_(ptr, control));
	localhold_end(hold);
	if (ret)
		*ret = value;

	return 0;
}

int lisp_throw_(addr symbol)
{
	if (! symbolp(symbol))
		return fmte_("THROW argument ~S must be a symbol.", symbol, NULL);
	else
		return throw_control_(Execute_Thread, symbol);
}

