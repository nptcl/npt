#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "extern_control.h"
#include "extern_error.h"
#include "extern_object.h"
#include "extern_sequence.h"
#include "format.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "symbol.h"

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


/*
 *  special
 */
int lisp_push_special_(addr symbol, addr value)
{
	hold_value(symbol, &symbol);
	hold_value(value, &value);
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	pushspecial_control(Execute_Thread, symbol, value);
	return 0;
}

int lisp_push_special8_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special32_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp0_get_special_(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	}
	getspecial_local(Execute_Thread, symbol, &symbol);
	return Result(ret, (symbol == Unbound)? NULL: symbol);
}

int lisp0_get_special8_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp0_get_special16_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp0_get_special32_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp_get_special_(addr x, addr symbol)
{
	Return(lisp0_get_special_(&symbol, symbol));
	hold_set(x, symbol);
	return 0;
}

int lisp_get_special8_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special8_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_get_special16_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special16_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_get_special32_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special32_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_set_special_(addr symbol, addr value)
{
	hold_value(symbol, &symbol);
	hold_value(value, &value);
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	setspecial_local(Execute_Thread, symbol, value);
	return 0;
}

int lisp_set_special8_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special32_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
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

