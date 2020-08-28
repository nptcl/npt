#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute_object.h"
#include "extern_control.h"
#include "extern_develop.h"
#include "extern_error.h"
#include "extern_object.h"
#include "extern_sequence.h"
#include "format.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "restart.h"
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
 *  unwind-protect
 */
void lisp_unwind_protect(addr clean)
{
	hold_value(clean, &clean);
	set_protect_control(Execute_Thread, clean);
}


/*
 *  throw
 */
void lisp_catch(addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		lisp_abortf("Invalid catch symbol.");
		return;
	}
	catch_control(Execute_Thread, symbol);
}

int lisp_throw_(addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol))
		return fmte_("THROW argument ~S must be a symbol.", symbol, NULL);
	else
		return throw_control_(Execute_Thread, symbol);
}


/*
 *  signal
 */
int lisp_handler_bind_(addr name, addr call)
{
	hold_value(name, &name);
	hold_value(call, &call);
	return pushhandler_common_(Execute_Thread, name, call, 0);
}

int lisp_handler_case_(addr name, addr call)
{
	hold_value(name, &name);
	hold_value(call, &call);
	return pushhandler_common_(Execute_Thread, name, call, 1);
}

void lisp_handler_reverse(void)
{
	reverse_handler_control(Execute_Thread);
}


/*
 *  restart
 */
void lisp0_restart_make(addr *ret, addr name, addr call, int casep)
{
	addr restart;

	hold_value(name, &name);
	hold_value(call, &call);
	restart_heap(&restart, name);
	setfunction_restart(restart, call);
	setescape_restart(restart, casep);
	*ret = restart;
}

void lisp_restart_make(addr x, addr name, addr call, int casep)
{
	lisp0_restart_make(&name, name, call, casep);
	hold_set(x, name);
}

void lisp_restart_interactive(addr restart, addr call)
{
	hold_value(restart, &restart);
	if (! restartp(restart)) {
		Lisp_abort_type(restart, RESTART);
		return;
	}
	hold_value(call, &call);
	setinteractive_restart(restart, call);
}

void lisp_restart_report(addr restart, addr call)
{
	hold_value(restart, &restart);
	if (! restartp(restart)) {
		Lisp_abort_type(restart, RESTART);
		return;
	}
	hold_value(call, &call);
	setreport_restart(restart, call);
}

void lisp_restart_test(addr restart, addr call)
{
	hold_value(restart, &restart);
	if (! restartp(restart)) {
		Lisp_abort_type(restart, RESTART);
		return;
	}
	hold_value(call, &call);
	settest_restart(restart, call);
}

void lisp_restart_push(addr restart)
{
	hold_value(restart, &restart);
	pushrestart_control(Execute_Thread, restart);
}

void lisp_restart_reverse(void)
{
	reverse_restart_control(Execute_Thread);
}

