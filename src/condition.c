#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "condition_common.h"
#include "condition_debugger.h"
#include "control.h"
#include "copy.h"
#include "function.h"
#include "gc.h"
#include "restart.h"
#include "strvect.h"
#include "symbol.h"
#include "type_parse.h"
#include "type_typep.h"

/*
 *  condition for clang
 */
_g int conditionp(addr pos)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_CONDITION, &super);
	return clos_subclass_p(pos, super);
}

_g int condition_instance_p(addr pos)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_CONDITION, &super);
	return clos_subtype_p(pos, super);
}

_g int signal_function(Execute ptr, addr condition)
{
	int check;
	addr signals, type;

	/* break-on-signals */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &signals);
	getspecialcheck_local(ptr, signals, &signals);
	if (parse_type(ptr, &type, signals, Nil))
		_fmte("Invalid *break-on-signals* type ~S.", signals, NULL);
	if (typep_asterisk_clang(ptr, condition, type, &check))
		_fmte("Invalid typep ~S.", type, NULL);
	if (check)
		return invoke_debugger(ptr, condition);
	/* signal */
	return invoke_handler_control(ptr, condition);
}

static int error_function_execute(Execute ptr, addr condition)
{
	gchold_push_local(ptr->local, condition);
	return signal_function(ptr, condition)
		|| invoke_debugger(ptr, condition);
}

_g void error_function(addr condition)
{
	if (error_function_execute(Execute_Thread, condition)) {
		_fmte("~&Invalid signal call.~%", NULL);
		abortthis();
		return;
	}
}

static int function_restart_warning(Execute ptr)
{
	setresult_control(ptr, Nil);
	return 0;
}

static void warning_restart_make(addr *ret)
{
	addr inst, pos;

	GetConst(COMMON_MUFFLE_WARNING, &pos);
	restart_heap(&inst, pos);
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_restart_warning);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	setreport_restart(inst, Nil);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

_g int warning_restart_case(Execute ptr, addr instance)
{
	int check;
	addr control, pos;
	codejump jump;

	/* execute */
	push_restart_initialize_control(ptr, &control);
	check = 0;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		warning_restart_make(&pos);
		pushobject_restart_control(ptr, pos);
		check = signal_function(ptr, instance);
	}
	end_switch(&jump);
	if (check)
		return 1;

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		if (! equal_control_restart(ptr, control))
			throw_switch(&jump);
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	setresult_control(ptr, Nil);
	return free_control(ptr, control);
}

_g void OBSOLETE_format_error(const char *str, ...)
{
	addr format, args;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	simple_error(format, args);
}

_g void OBSOLETE_format_warning(const char *str, ...)
{
	addr format, args, instance;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	/* instance */
	instance_simple_warning(&instance, format, args);
	if (warning_restart_case(Execute_Thread, instance))
		_fmte("signal error.", NULL);
}


/*
 *  signal
 */
_g int signal_function_(Execute ptr, addr condition)
{
	int check;
	addr signals, type;

	/* break-on-signals */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &signals);
	getspecialcheck_local(ptr, signals, &signals);
	Return(parse_type(ptr, &type, signals, Nil));
	Return(typep_asterisk_clang(ptr, condition, type, &check));
	if (check)
		return invoke_debugger(ptr, condition);
	else
		return invoke_handler_control_(ptr, condition);
}

_g int error_function_(Execute ptr, addr condition)
{
	gchold_push_local(ptr->local, condition);
	Return(signal_function_(ptr, condition))
	Return(invoke_debugger(ptr, condition));
	return 0;
}

_g int callclang_error_(const char *str, ...)
{
	addr format, args;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_error_(Execute_Thread, format, args);
}

_g int callclang_warning_(const char *str, ...)
{
	addr format, args, instance;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	instance_simple_warning(&instance, format, args);
	return warning_restart_case(Execute_Thread, instance);
}


/*
 *  initialize
 */
_g void build_condition(Execute ptr)
{
	build_condition_debugger(ptr);
}

_g void init_condition(void)
{
	SetPointerCall(defun, empty, restart_warning);
	init_condition_common();
	init_condition_debugger();
}

