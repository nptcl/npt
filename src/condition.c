#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "condition_debugger.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "execute_object.h"
#include "function.h"
#include "hold.h"
#include "restart.h"
#include "strvect.h"
#include "symbol.h"
#include "type_parse.h"
#include "type_typep.h"

/*
 *  condition for clang
 */
int conditionp_(addr pos, int *ret)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	GetConst(CLOS_CONDITION, &super);
	return clos_subclass_p_(pos, super, ret);
}

int conditionp_debug(addr pos)
{
	int check = 0;
	Error(conditionp_(pos, &check));
	return check;
}

int condition_instance_p_(addr pos, int *ret)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	GetConst(CLOS_CONDITION, &super);
	return clos_subtype_p_(pos, super, ret);
}

static void signal_restart(addr *ret)
{
	addr restart, pos, str;

	strvect_char_heap(&str, "Return to SIGNAL.");
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setinteractive_restart(restart, Nil);
	setreport_restart(restart, str);
	settest_restart(restart, Nil);
	setescape_restart(restart, 1);
	*ret = restart;
}

static int signal_invoke_debugger_(Execute ptr, addr condition)
{
	addr control, restart;

	push_control(ptr, &control);
	signal_restart(&restart);
	pushrestart_control(ptr, restart);

	/* debugger */
	(void)invoke_debugger_(ptr, condition);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == restart) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}

static int break_on_signals_p_(Execute ptr, addr condition, int *ret)
{
	addr pos;

	GetConst(SPECIAL_BREAK_ON_SIGNALS, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	Return(parse_type_(ptr, &pos, pos, Nil));
	return typep_asterisk_clang_(ptr, condition, pos, ret);
}

int signal_function_(Execute ptr, addr condition)
{
	int check;

	if (ptr == NULL)
		ptr = Execute_Thread;

	/* break-on-signals */
	Return(break_on_signals_p_(ptr, condition, &check));
	if (check)
		return signal_invoke_debugger_(ptr, condition);

	/* signal */
	return invoke_handler_control_(ptr, condition);
}

int error_function_(Execute ptr, addr condition)
{
	int check;

	if (ptr == NULL)
		ptr = Execute_Thread;
	gchold_push_local(ptr->local, condition);

	/* break-on-signals */
	Return(break_on_signals_p_(ptr, condition, &check));
	if (check) {
		Return(signal_invoke_debugger_(ptr, condition));
	}

	/* error */
	Return(invoke_handler_control_(ptr, condition));
	return invoke_debugger_(ptr, condition);
}

int callclang_error_(const char *str, ...)
{
	addr format, args;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_error_(NULL, format, args);
}

static void warning_restart_make(addr *ret)
{
	addr inst, pos;

	GetConst(COMMON_MUFFLE_WARNING, &pos);
	restart_heap(&inst, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	strvect_char_heap(&pos, "Skip warning.");
	setreport_restart(inst, pos);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

int warning_restart_case_(Execute ptr, addr instance)
{
	int check;
	addr type, control, restart;

	/* type check */
	GetConst(CONDITION_WARNING, &type);
	Return(clos_subtype_p_(instance, type, &check));
	if (! check) {
		return call_type_error_va_(ptr, instance, type,
				"The instance ~S must be a WARNING type.", instance, NULL);
	}

	/* warn */
	if (ptr == NULL)
		ptr = Execute_Thread;
	push_control(ptr, &control);
	warning_restart_make(&restart);
	(void)restart1_control(ptr, restart, signal_function_, instance);
	return pop_control_(ptr, control);
}

int callclang_warning_(const char *str, ...)
{
	addr format, args, instance;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	Return(instance_simple_warning_(&instance, format, args));
	return warning_restart_case_(NULL, instance);
}


/*
 *  initialize
 */
void build_condition(Execute ptr)
{
	build_condition_debugger(ptr);
}

void init_condition(void)
{
	init_condition_debugger();
}

