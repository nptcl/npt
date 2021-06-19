#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "condition_debugger.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
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

int signal_function_(Execute ptr, addr condition)
{
	int check;
	addr signals, type;

	if (ptr == NULL)
		ptr = Execute_Thread;
	/* break-on-signals */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &signals);
	Return(getspecialcheck_local_(ptr, signals, &signals));
	Return(parse_type(ptr, &type, signals, Nil));
	Return(typep_asterisk_clang_(ptr, condition, type, &check));
	if (check)
		return invoke_debugger_(ptr, condition);
	else
		return invoke_handler_control_(ptr, condition);
}

int error_function_(Execute ptr, addr condition)
{
	if (ptr == NULL)
		ptr = Execute_Thread;
	gchold_push_local(ptr->local, condition);
	Return(signal_function_(ptr, condition));
	Return(invoke_debugger_(ptr, condition));
	return 0;
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
	setreport_restart(inst, Nil);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

int warning_restart_case_(Execute ptr, addr instance)
{
	addr control, restart;

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

