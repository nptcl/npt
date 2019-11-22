#include "charqueue.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_type.h"
#include "condition.h"
#include "copy.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "eval.h"
#include "execute.h"
#include "file.h"
#include "format.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "memory.h"
#include "number.h"
#include "pointer.h"
#include "print.h"
#include "prompt.h"
#include "readtable.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "type.h"
#include "type_copy.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_table.h"
#include "type_typep.h"

/*
 *  restart
 */
enum Restart_Index {
	Restart_Name,
	Restart_Function,
	Restart_Interactive,
	Restart_Report,
	Restart_Test,
	Restart_Condition,
	Restart_Reference,
	Restart_Size
};

#define RefRestart		RefArrayA2
#define GetRestart		GetArrayA2
#define SetRestart		SetArrayA2

_g void restart_heap(addr *ret, addr name)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_RESTART, Restart_Size);
	SetUser(pos, 0);
	setenable_restart(pos, 1);
	SetRestart(pos, Restart_Name, name);

	*ret = pos;
}

_g void getname_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Name, ret);
}

_g void setname_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Name, value);
}

_g void getfunction_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Function, ret);
}

_g void setfunction_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Function, value);
}

_g void getinteractive_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Interactive, ret);
}

_g void setinteractive_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Interactive, value);
}

_g void getreport_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Report, ret);
}

_g void setreport_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Report, value);
}

_g void gettest_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Test, ret);
}

_g void settest_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Test, value);
}

_g void getcondition_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Condition, ret);
}

_g void setcondition_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Condition, value);
}

_g void getreference_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Reference, ret);
}

_g void setreference_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Reference, value);
}

_g void setescape_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 0, value);
	SetUser(pos, u);
}

_g int getescape_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 0);
}

_g void setenable_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 1, value);
	SetUser(pos, u);
}

_g int getenable_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 1);
}

_g void setredirect_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 2, value);
	SetUser(pos, u);
}

_g int getredirect_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 2);
}


/*
 *  restart code
 */
_g void function_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	GetFunctionSymbol(symbol, &pos);
	if (pos == Unbound)
		undefined_function(symbol);
	*ret = pos;
}

_g void function_local_restart(Execute ptr, addr symbol, addr *ret)
{
	getfunctioncheck_local(ptr, symbol, ret);
}

_g void setf_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	getsetf_symbol(symbol, &pos);
	if (pos == Unbound)
		undefined_function_setf(symbol);
	*ret = pos;
}

_g void setf_local_restart(Execute ptr, addr symbol, addr *ret)
{
	getsetfcheck_local(ptr, symbol, ret);
}

_g void value_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	GetValueSymbol(symbol, &pos);
	if (pos == Unbound)
		unbound_variable(symbol);
	*ret = pos;
}

_g void value_lexical_restart(Execute ptr, addr symbol, addr *ret)
{
	getlexicalcheck_local(ptr, symbol, ret);
}

_g void value_special_restart(Execute ptr, addr symbol, addr *ret)
{
	getspecialcheck_local(ptr, symbol, ret);
}


/*
 *  (handler-bind
 *    ((warning #'function-handler-warning))
 *    ...)
 */
static void function_handler_warning(Execute ptr, addr condition)
{
	addr pos, stream, format, args;

	error_output_stream(ptr, &stream);
	GetConst(CONDITION_SIMPLE_WARNING, &pos);
	if (clos_subtype_p(condition, pos)) {
		simple_condition_format_control(condition, &format);
		simple_condition_format_arguments(condition, &args);
		fmts(stream, "~&WARNING: ", NULL);
		if (format_lisp(ptr, stream, format, args, &args))
			fmte("Invalid format result.", NULL);
		fresh_line_stream(stream);
	}
	else {
		fmts(stream, "~&WARNING: ~S~%", condition, NULL);
	}
}

_g void handler_warning(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_warning);
	pushhandler_control(ptr, pos, call, 0);
}


/*
 *  (handler-case
 *    ...
 *    ((system::savecore #'function-handler-savecore)))
 */
static void function_handler_savecore(Execute ptr, addr condition)
{
	/* do-nothing */
}

_g void handler_savecore(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_SAVECORE, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_savecore);
	pushhandler_control(ptr, pos, call, 1);
}


/*
 *  debugger
 */
static void output_unbound_variable(Execute ptr, addr stream, addr condition)
{
	cell_error_name(condition, &condition);
	fmts(stream, "Unbound variable ~S.~%", condition, NULL);
}

static void output_undefined_function(Execute ptr, addr stream, addr condition)
{
	cell_error_name(condition, &condition);
	fmts(stream, "Undefined function ~S.~%", condition, NULL);
}

static void output_simple_error(Execute ptr, addr stream, addr condition)
{
	addr control, arguments;

	simple_condition_format(condition, &control, &arguments);
	format_stream_lisp(ptr, stream, control, arguments);
	fresh_line_stream(stream);
	terpri_stream(stream);
}

static void output_type_error(addr stream, addr instance)
{
	addr datum, expected;

	type_error_datum(instance, &datum);
	type_error_expected(instance, &expected);
	if (GetType(expected) == LISPTYPE_TYPE)
		type_object(&expected, expected);
	fmts(stream, "Value ~S must be a ~S type.~%", datum, expected, NULL);
}

static void output_condition(addr stream, addr condition)
{
}

static int condition_check_p(constindex index, addr condition)
{
	addr super;

	if (! condition_instance_p(condition)) return 0;
	GetConstant(index, &super);
	return clos_subtype_p(condition, super);
}
#define ConditionCheck(x,y) condition_check_p(CONSTANT_CONDITION_##x,(y))

static void output_debugger(Execute ptr, addr stream, addr pos)
{
	if (ConditionCheck(UNBOUND_VARIABLE, pos)) {
		output_unbound_variable(ptr, stream, pos);
	}
	if (ConditionCheck(UNDEFINED_FUNCTION, pos)) {
		output_undefined_function(ptr, stream, pos);
	}
	else if (ConditionCheck(SIMPLE_CONDITION, pos)) {
		output_simple_error(ptr, stream, pos);
	}
	else if (ConditionCheck(TYPE_ERROR, pos)) {
		output_type_error(stream, pos);
	}
	else if (condition_instance_p(pos)) {
		output_condition(stream, pos);
	}
	else {
		fmts(stream, "Invalid condition type ~S~%", pos, NULL);
	}
}

static void output_restarts_debugger(Execute ptr, addr io, addr list)
{
	int check;
	addr pos, symbol, name, str;
	size_t index;

	for (index = 0; list != Nil; index++) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_RESTART);
		getname_restart(pos, &symbol);
		getreport_restart(pos, &name);
		if (name != Nil) {
			if (! stringp(name)) {
				open_output_string_stream(&str, 0);
				check = callclang_funcall(ptr, &name, name, str, NULL);
				if (check)
					fmte("Invalid restart report.", NULL);
				string_stream_heap(str, &name);
				close_stream(str);
			}
		}
		fmts(io, "~2@A. ~16A ~A~%", intsizeh(index), symbol, name, NULL);
	}
}

static int eval_debugger(Execute ptr, addr io, addr eval)
{
	addr control;

	push_close_control(ptr, &control);
	if (eval_execute(ptr, eval))
		return runcode_free_control(ptr, control);
	eval_loop_output(ptr, io, control);
	return free_control(ptr, control);
}

static int enter_debugger(addr condition)
{
	int check, result;
	addr io, pos, list, exit;
	Execute ptr;
	size_t index, select, size;

	/* restarts */
	ptr = Execute_Thread;
	debug_io_stream(ptr, &io);
	compute_restarts_control(ptr, condition, &list);
	if (list == Nil) {
		fmts(io, "There is no restarts, abort.~%", NULL);
		abortthis();
	}
	output_restarts_debugger(ptr, io, list);
	size = length_list_unsafe(list);

	/* eval loop */
	index = getindex_prompt(ptr) + 1ULL;
	GetConst(KEYWORD_EXIT, &exit);

loop:
	setindex_prompt(ptr, index);
	setshow_prompt(ptr, 1);
	clear_input_stream(io);
	check = read_stream(ptr, io, &result, &pos);
	/* Interupt */
	if (check) {
		fmte("Invalid operation.", NULL);
		goto exit;
	}
	/* EOF */
	if (result) {
		terpri_stream(io);
		goto exit;
	}
	/* :exit */
	if (getbreak_prompt(ptr) || pos == exit) {
		terpri_stream(io);
		goto exit;
	}
	/* check */
	if (! fixnump(pos)) {
		if (eval_debugger(ptr, io, pos))
			return 1;
		goto loop;
	}
	if (getindex_integer(pos, &select)) {
		fmts(io, "Illegal integer value ~A.~%", pos, NULL);
		goto loop;
	}
	if (size <= select) {
		fmts(io, "Too large index value ~A.~%", pos, NULL);
		goto loop;
	}
	/* execute */
	getnth_unsafe(list, select, &pos);
	if (invoke_restart_interactively_control(ptr, pos)) return 1;
	goto loop;

exit:
	exit_code_thread(LISPCODE_ERROR);
	return 0;
}

static int enable_debugger_p(void)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &pos);
	getspecialcheck_local(Execute_Thread, pos, &pos);
	return pos != Nil;
}

static int invoke_standard_debugger(Execute ptr, addr condition)
{
	addr io, pos;

	/* output condition */
	debug_io_stream(ptr, &io);
	clos_class_of(condition, &pos);
	stdget_class_name(pos, &pos);
	fmts(io, "~&ERROR: ~S~%", pos, NULL);
	output_debugger(ptr, io, condition);

	/* no-debugger */
	if (! enable_debugger_p()) {
		fmts(io, "~2&Debugger is not enabled.~%", NULL);
		abortthis();
		return 1;
	}

	/* debugger */
	return enter_debugger(condition);
}

_g int invoke_debugger(Execute ptr, addr condition)
{
	int check;
	addr symbol, prior, call, control;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	getspecialcheck_local(ptr, symbol, &prior);
	if (prior == Nil)
		return invoke_standard_debugger(ptr, condition);
	/* call function */
	call = prior;
	if (symbolp(call))
		getfunctioncheck_local(ptr, call, &call);
	push_close_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	check = funcall_control(ptr, call, condition, prior, NULL);
	if (free_check_control(ptr, control, check))
		return 1;
	/* invoke-debugger is not returned. */
	return invoke_standard_debugger(ptr, condition);
}

_g void set_enable_debugger(int value)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &pos);
	setspecial_local(Execute_Thread, pos, value? T: Nil);
}


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

_g int signal_function(addr condition)
{
	int check;
	addr signals, type;
	Execute ptr;

	ptr = Execute_Thread;
	/* break-on-signals */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &signals);
	getspecialcheck_local(ptr, signals, &signals);
	if (parse_type(ptr, &type, signals, Nil))
		fmte("Invalid *break-on-signals* type ~S.", signals, NULL);
	if (typep_asterisk_clang(condition, type, &check))
		fmte("Invalid typep ~S.", type, NULL);
	if (check)
		return invoke_debugger(ptr, condition);
	/* signal */
	if (invoke_handler_control(ptr, condition, &check))
		return 1;

	return 0;
}

_g int error_common(Execute ptr, addr condition)
{
	return signal_function(condition)
		|| invoke_debugger(ptr, condition);
}

_g void error_function(addr condition)
{
	if (error_common(Execute_Thread, condition)) {
		fmte("~&Invalid signal call.~%", NULL);
		abortthis();
	}
}

_g void format_error(const char *str, ...)
{
	addr format, args;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	simple_error(format, args);
}

static void function_restart_warning(Execute ptr)
{
	setresult_control(ptr, Nil);
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
		check = signal_function(instance);
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

_g int signal_warning(const char *str, ...)
{
	addr format, args, instance;
	va_list va;

	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	/* instance */
	instance_simple_warning(&instance, format, args);
	return warning_restart_case(Execute_Thread, instance);
}

_g void format_warning(const char *str, ...)
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
		fmte("signal error.", NULL);
}

/* serious_condition (condition) */
static void instance_condition(addr *ret, constindex condition)
{
	addr pos;
	GetConstant(condition, &pos);
	clos_instance_heap(pos, ret);
}
_g void instance_serious_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_SERIOUS_CONDITION);
}
_g void serious_condition(void)
{
	addr instance;
	instance_serious_condition(&instance);
	error_function(instance);
}

/* simple_condition (condition) :format-control :format-arguments*/
static void instance_condition2(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2)
{
	addr instance;

	GetConstant(index, &instance);
	clos_instance_heap(instance, &instance);
	clos_setconst(instance, index1, pos1);
	clos_setconst(instance, index2, pos2);
	*ret = instance;
}
_g void instance_simple_condition(addr *ret, addr control, addr args)
{
	instance_condition2(ret, CONSTANT_CONDITION_SIMPLE_CONDITION,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}
_g int simple_condition(addr control, addr args)
{
	addr instance;
	instance_simple_condition(&instance, control, args);
	return signal_function(instance);
}
_g void simple_condition_format(addr condition, addr *control, addr *arguments)
{
	ClosCheckConst(condition, CLOSNAME_FORMAT_CONTROL, control);
	ClosCheckConst(condition, CLOSNAME_FORMAT_ARGUMENTS, arguments);
}
_g void simple_condition_format_control(addr condition, addr *ret)
{
	ClosCheckConst(condition, CLOSNAME_FORMAT_CONTROL, ret);
}
_g void simple_condition_format_arguments(addr condition, addr *ret)
{
	ClosCheckConst(condition, CLOSNAME_FORMAT_ARGUMENTS, ret);
}

/* simple_error (simple_condition) :format-control :format-arguments */
_g void instance_simple_error(addr *ret, addr control, addr args)
{
	instance_condition2(ret, CONSTANT_CONDITION_SIMPLE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}
_g void simple_error(addr control, addr args)
{
	addr instance;
	instance_simple_error(&instance, control, args);
	error_function(instance);
}

/* error (serious_condition) */
_g void instance_error_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_ERROR);
}
_g void error_condition(void)
{
	addr instance;
	instance_error_condition(&instance);
	error_function(instance);
}

/* warning (condition) */
_g void instance_warning_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_WARNING);
}
_g void warning_condition(void)
{
	addr instance;
	instance_warning_condition(&instance);
	error_function(instance);
}

/* simple_warning (simple_condition warning) :format-control :format-arguments */
_g void instance_simple_warning(addr *ret, addr control, addr args)
{
	instance_condition2(ret, CONSTANT_CONDITION_SIMPLE_WARNING,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}
_g int simple_warning(addr control, addr args)
{
	addr instance;
	instance_simple_warning(&instance, control, args);
	return signal_function(instance);
}

/* storage_condition (serious_condition) */
_g void instance_storage_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_STORAGE_CONDITION);
}
_g void storage_condition(void)
{
	addr instance;
	instance_storage_condition(&instance);
	error_function(instance);
}

/* arithmetic_error (error) :operation :operands */
_g void instance_arithmetic_error(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_ARITHMETIC_ERROR,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g void arithmetic_error(addr operation, addr operands)
{
	addr instance;
	instance_arithmetic_error(&instance, operation, operands);
	error_function(instance);
}
_g void arithmetic_error_operation(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_OPERATION, ret);
}
_g void arithmetic_error_operands(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_OPERANDS, ret);
}

/* floating_point_inexact (arithmetic_error) :operation :operands */
_g void instance_floating_point_inexact(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_INEXACT,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g void floating_point_inexact(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_inexact(&instance, operation, operands);
	error_function(instance);
}
_g void floating_point_inexact_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_inexact(operation, operands);
}
_g void floating_point_inexact_stdarg(constindex index, ...)
{
	addr operands;
	va_list va;

	va_start(va, index);
	list_alloc_stdarg(NULL, &operands, va);
	va_end(va);
	floating_point_inexact_constant(index, operands);
}

/* floating_point_invalid_operation (arithmetic_error) :operation :operands */
_g void instance_floating_point_invalid_operation(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g void floating_point_invalid_operation(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_invalid_operation(&instance, operation, operands);
	error_function(instance);
}
_g void floating_point_invalid_operation_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_invalid_operation(operation, operands);
}
_g void floating_point_invalid_operation_stdarg(constindex index, ...)
{
	addr operands;
	va_list va;

	va_start(va, index);
	list_alloc_stdarg(NULL, &operands, va);
	va_end(va);
	floating_point_invalid_operation_constant(index, operands);
}

/* floating_point_overflow (arithmetic_error) :operation :operands */
_g void instance_floating_point_overflow(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_OVERFLOW,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g void floating_point_overflow(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_overflow(&instance, operation, operands);
	error_function(instance);
}
_g void floating_point_overflow_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_overflow(operation, operands);
}
_g void floating_point_overflow_stdarg(constindex index, ...)
{
	addr operands;
	va_list va;

	va_start(va, index);
	list_alloc_stdarg(NULL, &operands, va);
	va_end(va);
	floating_point_overflow_constant(index, operands);
}

/* floating_point_underflow (arithmetic_error) :operation :operands */
_g void instance_floating_point_underflow(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_UNDERFLOW,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g void floating_point_underflow(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_underflow(&instance, operation, operands);
	error_function(instance);
}
_g void floating_point_underflow_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_underflow(operation, operands);
}
_g void floating_point_underflow_stdarg(constindex index, ...)
{
	addr operands;
	va_list va;

	va_start(va, index);
	list_alloc_stdarg(NULL, &operands, va);
	va_end(va);
	floating_point_underflow_constant(index, operands);
}

/* division_by_zero (arithmetic_error) :operation :operands */
_g void instance_division_by_zero(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_DIVISION_BY_ZERO,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g void division_by_zero(addr operation, addr operands)
{
	addr instance;
	instance_division_by_zero(&instance, operation, operands);
	error_function(instance);
}
_g void division_by_zero_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	division_by_zero(operation, operands);
}
_g void division_by_zero_stdarg(constindex index, ...)
{
	addr operands;
	va_list va;

	va_start(va, index);
	list_alloc_stdarg(NULL, &operands, va);
	va_end(va);
	division_by_zero_constant(index, operands);
}

_g void division_by_zero_real1(constindex index, addr left)
{
	number_throw_heap(left, &left);
	list_heap(&left, left, NULL);
	division_by_zero_constant(index, left);
}

_g void division_by_zero_real2(constindex index, addr left, addr right)
{
	number_throw_heap(left, &left);
	number_throw_heap(right, &right);
	list_heap(&left, left, right, NULL);
	division_by_zero_constant(index, left);
}

_g void division_by_zero0(void)
{
	division_by_zero_constant(CONSTANT_COMMON_SLASH, Nil);
}

_g void division_by_zero1(addr left)
{
	division_by_zero_real1(CONSTANT_COMMON_SLASH, left);
}

_g void division_by_zero2(addr left, addr right)
{
	division_by_zero_real2(CONSTANT_COMMON_SLASH, left, right);
}


/* cell_error (error) :name */
static void instance_condition1(addr *ret, constindex index,
		constindex index1, addr pos1)
{
	addr instance;

	GetConstant(index, &instance);
	clos_instance_heap(instance, &instance);
	clos_setconst(instance, index1, pos1);
	*ret = instance;
}
_g void instance_cell_error(addr *ret, addr name)
{
	instance_condition1(ret, CONSTANT_CONDITION_CELL_ERROR,
			CONSTANT_CLOSNAME_NAME, name);
}
_g void cell_error(addr name)
{
	addr instance;
	instance_cell_error(&instance, name);
	error_function(instance);
}
_g void cell_error_name(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_NAME, ret);
}

/* control_error (error) */
_g void instance_control_error(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_CONTROL_ERROR);
}
_g void control_error(void)
{
	addr instance;
	instance_control_error(&instance);
	error_function(instance);
}

/* stream_error (error) :stream */
_g void instance_stream_error(addr *ret, addr stream)
{
	instance_condition1(ret, CONSTANT_CONDITION_STREAM_ERROR,
			CONSTANT_CLOSNAME_STREAM, stream);
}
_g void stream_error(addr stream)
{
	addr instance;
	instance_stream_error(&instance, stream);
	error_function(instance);
}
_g void stream_error_stream(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_STREAM, ret);
}

/* end_of_file (stream_error) :stream */
_g void instance_end_of_file(addr *ret, addr stream)
{
	instance_condition1(ret, CONSTANT_CONDITION_END_OF_FILE,
			CONSTANT_CLOSNAME_STREAM, stream);
}
_g void end_of_file(addr stream)
{
	addr instance;
	instance_end_of_file(&instance, stream);
	error_function(instance);
}

/* reader_error (parse_error stream_error) :stream */
_g void instance_reader_error(addr *ret, addr stream)
{
	instance_condition1(ret, CONSTANT_CONDITION_READER_ERROR,
			CONSTANT_CLOSNAME_STREAM, stream);
}
_g void reader_error(addr stream)
{
	addr instance;
	instance_reader_error(&instance, stream);
	error_function(instance);
}

/* file_error (error) :pathname */
_g void instance_file_error(addr *ret, addr pathname)
{
	instance_condition1(ret, CONSTANT_CONDITION_FILE_ERROR,
			CONSTANT_CLOSNAME_PATHNAME, pathname);
}
_g void file_error(addr pathname)
{
	addr instance;
	instance_file_error(&instance, pathname);
	error_function(instance);
}
_g void file_error_pathname(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_PATHNAME, ret);
}

/* package_error (error) :package */
_g void instance_package_error(addr *ret, addr package)
{
	instance_condition1(ret, CONSTANT_CONDITION_PACKAGE_ERROR,
			CONSTANT_CLOSNAME_PACKAGE, package);
}
_g void package_error(addr package)
{
	addr instance;
	instance_reader_error(&instance, package);
	error_function(instance);
}
_g void package_error_package(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_PACKAGE, ret);
}

/* parse_error (error) */
_g void instance_parse_error(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_PARSE_ERROR);
}
_g void parse_error(void)
{
	addr instance;
	instance_parse_error(&instance);
	error_function(instance);
}

/* print_not_readable (error) :object */
_g void instance_print_not_readable(addr *ret, addr object)
{
	instance_condition1(ret, CONSTANT_CONDITION_PRINT_NOT_READABLE,
			CONSTANT_CLOSNAME_OBJECT, object);
}
_g void print_not_readable(addr object)
{
	addr instance;
	instance_reader_error(&instance, object);
	error_function(instance);
}

_g void print_not_readable_object(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_OBJECT, ret);
}


/* program_error (error) */
_g void instance_program_error(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_PROGRAM_ERROR);
}
_g void program_error(void)
{
	addr instance;
	instance_program_error(&instance);
	error_function(instance);
}

/* style_warning (warning) */
_g void instance_style_warning(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_STYLE_WARNING);
}
_g void style_warning(void)
{
	addr instance;
	instance_style_warning(&instance);
	error_function(instance);
}

/* type_error (error) :datum :expected-type */
_g void instance_type_error(addr *ret, addr datum, addr expected)
{
	instance_condition2(ret, CONSTANT_CONDITION_TYPE_ERROR,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}
_g void type_error(addr datum, addr expected)
{
	addr instance;
	copylocal_object(NULL, &datum, datum);
	copylocal_object(NULL, &expected, expected);
	instance_type_error(&instance, datum, expected);
	error_function(instance);
}

_g void type_error_constant(addr datum, constindex expected)
{
	addr type;
	GetConstant(expected, &type);
	type_error(datum, type);
}

_g void type_error_datum(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_DATUM, ret);
}

_g void type_error_expected(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_EXPECTED_TYPE, ret);
}

_g int typep_error(addr value, addr type)
{
	int check;

	if (typep_clang(value, type, &check)) {
		return 1;
	}
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		type_error(value, type);
	}

	return 0;
}

_g int typep_asterisk_error(addr value, addr type)
{
	int check;

	if (typep_asterisk_clang(value, type, &check)) {
		return 1;
	}
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		type_error(value, type);
	}

	return 0;
}

_g int typep_typetable(addr value, enum TypeTable type)
{
	addr pos;
	gettypetable(type, &pos);
	return typep_asterisk_error(value, pos);
}

/* simple_type_error (simple_condition type_error)
 *   :format-control :format-arguments :datum :expected-type */
static void instance_condition4(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2,
		constindex index3, addr pos3,
		constindex index4, addr pos4)
{
	addr instance;

	GetConstant(index, &instance);
	clos_instance_heap(instance, &instance);
	clos_setconst(instance, index1, pos1);
	clos_setconst(instance, index2, pos2);
	clos_setconst(instance, index3, pos3);
	clos_setconst(instance, index4, pos4);
	*ret = instance;
}
_g void instance_simple_type_error(addr *ret,
		addr control, addr args, addr datum, addr expected)
{
	instance_condition4(ret, CONSTANT_CONDITION_SIMPLE_TYPE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}
_g void simple_type_error(addr control, addr args, addr datum, addr expected)
{
	addr instance;
	instance_simple_type_error(&instance, control, args, datum, expected);
	error_function(instance);
}

_g void type_error_stdarg(addr datum, addr expected, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	copylocal_object(NULL, &datum, datum);
	copylocal_object(NULL, &expected, expected);
	simple_type_error(control, args, datum, expected);
}

_g void type_error_fill_pointer(addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	type_error_stdarg(datum, expected,
			"The vector ~S don't have a fill-pointer.", datum, NULL);
}

_g void type_error_fill_pointer_zero(addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	type_error_stdarg(datum, expected,
			"The vector ~S fill-pointer is 0.", datum, NULL);
}

_g void type_error_adjustable(addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	type_error_stdarg(datum, expected,
			"The vector ~S is not adjustable.", datum, NULL);
}

/* unbound_slot (cell_error) :instance :name */
_g void instance_unbound_slot(addr *ret, addr instance, addr name)
{
	instance_condition2(ret, CONSTANT_CONDITION_UNBOUND_SLOT,
			CONSTANT_CLOSNAME_INSTANCE, instance,
			CONSTANT_CLOSNAME_NAME, name);
}
_g void unbound_slot(addr argument, addr name)
{
	addr instance;
	instance_unbound_slot(&instance, argument, name);
	error_function(instance);
}
_g void unbound_slot_instance(addr instance, addr *ret)
{
	ClosCheckConst(instance, CLOSNAME_INSTANCE, ret);
}

/* unbound_variable (cell_error) :name */
_g void instance_unbound_variable(addr *ret, addr name)
{
	instance_condition1(ret, CONSTANT_CONDITION_UNBOUND_VARIABLE,
			CONSTANT_CLOSNAME_NAME, name);
}
_g void unbound_variable(addr name)
{
	addr instance;
	instance_unbound_variable(&instance, name);
	error_function(instance);
}

/* undefined_function (cell_error) :name */
_g void instance_undefined_function(addr *ret, addr name)
{
	instance_condition1(ret, CONSTANT_CONDITION_UNDEFINED_FUNCTION,
			CONSTANT_CLOSNAME_NAME, name);
}
_g void undefined_function(addr name)
{
	addr instance;
	instance_undefined_function(&instance, name);
	error_function(instance);
}
_g void undefined_function_setf(addr name)
{
	addr setf;
	GetConst(COMMON_SETF, &setf);
	list_heap(&name, setf, name, NULL);
	undefined_function(name);
}

/* savecore */
_g void instance_savecore_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_SAVECORE);
}

_g void savecore_condition(void)
{
	addr instance;
	instance_savecore_condition(&instance);
	error_function(instance);
}

/* simple_file_error */
_g void instance_simple_file_error(addr *ret, addr pathname, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_FILE_ERROR, &instance);
	clos_instance_heap(instance, &instance);
	clos_setconst(instance, CONSTANT_CLOSNAME_PATHNAME, pathname);
	clos_setconst(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control);
	clos_setconst(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
	*ret = instance;
}

_g void simple_file_error(addr pathname, addr control, addr args)
{
	addr instance;
	instance_simple_file_error(&instance, pathname, control, args);
	error_function(instance);
}

_g void simple_file_error_stdarg(addr pathname, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	copylocal_object(NULL, &pathname, pathname);
	simple_file_error(pathname, control, args);
}


/*
 *  build_condition
 */
_g void build_condition(Execute ptr)
{
	addr symbol;

	/* debugger */
	GetConst(SYSTEM_ENABLE_DEBUGGER, &symbol);
	SetValueSymbol(symbol, T);
	set_enable_debugger(1);
}

_g void init_condition(void)
{
	SetPointerCall(defun, empty, restart_warning);
	SetPointerCall(defun, var1, handler_warning);
	SetPointerCall(defun, var1, handler_savecore);
}

