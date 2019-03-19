#include "charqueue.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "condition.h"
#include "copy.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "execute.h"
#include "file.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "memory.h"
#include "number.h"
#include "print.h"
#include "prompt.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"
#include "type.h"
#include "type_copy.h"
#include "type_object.h"
#include "type_parse.h"
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

void restart_heap(addr *ret, addr name)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_RESTART, Restart_Size);
	SetUser(pos, 0);
	setenable_restart(pos, 1);
	SetRestart(pos, Restart_Name, name);

	*ret = pos;
}

void getname_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Name, ret);
}

void setname_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Name, value);
}

void getfunction_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Function, ret);
}

void setfunction_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Function, value);
}

void getinteractive_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Interactive, ret);
}

void setinteractive_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Interactive, value);
}

void getreport_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Report, ret);
}

void setreport_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Report, value);
}

void gettest_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Test, ret);
}

void settest_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Test, value);
}

void getcondition_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Condition, ret);
}

void setcondition_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Condition, value);
}

void getreference_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Reference, ret);
}

void setreference_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Reference, value);
}

void setescape_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 0, value);
	SetUser(pos, u);
}

int getescape_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 0);
}

void setenable_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 1, value);
	SetUser(pos, u);
}

int getenable_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 1);
}

void setredirect_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 2, value);
	SetUser(pos, u);
}

int getredirect_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 2);
}


/*
 *  restart code
 */
void function_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	GetFunctionSymbol(symbol, &pos);
	if (pos == Unbound)
		undefined_function(symbol);
	*ret = pos;
}

void function_local_restart(Execute ptr, addr symbol, addr *ret)
{
	getfunctioncheck_local(ptr, symbol, ret);
}

void setf_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	getsetf_symbol(symbol, &pos);
	if (pos == Unbound)
		undefined_function_setf(symbol);
	*ret = pos;
}

void setf_local_restart(Execute ptr, addr symbol, addr *ret)
{
	getsetfcheck_local(ptr, symbol, ret);
}

void value_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	GetValueSymbol(symbol, &pos);
	if (pos == Unbound)
		unbound_variable(symbol);
	*ret = pos;
}

void value_lexical_restart(Execute ptr, addr symbol, addr *ret)
{
	getlexicalcheck_local(ptr, symbol, ret);
}

void value_special_restart(Execute ptr, addr symbol, addr *ret)
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
	if (std_subtype_p(condition, pos)) {
		simple_condition_format_control(condition, &format);
		simple_condition_format_arguments(condition, &args);
		fmts(stream, "~&WARNING: ", NULL);
		if (format_lisp(ptr, stream, format, args, &args))
			fmte("Invalid format result.", NULL);
	}
	else {
		fmts(stream, "~&WARNING: ~S~%", condition, NULL);
	}
}

void handler_warning(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, function_handler_warning);
	pushhandler_control(ptr, pos, call, 0);
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
	return std_subtype_p(condition, super);
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
			open_output_string_stream(&str, 0);
			check = callclang_funcall(ptr, &name, name, str, NULL);
			if (check)
				fmte("Invalid restart report.", NULL);
			string_stream_heap(str, &name);
			close_stream(str);
		}
		fmts(io, "~2@A. ~16A ~A~%", intsizeh(index), symbol, name, NULL);
	}
}

static int enter_debugger(addr condition)
{
	int check, result;
	addr io, pos, list, exit;
	Execute ptr;
	size_t index, select, size;

	ptr = Execute_Thread;
	debug_io_stream(ptr, &io);
	clos_class_of(condition, &pos);
	clos_elt(pos, Clos_class_name, &pos);
	fmts(io, "~&ERROR: ~S~%", pos, NULL);
	output_debugger(ptr, io, condition);

	/* restarts */
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
		goto error;
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
		fmts(io, "Invalid integer value ~A.~%", pos, NULL);
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

error:
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

static void abort_debugger(void)
{
	addr io;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SPECIAL_DEBUG_IO, &io);
	getspecialcheck_local(ptr, io, &io);
	fmts(io, "~&abort.~%", NULL);
	abortthis();
}

int invoke_debugger(addr condition)
{
	if (enable_debugger_p()) {
		if (enter_debugger(condition))
			return 1;
	}
	else {
		abort_debugger();
	}

	return 0;
}

void set_enable_debugger(int value)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &pos);
	setspecial_local(Execute_Thread, pos, value? T: Nil);
}

void set_enable_interactive(int value)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_INTERACTIVE, &pos);
	setspecial_local(Execute_Thread, pos, value? T: Nil);
}


#if 0
void invoke_restart(Execute ptr, addr restart, addr args)
{
}
#endif


/*
 *  condition for clang
 */
int conditionp(addr pos)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_CONDITION, &super);
	return std_subclass_p(pos, super);
}

int condition_instance_p(addr pos)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_CONDITION, &super);
	return std_subtype_p(pos, super);
}

int signal_function(addr condition)
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
		return invoke_debugger(condition);
	/* signal */
	return invoke_handler_control(ptr, condition);
}

void error_function(addr condition)
{
	if (! signal_function(condition)) {
		if (invoke_debugger(condition)) {
			abort_debugger();
		}
	}
}

void format_error(const char *str, ...)
{
	addr format, cons;
	va_list args;

	strvect_char_heap(&format, str);
	va_start(args, str);
	copylocal_list_stdarg(NULL, &cons, args);
	va_end(args);
	simple_error(format, cons);
}

void format_warning(const char *str, ...)
{
	addr format, cons;
	va_list args;

	strvect_char_heap(&format, str);
	va_start(args, str);
	copylocal_list_stdarg(NULL, &cons, args);
	va_end(args);
	simple_warning(format, cons);
}

/* serious_condition (condition) */
static void instance_condition(addr *ret, constindex condition)
{
	addr pos;
	GetConstant(condition, &pos);
	make_instance_restrict_heap(pos, ret);
}
void instance_serious_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_SERIOUS_CONDITION);
}
void serious_condition(void)
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
	addr instance, pos;

	GetConstant(index, &pos);
	make_instance_restrict_heap(pos, &instance);
	GetConstant(index1, &pos);
	setf_clos_value(instance, pos, pos1);
	GetConstant(index2, &pos);
	setf_clos_value(instance, pos, pos2);
	*ret = instance;
}
void instance_simple_condition(addr *ret, addr control, addr args)
{
	instance_condition2(ret, CONSTANT_CONDITION_SIMPLE_CONDITION,
			CONSTANT_KEYWORD_FORMAT_CONTROL, control,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS, args);
}
void simple_condition(addr control, addr args)
{
	addr instance;
	instance_simple_condition(&instance, control, args);
	error_function(instance);
}
void simple_condition_format(addr condition, addr *control, addr *arguments)
{
	addr key;

	GetConst(KEYWORD_FORMAT_CONTROL, &key);
	clos_value(condition, key, control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &key);
	clos_value(condition, key, arguments);
}
void simple_condition_format_control(addr condition, addr *ret)
{
	addr key;
	GetConst(KEYWORD_FORMAT_CONTROL, &key);
	clos_value(condition, key, ret);
}
void simple_condition_format_arguments(addr condition, addr *ret)
{
	addr key;
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &key);
	clos_value(condition, key, ret);
}

/* simple_error (simple_condition) :format-control :format-arguments */
void instance_simple_error(addr *ret, addr control, addr args)
{
	instance_condition2(ret, CONSTANT_CONDITION_SIMPLE_ERROR,
			CONSTANT_KEYWORD_FORMAT_CONTROL, control,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS, args);
}
void simple_error(addr control, addr args)
{
	addr instance;
	instance_simple_error(&instance, control, args);
	error_function(instance);
}

/* error (serious_condition) */
void instance_error_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_ERROR);
}
void error_condition(void)
{
	addr instance;
	instance_error_condition(&instance);
	error_function(instance);
}

/* warning (condition) */
void instance_warning_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_WARNING);
}
void warning_condition(void)
{
	addr instance;
	instance_warning_condition(&instance);
	error_function(instance);
}

/* simple_warning (simple_condition warning) :format-control :format-arguments */
void instance_simple_warning(addr *ret, addr control, addr args)
{
	instance_condition2(ret, CONSTANT_CONDITION_SIMPLE_WARNING,
			CONSTANT_KEYWORD_FORMAT_CONTROL, control,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS, args);
}
void simple_warning(addr control, addr args)
{
	addr instance;
	instance_simple_warning(&instance, control, args);
	if (signal_function(instance)) {
		abort_debugger();
	}
}

/* storage_condition (serious_condition) */
void instance_storage_condition(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_STORAGE_CONDITION);
}
void storage_condition(void)
{
	addr instance;
	instance_storage_condition(&instance);
	error_function(instance);
}

/* arithmetic_error (error) :operation :operands */
void instance_arithmetic_error(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_ARITHMETIC_ERROR,
			CONSTANT_KEYWORD_OPERATION, operation,
			CONSTANT_KEYWORD_OPERANDS, operands);
}
void arithmetic_error(addr operation, addr operands)
{
	addr instance;
	instance_arithmetic_error(&instance, operation, operands);
	error_function(instance);
}
void arithmetic_error_operation(addr instance, addr *ret)
{
	addr key;
	GetConst(KEYWORD_OPERATION, &key);
	clos_value(instance, key, ret);
}
void arithmetic_error_operands(addr instance, addr *ret)
{
	addr key;
	GetConst(KEYWORD_OPERANDS, &key);
	clos_value(instance, key, ret);
}

/* floating_point_inexact (arithmetic_error) :operation :operands */
void instance_floating_point_inexact(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_INEXACT,
			CONSTANT_KEYWORD_OPERATION, operation,
			CONSTANT_KEYWORD_OPERANDS, operands);
}
void floating_point_inexact(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_inexact(&instance, operation, operands);
	error_function(instance);
}
void floating_point_inexact_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_inexact(operation, operands);
}
void floating_point_inexact_stdarg(constindex index, ...)
{
	addr operands;
	va_list args;

	va_start(args, index);
	list_alloc_stdarg(NULL, &operands, args);
	va_end(args);
	floating_point_inexact_constant(index, operands);
}

/* floating_point_invalid_operation (arithmetic_error) :operation :operands */
void instance_floating_point_invalid_operation(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONSTANT_KEYWORD_OPERATION, operation,
			CONSTANT_KEYWORD_OPERANDS, operands);
}
void floating_point_invalid_operation(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_invalid_operation(&instance, operation, operands);
	error_function(instance);
}
void floating_point_invalid_operation_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_invalid_operation(operation, operands);
}
void floating_point_invalid_operation_stdarg(constindex index, ...)
{
	addr operands;
	va_list args;

	va_start(args, index);
	list_alloc_stdarg(NULL, &operands, args);
	va_end(args);
	floating_point_invalid_operation_constant(index, operands);
}

/* floating_point_overflow (arithmetic_error) :operation :operands */
void instance_floating_point_overflow(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_OVERFLOW,
			CONSTANT_KEYWORD_OPERATION, operation,
			CONSTANT_KEYWORD_OPERANDS, operands);
}
void floating_point_overflow(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_overflow(&instance, operation, operands);
	error_function(instance);
}
void floating_point_overflow_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_overflow(operation, operands);
}
void floating_point_overflow_stdarg(constindex index, ...)
{
	addr operands;
	va_list args;

	va_start(args, index);
	list_alloc_stdarg(NULL, &operands, args);
	va_end(args);
	floating_point_overflow_constant(index, operands);
}

/* floating_point_underflow (arithmetic_error) :operation :operands */
void instance_floating_point_underflow(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_FLOATING_POINT_UNDERFLOW,
			CONSTANT_KEYWORD_OPERATION, operation,
			CONSTANT_KEYWORD_OPERANDS, operands);
}
void floating_point_underflow(addr operation, addr operands)
{
	addr instance;
	instance_floating_point_underflow(&instance, operation, operands);
	error_function(instance);
}
void floating_point_underflow_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	floating_point_underflow(operation, operands);
}
void floating_point_underflow_stdarg(constindex index, ...)
{
	addr operands;
	va_list args;

	va_start(args, index);
	list_alloc_stdarg(NULL, &operands, args);
	va_end(args);
	floating_point_underflow_constant(index, operands);
}

/* division_by_zero (arithmetic_error) :operation :operands */
void instance_division_by_zero(addr *ret, addr operation, addr operands)
{
	instance_condition2(ret, CONSTANT_CONDITION_DIVISION_BY_ZERO,
			CONSTANT_KEYWORD_OPERATION, operation,
			CONSTANT_KEYWORD_OPERANDS, operands);
}
void division_by_zero(addr operation, addr operands)
{
	addr instance;
	instance_division_by_zero(&instance, operation, operands);
	error_function(instance);
}
void division_by_zero_constant(constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	division_by_zero(operation, operands);
}
void division_by_zero_stdarg(constindex index, ...)
{
	addr operands;
	va_list args;

	va_start(args, index);
	list_alloc_stdarg(NULL, &operands, args);
	va_end(args);
	division_by_zero_constant(index, operands);
}

void division_by_zero_real1(constindex index, addr left)
{
	number_throw_heap(left, &left);
	list_heap(&left, left, NULL);
	division_by_zero_constant(index, left);
}

void division_by_zero_real2(constindex index, addr left, addr right)
{
	number_throw_heap(left, &left);
	number_throw_heap(right, &right);
	list_heap(&left, left, right, NULL);
	division_by_zero_constant(index, left);
}

void division_by_zero0(void)
{
	division_by_zero_constant(CONSTANT_COMMON_SLASH, Nil);
}

void division_by_zero1(addr left)
{
	division_by_zero_real1(CONSTANT_COMMON_SLASH, left);
}

void division_by_zero2(addr left, addr right)
{
	division_by_zero_real2(CONSTANT_COMMON_SLASH, left, right);
}


/* cell_error (error) :name */
static void instance_condition1(addr *ret, constindex index,
		constindex index1, addr pos1)
{
	addr instance, pos;

	GetConstant(index, &pos);
	make_instance_restrict_heap(pos, &instance);
	GetConstant(index1, &pos);
	setf_clos_value(instance, pos, pos1);
	*ret = instance;
}
void instance_cell_error(addr *ret, addr name)
{
	instance_condition1(ret, CONSTANT_CONDITION_CELL_ERROR,
			CONSTANT_KEYWORD_NAME, name);
}
void cell_error(addr name)
{
	addr instance;
	instance_cell_error(&instance, name);
	error_function(instance);
}
void cell_error_name(addr instance, addr *ret)
{
	addr key;
	GetConst(KEYWORD_NAME, &key);
	clos_value(instance, key, ret);
}

/* control_error (error) */
void instance_control_error(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_CONTROL_ERROR);
}
void control_error(void)
{
	addr instance;
	instance_control_error(&instance);
	error_function(instance);
}

/* stream_error (error) :stream */
void instance_stream_error(addr *ret, addr stream)
{
	instance_condition1(ret, CONSTANT_CONDITION_STREAM_ERROR,
			CONSTANT_KEYWORD_STREAM, stream);
}
void stream_error(addr stream)
{
	addr instance;
	instance_stream_error(&instance, stream);
	error_function(instance);
}
void stream_error_stream(addr instance, addr *ret)
{
	addr name;
	GetConst(KEYWORD_STREAM, &name);
	clos_value(instance, name, ret);
}

/* end_of_file (stream_error) :stream */
void instance_end_of_file(addr *ret, addr stream)
{
	instance_condition1(ret, CONSTANT_CONDITION_END_OF_FILE,
			CONSTANT_KEYWORD_STREAM, stream);
}
void end_of_file(addr stream)
{
	addr instance;
	instance_end_of_file(&instance, stream);
	error_function(instance);
}

/* reader_error (parse_error stream_error) :stream */
void instance_reader_error(addr *ret, addr stream)
{
	instance_condition1(ret, CONSTANT_CONDITION_READER_ERROR,
			CONSTANT_KEYWORD_STREAM, stream);
}
void reader_error(addr stream)
{
	addr instance;
	instance_reader_error(&instance, stream);
	error_function(instance);
}

/* file_error (error) :pathname */
void instance_file_error(addr *ret, addr pathname)
{
	instance_condition1(ret, CONSTANT_CONDITION_FILE_ERROR,
			CONSTANT_KEYWORD_PATHNAME, pathname);
}
void file_error(addr pathname)
{
	addr instance;
	instance_file_error(&instance, pathname);
	error_function(instance);
}
void file_error_pathname(addr *ret, addr instance)
{
	addr name;
	GetConst(KEYWORD_PATHNAME, &name);
	clos_value(instance, name, ret);
}

/* package_error (error) :package */
void instance_package_error(addr *ret, addr package)
{
	instance_condition1(ret, CONSTANT_CONDITION_PACKAGE_ERROR,
			CONSTANT_KEYWORD_PACKAGE, package);
}
void package_error(addr package)
{
	addr instance;
	instance_reader_error(&instance, package);
	error_function(instance);
}
void package_error_package(addr *ret, addr instance)
{
	addr name;
	GetConst(KEYWORD_PACKAGE, &name);
	clos_value(instance, name, ret);
}

/* parse_error (error) */
void instance_parse_error(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_PARSE_ERROR);
}
void parse_error(void)
{
	addr instance;
	instance_parse_error(&instance);
	error_function(instance);
}

/* print_not_readable (error) :object */
void instance_print_not_readable(addr *ret, addr object)
{
	instance_condition1(ret, CONSTANT_CONDITION_PRINT_NOT_READABLE,
			CONSTANT_KEYWORD_OBJECT, object);
}
void print_not_readable(addr object)
{
	addr instance;
	instance_reader_error(&instance, object);
	error_function(instance);
}

/* program_error (error) */
void instance_program_error(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_PROGRAM_ERROR);
}
void program_error(void)
{
	addr instance;
	instance_program_error(&instance);
	error_function(instance);
}

/* style_warning (warning) */
void instance_style_warning(addr *ret)
{
	instance_condition(ret, CONSTANT_CONDITION_STYLE_WARNING);
}
void style_warning(void)
{
	addr instance;
	instance_style_warning(&instance);
	error_function(instance);
}

/* type_error (error) :datum :expected-type */
void instance_type_error(addr *ret, addr datum, addr expected)
{
	instance_condition2(ret, CONSTANT_CONDITION_TYPE_ERROR,
			CONSTANT_KEYWORD_DATUM, datum,
			CONSTANT_KEYWORD_EXPECTED_TYPE, expected);
}
void type_error(addr datum, addr expected)
{
	addr instance;
	copylocal_object(NULL, &datum, datum);
	copylocal_object(NULL, &expected, expected);
	instance_type_error(&instance, datum, expected);
	error_function(instance);
}

void type_error_constant(addr datum, constindex expected)
{
	addr type;
	GetConstant(expected, &type);
	type_error(datum, type);
}

void type_error_datum(addr instance, addr *ret)
{
	addr key;
	GetConst(KEYWORD_DATUM, &key);
	clos_value(instance, key, ret);
}

void type_error_expected(addr instance, addr *ret)
{
	addr key;
	GetConst(KEYWORD_EXPECTED_TYPE, &key);
	clos_value(instance, key, ret);
}

int typep_error(addr value, addr type)
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

int typep_asterisk_error(addr value, addr type)
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

/* simple_type_error (simple_condition type_error)
 *   :format-control :format-arguments :datum :expected-type */
static void instance_condition4(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2,
		constindex index3, addr pos3,
		constindex index4, addr pos4)
{
	addr instance, pos;

	GetConstant(index, &pos);
	make_instance_restrict_heap(pos, &instance);
	GetConstant(index1, &pos);
	setf_clos_value(instance, pos, pos1);
	GetConstant(index2, &pos);
	setf_clos_value(instance, pos, pos2);
	GetConstant(index3, &pos);
	setf_clos_value(instance, pos, pos3);
	GetConstant(index4, &pos);
	setf_clos_value(instance, pos, pos4);
	*ret = instance;
}
void instance_simple_type_error(addr *ret,
		addr control, addr args, addr datum, addr expected)
{
	instance_condition4(ret, CONSTANT_CONDITION_SIMPLE_TYPE_ERROR,
			CONSTANT_KEYWORD_FORMAT_CONTROL, control,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS, args,
			CONSTANT_KEYWORD_DATUM, datum,
			CONSTANT_KEYWORD_EXPECTED_TYPE, expected);
}
void simple_type_error(addr control, addr args, addr datum, addr expected)
{
	addr instance;
	instance_simple_type_error(&instance, control, args, datum, expected);
	error_function(instance);
}

void type_error_stdarg(addr datum, addr expected, const char *fmt, ...)
{
	addr control, list;
	va_list args;

	strvect_char_heap(&control, fmt);
	va_start(args, fmt);
	copylocal_list_stdarg(NULL, &list, args);
	va_end(args);
	copylocal_object(NULL, &datum, datum);
	copylocal_object(NULL, &expected, expected);
	simple_type_error(control, list, datum, expected);
}

void type_error_fill_pointer(addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	type_error_stdarg(datum, expected,
			"The vector ~S don't have a fill-pointer.", datum, NULL);
}

void type_error_fill_pointer_zero(addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	type_error_stdarg(datum, expected,
			"The vector ~S fill-pointer is 0.", datum, NULL);
}

void type_error_adjustable(addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	type_error_stdarg(datum, expected,
			"The vector ~S is not adjustable.", datum, NULL);
}

/* unbound_slot (cell_error) :instance :name */
void instance_unbound_slot(addr *ret, addr instance, addr name)
{
	instance_condition2(ret, CONSTANT_CONDITION_UNBOUND_SLOT,
			CONSTANT_KEYWORD_INSTANCE, instance,
			CONSTANT_KEYWORD_NAME, name);
}
void unbound_slot(addr argument, addr name)
{
	addr instance;
	instance_unbound_slot(&instance, argument, name);
	error_function(instance);
}

/* unbound_variable (cell_error) :name */
void instance_unbound_variable(addr *ret, addr name)
{
	instance_condition1(ret, CONSTANT_CONDITION_UNBOUND_VARIABLE,
			CONSTANT_KEYWORD_NAME, name);
}
void unbound_variable(addr name)
{
	addr instance;
	instance_unbound_variable(&instance, name);
	error_function(instance);
}

/* undefined_function (cell_error) :name */
void instance_undefined_function(addr *ret, addr name)
{
	instance_condition1(ret, CONSTANT_CONDITION_UNDEFINED_FUNCTION,
			CONSTANT_KEYWORD_NAME, name);
}
void undefined_function(addr name)
{
	addr instance;
	instance_undefined_function(&instance, name);
	error_function(instance);
}
void undefined_function_setf(addr name)
{
	addr setf;
	GetConst(COMMON_SETF, &setf);
	list_heap(&name, setf, name, NULL);
	undefined_function(name);
}


/*
 *  build_condition
 */
static void make_slots2(addr *ret,
		constindex index1, constindex index2)
{
	addr slots;

	vector4_heap(&slots, 2);
	default_slot_name(slots, 0, index1);
	default_slot_name(slots, 1, index2);
	set_slots_localtion(slots);
	*ret = slots;
}

static void make_slots1(addr *ret, constindex index)
{
	addr slots;

	vector4_heap(&slots, 1);
	default_slot_name(slots, 0, index);
	set_slots_localtion(slots);
	*ret = slots;
}

void make_default_condition(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* serious_condition (condition) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_SERIOUS_CONDITION,
			CONSTANT_COMMON_CONDITION,
			CONSTANT_EMPTY);
	/* simple_condition (condition) :format-control :format-arguments*/
	make_slots2(&slots,
			CONSTANT_KEYWORD_FORMAT_CONTROL,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_SIMPLE_CONDITION,
			CONSTANT_COMMON_CONDITION);
	/* warning (condition) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_WARNING,
			CONSTANT_COMMON_CONDITION,
			CONSTANT_EMPTY);
	/* error (serious_condition) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_ERROR,
			CONSTANT_COMMON_SERIOUS_CONDITION,
			CONSTANT_EMPTY);
	/* storage_condition (serious_condition) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_STORAGE_CONDITION,
			CONSTANT_COMMON_SERIOUS_CONDITION,
			CONSTANT_EMPTY);
	/* arithmetic_error (error) :operation :operands */
	make_slots2(&slots, CONSTANT_KEYWORD_OPERATION, CONSTANT_KEYWORD_OPERANDS);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_ARITHMETIC_ERROR,
			CONSTANT_COMMON_ERROR);
	/* cell_error (error) :name */
	make_slots1(&slots, CONSTANT_KEYWORD_NAME);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_CELL_ERROR,
			CONSTANT_COMMON_ERROR);
	/* control_error (error) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_CONTROL_ERROR,
			CONSTANT_COMMON_ERROR,
			CONSTANT_EMPTY);
	/* file_error (error) :pathname */
	make_slots1(&slots, CONSTANT_KEYWORD_PATHNAME);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_FILE_ERROR,
			CONSTANT_COMMON_ERROR);
	/* package_error (error) :package */
	make_slots1(&slots, CONSTANT_KEYWORD_PACKAGE);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_PACKAGE_ERROR,
			CONSTANT_COMMON_ERROR);
	/* parse_error (error) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_PARSE_ERROR,
			CONSTANT_COMMON_ERROR,
			CONSTANT_EMPTY);
	/* print_not_readable (error) :object */
	make_slots1(&slots, CONSTANT_KEYWORD_OBJECT);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_PRINT_NOT_READABLE,
			CONSTANT_COMMON_ERROR);
	/* program_error (error) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_PROGRAM_ERROR,
			CONSTANT_COMMON_ERROR,
			CONSTANT_EMPTY);
	/* stream_error (error) :stream */
	make_slots1(&slots, CONSTANT_KEYWORD_STREAM);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_STREAM_ERROR,
			CONSTANT_COMMON_ERROR);
	/* type_error (error) :datum :expected-type */
	make_slots2(&slots, CONSTANT_KEYWORD_DATUM, CONSTANT_KEYWORD_EXPECTED_TYPE);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_TYPE_ERROR,
			CONSTANT_COMMON_ERROR);
	/* unbound_slot (cell_error) :instance :name */
	make_slots1(&slots, CONSTANT_KEYWORD_INSTANCE);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_UNBOUND_SLOT,
			CONSTANT_COMMON_CELL_ERROR);
	/* unbound_variable (cell_error) :name */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_UNBOUND_VARIABLE,
			CONSTANT_COMMON_CELL_ERROR,
			CONSTANT_EMPTY);
	/* undefined_function (cell_error) :name */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_UNDEFINED_FUNCTION,
			CONSTANT_COMMON_CELL_ERROR,
			CONSTANT_EMPTY);
	/* style_warning (warning) */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_STYLE_WARNING,
			CONSTANT_COMMON_WARNING,
			CONSTANT_EMPTY);
	/* simple_error (simple_condition error) :format-control :format-arguments */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_SIMPLE_ERROR,
			CONSTANT_COMMON_SIMPLE_CONDITION,
			CONSTANT_COMMON_ERROR,
			CONSTANT_EMPTY);
	/* simple_type_error (simple_condition type_error)
	 *   :format-control :format-arguments :datum :expected-type */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_SIMPLE_TYPE_ERROR,
			CONSTANT_COMMON_SIMPLE_CONDITION,
			CONSTANT_COMMON_TYPE_ERROR,
			CONSTANT_EMPTY);
	/* simple_warning (simple_condition warning) :format-control :format-arguments */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_SIMPLE_WARNING,
			CONSTANT_COMMON_SIMPLE_CONDITION,
			CONSTANT_COMMON_WARNING,
			CONSTANT_EMPTY);
	/* division_by_zero (arithmetic_error) :operation :operands */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_DIVISION_BY_ZERO,
			CONSTANT_COMMON_ARITHMETIC_ERROR,
			CONSTANT_EMPTY);
	/* floating_point_inexact (arithmetic_error) :operation :operands */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_FLOATING_POINT_INEXACT,
			CONSTANT_COMMON_ARITHMETIC_ERROR,
			CONSTANT_EMPTY);
	/* floating_point_invalid_operation (arithmetic_error) :operation :operands */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_FLOATING_POINT_INVALID_OPERATION,
			CONSTANT_COMMON_ARITHMETIC_ERROR,
			CONSTANT_EMPTY);
	/* floating_point_overflow (arithmetic_error) :operation :operands */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_FLOATING_POINT_OVERFLOW,
			CONSTANT_COMMON_ARITHMETIC_ERROR,
			CONSTANT_EMPTY);
	/* floating_point_underflow (arithmetic_error) :operation :operands */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_FLOATING_POINT_UNDERFLOW,
			CONSTANT_COMMON_ARITHMETIC_ERROR,
			CONSTANT_EMPTY);
	/* end_of_file (stream_error) :stream */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_END_OF_FILE,
			CONSTANT_COMMON_STREAM_ERROR,
			CONSTANT_EMPTY);
	/* reader_error (parse_error stream_error) :stream */
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_READER_ERROR,
			CONSTANT_COMMON_PARSE_ERROR,
			CONSTANT_COMMON_STREAM_ERROR,
			CONSTANT_EMPTY);
}

static void build_variables(void)
{
	addr symbol;

	/* common-lisp::*break-on-signals* */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &symbol);
	SetValueSymbol(symbol, Nil);

	/* debugger */
	GetConst(SYSTEM_ENABLE_DEBUGGER, &symbol);
	SetValueSymbol(symbol, T);

	/* interactive */
	GetConst(SYSTEM_ENABLE_INTERACTIVE, &symbol);
	SetValueSymbol(symbol, T);
}

#define findsetcondition(x) \
	find_class_constant(CONSTANT_COMMON_##x, CONSTANT_CONDITION_##x)

void build_condition(Execute ptr)
{
	/* conditions */
	make_default_condition(ptr);
	findsetcondition(ARITHMETIC_ERROR);
	findsetcondition(CELL_ERROR);
	findsetcondition(CONTROL_ERROR);
	findsetcondition(DIVISION_BY_ZERO);
	findsetcondition(END_OF_FILE);
	findsetcondition(ERROR);
	findsetcondition(FILE_ERROR);
	findsetcondition(FLOATING_POINT_INEXACT);
	findsetcondition(FLOATING_POINT_INVALID_OPERATION);
	findsetcondition(FLOATING_POINT_OVERFLOW);
	findsetcondition(FLOATING_POINT_UNDERFLOW);
	findsetcondition(PACKAGE_ERROR);
	findsetcondition(PARSE_ERROR);
	findsetcondition(PRINT_NOT_READABLE);
	findsetcondition(PROGRAM_ERROR);
	findsetcondition(READER_ERROR);
	findsetcondition(SERIOUS_CONDITION);
	findsetcondition(SIMPLE_CONDITION);
	findsetcondition(SIMPLE_ERROR);
	findsetcondition(SIMPLE_TYPE_ERROR);
	findsetcondition(SIMPLE_WARNING);
	findsetcondition(STORAGE_CONDITION);
	findsetcondition(STREAM_ERROR);
	findsetcondition(STYLE_WARNING);
	findsetcondition(TYPE_ERROR);
	findsetcondition(UNBOUND_SLOT);
	findsetcondition(UNBOUND_VARIABLE);
	findsetcondition(UNDEFINED_FUNCTION);
	findsetcondition(WARNING);
	/* variables */
	build_variables();
	set_enable_debugger(1);
}

