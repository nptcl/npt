#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "copy.h"
#include "number.h"
#include "strvect.h"
#include "type_copy.h"
#include "type_table.h"
#include "type_typep.h"

static int instance_condition_(addr *ret, constindex condition)
{
	addr pos;
	GetConstant(condition, &pos);
	return clos_instance_heap_(pos, ret);
}
static int instance_condition1_(addr *ret, constindex index,
		constindex index1, addr pos1)
{
	addr instance;

	GetConstant(index, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, index1, pos1));
	return Result(ret, instance);
}
static int instance_condition2_(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2)
{
	addr instance;

	GetConstant(index, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, index1, pos1));
	Return(clos_setconst_(instance, index2, pos2));
	return Result(ret, instance);
}
static int instance_condition4_(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2,
		constindex index3, addr pos3,
		constindex index4, addr pos4)
{
	addr instance;

	GetConstant(index, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, index1, pos1));
	Return(clos_setconst_(instance, index2, pos2));
	Return(clos_setconst_(instance, index3, pos3));
	Return(clos_setconst_(instance, index4, pos4));
	return Result(ret, instance);
}

/* for debug */
static void instance_condition(addr *ret, constindex condition)
{
	*ret = Nil;
	Error(instance_condition_(ret, condition));
}
static void instance_condition1(addr *ret, constindex index,
		constindex index1, addr pos1)
{
	*ret = Nil;
	Error(instance_condition1_(ret, index, index1, pos1));
}
static void instance_condition2(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2)
{
	*ret = Nil;
	Error(instance_condition2_(ret, index, index1, pos1, index2, pos2));
}
static void instance_condition4(addr *ret, constindex index,
		constindex index1, addr pos1,
		constindex index2, addr pos2,
		constindex index3, addr pos3,
		constindex index4, addr pos4)
{
	*ret = Nil;
	Error(instance_condition4_(ret, index,
				index1, pos1, index2, pos2,
				index3, pos3, index4, pos4));
}

/* serious_condition (condition) */
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
	return signal_function_(Execute_Thread, instance);
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

_g int instance_simple_error_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}
_g int call_simple_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_error_(&instance, control, args));
	return error_function_(ptr, instance);
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
	return signal_function_(Execute_Thread, instance);
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
	list_stdarg_alloc(NULL, &operands, va);
	va_end(va);
	floating_point_inexact_constant(index, operands);
}

_g int instance_floating_point_inexact_(addr *ret, addr operation, addr operands)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_INEXACT,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g int call_floating_point_inexact_(Execute ptr, addr operation, addr operands)
{
	addr instance;
	Return(instance_floating_point_inexact_(&instance, operation, operands));
	return error_function_(ptr, instance);
}
_g int call_floating_point_inexact_const_(Execute ptr, constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	return call_floating_point_inexact_(ptr, operation, operands);
}
_g int call_floating_point_inexact_va_(Execute ptr, constindex index, ...)
{
	addr operands;
	va_list va;

	va_start(va, index);
	list_stdarg_alloc(NULL, &operands, va);
	va_end(va);
	return call_floating_point_inexact_const_(ptr, index, operands);
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
	list_stdarg_alloc(NULL, &operands, va);
	va_end(va);
	floating_point_invalid_operation_constant(index, operands);
}

static int instance_float_invalid_(addr *ret, addr operation, addr operands)
{
	return instance_condition2_(ret,
			CONSTANT_CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}

_g int call_float_invalid_(Execute ptr, addr operation, addr operands)
{
	addr instance;
	Return(instance_float_invalid_(&instance, operation, operands));
	return error_function_(ptr, instance);
}

_g int call_float_invalid_const_(Execute ptr, constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	return call_float_invalid_(ptr, operation, operands);
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
	list_stdarg_alloc(NULL, &operands, va);
	va_end(va);
	floating_point_overflow_constant(index, operands);
}

static int instance_floating_point_overflow_(addr *ret, addr operation, addr operands)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_OVERFLOW,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g int call_floating_point_overflow_(Execute ptr, addr operation, addr operands)
{
	addr instance;
	Return(instance_floating_point_overflow_(&instance, operation, operands));
	return error_function_(ptr, instance);
}
_g int call_floating_point_overflow_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_floating_point_overflow_(ptr, pos, list);
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
	list_stdarg_alloc(NULL, &operands, va);
	va_end(va);
	floating_point_underflow_constant(index, operands);
}

static int instance_floating_point_underflow_(addr *ret, addr operation, addr operands)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_UNDERFLOW,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g int call_floating_point_underflow_(Execute ptr, addr operation, addr operands)
{
	addr instance;
	Return(instance_floating_point_underflow_(&instance, operation, operands));
	return error_function_(ptr, instance);
}

_g int call_floating_point_underflow_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_floating_point_underflow_(ptr, pos, list);
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
	list_stdarg_alloc(NULL, &operands, va);
	va_end(va);
	division_by_zero_constant(index, operands);
}

_g void division_by_zero_real1(constindex index, addr left)
{
	Error(number_throw_heap_(left, &left));
	list_heap(&left, left, NULL);
	division_by_zero_constant(index, left);
}

_g void division_by_zero_real2(constindex index, addr left, addr right)
{
	Error(number_throw_heap_(left, &left));
	Error(number_throw_heap_(right, &right));
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


_g int instance_division_by_zero_(addr *ret, addr operation, addr operands)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_DIVISION_BY_ZERO,
			CONSTANT_CLOSNAME_OPERATION, operation,
			CONSTANT_CLOSNAME_OPERANDS, operands);
}
_g int call_division_by_zero_(Execute ptr, addr operation, addr operands)
{
	addr instance;
	Return(instance_division_by_zero_(&instance, operation, operands));
	return error_function_(ptr, instance);
}
_g int call_division_by_zero_const_(Execute ptr, constindex index, addr operands)
{
	addr operation;
	GetConstant(index, &operation);
	return call_division_by_zero_(ptr, operation, operands);
}
_g int call_division_by_zero_real1_(Execute ptr, constindex index, addr x)
{
	Return(number_throw_heap_(x, &x));
	list_heap(&x, x, NULL);
	return call_division_by_zero_const_(ptr, index, x);
}
_g int call_division_by_zero_real2_(Execute ptr, constindex index, addr x, addr y)
{
	Return(number_throw_heap_(x, &x));
	Return(number_throw_heap_(y, &y));
	list_heap(&x, x, y, NULL);
	return call_division_by_zero_const_(ptr, index, x);
}
_g int call_division_by_zero1_(Execute ptr, addr left)
{
	return call_division_by_zero_real1_(ptr, CONSTANT_COMMON_SLASH, left);
}
_g int call_division_by_zero2_(Execute ptr, addr left, addr right)
{
	return call_division_by_zero_real2_(ptr, CONSTANT_COMMON_SLASH, left, right);
}


/* cell_error (error) :name */
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

_g int instance_control_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_CONTROL_ERROR);
}
_g int call_control_error_(Execute ptr)
{
	addr instance;
	Return(instance_control_error_(&instance));
	return error_function_(ptr, instance);
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

_g int instance_end_of_file_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_END_OF_FILE,
			CONSTANT_CLOSNAME_STREAM, stream);
}
_g int call_end_of_file_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_end_of_file_(&instance, stream));
	return error_function_(ptr, instance);
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

_g int instance_file_error_(addr *ret, addr pathname)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_FILE_ERROR,
			CONSTANT_CLOSNAME_PATHNAME, pathname);
}
_g int call_file_error_(Execute ptr, addr pathname)
{
	addr instance;
	Return(instance_file_error_(&instance, pathname));
	return error_function_(ptr, instance);
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

_g int typep_error(Execute ptr, addr value, addr type)
{
	int check;

	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		type_error(value, type);
	}

	return 0;
}

_g int typep_asterisk_error(Execute ptr, addr value, addr type)
{
	int check;

	Return(typep_asterisk_clang_(ptr, value, type, &check));
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		type_error(value, type);
	}

	return 0;
}

_g int typep_typetable(Execute ptr, addr value, enum TypeTable type)
{
	addr pos;
	gettypetable(type, &pos);
	return typep_asterisk_error(ptr, value, pos);
}

_g int typep_unbound_error(Execute ptr, addr value, addr type)
{
	return (value == Unbound)? 0: typep_error(ptr, value, type);
}

_g int instance_type_error_(addr *ret, addr datum, addr expected)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_TYPE_ERROR,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}
_g int call_type_error_(Execute ptr, addr datum, addr expected)
{
	addr instance;

	copyheap(&datum, datum);
	copyheap(&expected, expected);
	Return(instance_type_error_(&instance, datum, expected));
	return error_function_(ptr, instance);
}
_g int call_type_error_const_(Execute ptr, addr datum, constindex expected)
{
	addr type;
	GetConstant(expected, &type);
	return call_type_error_(ptr, datum, type);
}

/* simple_type_error (simple_condition type_error)
 *   :format-control :format-arguments :datum :expected-type */
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

_g int instance_simple_type_error_(addr *ret,
		addr control, addr args, addr datum, addr expected)
{
	return instance_condition4_(ret, CONSTANT_CONDITION_SIMPLE_TYPE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}
_g int call_simple_type_error_(Execute ptr,
		addr control, addr args, addr datum, addr expected)
{
	addr instance;
	Return(instance_simple_type_error_(&instance, control, args, datum, expected));
	return error_function_(ptr, instance);
}
_g int call_type_error_va_(Execute ptr,
		addr datum, addr expected, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	copylocal_object(NULL, &datum, datum);
	copylocal_object(NULL, &expected, expected);
	return call_simple_type_error_(ptr, control, args, datum, expected);
}
_g int call_type_error_fill_pointer_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_ARRAY, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S don't have a fill-pointer.", datum, NULL);
}
_g int call_type_error_fill_pointer_zero_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S fill-pointer is 0.", datum, NULL);
}
_g int call_type_error_adjustable_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	return call_type_error_va_(ptr, datum, expected,
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

_g int instance_unbound_slot_(addr *ret, addr instance, addr name)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_UNBOUND_SLOT,
			CONSTANT_CLOSNAME_INSTANCE, instance,
			CONSTANT_CLOSNAME_NAME, name);
}
_g int call_unbound_slot_(Execute ptr, addr argument, addr name)
{
	addr instance;
	Return(instance_unbound_slot_(&instance, argument, name));
	return error_function_(ptr, instance);
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

_g int instance_unbound_variable_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_UNBOUND_VARIABLE,
			CONSTANT_CLOSNAME_NAME, name);
}
_g int call_unbound_variable_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_unbound_variable_(&instance, name));
	return error_function_(ptr, instance);
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

_g int instance_undefined_function_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_UNDEFINED_FUNCTION,
			CONSTANT_CLOSNAME_NAME, name);
}
_g int call_undefined_function_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_undefined_function_(&instance, name));
	return error_function_(ptr, instance);
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
_g int instance_simple_file_error_(addr *ret, addr pathname, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_FILE_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_PATHNAME, pathname));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

_g void instance_simple_file_error(addr *ret, addr pathname, addr control, addr args)
{
	*ret = Nil;
	Error(instance_simple_file_error_(ret, pathname, control, args));
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

_g int call_simple_file_error_(Execute ptr, addr pathname, addr control, addr args)
{
	addr instance;
	Return(instance_simple_file_error_(&instance, pathname, control, args));
	return error_function_(ptr, instance);
}

_g int call_simple_file_error_va_(Execute ptr, addr pathname, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	copylocal_object(NULL, &pathname, pathname);
	return call_simple_file_error_(ptr, pathname, control, args);
}

