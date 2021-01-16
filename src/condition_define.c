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


/* serious_condition (condition) */
int instance_serious_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_SERIOUS_CONDITION);
}

int call_serious_condition_(Execute ptr)
{
	addr instance;
	Return(instance_serious_condition_(&instance));
	return error_function_(ptr, instance);
}


/* simple_condition (condition) :format-control :format-arguments*/
int instance_simple_condition_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_CONDITION,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}

int call_simple_condition_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_condition_(&instance, control, args));
	return signal_function_(ptr, instance);
}

int simple_condition_format_(addr condition, addr *control, addr *arguments)
{
	Return(ClosCheckConst_(condition, CLOSNAME_FORMAT_CONTROL, control));
	Return(ClosCheckConst_(condition, CLOSNAME_FORMAT_ARGUMENTS, arguments));
	return 0;
}

int simple_condition_format_control_(addr condition, addr *ret)
{
	return ClosCheckConst_(condition, CLOSNAME_FORMAT_CONTROL, ret);
}

int simple_condition_format_arguments_(addr condition, addr *ret)
{
	return ClosCheckConst_(condition, CLOSNAME_FORMAT_ARGUMENTS, ret);
}


/* simple_error (simple_condition) :format-control :format-arguments */
int instance_simple_error_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}

int call_simple_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_error_(&instance, control, args));
	return error_function_(ptr, instance);
}


/* error (serious_condition) */
int instance_error_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_ERROR);
}

int call_error_condition_(Execute ptr)
{
	addr instance;
	Return(instance_error_condition_(&instance));
	return error_function_(ptr, instance);
}


/* warning (condition) */
int instance_warning_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_WARNING);
}

int call_warning_condition_(Execute ptr)
{
	addr instance;
	Return(instance_warning_condition_(&instance));
	return error_function_(ptr, instance);
}


/* simple_warning (simple_condition warning) :format-control :format-arguments */
int instance_simple_warning_(addr *ret, addr control, addr args)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_SIMPLE_WARNING,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args);
}

int call_simple_warning_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_warning_(&instance, control, args));
	return signal_function_(ptr, instance);
}


/* storage_condition (serious_condition) */
int instance_storage_condition_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_STORAGE_CONDITION);
}

int call_storage_condition_(Execute ptr)
{
	addr instance;
	Return(instance_storage_condition_(&instance));
	return error_function_(ptr, instance);
}


/* arithmetic_error (error) :operation :operands */
int instance_arithmetic_error_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_ARITHMETIC_ERROR,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_arithmetic_error_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_arithmetic_error_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int arithmetic_error_operation_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_OPERATION, ret);
}

int arithmetic_error_operands_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_OPERANDS, ret);
}


/* floating_point_inexact (arithmetic_error) :operation :operands */
int instance_float_inexact_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_INEXACT,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_inexact_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_inexact_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_inexact_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_inexact_(ptr, pos, list);
}

int call_float_inexact_va_(Execute ptr, constindex id, ...)
{
	addr list;
	va_list va;

	va_start(va, id);
	list_stdarg_alloc(NULL, &list, va);
	va_end(va);
	return call_float_inexact_const_(ptr, id, list);
}


/* floating_point_invalid_operation (arithmetic_error) :operation :operands */
int instance_float_invalid_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret,
			CONSTANT_CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_invalid_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_invalid_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_invalid_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_invalid_(ptr, pos, list);
}


/* floating_point_overflow (arithmetic_error) :operation :operands */
int instance_float_overflow_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_OVERFLOW,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_overflow_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_overflow_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_overflow_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_overflow_(ptr, pos, list);
}

int call_float_overflow_va_(Execute ptr, constindex id, ...)
{
	addr list;
	va_list va;

	va_start(va, id);
	copylocal_list_stdarg(NULL, &list, va);
	va_end(va);
	return call_float_overflow_const_(ptr, id, list);
}


/* floating_point_underflow (arithmetic_error) :operation :operands */
int instance_float_underflow_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_FLOATING_POINT_UNDERFLOW,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_float_underflow_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_float_underflow_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_float_underflow_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_float_underflow_(ptr, pos, list);
}

int call_float_underflow_va_(Execute ptr, constindex id, ...)
{
	addr list;
	va_list va;

	va_start(va, id);
	copylocal_list_stdarg(NULL, &list, va);
	va_end(va);
	return call_float_underflow_const_(ptr, id, list);
}


/* division_by_zero (arithmetic_error) :operation :operands */
int instance_division_by_zero_(addr *ret, addr pos, addr list)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_DIVISION_BY_ZERO,
			CONSTANT_CLOSNAME_OPERATION, pos,
			CONSTANT_CLOSNAME_OPERANDS, list);
}

int call_division_by_zero_(Execute ptr, addr pos, addr list)
{
	addr instance;
	Return(instance_division_by_zero_(&instance, pos, list));
	return error_function_(ptr, instance);
}

int call_division_by_zero_const_(Execute ptr, constindex id, addr list)
{
	addr pos;
	GetConstant(id, &pos);
	return call_division_by_zero_(ptr, pos, list);
}

int call_division_by_zero_real1_(Execute ptr, constindex id, addr x)
{
	Return(number_throw_heap_(x, &x));
	list_heap(&x, x, NULL);
	return call_division_by_zero_const_(ptr, id, x);
}

int call_division_by_zero_real2_(Execute ptr, constindex id, addr x, addr y)
{
	Return(number_throw_heap_(x, &x));
	Return(number_throw_heap_(y, &y));
	list_heap(&x, x, y, NULL);
	return call_division_by_zero_const_(ptr, id, x);
}

int call_division_by_zero1_(Execute ptr, addr left)
{
	return call_division_by_zero_real1_(ptr, CONSTANT_COMMON_SLASH, left);
}

int call_division_by_zero2_(Execute ptr, addr left, addr right)
{
	return call_division_by_zero_real2_(ptr, CONSTANT_COMMON_SLASH, left, right);
}


/* cell_error (error) :name */
int instance_cell_error_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_CELL_ERROR,
			CONSTANT_CLOSNAME_NAME, name);
}

int call_cell_error_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_cell_error_(&instance, name));
	return error_function_(ptr, instance);
}

int cell_error_name_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_NAME, ret);
}


/* control_error (error) */
int instance_control_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_CONTROL_ERROR);
}

int call_control_error_(Execute ptr)
{
	addr instance;
	Return(instance_control_error_(&instance));
	return error_function_(ptr, instance);
}


/* stream_error (error) :stream */
int instance_stream_error_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_STREAM_ERROR,
			CONSTANT_CLOSNAME_STREAM, stream);
}

int call_stream_error_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_stream_error_(&instance, stream));
	return error_function_(ptr, instance);
}

int stream_error_stream_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_STREAM, ret);
}


/* end_of_file (stream_error) :stream */
int instance_end_of_file_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_END_OF_FILE,
			CONSTANT_CLOSNAME_STREAM, stream);
}

int call_end_of_file_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_end_of_file_(&instance, stream));
	return error_function_(ptr, instance);
}


/* reader_error (parse_error stream_error) :stream */
int instance_reader_error_(addr *ret, addr stream)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_READER_ERROR,
			CONSTANT_CLOSNAME_STREAM, stream);
}

int call_reader_error_(Execute ptr, addr stream)
{
	addr instance;
	Return(instance_reader_error_(&instance, stream));
	return error_function_(ptr, instance);
}


/* file_error (error) :pathname */
int instance_file_error_(addr *ret, addr pathname)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_FILE_ERROR,
			CONSTANT_CLOSNAME_PATHNAME, pathname);
}

int call_file_error_(Execute ptr, addr pathname)
{
	addr instance;
	Return(instance_file_error_(&instance, pathname));
	return error_function_(ptr, instance);
}

int file_error_pathname_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_PATHNAME, ret);
}


/* package_error (error) :package */
int instance_package_error_(addr *ret, addr package)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_PACKAGE_ERROR,
			CONSTANT_CLOSNAME_PACKAGE, package);
}

int call_package_error_(Execute ptr, addr package)
{
	addr instance;
	Return(instance_reader_error_(&instance, package));
	return error_function_(ptr, instance);
}

int package_error_package_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_PACKAGE, ret);
}


/* parse_error (error) */
int instance_parse_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_PARSE_ERROR);
}

int call_parse_error_(Execute ptr)
{
	addr instance;
	Return(instance_parse_error_(&instance));
	return error_function_(ptr, instance);
}


/* print_not_readable (error) :object */
int instance_print_not_readable_(addr *ret, addr object)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_PRINT_NOT_READABLE,
			CONSTANT_CLOSNAME_OBJECT, object);
}

int call_print_not_readable_(Execute ptr, addr object)
{
	addr instance;
	Return(instance_reader_error_(&instance, object));
	return error_function_(ptr, instance);
}

int print_not_readable_object_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_OBJECT, ret);
}


/* program_error (error) */
int instance_program_error_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_PROGRAM_ERROR);
}

int call_program_error_(Execute ptr)
{
	addr instance;
	Return(instance_program_error_(&instance));
	return error_function_(ptr, instance);
}


/* style_warning (warning) */
int instance_style_warning_(addr *ret)
{
	return instance_condition_(ret, CONSTANT_CONDITION_STYLE_WARNING);
}

int call_style_warning_(Execute ptr)
{
	addr instance;
	Return(instance_style_warning_(&instance));
	return error_function_(ptr, instance);
}


/* type_error (error) :datum :expected-type */
int instance_type_error_(addr *ret, addr datum, addr expected)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_TYPE_ERROR,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}

int call_type_error_(Execute ptr, addr datum, addr expected)
{
	addr instance;

	copyheap(&datum, datum);
	copyheap(&expected, expected);
	Return(instance_type_error_(&instance, datum, expected));
	return error_function_(ptr, instance);
}

int call_type_error_const_(Execute ptr, addr datum, constindex expected)
{
	addr type;
	GetConstant(expected, &type);
	return call_type_error_(ptr, datum, type);
}

int type_error_datum_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_DATUM, ret);
}

int type_error_expected_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_EXPECTED_TYPE, ret);
}

int call_typep_error_(Execute ptr, addr value, addr type)
{
	int check;

	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		return call_type_error_(ptr, value, type);
	}

	return 0;
}

int call_typep_asterisk_error_(Execute ptr, addr value, addr type)
{
	int check;

	Return(typep_asterisk_clang_(ptr, value, type, &check));
	if (! check) {
		copyheap(&value, value);
		type_copy_heap(&type, type);
		return call_type_error_(ptr, value, type);
	}

	return 0;
}

int call_typep_unbound_error_(Execute ptr, addr value, addr type)
{
	return (value == Unbound)? 0: call_typep_error_(ptr, value, type);
}


/* simple_type_error (simple_condition type_error)
 *   :format-control :format-arguments :datum :expected-type */
int instance_simple_type_error_(addr *ret,
		addr control, addr args, addr datum, addr expected)
{
	return instance_condition4_(ret, CONSTANT_CONDITION_SIMPLE_TYPE_ERROR,
			CONSTANT_CLOSNAME_FORMAT_CONTROL, control,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args,
			CONSTANT_CLOSNAME_DATUM, datum,
			CONSTANT_CLOSNAME_EXPECTED_TYPE, expected);
}

int call_simple_type_error_(Execute ptr,
		addr control, addr args, addr datum, addr expected)
{
	addr instance;
	Return(instance_simple_type_error_(&instance, control, args, datum, expected));
	return error_function_(ptr, instance);
}

int call_type_error_va_(Execute ptr,
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

int call_type_error_fill_pointer_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_ARRAY, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S don't have a fill-pointer.", datum, NULL);
}

int call_type_error_fill_pointer_zero_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S fill-pointer is 0.", datum, NULL);
}

int call_type_error_adjustable_(Execute ptr, addr datum)
{
	addr expected;
	GetConst(COMMON_VECTOR, &expected);
	return call_type_error_va_(ptr, datum, expected,
			"The vector ~S is not adjustable.", datum, NULL);
}


/* unbound_slot (cell_error) :instance :name */
int instance_unbound_slot_(addr *ret, addr instance, addr name)
{
	return instance_condition2_(ret, CONSTANT_CONDITION_UNBOUND_SLOT,
			CONSTANT_CLOSNAME_INSTANCE, instance,
			CONSTANT_CLOSNAME_NAME, name);
}

int unbound_slot_instance_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_INSTANCE, ret);
}

int call_unbound_slot_(Execute ptr, addr argument, addr name)
{
	addr instance;
	Return(instance_unbound_slot_(&instance, argument, name));
	return error_function_(ptr, instance);
}


/* unbound_variable (cell_error) :name */
int instance_unbound_variable_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_UNBOUND_VARIABLE,
			CONSTANT_CLOSNAME_NAME, name);
}

int call_unbound_variable_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_unbound_variable_(&instance, name));
	return error_function_(ptr, instance);
}


/* undefined_function (cell_error) :name */
int instance_undefined_function_(addr *ret, addr name)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_UNDEFINED_FUNCTION,
			CONSTANT_CLOSNAME_NAME, name);
}

int call_undefined_function_(Execute ptr, addr name)
{
	addr instance;
	Return(instance_undefined_function_(&instance, name));
	return error_function_(ptr, instance);
}


/* savecore */
int instance_savecore_condition_(addr *ret, addr file)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_SAVECORE,
			CONSTANT_CLOSNAME_PATHNAME, file);
}

int call_savecore_condition_(Execute ptr, addr file)
{
	addr instance;
	Return(instance_savecore_condition_(&instance, file));
	return error_function_(ptr, instance);
}


/* exit */
int instance_exit_condition_(addr *ret, addr value)
{
	return instance_condition1_(ret, CONSTANT_CONDITION_EXIT,
			CONSTANT_CLOSNAME_VALUE, value);
}

int call_exit_condition_(Execute ptr, addr value)
{
	addr instance;
	Return(instance_exit_condition_(&instance, value));
	return error_function_(ptr, instance);
}

int exit_condition_value_(addr instance, addr *ret)
{
	return ClosCheckConst_(instance, CLOSNAME_VALUE, ret);
}


/* simple_file_error */
int instance_simple_file_error_(addr *ret, addr pathname, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_FILE_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_PATHNAME, pathname));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_file_error_(Execute ptr, addr pathname, addr control, addr args)
{
	addr instance;
	Return(instance_simple_file_error_(&instance, pathname, control, args));
	return error_function_(ptr, instance);
}

int call_simple_file_error_va_(Execute ptr, addr pathname, const char *fmt, ...)
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


/* simple_program_error */
int instance_simple_program_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_PROGRAM_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_program_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_program_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_program_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_program_error_(ptr, control, args);
}


/* simple_package_error */
int instance_simple_package_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_PACKAGE_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_package_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_package_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_package_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_package_error_(ptr, control, args);
}


/* simple_control_error */
int instance_simple_control_error_(addr *ret, addr control, addr args)
{
	addr instance;

	GetConst(CONDITION_SIMPLE_CONTROL_ERROR, &instance);
	Return(clos_instance_heap_(instance, &instance));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_CONTROL, control));
	Return(clos_setconst_(instance, CONSTANT_CLOSNAME_FORMAT_ARGUMENTS, args));
	return Result(ret, instance);
}

int call_simple_control_error_(Execute ptr, addr control, addr args)
{
	addr instance;
	Return(instance_simple_control_error_(&instance, control, args));
	return error_function_(ptr, instance);
}

int call_simple_control_error_va_(Execute ptr, const char *fmt, ...)
{
	addr control, args;
	va_list va;

	strvect_char_heap(&control, fmt);
	va_start(va, fmt);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return call_simple_control_error_(ptr, control, args);
}

