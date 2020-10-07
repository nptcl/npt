#ifndef __CONDITION_DEFINE_HEADER__
#define __CONDITION_DEFINE_HEADER__

#include "constant.h"
#include "typedef.h"
#include "type_constant.h"

#define instance_serious_condition_ _n(instance_serious_condition_)
#define call_serious_condition_ _n(call_serious_condition_)

#define instance_simple_condition_ _n(instance_simple_condition_)
#define call_simple_condition_ _n(call_simple_condition_)
#define simple_condition_format_ _n(simple_condition_format_)
#define simple_condition_format_control_ _n(simple_condition_format_control_)
#define simple_condition_format_arguments_ _n(simple_condition_format_arguments_)

#define instance_simple_error_ _n(instance_simple_error_)
#define call_simple_error_ _n(call_simple_error_)

#define instance_error_condition_ _n(instance_error_condition_)
#define call_error_condition_ _n(call_error_condition_)

#define instance_warning_condition_ _n(instance_warning_condition_)
#define call_warning_condition_ _n(call_warning_condition_)

#define instance_simple_warning_ _n(instance_simple_warning_)
#define call_simple_warning_ _n(call_simple_warning_)

#define instance_storage_condition_ _n(instance_storage_condition_)
#define call_storage_condition_ _n(call_storage_condition_)

#define instance_arithmetic_error_ _n(instance_arithmetic_error_)
#define call_arithmetic_error_ _n(call_arithmetic_error_)
#define arithmetic_error_operation_ _n(arithmetic_error_operation_)
#define arithmetic_error_operands_ _n(arithmetic_error_operands_)

#define instance_float_inexact_ _n(instance_float_inexact_)
#define call_float_inexact_ _n(call_float_inexact_)
#define call_float_inexact_const_ _n(call_float_inexact_const_)
#define call_float_inexact_va_ _n(call_float_inexact_va_)

#define instance_float_invalid_ _n(instance_float_invalid_)
#define call_float_invalid_ _n(call_float_invalid_)
#define call_float_invalid_const_ _n(call_float_invalid_const_)

#define instance_float_overflow_ _n(instance_float_overflow_)
#define call_float_overflow_ _n(call_float_overflow_)
#define call_float_overflow_const_ _n(call_float_overflow_const_)
#define call_float_overflow_va_ _n(call_float_overflow_va_)

#define instance_float_underflow_ _n(instance_float_underflow_)
#define call_float_underflow_ _n(call_float_underflow_)
#define call_float_underflow_const_ _n(call_float_underflow_const_)
#define call_float_underflow_va_ _n(call_float_underflow_va_)

#define instance_division_by_zero_ _n(instance_division_by_zero_)
#define call_division_by_zero_ _n(call_division_by_zero_)
#define call_division_by_zero_const_ _n(call_division_by_zero_const_)
#define call_division_by_zero_real1_ _n(call_division_by_zero_real1_)
#define call_division_by_zero_real2_ _n(call_division_by_zero_real2_)
#define call_division_by_zero1_ _n(call_division_by_zero1_)
#define call_division_by_zero2_ _n(call_division_by_zero2_)

#define instance_cell_error_ _n(instance_cell_error_)
#define call_cell_error_ _n(call_cell_error_)
#define cell_error_name_ _n(cell_error_name_)

#define instance_control_error_ _n(instance_control_error_)
#define call_control_error_ _n(call_control_error_)

#define instance_stream_error_ _n(instance_stream_error_)
#define call_stream_error_ _n(call_stream_error_)
#define stream_error_stream_ _n(stream_error_stream_)

#define instance_end_of_file_ _n(instance_end_of_file_)
#define call_end_of_file_ _n(call_end_of_file_)

#define instance_reader_error_ _n(instance_reader_error_)
#define call_reader_error_ _n(call_reader_error_)

#define instance_file_error_ _n(instance_file_error_)
#define call_file_error_ _n(call_file_error_)
#define file_error_pathname_ _n(file_error_pathname_)

#define instance_package_error_ _n(instance_package_error_)
#define call_package_error_ _n(call_package_error_)
#define package_error_package_ _n(package_error_package_)

#define instance_parse_error_ _n(instance_parse_error_)
#define call_parse_error_ _n(call_parse_error_)

#define instance_print_not_readable_ _n(instance_print_not_readable_)
#define call_print_not_readable_ _n(call_print_not_readable_)
#define print_not_readable_object_ _n(print_not_readable_object_)

#define instance_program_error_ _n(instance_program_error_)
#define call_program_error_ _n(call_program_error_)

#define instance_style_warning_ _n(instance_style_warning_)
#define call_style_warning_ _n(call_style_warning_)

#define instance_type_error_ _n(instance_type_error_)
#define call_type_error_ _n(call_type_error_)
#define call_type_error_const_ _n(call_type_error_const_)
#define type_error_datum_ _n(type_error_datum_)
#define type_error_expected_ _n(type_error_expected_)
#define call_typep_error_ _n(call_typep_error_)
#define call_typep_asterisk_error_ _n(call_typep_asterisk_error_)
#define call_typep_unbound_error_ _n(call_typep_unbound_error_)

#define instance_simple_type_error_ _n(instance_simple_type_error_)
#define call_simple_type_error_ _n(call_simple_type_error_)
#define call_type_error_va_ _n(call_type_error_va_)
#define call_type_error_fill_pointer_ _n(call_type_error_fill_pointer_)
#define call_type_error_fill_pointer_zero_ _n(call_type_error_fill_pointer_zero_)
#define call_type_error_adjustable_ _n(call_type_error_adjustable_)

#define instance_unbound_slot_ _n(instance_unbound_slot_)
#define call_unbound_slot_ _n(call_unbound_slot_)
#define unbound_slot_instance_ _n(unbound_slot_instance_)

#define instance_unbound_variable_ _n(instance_unbound_variable_)
#define call_unbound_variable_ _n(call_unbound_variable_)

#define instance_undefined_function_ _n(instance_undefined_function_)
#define call_undefined_function_ _n(call_undefined_function_)

#define instance_savecore_condition_ _n(instance_savecore_condition_)
#define call_savecore_condition_ _n(call_savecore_condition_)

#define instance_exit_condition_ _n(instance_exit_condition_)
#define call_exit_condition_ _n(call_exit_condition_)
#define exit_condition_value_ _n(exit_condition_value_)

#define instance_simple_file_error_ _n(instance_simple_file_error_)
#define call_simple_file_error_ _n(call_simple_file_error_)
#define call_simple_file_error_va_ _n(call_simple_file_error_va_)

#define instance_simple_program_error_ _n(instance_simple_program_error_)
#define call_simple_program_error_ _n(call_simple_program_error_)
#define call_simple_program_error_va_ _n(call_simple_program_error_va_)

/* serious_condition */
_g int instance_serious_condition_(addr *ret);
_g int call_serious_condition_(Execute ptr);

/* simple_condition */
_g int instance_simple_condition_(addr *ret, addr control, addr args);
_g int call_simple_condition_(Execute ptr, addr control, addr args);
_g int simple_condition_format_(addr condition, addr *control, addr *arguments);
_g int simple_condition_format_control_(addr condition, addr *ret);
_g int simple_condition_format_arguments_(addr condition, addr *ret);

/* simple_error */
_g int instance_simple_error_(addr *ret, addr control, addr args);
_g int call_simple_error_(Execute ptr, addr control, addr args);

/* error */
_g int instance_error_condition_(addr *ret);
_g int call_error_condition_(Execute ptr);

/* warning */
_g int instance_warning_condition_(addr *ret);
_g int call_warning_condition_(Execute ptr);

/* simple_warning */
_g int instance_simple_warning_(addr *ret, addr control, addr args);
_g int call_simple_warning_(Execute ptr, addr control, addr args);

/* storage_condition */
_g int instance_storage_condition_(addr *ret);
_g int call_storage_condition_(Execute ptr);

/* arithmetic_error */
_g int instance_arithmetic_error_(addr *ret, addr pos, addr list);
_g int call_arithmetic_error_(Execute ptr, addr pos, addr list);
_g int arithmetic_error_operation_(addr condition, addr *ret);
_g int arithmetic_error_operands_(addr condition, addr *ret);

/* floating_point_inexact */
_g int instance_float_inexact_(addr *ret, addr pos, addr list);
_g int call_float_inexact_(Execute ptr, addr pos, addr list);
_g int call_float_inexact_const_(Execute ptr, constindex id, addr list);
_g int call_float_inexact_va_(Execute ptr, constindex id, ...);

/* floating_point_invalid_operation */
_g int instance_float_invalid_(addr *ret, addr pos, addr list);
_g int call_float_invalid_(Execute ptr, addr pos, addr list);
_g int call_float_invalid_const_(Execute ptr, constindex id, addr list);

/* floating_point_overflow */
_g int instance_float_overflow_(addr *ret, addr pos, addr list);
_g int call_float_overflow_(Execute ptr, addr operation, addr operands);
_g int call_float_overflow_const_(Execute ptr, constindex id, addr list);
_g int call_float_overflow_va_(Execute ptr, constindex id, ...);

/* floating_point_underflow */
_g int instance_float_underflow_(addr *ret, addr operation, addr operands);
_g int call_float_underflow_(Execute ptr, addr operation, addr operands);
_g int call_float_underflow_const_(Execute ptr, constindex id, addr list);
_g int call_float_underflow_va_(Execute ptr, constindex id, ...);

/* division_by_zero */
_g int instance_division_by_zero_(addr *ret, addr pos, addr list);
_g int call_division_by_zero_(Execute ptr, addr pos, addr list);
_g int call_division_by_zero_const_(Execute ptr, constindex id, addr list);
_g int call_division_by_zero_real1_(Execute ptr, constindex id, addr x);
_g int call_division_by_zero_real2_(Execute ptr, constindex id, addr x, addr y);
_g int call_division_by_zero1_(Execute ptr, addr left);
_g int call_division_by_zero2_(Execute ptr, addr left, addr right);

/* cell_error */
_g int instance_cell_error_(addr *ret, addr name);
_g int call_cell_error_(Execute ptr, addr name);
_g int cell_error_name_(addr instance, addr *ret);

/* control_error */
_g int instance_control_error_(addr *ret);
_g int call_control_error_(Execute ptr);

/* stream_error */
_g int instance_stream_error_(addr *ret, addr stream);
_g int call_stream_error_(Execute ptr, addr stream);
_g int stream_error_stream_(addr instance, addr *ret);

/* end_of_file */
_g int instance_end_of_file_(addr *ret, addr stream);
_g int call_end_of_file_(Execute ptr, addr stream);

/* reader_error */
_g int instance_reader_error_(addr *ret, addr stream);
_g int call_reader_error_(Execute ptr, addr stream);

/* file_error */
_g int instance_file_error_(addr *ret, addr pathname);
_g int call_file_error_(Execute ptr, addr pathname);
_g int file_error_pathname_(addr instance, addr *ret);

/* package_error */
_g int instance_package_error_(addr *ret, addr package);
_g int call_package_error_(Execute ptr, addr package);
_g int package_error_package_(addr instance, addr *ret);

/* parse_error */
_g int instance_parse_error_(addr *ret);
_g int call_parse_error_(Execute ptr);

/* print_not_readable */
_g int instance_print_not_readable_(addr *ret, addr object);
_g int call_print_not_readable_(Execute ptr, addr object);
_g int print_not_readable_object_(addr instance, addr *ret);

/* program_error */
_g int instance_program_error_(addr *ret);
_g int call_program_error_(Execute ptr);

/* style_warning */
_g int instance_style_warning_(addr *ret);
_g int call_style_warning_(Execute ptr);

/* type_error */
_g int instance_type_error_(addr *ret, addr datum, addr expected);
_g int call_type_error_(Execute ptr, addr datum, addr expected);
_g int call_type_error_const_(Execute ptr, addr datum, constindex expected);
_g int type_error_datum_(addr instance, addr *ret);
_g int type_error_expected_(addr instance, addr *ret);
#define TypeError_(a,b) call_type_error_const_(NULL, (a), CONSTANT_COMMON_##b)
_g int call_typep_error_(Execute ptr, addr value, addr type);
_g int call_typep_asterisk_error_(Execute ptr, addr value, addr type);
_g int call_typep_unbound_error_(Execute ptr, addr value, addr type);

/* simple_type_error */
_g int instance_simple_type_error_(addr *ret,
		addr control, addr args, addr datum, addr expected);
_g int call_simple_type_error_(Execute ptr,
		addr control, addr args, addr datum, addr expected);
_g int call_type_error_va_(Execute ptr,
		addr datum, addr expected, const char *fmt, ...);
_g int call_type_error_fill_pointer_(Execute ptr, addr datum);
_g int call_type_error_fill_pointer_zero_(Execute ptr, addr datum);
_g int call_type_error_adjustable_(Execute ptr, addr datum);

/* unbound_slot */
_g int instance_unbound_slot_(addr *ret, addr instance, addr name);
_g int call_unbound_slot_(Execute ptr, addr argument, addr name);
_g int unbound_slot_instance_(addr instance, addr *ret);

/* unbound_variable */
_g int instance_unbound_variable_(addr *ret, addr name);
_g int call_unbound_variable_(Execute ptr, addr name);

/* undefined_function */
_g int instance_undefined_function_(addr *ret, addr name);
_g int call_undefined_function_(Execute ptr, addr name);

/* savecore */
_g int instance_savecore_condition_(addr *ret, addr file);
_g int call_savecore_condition_(Execute ptr, addr file);

/* exit */
_g int instance_exit_condition_(addr *ret, addr value);
_g int call_exit_condition_(Execute ptr, addr value);
_g int exit_condition_value_(addr instance, addr *ret);

/* simple_file_error */
_g int instance_simple_file_error_(addr *ret, addr pathname, addr control, addr args);
_g int call_simple_file_error_(Execute ptr, addr pathname, addr control, addr args);
_g int call_simple_file_error_va_(Execute ptr, addr pathname, const char *fmt, ...);

/* simple_program_error */
_g int instance_simple_program_error_(addr *ret, addr control, addr args);
_g int call_simple_program_error_(Execute ptr, addr control, addr args);
_g int call_simple_program_error_va_(Execute ptr, const char *fmt, ...);

#endif

