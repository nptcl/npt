#ifndef __CONDITION_DEFINE_HEADER__
#define __CONDITION_DEFINE_HEADER__

#include "constant.h"
#include "typedef.h"
#include "type_constant.h"

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

#endif

