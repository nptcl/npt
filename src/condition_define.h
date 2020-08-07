#ifndef __CONDITION_DEFINE_HEADER__
#define __CONDITION_DEFINE_HEADER__

#include "constant.h"
#include "typedef.h"
#include "type_constant.h"

/* serious_condition */
_g void instance_serious_condition(addr *ret);
_g void serious_condition(void);

/* simple_condition */
_g void instance_simple_condition(addr *ret, addr control, addr args);
_g int simple_condition(addr control, addr args);
_g void simple_condition_format(addr condition, addr *control, addr *arguments);
_g void simple_condition_format_control(addr condition, addr *ret);
_g void simple_condition_format_arguments(addr condition, addr *ret);

/* simple_error */
_g void instance_simple_error(addr *ret, addr control, addr args);
_g void simple_error(addr control, addr args);
_g int call_simple_error_(Execute ptr, addr control, addr args);

/* error */
_g void instance_error_condition(addr *ret);
_g void error_condition(void);

/* warning */
_g void instance_warning_condition(addr *ret);
_g void warning_condition(void);

/* simple_warning */
_g void instance_simple_warning(addr *ret, addr control, addr args);
_g int simple_warning(addr control, addr args);

/* storage_condition */
_g void instance_storage_condition(addr *ret);
_g void storage_condition(void);

/* arithmetic_error */
_g void instance_arithmetic_error(addr *ret, addr operation, addr operands);
_g void arithmetic_error(addr operation, addr operands);
_g void arithmetic_error_operation(addr condition, addr *ret);
_g void arithmetic_error_operands(addr condition, addr *ret);

/* floating_point_inexact */
_g void instance_floating_point_inexact(addr *ret, addr operation, addr operands);
_g void floating_point_inexact(addr operation, addr operands);
_g void floating_point_inexact_constant(constindex index, addr operands);
_g void floating_point_inexact_stdarg(constindex index, ...);

_g int call_floating_point_inexact_(Execute ptr, addr operation, addr operands);
_g int call_floating_point_inexact_const_(Execute ptr, constindex index, addr operands);
_g int call_floating_point_inexact_va_(Execute ptr, constindex index, ...);

/* floating_point_invalid_operation */
_g void instance_floating_point_invalid_operation(addr *ret, addr operation, addr operands);
_g void floating_point_invalid_operation(addr operation, addr operands);
_g void floating_point_invalid_operation_constant(constindex index, addr operands);
_g void floating_point_invalid_operation_stdarg(constindex index, ...);

/* floating_point_overflow */
_g void instance_floating_point_overflow(addr *ret, addr operation, addr operands);
_g void floating_point_overflow(addr operation, addr operands);
_g void floating_point_overflow_constant(constindex index, addr operands);
_g void floating_point_overflow_stdarg(constindex index, ...);

/* floating_point_underflow */
_g void instance_floating_point_underflow(addr *ret, addr operation, addr operands);
_g void floating_point_underflow(addr operation, addr operands);
_g void floating_point_underflow_constant(constindex index, addr operands);
_g void floating_point_underflow_stdarg(constindex index, ...);

/* division_by_zero */
_g void instance_division_by_zero(addr *ret, addr operation, addr operands);
_g void division_by_zero(addr operation, addr operands);
_g void division_by_zero_constant(constindex index, addr operands);
_g void division_by_zero_stdarg(constindex index, ...);
_g void division_by_zero_real1(constindex index, addr left);
_g void division_by_zero_real2(constindex index, addr left, addr right);
_g void division_by_zero0(void);
_g void division_by_zero1(addr left);
_g void division_by_zero2(addr left, addr right);

_g int call_division_by_zero_(Execute ptr, addr operation, addr operands);
_g int call_division_by_zero_const_(Execute ptr, constindex index, addr operands);
_g int call_division_by_zero_real1_(Execute ptr, constindex index, addr x);
_g int call_division_by_zero_real2_(Execute ptr, constindex index, addr x, addr y);
_g int call_division_by_zero1_(Execute ptr, addr left);
_g int call_division_by_zero2_(Execute ptr, addr left, addr right);

/* cell_error */
_g void instance_cell_error(addr *ret, addr name);
_g void cell_error(addr name);
_g void cell_error_name(addr instance, addr *ret);

/* control_error */
_g void instance_control_error(addr *ret);
_g void control_error(void);
_g int call_control_error_(Execute ptr);

/* stream_error */
_g void instance_stream_error(addr *ret, addr stream);
_g void stream_error(addr stream);
_g void stream_error_stream(addr instance, addr *ret);

/* end_of_file */
_g void instance_end_of_file(addr *ret, addr stream);
_g void end_of_file(addr stream);
_g int call_end_of_file_(Execute ptr, addr stream);

/* reader_error */
_g void instance_reader_error(addr *ret, addr stream);
_g void reader_error(addr stream);

/* file_error */
_g void instance_file_error(addr *ret, addr pathname);
_g void file_error(addr pathname);
_g void file_error_pathname(addr instance, addr *ret);
_g int call_file_error_(Execute ptr, addr pathname);

/* package_error */
_g void instance_package_error(addr *ret, addr package);
_g void package_error(addr package);
_g void package_error_package(addr instance, addr *ret);

/* parse_error */
_g void instance_parse_error(addr *ret);
_g void parse_error(void);

/* print_not_readable */
_g void instance_print_not_readable(addr *ret, addr object);
_g void print_not_readable(addr object);
_g void print_not_readable_object(addr instance, addr *ret);

/* program_error */
_g void instance_program_error(addr *ret);
_g void program_error(void);

/* style_warning */
_g void instance_style_warning(addr *ret);
_g void style_warning(void);

/* type_error */
_g void instance_type_error(addr *ret, addr datum, addr expected);
_g void type_error(addr datum, addr expected);
_g void type_error_constant(addr datum, constindex expected);
#define TypeError(a,b) type_error_constant((a), CONSTANT_COMMON_##b)
_g void type_error_datum(addr instance, addr *ret);
_g void type_error_expected(addr instance, addr *ret);
_g int typep_error(Execute ptr, addr value, addr type);
_g int typep_asterisk_error(Execute ptr, addr value, addr type);
_g int typep_typetable(Execute ptr, addr value, enum TypeTable type);
#define TypepTypeTable(p,a,b) typep_typetable((p),(a),TypeTable_##b)
_g int typep_unbound_error(Execute ptr, addr value, addr type);

_g int call_type_error_(Execute ptr, addr datum, addr expected);
_g int call_type_error_const_(Execute ptr, addr datum, constindex expected);
#define TypeError_(a,b) call_type_error_const_(NULL, (a), CONSTANT_COMMON_##b)

/* simple_type_error */
_g void instance_simple_type_error(addr *ret,
		addr control, addr args, addr datum, addr expected);
_g void simple_type_error(addr control, addr args, addr datum, addr expected);
_g void type_error_stdarg(addr datum, addr expected, const char *fmt, ...);
_g void type_error_fill_pointer(addr datum);
_g void type_error_fill_pointer_zero(addr datum);
_g void type_error_adjustable(addr datum);

_g int call_simple_type_error_(Execute ptr,
		addr control, addr args, addr datum, addr expected);
_g int call_type_error_va_(Execute ptr,
		addr datum, addr expected, const char *fmt, ...);
_g int call_type_error_fill_pointer_(Execute ptr, addr datum);
_g int call_type_error_fill_pointer_zero_(Execute ptr, addr datum);
_g int call_type_error_adjustable_(Execute ptr, addr datum);

/* unbound_slot */
_g void instance_unbound_slot(addr *ret, addr instance, addr name);
_g void unbound_slot(addr argument, addr name);
_g void unbound_slot_instance(addr instance, addr *ret);
_g int call_unbound_slot_(Execute ptr, addr argument, addr name);

/* unbound_variable */
_g void instance_unbound_variable(addr *ret, addr name);
_g void unbound_variable(addr name);
_g int call_unbound_variable_(Execute ptr, addr name);

/* undefined_function */
_g void instance_undefined_function(addr *ret, addr name);
_g void undefined_function(addr name);
_g void undefined_function_setf(addr name);
_g int instance_undefined_function_(addr *ret, addr name);
_g int call_undefined_function_(Execute ptr, addr name);

/* savecore */
_g void instance_savecore_condition(addr *ret);
_g void savecore_condition(void);

/* simple_file_error */
_g void instance_simple_file_error(addr *ret, addr pathname, addr control, addr args);
_g void simple_file_error(addr pathname, addr control, addr args);
_g void simple_file_error_stdarg(addr pathname, const char *fmt, ...);
_g int call_simple_file_error_(Execute ptr, addr pathname, addr control, addr args);
_g int call_simple_file_error_va_(Execute ptr, addr pathname, const char *fmt, ...);

#endif

