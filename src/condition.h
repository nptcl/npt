#ifndef __CONDITION_HEADER__
#define __CONDITION_HEADER__

#include <stdarg.h>
#include "build.h"
#include "constant.h"
#include "execute.h"
#include "type_constant.h"

/*
 *  restart
 */
_g void restart_heap(addr *ret, addr name);
_g void getname_restart(addr pos, addr *ret);
_g void setname_restart(addr pos, addr value);
_g void getfunction_restart(addr pos, addr *ret);
_g void setfunction_restart(addr pos, addr value);
_g void getinteractive_restart(addr pos, addr *ret);
_g void setinteractive_restart(addr pos, addr value);
_g void getreport_restart(addr pos, addr *ret);
_g void setreport_restart(addr pos, addr value);
_g void gettest_restart(addr pos, addr *ret);
_g void settest_restart(addr pos, addr value);
_g void getcondition_restart(addr pos, addr *ret);
_g void setcondition_restart(addr pos, addr value);
_g void getreference_restart(addr pos, addr *ret);
_g void setreference_restart(addr pos, addr value);
_g void setescape_restart(addr pos, int value);
_g int getescape_restart(addr pos);
_g void setenable_restart(addr pos, int value);
_g int getenable_restart(addr pos);
_g void setredirect_restart(addr pos, int value);
_g int getredirect_restart(addr pos);


/*
 *  restart code
 */
_g void function_global_restart(Execute ptr, addr symbol, addr *ret);
_g void function_local_restart(Execute ptr, addr symbol, addr *ret);
_g void setf_global_restart(Execute ptr, addr symbol, addr *ret);
_g void setf_local_restart(Execute ptr, addr symbol, addr *ret);
_g void value_global_restart(Execute ptr, addr symbol, addr *ret);
_g void value_lexical_restart(Execute ptr, addr symbol, addr *ret);
_g void value_special_restart(Execute ptr, addr symbol, addr *ret);


/*
 *  handler warning
 */
_g void handler_warning(Execute ptr);
_g void handler_savecore(Execute ptr);


/*
 *  debugger
 */
_g int invoke_debugger(Execute ptr, addr condition);
_g void set_enable_debugger(int value);


/*
 *  default condition
 */
_g int conditionp(addr pos);
_g int condition_instance_p(addr pos);
_g int signal_function(addr condition);
_g int error_common(Execute ptr, addr condition);
_g void error_function(addr condition);
_g void format_error(const char *str, ...);
#define fmte format_error
_g int warning_restart_case(Execute ptr, addr instance);
_g int signal_warning(const char *str, ...);
_g void format_warning(const char *str, ...);
#define fmtw format_warning

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
/* cell_error */
_g void instance_cell_error(addr *ret, addr name);
_g void cell_error(addr name);
_g void cell_error_name(addr instance, addr *ret);
/* control_error */
_g void instance_control_error(addr *ret);
_g void control_error(void);
/* stream_error */
_g void instance_stream_error(addr *ret, addr stream);
_g void stream_error(addr stream);
_g void stream_error_stream(addr instance, addr *ret);
/* end_of_file */
_g void instance_end_of_file(addr *ret, addr stream);
_g void end_of_file(addr stream);
/* reader_error */
_g void instance_reader_error(addr *ret, addr stream);
_g void reader_error(addr stream);
/* file_error */
_g void instance_file_error(addr *ret, addr pathname);
_g void file_error(addr pathname);
_g void file_error_pathname(addr instance, addr *ret);
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
_g int typep_error(addr value, addr type);
_g int typep_asterisk_error(addr value, addr type);
_g int typep_typetable(addr value, enum TypeTable type);
#define TypepTypeTable(a,b) typep_typetable((a),TypeTable_##b)
/* simple_type_error */
_g void instance_simple_type_error(addr *ret,
		addr control, addr args, addr datum, addr expected);
_g void simple_type_error(addr control, addr args, addr datum, addr expected);
_g void type_error_stdarg(addr datum, addr expected, const char *fmt, ...);
_g void type_error_fill_pointer(addr datum);
_g void type_error_fill_pointer_zero(addr datum);
_g void type_error_adjustable(addr datum);
/* unbound_slot */
_g void instance_unbound_slot(addr *ret, addr instance, addr name);
_g void unbound_slot(addr argument, addr name);
_g void unbound_slot_instance(addr instance, addr *ret);
/* unbound_variable */
_g void instance_unbound_variable(addr *ret, addr name);
_g void unbound_variable(addr name);
/* undefined_function */
_g void instance_undefined_function(addr *ret, addr name);
_g void undefined_function(addr name);
_g void undefined_function_setf(addr name);
/* savecore */
_g void instance_savecore_condition(addr *ret);
_g void savecore_condition(void);

/* build */
_g void build_condition(Execute ptr);
_g void init_condition(void);

#endif

