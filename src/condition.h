#ifndef __CONDITION_HEADER__
#define __CONDITION_HEADER__

#include <stdarg.h>
#include "constant.h"
#include "execute.h"
#include "lisp.h"

/*
 *  restart
 */
void restart_heap(addr *ret, addr name);
void getname_restart(addr pos, addr *ret);
void setname_restart(addr pos, addr value);
void getfunction_restart(addr pos, addr *ret);
void setfunction_restart(addr pos, addr value);
void getinteractive_restart(addr pos, addr *ret);
void setinteractive_restart(addr pos, addr value);
void getreport_restart(addr pos, addr *ret);
void setreport_restart(addr pos, addr value);
void gettest_restart(addr pos, addr *ret);
void settest_restart(addr pos, addr value);
void getcondition_restart(addr pos, addr *ret);
void setcondition_restart(addr pos, addr value);
void getreference_restart(addr pos, addr *ret);
void setreference_restart(addr pos, addr value);
void setescape_restart(addr pos, int value);
int getescape_restart(addr pos);
void setenable_restart(addr pos, int value);
int getenable_restart(addr pos);
void setredirect_restart(addr pos, int value);
int getredirect_restart(addr pos);


/*
 *  restart code
 */
void function_global_restart(Execute ptr, addr symbol, addr *ret);
void function_local_restart(Execute ptr, addr symbol, addr *ret);
void setf_global_restart(Execute ptr, addr symbol, addr *ret);
void setf_local_restart(Execute ptr, addr symbol, addr *ret);
void value_global_restart(Execute ptr, addr symbol, addr *ret);
void value_lexical_restart(Execute ptr, addr symbol, addr *ret);
void value_special_restart(Execute ptr, addr symbol, addr *ret);


/*
 *  handler warning
 */
void handler_warning(Execute ptr);


/*
 *  debugger
 */
int invoke_debugger(addr condition);
void set_enable_debugger(int value);
void set_enable_interactive(int value);
//void invoke_restart(Execute ptr, addr restart, addr args);


/*
 *  default condition
 */
int conditionp(addr pos);
int condition_instance_p(addr pos);
int signal_function(addr condition);
void error_function(addr condition);
void format_error(const char *str, ...);
#define fmte format_error
void format_warning(const char *str, ...);
#define fmtw format_warning

/* serious_condition */
void instance_serious_condition(addr *ret);
void serious_condition(void);
/* simple_condition */
void instance_simple_condition(addr *ret, addr control, addr args);
void simple_condition(addr control, addr args);
void simple_condition_format(addr condition, addr *control, addr *arguments);
void simple_condition_format_control(addr condition, addr *ret);
void simple_condition_format_arguments(addr condition, addr *ret);
/* simple_error */
void instance_simple_error(addr *ret, addr control, addr args);
void simple_error(addr control, addr args);
/* error */
void instance_error_condition(addr *ret);
void error_condition(void);
/* warning */
void instance_warning_condition(addr *ret);
void warning_condition(void);
/* simple_warning */
void instance_simple_warning(addr *ret, addr control, addr args);
void simple_warning(addr control, addr args);
/* storage_condition */
void instance_storage_condition(addr *ret);
void storage_condition(void);
/* arithmetic_error */
void instance_arithmetic_error(addr *ret, addr operation, addr operands);
void arithmetic_error(addr operation, addr operands);
void arithmetic_error_operation(addr condition, addr *ret);
void arithmetic_error_operands(addr condition, addr *ret);
/* floating_point_inexact */
void instance_floating_point_inexact(addr *ret, addr operation, addr operands);
void floating_point_inexact(addr operation, addr operands);
void floating_point_inexact_constant(constindex index, addr operands);
void floating_point_inexact_stdarg(constindex index, ...);
/* floating_point_invalid_operation */
void instance_floating_point_invalid_operation(addr *ret, addr operation, addr operands);
void floating_point_invalid_operation(addr operation, addr operands);
void floating_point_invalid_operation_constant(constindex index, addr operands);
void floating_point_invalid_operation_stdarg(constindex index, ...);
/* floating_point_overflow */
void instance_floating_point_overflow(addr *ret, addr operation, addr operands);
void floating_point_overflow(addr operation, addr operands);
void floating_point_overflow_constant(constindex index, addr operands);
void floating_point_overflow_stdarg(constindex index, ...);
/* floating_point_underflow */
void instance_floating_point_underflow(addr *ret, addr operation, addr operands);
void floating_point_underflow(addr operation, addr operands);
void floating_point_underflow_constant(constindex index, addr operands);
void floating_point_underflow_stdarg(constindex index, ...);
/* division_by_zero */
void instance_division_by_zero(addr *ret, addr operation, addr operands);
void division_by_zero(addr operation, addr operands);
void division_by_zero_constant(constindex index, addr operands);
void division_by_zero_stdarg(constindex index, ...);
void division_by_zero_real1(constindex index, addr left);
void division_by_zero_real2(constindex index, addr left, addr right);
void division_by_zero0(void);
void division_by_zero1(addr left);
void division_by_zero2(addr left, addr right);
/* cell_error */
void instance_cell_error(addr *ret, addr name);
void cell_error(addr name);
void cell_error_name(addr instance, addr *ret);
/* control_error */
void instance_control_error(addr *ret);
void control_error(void);
/* stream_error */
void instance_stream_error(addr *ret, addr stream);
void stream_error(addr stream);
void stream_error_stream(addr instance, addr *ret);
/* end_of_file */
void instance_end_of_file(addr *ret, addr stream);
void end_of_file(addr stream);
/* reader_error */
void instance_reader_error(addr *ret, addr stream);
void reader_error(addr stream);
/* file_error */
void instance_file_error(addr *ret, addr pathname);
void file_error(addr pathname);
void file_error_pathname(addr *ret, addr instance);
/* package_error */
void instance_package_error(addr *ret, addr package);
void package_error(addr package);
void package_error_package(addr *ret, addr instance);
/* parse_error */
void instance_parse_error(addr *ret);
void parse_error(void);
/* print_not_readable */
void instance_print_not_readable(addr *ret, addr object);
void print_not_readable(addr object);
/* program_error */
void instance_program_error(addr *ret);
void program_error(void);
/* style_warning */
void instance_style_warning(addr *ret);
void style_warning(void);
/* type_error */
void instance_type_error(addr *ret, addr datum, addr expected);
void type_error(addr datum, addr expected);
void type_error_constant(addr datum, constindex expected);
#define TypeError(a,b) type_error_constant((a), CONSTANT_COMMON_##b)
void type_error_datum(addr instance, addr *ret);
void type_error_expected(addr instance, addr *ret);
int typep_error(addr value, addr type);
int typep_asterisk_error(addr value, addr type);
/* simple_type_error */
void instance_simple_type_error(addr *ret,
		addr control, addr args, addr datum, addr expected);
void simple_type_error(addr control, addr args, addr datum, addr expected);
void type_error_stdarg(addr datum, addr expected, const char *fmt, ...);
void type_error_fill_pointer(addr datum);
void type_error_fill_pointer_zero(addr datum);
void type_error_adjustable(addr datum);
/* unbound_slot */
void instance_unbound_slot(addr *ret, addr instance, addr name);
void unbound_slot(addr argument, addr name);
/* unbound_variable */
void instance_unbound_variable(addr *ret, addr name);
void unbound_variable(addr name);
/* undefined_function */
void instance_undefined_function(addr *ret, addr name);
void undefined_function(addr name);
void undefined_function_setf(addr name);

/* build */
void build_condition(Execute ptr);
void init_condition(void);

#endif

