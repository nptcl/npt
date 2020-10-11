#ifndef __CALL_CONDITIONS_HEADER__
#define __CALL_CONDITIONS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define assert_common _n(assert_common)
#define error_common _n(error_common)
#define cerror_common _n(cerror_common)
#define check_type_common _n(check_type_common)
#define invalid_method_error_common _n(invalid_method_error_common)
#define method_combination_error_common _n(method_combination_error_common)
#define signal_common _n(signal_common)
#define warn_common _n(warn_common)
#define break_common _n(break_common)
#define handler_bind_common _n(handler_bind_common)
#define handler_case_common _n(handler_case_common)
#define ignore_errors_common _n(ignore_errors_common)
#define make_condition_common _n(make_condition_common)
#define compute_restarts_common_ _n(compute_restarts_common_)
#define find_restart_common_ _n(find_restart_common_)
#define restart_bind_common _n(restart_bind_common)
#define restart_case_common _n(restart_case_common)
#define with_condition_restarts_common _n(with_condition_restarts_common)
#define with_simple_restart_common _n(with_simple_restart_common)
#define abort_common _n(abort_common)
#define continue_common _n(continue_common)
#define muffle_warning_common _n(muffle_warning_common)
#define store_value_common _n(store_value_common)
#define use_value_common _n(use_value_common)

_g int assert_common(Execute ptr, addr form, addr env, addr *ret);
_g int error_common(Execute ptr, addr datum, addr rest);
_g int cerror_common(Execute ptr, addr restart, addr datum, addr rest);
_g int check_type_common(Execute ptr, addr form, addr env, addr *ret);
_g int invalid_method_error_common(Execute ptr, addr method, addr format, addr args);
_g int method_combination_error_common(Execute ptr, addr format, addr args);
_g int signal_common(Execute ptr, addr datum, addr rest);
_g int warn_common(Execute ptr, addr datum, addr rest);
_g int break_common(Execute ptr, addr format, addr args);
_g int handler_bind_common(addr rest, addr env, addr *ret);
_g int handler_case_common(Execute ptr, addr right, addr env, addr *ret);
_g int ignore_errors_common(Execute ptr, addr form, addr env, addr *ret);
_g int make_condition_common(Execute ptr, addr args, addr *ret);
_g int compute_restarts_common_(Execute ptr, addr pos, addr *ret);
_g int find_restart_common_(Execute ptr, addr var, addr opt, addr *ret);
_g int restart_bind_common(addr right, addr env, addr *ret);
_g int restart_case_common(addr right, addr env, addr *ret);
_g int with_condition_restarts_common(addr right, addr env, addr *ret);
_g int with_simple_restart_common(addr form, addr env, addr *ret);
_g int abort_common(Execute ptr, addr opt);
_g int continue_common(Execute ptr, addr opt);
_g int muffle_warning_common(Execute ptr, addr opt);
_g int store_value_common(Execute ptr, addr var, addr opt);
_g int use_value_common(Execute ptr, addr var, addr opt);

#endif

