#ifndef __CALL_CONDITIONS_HEADER__
#define __CALL_CONDITIONS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define assert_common_ _n(assert_common_)
#define error_common_ _n(error_common_)
#define cerror_common_ _n(cerror_common_)
#define check_type_common_ _n(check_type_common_)
#define invalid_method_error_common_ _n(invalid_method_error_common_)
#define method_combination_error_common_ _n(method_combination_error_common_)
#define signal_common_ _n(signal_common_)
#define warn_common_ _n(warn_common_)
#define break_common_ _n(break_common_)
#define handler_bind_common_ _n(handler_bind_common_)
#define handler_case_common_ _n(handler_case_common_)
#define ignore_errors_common_ _n(ignore_errors_common_)
#define make_condition_common_ _n(make_condition_common_)
#define compute_restarts_common_ _n(compute_restarts_common_)
#define find_restart_common_ _n(find_restart_common_)
#define restart_bind_common_ _n(restart_bind_common_)
#define restart_case_common_ _n(restart_case_common_)
#define with_condition_restarts_common_ _n(with_condition_restarts_common_)
#define with_simple_restart_common_ _n(with_simple_restart_common_)
#define abort_common_ _n(abort_common_)
#define continue_common_ _n(continue_common_)
#define muffle_warning_common_ _n(muffle_warning_common_)
#define store_value_common_ _n(store_value_common_)
#define use_value_common_ _n(use_value_common_)

int assert_common_(Execute ptr, addr form, addr env, addr *ret);
int error_common_(Execute ptr, addr datum, addr rest);
int cerror_common_(Execute ptr, addr restart, addr datum, addr rest);
int check_type_common_(Execute ptr, addr form, addr env, addr *ret);
int invalid_method_error_common_(Execute ptr, addr method, addr format, addr args);
int method_combination_error_common_(Execute ptr, addr format, addr args);
int signal_common_(Execute ptr, addr datum, addr rest);
int warn_common_(Execute ptr, addr datum, addr rest);
int break_common_(Execute ptr, addr format, addr args);
int handler_bind_common_(addr rest, addr env, addr *ret);
int handler_case_common_(Execute ptr, addr right, addr env, addr *ret);
int ignore_errors_common_(Execute ptr, addr form, addr env, addr *ret);
int make_condition_common_(Execute ptr, addr args, addr *ret);
int compute_restarts_common_(Execute ptr, addr pos, addr *ret);
int find_restart_common_(Execute ptr, addr var, addr opt, addr *ret);
int restart_bind_common_(addr right, addr env, addr *ret);
int restart_case_common_(Execute ptr, addr form, addr env, addr *ret);
int with_condition_restarts_common_(addr form, addr *ret);
int with_simple_restart_common_(addr form, addr env, addr *ret);
int abort_common_(Execute ptr, addr opt);
int continue_common_(Execute ptr, addr opt);
int muffle_warning_common_(Execute ptr, addr opt);
int store_value_common_(Execute ptr, addr var, addr opt);
int use_value_common_(Execute ptr, addr var, addr opt);

#endif

