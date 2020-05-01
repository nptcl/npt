#ifndef __CONDITION_COMMON_HEADER__
#define __CONDITION_COMMON_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

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

_g void init_condition_common(void);

#endif

