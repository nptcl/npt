#ifndef __CONDITION_HEADER__
#define __CONDITION_HEADER__

#include <stdarg.h>
#include "condition_define.h"
#include "constant.h"
#include "execute.h"

_g int conditionp(addr pos);
_g int condition_instance_p(addr pos);
_g int signal_function_(Execute ptr, addr condition);
_g int error_function_(Execute ptr, addr condition);
_g int warning_restart_case_(Execute ptr, addr instance);
_g int callclang_error_(const char *str, ...);
_g int callclang_warning_(const char *str, ...);
#define fmte_ callclang_error_
#define fmtw_ callclang_warning_

_g void build_condition(Execute ptr);
_g void init_condition(void);

/* deprecated */
_g void error_function(addr condition);
_g void callclang_error(const char *str, ...);
#define fmte callclang_error

#endif

