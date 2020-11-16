#ifndef __CONDITION_HEADER__
#define __CONDITION_HEADER__

#include <stdarg.h>
#include "condition_define.h"
#include "constant.h"
#include "execute.h"

#define conditionp_ _n(conditionp_)
#define conditionp_debug _n(conditionp_debug)
#define condition_instance_p_ _n(condition_instance_p_)
#define signal_function_ _n(signal_function_)
#define error_function_ _n(error_function_)
#define warning_restart_case_ _n(warning_restart_case_)
#define callclang_error_ _n(callclang_error_)
#define callclang_warning_ _n(callclang_warning_)
#define build_condition _n(build_condition)
#define init_condition _n(init_condition)

int conditionp_(addr pos, int *ret);
int conditionp_debug(addr pos);
int condition_instance_p_(addr pos, int *ret);
int signal_function_(Execute ptr, addr condition);
int error_function_(Execute ptr, addr condition);
int warning_restart_case_(Execute ptr, addr instance);
int callclang_error_(const char *str, ...);
int callclang_warning_(const char *str, ...);
#define fmte_ callclang_error_
#define fmtw_ callclang_warning_

void build_condition(Execute ptr);
void init_condition(void);

#endif

