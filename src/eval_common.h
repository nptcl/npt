#ifndef __EVAL_COMMON_HEADER__
#define __EVAL_COMMON_HEADER__

#include "define.h"
#include "typedef.h"

_g void compiler_macro_function_common(addr var, addr env, addr *ret);
_g void setf_compiler_macro_function_common(addr value, addr var, addr env);
_g void define_compiler_macro_common(Execute ptr, addr form, addr env, addr *ret);
_g void set_define_compiler_macro(addr callname, addr value);
_g int compile_common(Execute ptr, addr var, addr opt,
		addr *ret1, addr *ret2, addr *ret3);

#endif

