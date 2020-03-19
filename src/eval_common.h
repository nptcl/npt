#ifndef __EVAL_COMMON_HEADER__
#define __EVAL_COMMON_HEADER__

#include "define.h"
#include "typedef.h"

_g void lambda_common(addr form, addr *ret);
_g int eval_common(Execute ptr, addr var);
_g void compiler_macro_function_common(addr var, addr env, addr *ret);
_g void setf_compiler_macro_function_common(addr value, addr var, addr env);
_g void define_compiler_macro_common(Execute ptr, addr form, addr env, addr *ret);
_g void set_define_compiler_macro(addr callname, addr value);
_g int compile_common(Execute ptr, addr var, addr opt,
		addr *ret1, addr *ret2, addr *ret3);
_g int defmacro_common(Execute ptr, addr right, addr env, addr *ret);
_g void macro_function_common(addr symbol, addr env, addr *ret);
_g int macroexpand_common(Execute ptr, addr form, addr env, addr *ret, addr *sec);
_g int macroexpand_1_common(Execute ptr, addr form, addr env, addr *ret, addr *sec);
_g int define_symbol_macro_common(addr form, addr env, addr *ret);
_g int declaim_common(Execute ptr, addr form, addr env, addr *ret);
_g int constantp_common(Execute ptr, addr var, addr opt, addr *ret);

#endif

